$(document).ready(function() {
	$.getJSON('buf', function(resp) {
		render(resp);

		setSize();
		$(window).resize(function() {
			waitForFinalEvent(setSize, 500, "resize window");
		});
	});
});

//https://stackoverflow.com/questions/2854407/javascript-jquery-window-resize-how-to-fire-after-the-resize-is-completed/4541963#4541963
var waitForFinalEvent = (function () {
	var timers = {};
	return function (callback, ms, uniqueId) {
		if (!uniqueId) {
			uniqueId = "Don't call this twice without a uniqueId";
		}
		if (timers[uniqueId]) {
			clearTimeout (timers[uniqueId]);
		}
		timers[uniqueId] = setTimeout(callback, ms);
	};
})();
		
function setSize() {
	var sw = $('.buffer')[0].offsetHeight;
	var w = Math.floor($('.lines')[0].offsetWidth/9.57);
	var h = Math.floor(sw/21);
	$.getJSON('resize/'+w+'/'+h);
	
	$('.lines').css('padding-bottom', sw-21); //scroll beyond last line, leave at least one line
}

function isChinese(c) {
	return /[\ufe30-\uffa0\u4e00-\u9eff\u3000-\u303F]/.test(c);
}

//TODO use regexp
function textWidth(txt, a, b) {
	var x = 0;
	var vx = a;
	for (var i = a; i < b; i++) {
		if (txt[i] == '\t') {
			x += (4-vx%4)*9.57;
			vx += (4-vx%4);
		} else if (isChinese(txt[i])) {
			x+=16;
			vx++;
		} else {
			x+=9.57;
			vx++;
		}
	}
	return x;
}

function replaceBinding(html, data, ifEncode) {
	for (var p in data) {
		if (data.hasOwnProperty(p)) {
			var v = data[p];
			html = html.replace(new RegExp('{' + p + '}', 'g'), v);
		}
	}
	return html;
}

var lineTemplate = '<div id="line-{row}" class="line"><pre>{line}</pre></div>';
var gutterLineTemplate = '<div id="line-num-{row}" class="line-num">{incrow}</div>';

function lineid(i) {
	return '#line-'+i;
}

//local states for each buffer, doesn't contains text content since it might be too large.
var buffers = {};

//TODO: Future improvements: 
//1. put syntax highlight to a dedicate web worker (or just use setTimeout)
//2. only render “visible” parts and still make scrolling fast
//3. render difflines into an offline DOM object first
function render(buf) {
	if (!buf) return;
	
	var hl;

	//render lines
	if (buf.lines) {
		hl = newHighlight(buf.lang);
		buffers[buf.id] = { hl: hl };

		var css = $('.lines').remove().attr('style');
		var $lines = document.createElement('DIV');
		$lines.className = 'lines';
		$lines.style.cssText = css;

		$('.gutter').remove();
		var $gutter = document.createElement('DIV');
		$gutter.className = 'gutter';

		var lines = buf.lines;
		hl.states = new Array(lines.length+1);
		lines.each(function(line, i) {
			if (line) {
				$lines.appendChild(renderLine(i, hl.parseLine(line, i)));

				var g = document.createElement('DIV');
				g.id = 'line-num-'+i;
				g.className = 'line-num';
				g.textContent = i+1;
				$gutter.appendChild(g);
			}
		});

		$('.buffer').append($gutter);
		$('.buffer').append($lines);
	}

	var $gutter = $('.gutter');
	hl = hl || buffers[buf.id].hl;

	if (buf.difflines) {
		var diff = buf.difflines;
		var row = diff.row;
		var rowstate = hl.states[row];
		var add = diff.add;
		if (add && add.length > 0) {
			if (add[add.length-1] == '') {
				add.pop();
			}
		}

		var sub = diff.sub;
		var oldcnt = parseInt($gutter.find(':last-child').text());
		var newcnt = oldcnt+add.length-sub;

		//update gutter
		if (newcnt > oldcnt) {
			for (var i = oldcnt; i < newcnt; i++) {
				$gutter.append(replaceBinding(gutterLineTemplate, {row:i,incrow:i+1}));
			}
		} else if (newcnt < oldcnt) {
			for (var i = 0; i < oldcnt-newcnt; i++) {
				$gutter.find(':last-child').remove();
			}
		}


		//remove sub
		for (var i = row+sub-1; i >= row; i--) {
			$(lineid(i)).remove();

			hl.states.splice(i, 1);
		}

		var shiftcnt = -sub+add.length;
		if (shiftcnt < 0) {
			//shift left
			for (var i = row+sub; i < oldcnt; i++) {
				$(lineid(i)).attr('id', 'line-'+(i+shiftcnt));
			}
		} else if (shiftcnt > 0) {
			//shift right
			for (var i = oldcnt; i >= row+sub; i--) {
				$(lineid(i)).attr('id', 'line-'+(i+shiftcnt));
			}
		}

		//insert add
		for (var i = 0; i < add.length; i++) {
			hl.states.splice(row, 0, []);
		}

		//recover row state
		hl.states[row] = rowstate;
		for (var i = row,len=row+add.length; i < len; i++) {
			var line = renderLine(i, hl.parseLine(add[i-row], i));
			if (i > 0) {
				$(line).insertAfter($(lineid(i-1)));
			} else {
				$('.lines').prepend(line);
			}
		}

		//continue parse until equal state or EOF
		hl.refreshLines(row+add.length, newcnt, function(row, items) {
			var pre = renderLineInner(items);
			var line = document.getElementById('line-'+row);
			line.replaceChild(pre, line.firstChild);
		});

		for (var i = newcnt; i < oldcnt; i++) {
			$(lineid(i)).remove();
		}
	} 

	//emtpy file has 1 line too
	if ($gutter[0].firstChild==null) { 
		$gutter.append(replaceBinding(gutterLineTemplate, {row:0,incrow:1}));
	}


	if (typeof buf.cursor != 'undefined') {
		renderCursor(buf);
	}
	
	//render ex
	if (buf.ex && buf.ex.length > 0) {
		$('.status-bar .ex').empty().text(buf.ex);
		$('.status-bar .ex').append('<span class="cursor"> </span>');
	} else if (buf.message) {
		$('.status-bar .ex').empty().text(buf.message);
	} else {
		if (typeof buf.mode != 'undefined' && buf.mode < MODES.length) {
			$('.status-bar .ex').empty().text(MODES[buf.mode]);
			keymap = keymaps[buf.mode];
		}
	}

	//render unsaved
	$('.status-bar .buf-name').toggleClass('buf-unsaved', !!buf.unsaved);

	//render ongoing keys
	if (typeof buf.keys != 'undefined') {
		if (buf.keys && buf.keys.length > 0) {
			var keysstr = '';
			for (var i = 0; i < buf.keys.length; i++) {
				var k = buf.keys[i];
				if (k.length > 1) k = '<'+k+'>';
				keysstr += k;
			}
			$('.status-bar .ongoing-keys').text(keysstr);
		} else {
			$('.status-bar .ongoing-keys').empty();
		}
	}

	//render visual
	$('.lines .selections').empty();
	if (buf.visual) {
		if (!$('.lines .selections').get(0)) {
			$('.lines').append('<div class="selections"></div>');
		}
		renderSelections($('.lines .selections'), buf.visual.ranges)
	}

	//render hlsearch
	$('.lines .highlights').empty();
	if (buf.highlights) {
		if (!$('.lines .highlights').get(0)) {
			$('.lines').append('<div class="highlights"></div>');
		}
		renderSelections($('.lines .highlights'), buf.highlights, true)
	}

	//render matched brace pair
	$('.lines .highlight-brace-pair').empty();
	if (buf.braces) {
		if (!$('.lines .highlight-brace-pair').get(0)) {
			$('.lines').append('<div class="highlight-brace-pair"></div>');
		}
		var cr, cc;
		if (buf.cursor) {
			cr = parseInt(buf.cursor.row);
			cc = parseInt(buf.cursor.col);
		}

		var ranges = [];
		for (var i = 0; i < buf.braces.length; i++) {
			var pt = buf.braces[i];
			//skip cursor, don't draw twice in same point
			if (buf.cursor && pt.row == cr && pt.col == cc) continue; 

			ranges.push(pt, {row:pt.row, col:pt.col+1});
		}
		renderSelections($('.lines .highlight-brace-pair'), ranges);
	}

	scrollToCursor(buf['scroll-top'] || 0, buf.lines);

	//render autocompl suggestions
	if (buf.autocompl && buf.autocompl.suggestions && buf.autocompl.suggestions.length > 1) {
		var lastSelectedIndex = 0;
		var lastScrollTop = 0;
		var $autocompl = $('.lines .autocompl');
		if (!$autocompl[0]) {
			$('.lines').append('<div class="autocompl"></div>');
			$autocompl = $('.lines .autocompl');
		} else if ($('.lines .autocompl .highlight')[0]) {
			lastSelectedIndex = parseInt($('.lines .autocompl .highlight').attr('id').split('-')[1]);
			lastScrollTop = $autocompl.scrollTop();
		}

		var selectedIndex = parseInt(buf.autocompl['suggestions-index']);
		var currentWord = buf.autocompl.suggestions[selectedIndex];
		$autocompl.empty().css('left', x-currentWord.length*9.57-10+'px')
		$(buf.autocompl.suggestions).each(function(i, word) {
			if (i > 0) {
				var ele = $('<pre id="suggestion-'+i+'"></pre>').text(word)
					.appendTo('.lines .autocompl');
				if (i == selectedIndex) {
					ele.addClass('highlight');
				}
			}
		});

		if (y+21+$autocompl.height()-$('.lines').scrollTop() < $('.lines').height()) {
			$autocompl.css('top', y+21+'px');
		} else {
			$autocompl.css('top', y-$autocompl.height()+'px');
		}

		var viewportTop = lastScrollTop;
		var viewportBottom = lastScrollTop+240;
		var currentPos = (selectedIndex-1) * 24;
		if (currentPos < viewportTop) {
			$autocompl.scrollTop(currentPos);
		} else if (currentPos+24 >= viewportBottom) {
			$autocompl.scrollTop(currentPos-216);
		}
	} else {
		$('.lines .autocompl').empty();
	}

	//title
	if (buf.name) {
		document.title = buf.name;
		$('.status-bar .buf-name').text(buf.name);
	}
}

function renderLineInner(items) {
	var pre = document.createElement('PRE');
	items.each(function(item){
		var className = item[0];
		var text = item[1];
		var node;
		if (className) {
			node = document.createElement('SPAN');
			node.className = className;
			node.textContent = text;
		} else {
			node = document.createTextNode(text);
		}
		pre.appendChild(node);
	});
	return pre;
}

function renderLine(row, items) {
	var line = document.createElement('DIV');
	line.id = 'line-'+row;
	line.appendChild(renderLineInner(items));
	return line;
}

var aligntop = true;
function scrollToCursor(scrollTopRow, instant) {
	var el = $('.cursor')[0];
	var $lines = $('.lines');
	var $buf = $('.buffer');
    
	var scrleft = $lines.scrollLeft();
	var width = $lines.width();

	var oldst = $buf.scrollTop();
	var newst = scrollTopRow * 21;
	if (!instant && Math.abs(oldst - newst) > 3*21) {
		$buf.animate({ scrollTop: newst}, 60);
	} else {
		$buf.scrollTop(newst);
	}

	if (el.offsetLeft+el.offsetWidth > scrleft+width) {
		$lines.scrollLeft(el.offsetLeft+el.offsetWidth-width);
	} else if (el.offsetLeft < scrleft) {
		$lines.scrollLeft(el.offsetLeft);
	}
}

function renderSelections($p, ranges, reverseTextColor) {
	for (var i = 0; i < ranges.length; i+=2) {
		var s = ranges[i];
		var e = ranges[i+1];
		renderSelection($p, s, e, reverseTextColor);
	}
}

//if reverseTextColor==true copy text from lines append to line-selected
function renderSelection($p, s, e, reverseTextColor) {
	//sort
	if (s.row > e.row || (s.row == e.row && s.col > e.col)) {
		var t = s;
		s = e;
		e = t;
	}

	for (var i = s.row; i <= e.row; i++) {
		var line = $('<div class="line-selected"></div>');
		var w = 0;
		var l = 0;
		var cline = $('#line-'+i)[0].textContent;

		if (i == s.row) {
			w = textWidth(cline, s.col, (e.row==i)?e.col:cline.length);
			l = textWidth(cline, 0, s.col);
		} else if (i == e.row) {
			w = textWidth(cline, 0, e.col);
			l = 0;
		} else if (cline.length==0) {
			//empty line always contains '\n'
			w = 9.57;
			l = 0;
		} else {
			w = textWidth(cline, 0, cline.length);
			l = 0;
		}

		line.css('left', l+'px')
			.css('top', (i*21+1)+'px')
			.css('width', w+'px');

		if (reverseTextColor) {
			var txt;
			if (i == s.row) {
				txt = cline.substring(s.col, (e.row==i)?e.col:cline.length);
			} else if (i == e.row) {
				txt = cline.substring(0, e.col);
			} else if (cline.length==0) {
				txt = '\n';
			} else {
				txt = cline;
			}
			line.append(txt);
		}

		$p.append(line);
	}
}

function renderCursor(buf) {
	if (!$('.lines .cursor').get(0)) {
		$('.lines').append('<div class="cursor"></div>');
	}

	var cr = parseInt(buf.cursor.row);
	var cc = parseInt(buf.cursor.col);
	if (document.getElementById('line-0') == null) {
		x = 0, y = 0, w = 9.57;
	} else {
		var cline = $('#line-'+cr)[0].textContent;

		var x = textWidth(cline, 0, cc);
		var y = cr*21+1;
		var w = 9.57;
		if (isChinese(cline[cc])) {
			w = 16;
		}
	}
	$('.cursor').attr('style', 'left:'+x+'px;top:'+y+'px;width:'+w+'px;');
}

var MODES = ['-- NORMAL --', '-- INSERT --', '-- VISUAL --'];

