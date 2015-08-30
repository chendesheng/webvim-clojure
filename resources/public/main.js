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

String.prototype.eachLine = function(fn) {
	var str = this;
	var num = 0;
	while(true) {
		var i = str.indexOf('\n')+1;
		if (i == 0) {
			fn(str, num);
			break;
		} else {
			fn(str.substring(0, i), num);
			str = str.substring(i);
		}
		num++;
	}
};

String.prototype.eachBlock = function(fn, n) {
	var str = this;
	var num = 0;
	while(true) {
		var i = str.substring(n).search(/\s/)+1;

		if (i==0) {
			fn(str, num);
			break;
		} else {
			i += n;
			fn(str.substring(0, i), num);
			str = str.substring(i);
		}
		num++;
	}
};

//local states for each buffer, doesn't contains text content since it might be too large.
var buffers = {};

function outOfRange(ele) {
	return ele == null || !/\bcode\b/.test(ele.className);
}

function getElementByPos(buf, pos) {
	var i = buf.pos;
	var ele = buf.currentBlock;
	var num = buf.currentBlockNumber;
	if (i <= pos) {
		while (!outOfRange(ele)) {
			var j = i + ele.textContent.length;
			if (i <= pos && pos < j) {
				buf.currentBlock = ele;
				buf.pos = i;
				buf.currentBlockNumber = num;
				return {e: ele, pos: i, num: num}
			}

			i = j;
			num++;
			ele = ele.nextSibling;
		}
	} else {
		while (true) {
			num--;
			ele = ele.previousSibling;
			if (outOfRange(ele)) {
				break;
			}

			var j = i - ele.textContent.length;

			if (j <= pos && pos < i) {
				buf.currentBlock = ele;
				buf.pos = j;
				buf.currentBlockNumber = num;
				return {e: ele, pos: j, num: num}
			}

			i = j;
		}
	}

	return null;
}

function getLineByPos(buf, pos) {
	var res = getElementByPos(buf, pos);
	var ele = res.e;
	var elepos = res.pos;
	var txt = ele.textContent.substring(0, pos-elepos);
	var line = ''
	while(true) {
		var br = txt.indexOf('\n');
		line = txt.substring(br+1) + line;
		if (br == -1) {
			break;
		}
		ele = ele.previousSibling;
		if (ele == null) break;
		txt = ele.textContent;
	}

	return line;
}

function refreshIter(index, currentBlock, states, parentNode) {
	var i = index;
	var ele = currentBlock;
	return {
		text: function() {
			return ele.textContent;
		},
		index: function() {
			return i;
		},
		render: function(items) {
			var newele = renderLine(items);
			parentNode.replaceChild(newele, ele);
			ele = newele;
		},
		next: function() {
			ele = ele.nextSibling;
			i++;
			return !outOfRange(ele);
		}
	}
}

//TODO: Future improvements: 
//1. put syntax highlight to a dedicate web worker (or just use setTimeout)
//2. only render “visible” parts and still make scrolling fast
//3. render difflines into an offline DOM object first
function render(buf) {
	if (!buf) return;
	
	var hl;

	//render lines
	if (typeof buf.str != 'undefined') {
		hl = newHighlight(buf.lang);
		buffers[buf.id] = { hl: hl };

		var css = $('.lines').remove().attr('style');
		var $lines = document.createElement('DIV');
		$lines.className = 'lines';
		$lines.style.cssText = css;

		$('.gutter').remove();
		var $gutter = document.createElement('DIV');
		$gutter.className = 'gutter';

		hl.states = [];
		buf.str.eachBlock(function(line, i) {
			hl.states.push(null);
			if (line) {
				$lines.appendChild(renderLine(hl.parseLine(line, i)));

				var g = document.createElement('DIV');
				g.id = 'line-num-'+i;
				g.className = 'line-num';
				g.textContent = i+1;
				$gutter.appendChild(g);
			}
		}, 100);
		buffers[buf.id].currentBlock = $lines.firstChild;
		buffers[buf.id].currentBlockNumber = 0;
		buffers[buf.id].pos = 0;
		hl.states.push(null);

		$('.buffer').append($gutter);
		$('.buffer').append($lines);
	}

	var $gutter = $('.gutter');
	hl = hl || buffers[buf.id].hl;
	var cursor = buf.cursor || buffers[buf.id].cursor;
	//buffers[buf.id].cursor = cursor;
	
	if (buf.changes) {
		var b = buffers[buf.id];
		var lineParent = $('.lines')[0];
		buf.changes.each(function(c) {
			var res = getElementByPos(b, c.pos);
			var ele = res.e;
			var offset = c.pos - res.pos;
			var oldtxt = ele.textContent;
			var newtxt = oldtxt.substring(0, offset) + c.to + oldtxt.substring(offset+c.len);
			var oldstate = hl.states[res.num+1]
			b.currentBlock = renderLine(hl.parseLine(newtxt, res.num));
			lineParent.replaceChild(b.currentBlock, ele);
			ele = b.currentBlock.nextSibling;
			if (!outOfRange(ele) && !oldstate.equal(hl.states[res.num+1])) {
				hl.refresh(refreshIter(res.num+1, ele, hl.states, lineParent));
			}
		});
	}

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
			var line = renderLine(hl.parseLine(add[i-row], i));
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


//	if (typeof buf.cursor != 'undefined') {
//		renderCursor(buf);
//	}
	renderCursor(buf);
	
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
		$('.status-bar .cursor').remove();
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
	//if (buf.braces) {
	//	if (!$('.lines .highlight-brace-pair').get(0)) {
	//		$('.lines').append('<div class="highlight-brace-pair"></div>');
	//	}
	//	var cr, cc;
	//	if (buf.cursor) {
	//		cr = parseInt(buf.cursor.row);
	//		cc = parseInt(buf.cursor.col);
	//	}

	//	var ranges = [];
	//	for (var i = 0; i < buf.braces.length; i++) {
	//		var pt = buf.braces[i];
	//		//skip cursor, don't draw twice in same point
	//		if (buf.cursor && pt.row == cr && pt.col == cc) continue; 

	//		ranges.push(pt, {row:pt.row, col:pt.col+1});
	//	}
	//	renderSelections($('.lines .highlight-brace-pair'), ranges);
	//}

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
		var x = $('.lines .cursor')[0].offsetLeft;
		var y = $('.lines .cursor')[0].offsetTop;
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
	if (typeof buf.name != 'undefined') {
		document.title = buf.name;
		$('.status-bar .buf-name').text(buf.name);
	}
}

function renderLineInner(items, parentNode) {
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
		parentNode.appendChild(node);
	});
}

function renderLine(items) {
	var block = document.createElement('SPAN');
	block.className = 'code';
	renderLineInner(items, block);
	return block;
}

var aligntop = true;
function scrollToCursor(scrollTopRow, instant) {
	var el = $('.lines .cursor')[0];
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

	var cr = buf.y;
	var cc = buf.x;
	var x, y, w;
	//if (document.getElementById('line-0') == null) {
	//	x = 0, y = 0, w = 9.57;
	//} else {
	var cline = getLineByPos(buffers[buf.id], buf.pos) //getElementByPos(buffers[buf.id], buf.pos).e.textContent;//$('#line-'+cr)[0].textContent;

	var x = textWidth(cline, 0, cc);
	var y = cr*21+1;
	var w = 9.57;
	if (isChinese(cline[cc])) {
		w = 16;
	}
	//}
	$('.lines .cursor').attr('style', 'left:'+x+'px;top:'+y+'px;width:'+w+'px;');
}

var MODES = ['-- NORMAL --', '-- INSERT --', '-- VISUAL --'];

