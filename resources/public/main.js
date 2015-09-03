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
	a = a || 0;
	b = b || txt.length;
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

String.prototype.count = function(ch) {
	var i = 0;
	var cnt = 0;
	while(true) {
		var nexti = this.indexOf(ch, i);
		if (nexti == -1)
			break;
		i = nexti+ch.length;
		cnt++;
	}

	return cnt;
};

String.prototype.eachBlock = function(fn, n) {
	n = n || 100;
	var str = this;
	var num = 0;
	while(str.length > 0) {
		//ensure callback string length (str.length) >= 0.5n, avoid fragment code block
		if (str.length < 1.5*n) {  
			fn(str, num);
			num++;
			break;
		} else {
			var i = str.substring(n).search(/\s/);

			if (i==-1) {
				fn(str, num);
				num++;
				break;
			} else {
				i = i+n+1;
				fn(str.substring(0, i), num);
				num++;
				str = str.substring(i);
			}
		}
	}

	return num;
};

//local states for each buffer, doesn't contains text content since it might be too large.
var buffers = {};

function outOfRange(ele) {
	return ele == null || !/\bcode\b/.test(ele.className);
}

function getCodeBlockByPos(buf, pos) {
	var i = buf.pos;
	var ele = buf.currentBlock;
	var num = buf.currentBlockNumber;
	var linenum = buf.currentLineNumber;
	if (i <= pos) {
		while (!outOfRange(ele)) {
			var j = i + ele.textContent.length;
			if (i <= pos && pos < j) {
				buf.currentBlock = ele;
				buf.pos = i;
				buf.currentBlockNumber = num;
				buf.currentLineNumber = linenum;
				return {e: ele, pos: i, num: num, linenum: linenum}
			}

			i = j;
			num++;
			linenum += ele.textContent.count('\n');
			ele = ele.nextSibling;
		}
	} else {
		while (true) {
			if (outOfRange(ele)) {
				break;
			}

			num--;
			ele = ele.previousSibling;
			linenum -= ele.textContent.count('\n');
			var j = i - ele.textContent.length;

			if (j <= pos && pos < i) {
				buf.currentBlock = ele;
				buf.pos = j;
				buf.currentBlockNumber = num;
				buf.currentLineNumber = linenum;
				return {e: ele, pos: j, num: num, linenum: linenum}
			}

			i = j;
		}
	}

	return null;
}

function getElementByPos(buf, pos) {
	var res = getCodeBlockByPos(buf, pos);
	if (res == null) return null;

	var ele = res.e.firstChild;
	var i = res.pos;
	while (true) {
		var l = ele.textContent.length;
		if (i <= pos && pos < i+l) {
			break;
		}

		i += l;
		ele = ele.nextSibling;
	}
	if (ele.nodeType != 3) {
		ele = ele.firstChild;
	}

	return {e: ele, offset: pos-i};
}

function getScreenXYByPos(buf, pos) {
	var res = getElementByPos(buf, pos);
	if (res == null) return;

	var range = document.createRange();
	range.setStart(res.e, res.offset);
	range.setEnd(res.e, res.offset);

	var list = range.getClientRects();
	var rect = list[0];
	if (list.length > 1 && list[0].top != list[1].top) {
		//line break;
		rect = list[1];
	}

	var scrollTop = $('.buffer').scrollTop();
	var ch = res.e.textContent[res.offset];	
	return {left: rect.left-50, top: rect.top+scrollTop, ch: ch, e: res.e};
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
			var newele = renderBlock(items);
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
		var linenum = 0;
		buf.str.eachBlock(function(block, i) {
			hl.states.push(null);
			if (block) {
				$lines.appendChild(renderBlock(hl.parseBlock(block, i)));

				var num = block.count('\n');
				for (var j = 0; j < num; j++) {
					var g = document.createElement('DIV');
					g.id = 'line-num-'+linenum;
					g.className = 'line-num';
					g.textContent = linenum+1;
					$gutter.appendChild(g);

					linenum++;
				}
			}
		});

		//put a pivot in the end
		//var pivot = document.createElement('SPAN');
		//pivot.className = 'code';
		//$lines.appendChild(pivot);

		buffers[buf.id].currentBlock = $lines.firstChild;
		buffers[buf.id].currentBlockNumber = 0;
		buffers[buf.id].currentLineNumber = 0;
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
		var $lines = $('.lines')[0];
		buf.changes.each(function(c) {
			var resa = getCodeBlockByPos(b, c.pos);
			var resb = getCodeBlockByPos(b, c.pos+c.len);
			var nextblock = getCodeBlockByPos(b, resb.pos+resb.e.textContent.length);
			var prefix = resa.e.textContent.substring(0, c.pos-resa.pos);
			var suffix = resb.e.textContent.substring(c.pos+c.len-resb.pos);
			var newtxt = prefix + c.to + suffix;

			//delete [resa.e, resb] both inclusive
			var ele = resa.e;
			while(true) {
				var toremove = ele;
				ele = ele.nextSibling;
				$lines.removeChild(toremove);

				if (toremove == resb.e) {
					break;
				}
			}

			//delete hl.states [resa.num+1, resb.num+1]
			var blocknumdeleted = resb.num-resa.num+1;
			var deletedstates = hl.states.splice(resa.num+1, blocknumdeleted);
			var savedstate = deletedstates.pop();

			//insert and keep track hl.states
			var blocknuminserted = newtxt.eachBlock(function(block, i) {
				var num = i + resa.num;
				var res = hl.parse(block, hl.states[num]);
				hl.states.splice(num+1, 0, res[0]);
				$lines.insertBefore(renderBlock(res[1]), ele);
			});


			//update local buffer
			var linenuminserted = newtxt.count('\n'); 
			var linenumdeleted = nextblock.linenum-resa.linenum;

			var linenumdiff = linenuminserted-linenumdeleted;
			var blocknumdiff = blocknuminserted - blocknumdeleted;
			var posdiff = c.to.length-c.len;

			b.pos = nextblock.pos + posdiff;
			b.currentBlock = nextblock.e;
			b.currentLineNumber = nextblock.linenum + linenumdiff;
			b.currentBlockNumber = nextblock.num+blocknumdiff;

			//update syntax highlight
			if (!outOfRange(ele) && !savedstate.equal(hl.states[resa.num+blocknuminserted])) {
				//currentBlock will change
				var saved = b.currentBlock.previousSibling;
				hl.refresh(refreshIter(resa.num+blocknuminserted, b.currentBlock, hl.states, $lines));
				b.currentBlock = saved.nextSibling;
			}

			//render gutter
			var $gutter = $('.gutter');
			var linenum = parseInt($gutter.find(':last-child').text());
			if (linenumdiff > 0) {
				for (var j = 0; j < linenumdiff; j++) {
					var g = document.createElement('DIV');
					g.id = 'line-num-'+linenum;
					g.className = 'line-num';
					g.textContent = linenum+1;
					$gutter.append(g);

					linenum++;
				}
			} else {
				for (var j = 0; j < -linenumdiff; j++) {
					$('#line-num-'+(linenum-1)).remove();
					linenum--;
				}
			}
		});
	}

	//emtpy file has 1 line too
	if ($gutter[0].firstChild==null) { 
		$gutter.append(replaceBinding(gutterLineTemplate, {row:0,incrow:1}));
	}


//	if (typeof buf.cursor != 'undefined') {
//		renderCursor(buf);
//	}
	if (typeof buf.pos != 'undefined') {
		renderCursor(buf.pos, buffers[buf.id]);
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
		if ($('.lines .selections')[0] == null) {
			$('.lines').append('<div class="selections"></div>');
		}

		var $p = $('.lines .selections')[0];
		renderSelections($p, buffers[buf.id], buf.visual.ranges);
	}

	//render hlsearch
	$('.lines .highlights').empty();
	if (buf.highlights) {
		if ($('.lines .highlights')[0] == null) {
			$('.lines').append('<div class="highlights"></div>');
		}

		var $p = $('.lines .highlights')[0];
		renderSelections($p, buffers[buf.id], buf.highlights, true);
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

function renderBlock(items) {
	var block = document.createElement('SPAN');
	block.className = 'code';

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
		block.appendChild(node);
	});

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

function renderSelections($p, buf, ranges, reverseTextColor) {
	for (var i = 0; i < ranges.length; i+=2) {
		var a = ranges[i];
		var b = ranges[i+1];
		renderSelection($p, a, b, reverseTextColor, buf);
	}
}

function newsubstring(buf, a, b) { 
	var resa = getElementByPos(buf, a); 
	var resb = getElementByPos(buf, b); 
	var range = document.createRange(); 
	range.setStart(resa.e, resa.offset); 
	range.setEnd(resb.e, resb.offset); 
	return range.toString();
}

//extract text from DOM: [a, b)
function substring(buf, a, b) {
	if (a == b) return '';
	if (a > b) throw "a must smaller or equal than b";

	var res = getCodeBlockByPos(buf, a);
	var ele = res.e;

	var txt = '';
	var start  = res.pos;  //pos of block
	//keep [a, b)
	while(true) {
		if (a >= b) {
			break;
		}

		txt += ele.textContent.substring(a-start, b-start);
		ele = ele.nextSibling;
		if (ele == null)
			break;

		start = start + ele.textContent.length;
		a = start;
	}

	return txt;
}

//if reverseTextColor==true copy text from lines append to line-selected
function renderSelection($p, a, b, reverseTextColor, buf) {
	//sort
	if (a > b) {
		var t = a;
		a = b;
		b = t;
	}
	b++;

	var div = document.createElement('DIV');

	//var resa = getElementByPos(buf, a);
	//var resb = getElementByPos(buf, b);
	//var range = document.createRange();
	//range.setStart(resa.e, resa.offset);
	//range.setEnd(resb.e, resb.offset);
	//var span = document.createElement('SPAN');
	//span.style.color = '#000';
	//span.style.background = 'rgb(255, 233, 32)';
	//range.surroundContents(span);
	

	var resa = getScreenXYByPos(buf, a);
	var resb = getScreenXYByPos(buf, b);

	if (resa.top != resb.top) {
		$('<span>').addClass('line-selected').css('left', resa.left).css('top', resa.top).width('100%').appendTo($p);
		var mh = resb.top-resa.top-21;
		if (mh > 0) {
			$('<span>').addClass('line-selected').css('left', 0).css('top', resa.top+21).width('100%').height(mh).appendTo($p);
		}
		$('<span>').addClass('line-selected').css('left', 0).css('top', resb.top).width(resb.left).appendTo($p);
	} else {
		$('<span>').addClass('line-selected').css('left', resa.left).css('top', resa.top).width(Math.abs(resa.left-resb.left)).appendTo($p);
	}
}

function renderCursor(pos, buf) {
	if (!$('.lines .cursor').get(0)) {
		$('.lines').append('<div class="cursor"></div>');
	}

	var res = getScreenXYByPos(buf, pos);
	if (!/\bcode\b/.test(res.e.parentNode.className)) {
		$('.lines .cursor').removeClass().addClass('cursor').addClass(res.e.parentNode.className);
	} else {
		$('.lines .cursor').removeClass().addClass('cursor');
	}
	var ch = (res.ch == '\n'||res.ch=='\t') ? ' ' : res.ch;
	$('.lines .cursor').text(ch).attr('style', 'left:'+res.left+'px;top:'+res.top+'px;');
}

var MODES = ['-- NORMAL --', '-- INSERT --', '-- VISUAL --'];

