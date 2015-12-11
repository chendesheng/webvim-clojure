//local states for each buffer, doesn't contains text content since it might be too large.
var buffers = {};
var viewport = {};
var lineHeight = 21;

function wrapActiveId(keys) {
	//minimize overhead of add id to every input keys
	return buffers.active.id+"!"+keys;
}

window.onload = function() { //use window.onload, not sure if stylesheet is loaded in document.ready
	var d = document.createElement('SPAN');
	d.className = 'line-num';
	d.style.opacity = 0;
	d.style.position = 'absolute';
	d.style.right = 0;
	d.style.bottom = 0;
	d.textContent = 'M';
	document.body.appendChild(d);
	lineHeight = d.offsetHeight;
	document.body.removeChild(d);

	$.getJSON('buf', function(resp) {
		render(resp);

		setSize(buffers.active.id);

		keyboardInit();
	});
};


var __updatingViewportSize = false;
function updateViewportSize(fnok) {
	var sz = setSize(buffers.active.id);
	if (sz.width != viewport.width || sz.height != viewport.height) {
		viewport.width = sz.width;
		viewport.height = sz.height;

		__updatingViewportSize = true;
		$.get('resize/'+sz.width+'/'+sz.height, function() {
			__updatingViewportSize = false;

			fnok();
		});
	} else {
		if (__updatingViewportSize == false) {
			fnok();
		}
	}
}

function setSize(bufid) {
	var zoom = window.innerWidth/document.body.offsetWidth;
	var pageh = $buffer(bufid).offsetHeight;
	var sw = pageh*zoom;
	var w = Math.floor($lines(bufid).offsetWidth*zoom/$cursor(bufid).offsetWidth);
	var h = Math.floor((window.innerHeight-$statusBar(bufid).offsetHeight)/lineHeight);
	$lines(bufid).style.paddingBottom = (pageh-lineHeight) + 'px'; //scroll beyond last line, leave at least one line
	return {width: w, height: h}
}

var gutterLineTemplate = '<div id="line-num-{row}" class="line-num">{incrow}</div>';

var _gutterWidth;
function gutterWidth(bufid, linenum) {
	if (linenum) {
		var w = 0;
		while(linenum>0) {
			linenum = Math.floor(linenum / 10);
			w++;
		}

		_gutterWidth = w;
		$gutter(bufid).style.width = _gutterWidth+'ch';
	} else {
		return _gutterWidth+2;//left padding 1ch, right padding 1ch
	}
}

function outOfRange(ele) {
	return ele == null || !/\bcode\b/.test(ele.className);
}
function endCode(ele) {
	return ele != null && /\bend\-code\b/.test(ele.className);
}

function getCodeBlockByPos(buf, pos) {
	var i = buf.pos;
	var ele = buf.currentBlock;
	var num = buf.currentBlockNumber;
	var linenum = buf.currentLineNumber;
	if (i <= pos) {
		while (!endCode(ele)) {
			var j = i + ele.textContent.length;
			if (i <= pos && pos < j) {
				break;
			}

			i = j;
			num++;
			linenum += ele.textContent.count('\n');
			ele = ele.nextSibling;
		}

		buf.currentBlock = ele;
		buf.pos = i;
		buf.currentBlockNumber = num;
		buf.currentLineNumber = linenum;
		return {e: ele, pos: i, num: num, linenum: linenum}
	} else {
		while (!outOfRange(ele)) {
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
	if (endCode(res.e)) return {e: res.e, offset: 0};

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

	var scrollTop = $buffer(buf.id).scrollTop;
	var scrollLeft = $lines(buf.id).scrollLeft;
	var ch = res.e.textContent[res.offset];	
	return {left: rect.left+scrollLeft, top: rect.top+scrollTop, ch: ch, e: res.e};
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
			return !endCode(ele);
		}
	}
}

function renderLines(buf) {
	//render lines
	var hl = newHighlight(buf.lang);
	buffers[buf.id] = { hl: hl, id: buf.id };

	var lines = $lines(buf.id);
	var gutter = $gutter(buf.id);

	hl.states = [];
	var linenum = 0;
	buf.str.eachBlock(function(block, i) {
		hl.states.push(null);
		if (block) {
			lines.appendChild(renderBlock(hl.parseBlock(block, i)));

			var num = block.count('\n');
			for (var j = 0; j < num; j++) {
				var g = document.createElement('DIV');
				g.id = 'line-num-'+linenum;
				g.className = 'line-num';
				g.textContent = linenum+1;
				gutter.appendChild(g);

				linenum++;
			}
		}
	});


	//put a pivot in the end
	var pivot = document.createElement('SPAN');
	pivot.className = 'code end-code';
	lines.appendChild(pivot);

	buffers[buf.id].currentBlock = lines.firstChild;
	buffers[buf.id].currentBlockNumber = 0;
	buffers[buf.id].currentLineNumber = 0;
	buffers[buf.id].pos = 0;
	hl.states.push(null);
	//buffers[buf.id].hl = hl;

	gutterWidth(buf.id, linenum);
}

function renderChanges(buf) {
	var localbuf = buffers[buf.id];
	var lines = $lines(buf.id);
	var hl = buffers[buf.id].hl;
	buf.changes.each(function(c) {
		var resa = getCodeBlockByPos(localbuf, c.pos);
		var resb = getCodeBlockByPos(localbuf, c.pos+c.len);
		var nextblock = getCodeBlockByPos(localbuf, resb.pos+resb.e.textContent.length);
		var prefix = resa.e.textContent.substring(0, c.pos-resa.pos);
		var suffix = resb.e.textContent.substring(c.pos+c.len-resb.pos);
		var newtxt = prefix + c.to + suffix;

		//delete [resa.e, resb.e] both inclusive
		//don't delete "end-code" pivot
		var ele = resa.e;
		var blocknumdeleted = 0;
		while(!endCode(ele)) {
			var toremove = ele;
			ele = ele.nextSibling;
			lines.removeChild(toremove);
			blocknumdeleted++;

			if (toremove == resb.e) {
				break;
			}
		}

		//delete hl.states [resa.num+1, resb.num+1]
		var deletedstates = hl.states.splice(resa.num+1, blocknumdeleted);
		var savedstate = deletedstates.pop();

		//insert and keep track hl.states
		var blocknuminserted = newtxt.eachBlock(function(block, i) {
			var num = i + resa.num;
			var res = hl.parse(block, hl.states[num]);
			hl.states.splice(num+1, 0, res[0]);
			lines.insertBefore(renderBlock(res[1]), ele);
		});


		//update local buffer
		var linenuminserted = newtxt.count('\n'); 
		var linenumdeleted = nextblock.linenum-resa.linenum;

		var linenumdiff = linenuminserted-linenumdeleted;
		var blocknumdiff = blocknuminserted - blocknumdeleted;
		var posdiff = c.to.length-c.len;

		localbuf.pos = nextblock.pos + posdiff;
		localbuf.currentBlock = nextblock.e;
		localbuf.currentLineNumber = nextblock.linenum + linenumdiff;
		localbuf.currentBlockNumber = nextblock.num+blocknumdiff;

		//update syntax highlight
		if (!endCode(ele) && !savedstate.equal(hl.states[resa.num+blocknuminserted])) {
			//currentBlock will change
			var saved = localbuf.currentBlock.previousSibling;
			hl.refresh(refreshIter(resa.num+blocknuminserted, localbuf.currentBlock, hl.states, lines));
			localbuf.currentBlock = saved.nextSibling;
		}

		//render gutter
		var gutter = $gutter(buf.id);
		var linenum = parseInt(gutter.lastChild.textContent);
		if (linenumdiff > 0) {
			for (var j = 0; j < linenumdiff; j++) {
				var g = document.createElement('DIV');
				g.id = 'line-'+buf.id+'-'+linenum;
				g.className = 'line-num';
				g.textContent = linenum+1;
				gutter.appendChild(g);

				linenum++;
			}
		} else {
			for (var j = 0; j < -linenumdiff; j++) {
				linenum--;
				$remove($lineNumber(buf.id, linenum));
			}
		}

		gutterWidth(buf.id, linenum);
	});
}

function renderAutocompl(buf) {
	if (buf.autocompl && buf.autocompl.suggestions && buf.autocompl.suggestions.length > 1) {
		var autocompl = $autocompl(buf.id);
		var lastScrollTop = autocompl.scrollTop;

		var selectedIndex = parseInt(buf.autocompl['suggestions-index']);
		var currentWord = buf.autocompl.suggestions[selectedIndex];
		var h = $cursor(buf.id).offsetHeight+3;
		var res = getScreenXYByPos(buffers[buf.id], buffers[buf.id].cursor-currentWord.length);
		autocompl.innerHTML = '';
		autocompl.style.left = res.left+$lines(buf.id).scrollLeft-10+'px';
		buf.autocompl.suggestions.each(function(word, i) {
			if (i > 0) {
				var ele = document.createElement('PRE');
				ele.textContent = word;
				autocompl.appendChild(ele);
				if (i == selectedIndex) {
					ele.className = 'highlight';
					ele.id = 'autocompl-'+buf.id+'-highlight';
				}
			}
		});

		var $buf = $buffer(buf.id);
		if (res.top+h < $buf.scrollTop+$buf.offsetHeight-$statusBar(buf.id).offsetHeight) {
			autocompl.style.top = res.top+h+'px';
		} else {
			autocompl.style.top = res.top-autocompl.offsetHeight-3+'px';
		}
		autocompl.style.marginLeft = -gutterWidth()+'ch';

		//TODO: use em instead of px
		var viewportTop = lastScrollTop;
		var viewportBottom = lastScrollTop+240;
		var currentPos = (selectedIndex-1) * 24;
		if (currentPos < viewportTop) {
			autocompl.scrollTop=currentPos;
		} else if (currentPos+24 >= viewportBottom) {
			autocompl.scrollTop=currentPos-216;
		}
	} else {
		$autocompl(buf.id).innerHTML = '';
	}
}

//TODO: Future improvements: 
//1. put syntax highlight to a dedicate web worker (or just use setTimeout)
//2. only render “visible” parts and still make scrolling fast
function render(buf) {
	if (!buf) return;
	
	var $buf = $buffer(buf.id);
	if (typeof buf.str != 'undefined') {
		if (buffers.active && buf.id != buffers.active.id) {
			$remove($buffer(buffers.active.id))
		}

		renderLines(buf);
		buffers.active = buffers[buf.id];
		setSize(buffers.active.id);
	}

	if (buf.changes) {
		renderChanges(buf);
	}

	//save current cursor for local use
	if (typeof buf.pos != 'undefined') {
		buffers[buf.id].cursor = buf.pos;
	}
	renderCursor(buffers[buf.id]);

	//render ex
	renderStatusBar(buf)
	//render unsaved
	if (typeof buf.dirty != 'undefined') {
		var statusName = $statusName(buf.id);
		if (!!buf.dirty) {
			statusName.className = 'buf-name buf-unsaved';
		} else {
			statusName.className = 'buf-name';
		}
	}

	//render ongoing keys
	if (typeof buf.keys != 'undefined') {
		if (buf.keys && buf.keys.length > 0 && buf.keys[0] != ':') {
			$statusKeys(buf.id).textContent = buf.keys.reverse().join('');
		} else {
			$statusKeys(buf.id).innerHTML = '';
		}
	}

	//render visual
	$selections(buf.id).innerHTML = '';
	if (buf.visual) {
		if (buf.visual.type == 0)
			renderSelections($selections(buf.id), buffers[buf.id], buf.visual.ranges);
		if (buf.visual.type == 1) {
			var ranges = buf.visual.ranges;
			ranges[0][1] = ranges[0][1]-2;
			renderSelections($selections(buf.id), buffers[buf.id], buf.visual.ranges);
		}
	}

	//render hlsearch
	if (typeof buf.highlights != 'undefined') {
		$highlights(buf.id).innerHTML = '';
		if (buf.highlights) renderSelections($highlights(buf.id), buffers[buf.id], buf.highlights, true);
	}

	//render matched brace pair
	if (typeof buf.braces != 'undefined') {
		$cursorBrace(buf.id).innerHTML = '';
		var $p = $cursorBrace(buf.id);

		if (buf.braces) {
			for (var i = 0; i < buf.braces.length; i++) {
				var pt = buf.braces[i];
				//skip cursor, don't draw twice at the same point
				if (buffers[buf.id].cursor == pt) continue; 

				renderSelection($p, pt, pt, buffers[buf.id]);
			}
		}
	}

	scrollToCursor(buf.id, buf['scroll-top'] || 0, buf.str);

	renderAutocompl(buf);

	//title
	if (typeof buf.name != 'undefined') {
		document.title = buf.name;
		$statusName(buf.id).textContent = buf.name;
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
function scrollToCursor(bufid, scrollTopRow, instant) {
	var el = $cursor(bufid);
	var lines = $lines(bufid);
	var buf = $buffer(bufid);
    
	var scrleft = lines.scrollLeft;
	var width = lines.offsetWidth;

	var oldst = buf.scrollTop;
	var newst = scrollTopRow * lineHeight;
	if (!instant && Math.abs(oldst - newst) > 3*lineHeight) {
		//buf.scrollTop = newst; //TODO: implement animate without jQuery
		$animateScroll(buf, newst);
	} else {
		buf.scrollTop = newst;
	}

	if (el.offsetLeft+el.offsetWidth > scrleft+width) {
		lines.scrollLeft = el.offsetLeft+el.offsetWidth-width;
	} else if (el.offsetLeft < scrleft) {
		lines.scrollLeft = el.offsetLeft;
	}
}

function renderSelections($p, buf, ranges) {
	for (var i = 0; i < ranges.length; i++) {
		renderSelection($p, ranges[i][0], ranges[i][1], buf);
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

function renderSelection($p, a, b, buf) {
	function append(x, y, w, h) {
		var sp = document.createElement('SPAN');
		sp.className = 'line-selected';
		sp.style.cssText = 'left:'+x+'px;'
			+ 'top:'+y+'px;'
			+ 'margin-left:'+(-gutterWidth(buf.id)+'ch') + ';'
			+ 'height:'+h+';'
			+ 'width:'+w+';';
		$p.appendChild(sp);
		return sp;
	}
	//sort
	if (a > b) {
		var t = a;
		a = b;
		b = t;
	}
	b++;

	var div = document.createElement('DIV');
	var resa = getScreenXYByPos(buf, a);
	var resb = getScreenXYByPos(buf, b);
	var lastline;
	if (resa.top != resb.top) {
		var w = $buffer(buf.id).offsetWidth;
		append(resa.left, resa.top, (w-resa.left)+'px', lineHeight+'px');
		var mh = resb.top-resa.top-lineHeight;
		if (mh > 0) {
			append(0, resa.top+lineHeight, w+'px', mh+'px');
		}
		lastline = append(0, resb.top, resb.left+'px', '1em');
	} else {
		lastline = append(resa.left, resa.top, Math.abs(resa.left-resb.left)+'px', '1em');
	}
	lastline.style.paddingBottom = '1px';
}

function renderCursor(localbuf) {
	var pos = localbuf.cursor;
	var res = getScreenXYByPos(localbuf, pos);
	if (/\r|\n|\t/.test(res.ch)) {
		res.ch = ' ';
	}
	//console.log(res);
	var color = getComputedStyle(res.e.parentNode, null).color;
	var background = getComputedStyle(document.body, null).backgroundColor;
	//console.log(color);
	//console.log(background);
	var cursor = $cursor(localbuf.id);
	cursor.textContent = res.ch;
	cursor.className = 'cursor';
	cursor.style.cssText = 'left:'+res.left+'px;'
		+'margin-left:' + (-gutterWidth(localbuf.id)+'ch') + ';'
		+'background-color:' + color + ';'
		+'color:' + background + ';'
		+'top:' + res.top + 'px;'
		+'padding-bottom:1px;';
}

function renderLineBuffer(buf) {
	var linebuf = buf['line-buffer'];
	var str = linebuf.str;
	var pos = linebuf.pos;
	var ex = $statusBuf(buf.id);
	ex.textContent = str;

	//cursor
	if (str[str.length-1] == '\n') {
		$statusCursor(buf.id).style.display= 'none';
	} else {
		$statusCursor(buf.id).style.display = 'inline';
	}
}

function renderStatusBar(buf) {
	if (buf.message) {
		$statusCursor(buf.id).style.cssText = 'display:none';

		var ex = $statusBuf(buf.id);
		ex.textContent = buf.message;
	} else if (typeof buf['line-buffer'] != 'undefined') {
		renderLineBuffer(buf);
	} else {
		$statusCursor(buf.id).style.cssText = 'display:none';

		if (typeof buf.mode != 'undefined' && buf.mode < MODES.length) {
			var ex = $statusBuf(buf.id);
			ex.textContent = MODES[buf.mode];
			keymap = keymaps[buf.mode];
		}
	}
}

var MODES = ['-- NORMAL --', '-- INSERT --', '-- VISUAL --'];

