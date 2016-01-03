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
	d.remove();

	keyboardInit();
	viewport.height = Math.floor((window.innerHeight-lineHeight*1.5)/lineHeight);
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
	var w = Math.floor($content(bufid).offsetWidth*zoom/10);
	var h = Math.floor((window.innerHeight-$statusBar(bufid).offsetHeight)/lineHeight);
	$content(bufid).style.paddingBottom = (pageh-lineHeight) + 'px'; //scroll beyond last line, leave at least one line
	return {width: w, height: h}
}

var __gutterWidth;
function gutterWidth(bufid, linenum) {
	if (linenum) {
		var w = 0;
		while(linenum>0) {
			linenum = Math.floor(linenum / 10);
			w++;
		}

		__gutterWidth = w;
		$gutter(bufid).style.width = __gutterWidth+'ch';
	} else {
		return __gutterWidth+2;//left padding 1ch, right padding 1ch
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
	var j = 0;
	while (true) {
		var l = ele.textContent.length;
		if (i <= pos && pos < i+l) {
			break;
		}

		i += l;
		ele = ele.nextSibling;
		j++;
	}
	if (ele.nodeType != 3) {
		ele = ele.firstChild;
	}

	return {e: ele, offset: pos-i, index: j};
}

function getLine(bufid, linenum) {
	var lines = $lines(bufid)
	var line;
	var e = lines.firstChild;
	var i = parseInt(lines.dataset.from)||0;
	//var i = Math.floor(getScrollTop(bufid)/lineHeight)-5;
	i = i<0?0:i;
	while(e) {
		if (i == linenum) {
			line = e;
			break;
		}
		e = e.nextSibling;
		i++;
	}
	return line;
}

function getNextLineBreakElement(localbuf, pos) {
	var res = getCodeBlockByPos(localbuf, pos);
	if (res == null) return null;
	if (endCode(res.e)) return {e: res.e, offset: 0};

	var ele = res.e.firstChild;
	var i = 0;
	var offset;
	while (true) {
		var l = ele.textContent.length;
		offset = ele.textContent.indexOf('\n');
		if (offset >= 0) {
			break;
		}

		ele = ele.nextSibling;
		i++;
	}

	if (ele.nodeType != 3) {
		ele = ele.firstChild;
	}

	return {e: ele, offset: offset, index: i};
}

function getScreenXYByPos(localbuf, pos) {
	var res = getElementByPos(localbuf, pos);
	return getScreenXYInner(localbuf, res);
}

function getNextLineBreakScreenXY(localbuf) {
	var res = getNextLineBreakElement(localbuf, localbuf.pos);
	return getScreenXYInner(localbuf, res);
}

function getScreenXYInner(localbuf, res) {
	var linenum = localbuf.currentLineNumber;
	var ch = res.e.textContent[res.offset];	

	var line = getLine(localbuf.id, linenum)
	if (!line) return {linenum: linenum};

	var node = line.childNodes[res.index];
	var e = (node.nodeType==3)?node:node.firstChild;

	var range = document.createRange();
	range.setStart(e, res.offset);
	range.setEnd(e, res.offset+1);

	var rect = range.getBoundingClientRect();
	var left = rect.left;
	var width = rect.width;
	//When a range contains only \n getBoundingClientRect returns empty rect. 
	//Perhaps because these chars are invisible. getClientRects still work.
	if (width == 0) { 
		if (res.offset > 0) { 
			range = document.createRange();
			range.setStart(e, res.offset-1);
			range.setEnd(e, res.offset);
			rect = range.getBoundingClientRect();
			left = rect.left + rect.width;
		} else {
			range = document.createRange();
			range.setStart(e, res.offset);
			range.setEnd(e, res.offset);

			var list = range.getClientRects();
			rect = list[0];
			if (list.length > 1 && list[0].top != list[1].top) {
				//line break;
				rect = list[1];
			}
			left = rect.left;
		}
	}

	return {top: linenum*lineHeight, left: left+$content(localbuf.id).scrollLeft, ch: ch, e: e, width:width};
}

function createDOMItem(className, text) {
	var node;
	if (className) {
		node = document.createElement('SPAN');
		node.className = className;
		node.textContent = text;
	} else {
		node = document.createTextNode(text);
	}
	return node;
}

var __id=0;
function uniqueId() {
	__id++;
	return (new Date).getTime()+__id;
}

function renderBlock(items) {
	var block = document.createElement('SPAN');
	block.className = 'code';
	block.id = uniqueId();

	items.each(function(item){
		var className = item[0];
		var text = item[1];
		block.appendChild(createDOMItem(className, text));
	});

	return block;
}

function refreshBlock(items, ele) {
	function nodetype(className) {
		if (className) return Node.ELEMENT_NODE;
		else return Node.TEXT_NODE;
	}
	
	if (ele.childNodes.length == items.length) {
		var changed = false;
		for (var i = 0; i < items.length; i++) {
			var item = items[i];
			var className = item[0];
			var text = item[1];
			
			var oldele = ele.childNodes[i];
			if (text != oldele.textContent) {
				ele.replaceChild(createDOMItem(className, text), oldele);
				changed = true;
			} else {
				var nodeType = nodetype(className);
				if (nodeType == oldele.nodeType) {
					if (nodeType == Node.ELEMENT_NODE && className != oldele.className) {
						oldele.className = className;
						changed = true;
					}
				} else {
					ele.replaceChild(createDOMItem(className, text), oldele);
					changed = true;
				}
			}

			if (changed) {
				ele.dataset.time = (new Date).getTime();
			}
		}
		return true;
	} else {
		return false;
	}
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
			if (!refreshBlock(items, ele)) {
				var newele = renderBlock(items);
				parentNode.replaceChild(newele, ele);
				ele = newele;
			}
		},
		next: function() {
			ele = ele.nextSibling;
			i++;
			return !endCode(ele);
		}
	}
}

function renderLines(buf) {
	var hl = newHighlight(buf.lang);
	var offscreenLines = document.createElement('DIV');

	hl.states = [];
	var linecnt = 0;
	var cnt = buf.str.eachLine(function(block, i) {
		hl.states.push(null);
		if (block) {
			offscreenLines.appendChild(renderBlock(hl.parseBlock(block, i)));
			linecnt += block.count('\n');
		}
	});

	$content(buf.id).style.height = cnt*lineHeight+'px';

	//put a pivot in the end
	var pivot = document.createElement('SPAN');
	pivot.className = 'code end-code';
	offscreenLines.appendChild(pivot);

	buffers[buf.id] = { 
		hl: hl,
		id: buf.id,
		offscreenLines: offscreenLines,
		currentBlock: offscreenLines.firstChild,
		currentBlockNumber: 0,
		currentLineNumber: 0,
		pos: 0,
		//track linecnt change
		linecnt: linecnt,
		lastlinecnt: -1
	};

	hl.states.push(null);
}

function renderChanges(buf) {
	var localbuf = buffers[buf.id];
	var offscreenLines = localbuf.offscreenLines;
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
			toremove.remove();
			blocknumdeleted++;

			if (toremove == resb.e) {
				break;
			}
		}

		//delete hl.states [resa.num+1, resb.num+1]
		var deletedstates = hl.states.splice(resa.num+1, blocknumdeleted);
		var savedstate = deletedstates.pop();

		//insert and keep track hl.states
		var blocknuminserted = newtxt.eachLine(function(block, i) {
			var num = i + resa.num;
			var res = hl.parse(block, hl.states[num]);
			hl.states.splice(num+1, 0, res[0]);
			offscreenLines.insertBefore(renderBlock(res[1]), ele);
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
			hl.refresh(refreshIter(resa.num+blocknuminserted, localbuf.currentBlock, hl.states, offscreenLines));
			localbuf.currentBlock = saved.nextSibling;
		}

		localbuf.linecnt += linenumdiff;
	});
}

function autocomplItem(subject, word) {
	if (subject.length == 0) return word;
	
	var j = 0;
	for (var i = 0; i < subject.length;i++) {
		var ch = subject[i];
		j=word.indexOf(ch, j)+1;
	}
	
	var splits=[j-1];
	j--;
	for (var i = subject.length-2; i >= 0; i--) {
		var ch = subject[i];
		j=word.substring(0, j).lastIndexOf(ch);
		splits.unshift(j)
	}
	splits.unshift(-1);
	//console.log(splits);

	var html = '';
	for (var i=1; i < splits.length; i++) {
		html += word.substring(splits[i-1]+1, splits[i])+
			'<span class="matched">'+word[splits[i]]+'</span>';
	}
	html += word.substring(splits[splits.length-1]+1);
	return html;
}

function appendAutocomplItems(bufid, suggestions, $a, selectedIndex) {
	$hide($a);
	$a.innerHTML = '';
	
	var subject = suggestions[0];
	suggestions.each(function(word, i) {
		if (i > 0) {
			var ele = document.createElement('PRE');
			ele.innerHTML = autocomplItem(subject, word);
			$a.appendChild(ele);
			if (i == selectedIndex) {
				ele.className = 'highlight';
				ele.id = 'autocompl-'+bufid+'-highlight';
			}
		}
	});
	$show($a);
}

function $hideAutocompl(bufid) {
	buffers[bufid].suggestions = null;
	$hide($autocompl(bufid));
	$hide($exAutocompl(bufid));
}

function renderAutocompl(buf) {
	var autocompl = buf.autocompl;
	var suggestions;
	if (!autocompl) {
		$hideAutocompl(buf.id);
		return;
	} else if (!autocompl.suggestions) {
		//use local cache
		suggestions = buffers[buf.id].suggestions;
	} else {
		//update local cache
		suggestions = autocompl.suggestions;
		buffers[buf.id].suggestions = suggestions;
	}

	if (suggestions == null || suggestions.length < 1) {
		//This should not happen, server problem
		$hideAutocompl(buf.id);
		return;
	}

	if (suggestions.length <= 2) {
		$hideAutocompl(buf.id);
		return;
	}


	var selectedIndex = autocompl.index;
	var lastScrollTop;
	var popupHeight;
	if (buffers[buf.id].mode == 2) {
		$a = $exAutocompl(buf.id);
		lastScrollTop = $a.scrollTop;

		appendAutocomplItems(buf.id, suggestions, $a, selectedIndex);
		itemHeight = $a.firstChild.offsetHeight;
		popupHeight = $a.offsetHeight;
	} else {
		$a = $autocompl(buf.id);
		lastScrollTop = $a.scrollTop;

		var h = $cursor(buf.id).offsetHeight+3;
		var currentWord = suggestions[selectedIndex];
		var res = getScreenXYByPos(buffers[buf.id], buffers[buf.id].cursor-currentWord.length);
		if (!res.e) return;

		$a.style.left = res.left+$content(buf.id).scrollLeft-10+'px';
		var top = res.top;

		appendAutocomplItems(buf.id, suggestions, $a, selectedIndex);
		itemHeight = $a.firstChild.offsetHeight;
		popupHeight = $a.offsetHeight;

		var $buf = $buffer(buf.id);
		if (top+h+popupHeight < $buf.scrollTop+$buf.offsetHeight-$statusBar(buf.id).offsetHeight) {
			$a.style.top = top+h+'px';
		} else {
			$a.style.top = top-popupHeight-3+'px';
		}

		$a.style.marginLeft = -gutterWidth()+'ch';
	}

	var viewportTop = lastScrollTop;
	var viewportBottom = lastScrollTop+popupHeight;
	var pos = (selectedIndex-1)*itemHeight;
	if (pos < viewportTop) {
		$a.scrollTop=pos;
	} else if (pos >= viewportBottom) {
		$a.scrollTop=pos-9*itemHeight;
	}
}

function getScrollTop(bufid, forceLayout) {
	var localbuf = buffers[bufid];
	if (localbuf.cacheScrollTop == null || forceLayout)
		localbuf.cacheScrollTop = $buffer(bufid).scrollTop;
	return localbuf.cacheScrollTop;
}

function removeUntouched($p, time) {
	var i = $p.firstChild;
	while(i) {
		var prev = i;
		i = i.nextSibling;
		if (prev.dataset.touch != time) {
			prev.remove();
		}
	}
}


function renderToScreen(paddinglines){
	paddinglines = paddinglines || 5;

	var localbuf = buffers.active;
	var bufid = localbuf.id;
	var from = Math.floor(getScrollTop(bufid, true)/lineHeight)-paddinglines;
	from = from < 0 ? 0 : from;
	var offscreenLines = localbuf.offscreenLines;
	var h = viewport.height+1+paddinglines*2;
	var lines = $lines(bufid);

	var time = (new Date).getTime();

	var offscreen = offscreenLines.childNodes[from];
	var lastline;
	var cnt = 0;
	while(offscreen && !offscreen.className.contains('end-code') && cnt < h) {
		var line = document.getElementById(offscreen.id);
		var linenum = from+cnt;
		if (line) {
			if (offscreen.dataset.time == line.dataset.time) {
				line.dataset.touch = time;
				lastline = line;
			} else {
				var newline = offscreen.cloneNode(true);
				newline.dataset.touch = time;
				lines.replaceChild(newline, line);

				lastline = newline;
			}
		} else {
			newline = offscreen.cloneNode(true);
			newline.dataset.touch = time;
			insertAfter(lines, newline, lastline);

			lastline = newline;
		}

		offscreen = offscreen.nextSibling;
		cnt++;
	}

	removeUntouched(lines, time);

	lines.style.top = from*lineHeight+'px';
	lines.dataset.from = from;
	$content(bufid).style.height = offscreenLines.childNodes.length*lineHeight+'px';

	renderGutter(localbuf, from, h)
	renderCursor(localbuf, from, h);

	var $sel = $selections(bufid);
	if (localbuf.selections && localbuf.selections.length>0) {
		renderSelections($selections(bufid), localbuf, localbuf.selections, time);
	} else {
		$empty($sel);
	}

	var $high = $highlights(bufid);
	if (localbuf.highlights && localbuf.highlights.length>0) {
		renderSelections($highlights(bufid), localbuf, localbuf.highlights, time);
	} else {
		$empty($high);
	}

	var $braces = $cursorBrace(bufid);
	if (localbuf.braces && localbuf.braces.length>0) {
		renderSelections($braces, localbuf, localbuf.braces, time);
	} else {
		$empty($braces);
	}
}

function renderGutter(localbuf, from, visibleLines) {
	var bufid = localbuf.id;
	var $g = $gutter(bufid);
	$g.style.top = from*lineHeight+'px';

	if (visibleLines == $g.childNodes.length 
		&& visibleLines>0 
		&& from == parseInt($g.firstChild.textContent)-1
		//linenum does not change or out of screen
		&& (localbuf.lastlinecnt == localbuf.linecnt ||
			(from+visibleLines < localbuf.lastlinecnt && from+visibleLines < localbuf.linecnt))) {
		return;
	}

	var times = visibleLines - $g.childNodes.length;
	if (times > 0) {
		for (var i = 0; i < times; i++) {
			var e = document.createElement('div');
			e.className = 'line-num';
			$g.appendChild(e);
		}
	} else {
		for (var i = 0; i < -times; i++) {
			if ($g.firstChild) {
				$g.firstChild.remove();
			}
		}
	}

	//console.log("equal? ", visibleLines == $g.childNodes.length, visibleLines, $g.childNodes.length);

	Array.prototype.forEach.call($g.childNodes, function(e, i) {
		var linenum = i+from+1;
		//console.log(linenum);
		if (linenum > localbuf.linecnt) {
			$hide(e);
		} else {
			e.textContent = linenum;
			if (linenum == localbuf.cursorLine) {
				addClass(e, 'highlight');
			} else {
				removeClass(e, 'highlight');
			}
			$show(e);
		}
	});

	if (localbuf.lastlinecnt != localbuf.linecnt) {
		localbuf.lastlinecnt = localbuf.linecnt;
		gutterWidth(localbuf.id, localbuf.linecnt);
	}
}
function startRenderToScreen(instant) {
	setTimeout(function(){
		renderToScreen();
		scrollToCursor(buffers.active, instant);
	}, 0);
}

var __renderTimer;
function onscrollRender() {
	if (__renderTimer) clearTimeout(__renderTimer);

	if (buffers.active.linecnt > 3000) {
		__renderTimer = setTimeout(function(){
			renderToScreen(15);
		}, 10);
	} else {
		renderToScreen(10);
	}
}

//TODO: Future improvements: 
//1. put syntax highlight to a dedicate web worker (or just use setTimeout)
function render(buf) {
	if (!buf) return;
	
	var $buf = $buffer(buf.id);
	if (!$buf.onscroll) {
		$buf.onscroll = onscrollRender;
	}

	if (typeof buf.str != 'undefined') {
		if (buffers.active && buf.id != buffers.active.id) {
			$remove($buffer(buffers.active.id))
		}

		renderLines(buf);
		buffers.active = buffers[buf.id];
		setSize(buffers.active.id);
	}
	
	if (buf.tabsize) {
		$tabsize(buf.tabsize);
	}

	if (buf.changes) {
		renderChanges(buf);
	}

	//save current cursor for local use
	if (typeof buf.pos != 'undefined') {
		buffers[buf.id].cursor = buf.pos;
	}
	if (typeof buf.mode != 'undefined') {
		buffers[buf.id].mode = buf.mode;
	}

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

	var localbuf = buffers[buf.id];

	//render visual
	if (buf.visual) {
		if (buf.visual.type > 0) {
			renderMode(buf, VISUAL_MODES[buf.visual.type]);
		}
		
		if (buf.visual.type == 0) {
			if (buffers[buf.id].mode < MODES.length) {
				renderMode(buf, MODES[buffers[buf.id].mode]);
			}
			localbuf.selections = [];
		} else if (buf.visual.type == 2) {
			var ranges = buf.visual.ranges;
			ranges[0][1] = ranges[0][1]-1;
			localbuf.selections = buf.visual.ranges;
		} else {
			localbuf.selections = buf.visual.ranges;
		}
	}

	//render hlsearch
	if (typeof buf.highlights != 'undefined') {
		if (buf.highlights) {
			localbuf.highlights = buf.highlights || [];
		}
	}

	//render matched brace pair
	if (typeof buf.braces != 'undefined') {
		$cursorBrace(buf.id).innerHTML = '';
		var $p = $cursorBrace(buf.id);

		if (buf.braces) {
			localbuf.braces = [];//buf.braces;

			for (var i = 0; i < buf.braces.length; i++) {
				var pt = buf.braces[i];
				//skip cursor, don't draw twice at the same point
				if (buffers[buf.id].cursor == pt) continue; 

				localbuf.braces.push([pt, pt]);
			}
		}
	}

	delete localbuf.cacheScrollTop;
	if (buf['scroll-top']!=null) {
		localbuf.scrollTopRow = buf['scroll-top'] || 0;
	}

	renderAutocompl(buf);

	//title
	if (typeof buf.name != 'undefined') {
		document.title = buf.name;
		$statusName(buf.id).textContent = buf.name;
	}

	startRenderToScreen(!!buf.str);
}

var aligntop = true;
function scrollToCursor(localbuf, instant) {
	var bufid = localbuf.id;
	var $cur = $cursor(bufid);
	var $buf = $buffer(bufid);
    
	var oldst = $buf.scrollTop;
	var newst = localbuf.scrollTopRow * lineHeight;
	if (!instant && Math.abs(oldst - newst) > 3*lineHeight && Math.abs(oldst - newst) < 5000) {
		$animateScroll($buf, newst);
	} else {
		$buf.scrollTop = newst;
	}

	var content = $content(bufid);
	var scrleft = content.scrollLeft;
	//console.log(content);
	var width = content.offsetWidth;
	if ($cur.offsetLeft+$cur.offsetWidth > scrleft+width) {
		content.scrollLeft = $cur.offsetLeft+$cur.offsetWidth-width+30;
	} else if ($cur.offsetLeft < scrleft) {
		content.scrollLeft = $cur.offsetLeft-30;
	}
}

function renderSelections($p, buf, ranges, time) {
	for (var i = 0; i < ranges.length; i++) {
		renderSelection($p, ranges[i][0], ranges[i][1], buf, time);
	}
	removeUntouched($p, time);
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

function renderSelection($p, a, b, buf, time) {
	if (a < 0) return;

	var __e = $p.firstChild;
	function getOrCreate() {
		while(__e) {
			if (__e.dataset.touch != time) {
				return __e;
			}
			__e = __e.nextSibling;
		}
		__e = document.createElement('SPAN');
		__e.className = 'line-selected';
		return __e;
	}

	function append(x, y, w, h) {
		var sp = getOrCreate();
		sp.dataset.touch = time;

		var styles = [['left', x+'px'],
				['top', y+'px'],
				['marginLeft', (-gutterWidth(buf.id)+'ch')],
				['width', w+'px'],
				['paddingBottom', '0px'],
				['height', h+'px']];

		styles.each(function(item) {
			var k = item[0];
			var v = item[1];
			if (sp.style[k] != v) {
				sp.style[k] = v;
			}
		});

		if (!sp.parentNode) {
			$p.appendChild(sp);
		}

		return sp;
	}
	//sort
	if (b >= 0) {
		if (a > b) {
			var t = a;
			a = b;
			b = t;
		}
		b++;
	}

	var resa = getScreenXYByPos(buf, a);
	var resb = b>=0 ? 
				getScreenXYByPos(buf, b) :
				getNextLineBreakScreenXY(buf);

	if (resa.e == null && resb.e == null) {
		//render nothing if resa and resb on "same side"
		var from = parseInt($lines(buf.id).dataset.from);
		if (resa.linenum < from &&  resb.linenum < from) {
			return;
		}

		var to = from+$lines(buf.id).childNodes.length;
		if (resa.linenum >= to &&  resb.linenum >= to) {
			return;
		}
	}

	if (resa.e == null) {
		var from = parseInt($lines(buf.id).dataset.from);
		resa = {
			top:from*lineHeight,
			left:0
		};
	}

	if (resb.e == null) {
		var from = parseInt($lines(buf.id).dataset.from);
		var to = from+$lines(buf.id).childNodes.length;
		resb = {
			top:(to-1)*lineHeight,
			left:0
		};
	}

	if (resa.top != resb.top) {
		var w = $buffer(buf.id).offsetWidth;
		append(resa.left, resa.top, (w-resa.left), lineHeight);
		var mh = resb.top-resa.top-lineHeight;
		if (mh > 0) {
			append(0, resa.top+lineHeight, w, mh);
		}
		append(0, resb.top, resb.left, lineHeight);
	} else {
		append(resa.left, resa.top, Math.abs(resa.left-resb.left), lineHeight);
	}
}

function renderCursor(localbuf, from, visibleLines) {
	var res = getScreenXYByPos(localbuf, localbuf.cursor);
	//out of screen
	if (res.e == null) return;

	//highlight current line number
	var currentLine = localbuf.currentLineNumber+1;
	var $g = $gutter(localbuf.id);
	if (localbuf.cursorLine != currentLine) {
		localbuf.cursorLine = currentLine;

		Array.prototype.forEach.call($g.childNodes, function(e) {
			if (parseInt(e.textContent) == currentLine) {
				addClass(e, 'highlight');
			} else {
				removeClass(e, 'highlight');
			}
		});
	}

	var alignright = (res.ch=='\t') && (localbuf.mode!=1); //insert mode always align left
	if (/\r|\n|\t/.test(res.ch)) {
		res.ch = ' ';
	}
	//console.log(res);
	var color = getComputedStyle(res.e.parentNode, null).color;
	var background = getComputedStyle(document.body, null).backgroundColor;
	//console.log(color);
	//console.log(background);
	var cursor = $cursor(localbuf.id);
	if (cursor.textContent != res.ch) {
		cursor.textContent = res.ch;
	}
	cursor.style.cssText = 'left:'+(res.left + (alignright ? res.width : 0) +'px;'  //right align
		+'margin-left:' + (((alignright?-1:0)-gutterWidth(localbuf.id)))+'ch') + ';'
		+'background-color:' + color + ';'
		+'color:' + background + ';'
		+'padding-bottom:2px;'
		+'top:' + res.top + 'px;';
}

function renderLineBuffer(buf) {
	var linebuf = buf['line-buffer'];
	var str = linebuf.str;
	var pos = linebuf.pos;
	var ex = $statusBuf(buf.id);

	//cursor
	if (str[str.length-1] == '\n') {
		ex.textContent = str.substring(0, str.length-1);

		$statusCursor(buf.id).style.display= 'none';
	} else {
		ex.textContent = str;

		var range = document.createRange();
		range.setStart(ex.firstChild, pos-1);
		range.setEnd(ex.firstChild, pos);
		var rect = range.getBoundingClientRect();
		var cursor = $statusCursor(buf.id)
		cursor.style.display = 'block';
		cursor.style.left = (rect.left+rect.width)+'px';
	}
}

function renderMode(buf, text) {
	$statusCursor(buf.id).style.display = 'none';
	
	var ex = $statusBuf(buf.id);
	ex.textContent = text;
}

function renderStatusBar(buf) {
	if (buf.message) {
		$statusCursor(buf.id).style.cssText = 'display:none';

		var ex = $statusBuf(buf.id);
		ex.textContent = buf.message;
	} else if (typeof buf['line-buffer'] != 'undefined') {
		renderLineBuffer(buf);
	} else {
		if (typeof buf.mode != 'undefined' && buf.mode < MODES.length) {
			renderMode(buf, MODES[buf.mode]);
			keymap = keymaps[buf.mode];
		}
	}
}

var MODES = ['-- NORMAL --', '-- INSERT --'];
var VISUAL_MODES = ['', '-- VISUAL --', '-- VISUAL LINE --', '-- VISUAL BLOCK']

