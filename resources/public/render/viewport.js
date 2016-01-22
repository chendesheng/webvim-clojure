function setSize(bufid) {
	var zoom = window.innerWidth/document.body.offsetWidth;
	var pageh = $buffer(bufid).offsetHeight;
	var sw = pageh*zoom;
	var w = Math.floor($content(bufid).offsetWidth*zoom/10);
	var h = Math.floor((window.innerHeight-$statusBar(bufid).offsetHeight)/lineHeight);
	$content(bufid).style.paddingBottom = (pageh-lineHeight*2.1) + 'px'; //scroll beyond last line, leave at least one line. 1 line + 1.1 line status bar.
	return {width: w, height: h}
}

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

function renderViewport(paddinglines, scrolling){
	paddinglines = paddinglines || 5;

	var buf = buffers.active;
	var bufid = buf.id;
	var $buf = $buffer(bufid);
	var from = Math.floor($buf.scrollTop/lineHeight)-paddinglines;
	from = from < 0 ? 0 : from;
	var offscreenLines = buf.offscreenLines;
	var h = viewport.height+1+paddinglines*2;
	var lines = $lines(bufid);


	var offscreen = offscreenLines.childNodes[from];
	var lastline;
	var cnt = 0;
	var usedIds = {};
	while(offscreen && !offscreen.className.contains('end-code') && cnt < h) {
		var line = document.getElementById(offscreen.id);
		var linenum = from+cnt;
		if (line) {
			if (buf.dirtyLines[line.id]) {
				var newline = offscreen.cloneNode(true);
				usedIds[newline.id] = true;
				lines.replaceChild(newline, line);

				lastline = newline;
				delete buf.dirtyLines[line.id]
			} else {
				usedIds[offscreen.id] = true;
				lastline = line;
			}
		} else {
			newline = offscreen.cloneNode(true);
			usedIds[newline.id] = true;
			insertAfter(lines, newline, lastline);

			lastline = newline;
		}

		offscreen = offscreen.nextSibling;
		cnt++;
	}

	removeUnused(lines, usedIds);

	lines.style.top = from*lineHeight+'px';
	lines.dataset.from = from;
	$content(bufid).style.height = offscreenLines.childNodes.length*lineHeight+'px';

	renderGutter(buf, from, h)
	renderCursor(buf, from, h);

	var $sel = $selections(bufid);
	if (buf.selections && buf.selections.length>0) {
		renderSelections($selections(bufid), buf, buf.selections);
	} else {
		$empty($sel);
	}

	var $high = $highlights(bufid);
	if (buf.highlights && buf.highlights.length>0) {
		renderSelections($highlights(bufid), buf, buf.highlights);
	} else {
		$empty($high);
	}

	var $brackets = $cursorBracket(bufid);
	if (buf.brackets && buf.brackets.length>0) {
		renderSelections($brackets, buf, buf.brackets);
	} else {
		$empty($brackets);
	}

	if (!scrolling) {
		renderAutocompl(buf);
	}
}

var __renderTimer;
function onscrollRender() {
	if (__renderTimer) clearTimeout(__renderTimer);

	if (buffers.active.linecnt > 3000) {
		__renderTimer = setTimeout(function(){
			renderViewport(15, true);
		}, 10);
	} else {
		renderViewport(10, true);
	}
}

function scrollToCursor(buf, instant) {
	var bufid = buf.id;
	var $cur = $cursor(bufid);
	var $buf = $buffer(bufid);
    
	var oldst = $buf.scrollTop;
	var newst = (buf['scroll-top']||0) * lineHeight;
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


