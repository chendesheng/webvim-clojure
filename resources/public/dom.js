function $newBuffer(bufid) {
	var ele = document.createElement('DIV');
	ele.id = 'buffer-'+bufid;
	ele.className = 'buffer';
	var tmpl = '<div id="gutter-{id}" class="gutter">'
			+'</div>'
			+'<div class="content">'
				+'<div id="cursor-{id}" class="cursor">'
				+'</div>'
				+'<div id="selections-{id}" class="selections">'
				+'</div>'
				+'<div id="highlights-{id}" class="highlights">'
				+'</div>'
				+'<div id="autocompl-{id}" class="autocompl">'
				+'</div>'
				+'<div id="cursor-brace-{id}" class="cursor-brace">'
				+'</div>'
				+'<div id="lines-{id}" class="lines">'
				+'</div>'
			+'</div>'
			+'<div id="status-bar-{id}" class="status-bar">'
				+'<span id="status-bar-ex-{id}" class="ex">'
				+'</span>'
				+'<span id="status-bar-keys-{id}" class="ongoing-keys">'
				+'</span>'
				+'<span id="status-bar-name-{id}" class="buf-name">'
				+'</span>'
			+'</div>';
	ele.innerHTML = replaceBinding(tmpl, {'id': bufid});
	document.body.appendChild(ele);
}

function $buffer(bufid) {
	var id = 'buffer-'+bufid;
	var $buf = document.getElementById(id);
	if (!$buf) {
		$newBuffer(bufid);
		$buf = document.getElementById(id);
	}

	return $buf;
}

function _$bufid(prefix, bufid) {
	var id = prefix+bufid;
	return document.getElementById(id);
}

function $lines(bufid) {
	return _$bufid('lines-', bufid);
}

function $gutter(bufid) {
	return _$bufid('gutter-', bufid);
}

function $statusBar(bufid) {
	return _$bufid('status-bar-', bufid);
}

function $selections(bufid) {
	return _$bufid('selections-', bufid);
}

function $highlights(bufid) {
	return _$bufid('highlights-', bufid);
}

function $cursorBrace(bufid) {
	return _$bufid('cursor-brace-', bufid);
}

function $cursor(bufid) {
	return _$bufid('cursor-', bufid);
}

function $statusEx(bufid) {
	return _$bufid('status-bar-ex-', bufid);
}

function $statusKeys(bufid) {
	return _$bufid('status-bar-keys-', bufid);
}

function $statusName(bufid) {
	return _$bufid('status-bar-name-', bufid);
}

function $statusCursor(bufid) {
	return _$bufid('status-bar-cursor-', bufid);
}

function $autocompl(bufid) {
	return _$bufid('autocompl-', bufid);
}

function $autocomplHeight(bufid) {
	return document.getElementById('autocompl-'+bufid+'-highlight');
}

function $remove(ele) {
	if (!ele || !ele.parentNode) return;
	ele.parentNode.removeChild(ele);
}

function $lineNumber(bufid, linenum) {
	return document.getElementById('line-'+bufid+'-'+linenum);
}

function _timerScroll(ele, scrollto, i) {
	if (i > 1) {
		ele.scrollTop = (ele.scrollTop+scrollto)/2;
		setTimeout(function() {
			_timerScroll(ele, scrollto, i-1);
		}, 15);  // 1000/15=66.66 frames per second
	} else {
		ele.scrollTop = scrollto;
	}
}

function $animateScroll(ele, scrollto) {
	_timerScroll(ele, scrollto, 5);
}
