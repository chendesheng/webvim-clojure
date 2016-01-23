watchLocalbufChange('str', function (buf) {
	renderLines(buf);
	setSize(buf.id);
})

watchLocalbufChange('changes', function (buf) {
	renderChanges(buf.changes);
});

watchLocalbufChange('tabsize', function(buf) {
	$tabsize(buf.tabsize);
});

watchLocalbufChange('message', function(buf) {
	$statusCursor(buf.id).style.cssText = 'display:none';

	var ex = $statusBuf(buf.id);
	ex.textContent = buf.message;

	if (buf.message == "") {
		renderMode(buf);
	}
	buf.focusStatusBar = false;
});

watchLocalbufChange('dirty', function(buf) {
	var statusName = $statusName(buf.id);
	if (!!buf.dirty) {
		statusName.className = 'buf-name buf-unsaved';
	} else {
		statusName.className = 'buf-name';
	}
});

watchLocalbufChange('keys', function(buf) {
	if (buf.keys && buf.keys.length > 0 && buf.keys[0] != ':') {
		$statusKeys(buf.id).textContent = buf.keys.reverse().join('');
	} else {
		$statusKeys(buf.id).innerHTML = '';
	}
});

watchLocalbufChange('visual', function(buf) {
	if (buf.visual.type == 0) {
		renderMode(buf);
		buf.selections = [];
	} else if (buf.visual.type == 2) {
		var ranges = buf.visual.ranges;
		ranges[0][1] = ranges[0][1]-1;
		buf.selections = buf.visual.ranges;
		renderMode(buf);
	} else {
		buf.selections = buf.visual.ranges;
		renderMode(buf);
	}
});

watchLocalbufChange('brackets', function(buf) {
	$cursorBracket(buf.id).innerHTML = '';
	buf.brackets = buf.brackets || [];
	var brackets = [];
	for (var i = 0; i < buf.brackets.length; i++) {
		var pt = buf.brackets[i];
		//skip cursor, don't draw twice at the same point
		if (buf.cursor == pt) continue; 

		brackets.push([pt, pt]);
	}

	buf.brackets = brackets;
});

watchLocalbufChange('name', function(buf) {
	document.title = buf.name;
	$statusName(buf.id).textContent = buf.name;
});

watchLocalbufChange('line-buffer', function (buf) {
	var linebuf = buf['line-buffer'];
	var str = linebuf.str;
	var pos = linebuf.pos;
	var ex = $statusBuf(buf.id);

	//cursor
	if (str[str.length-1] == '\n') {
		ex.textContent = str.substring(0, str.length-1);

		$statusCursor(buf.id).style.display= 'none';
		buf.focusStatusBar = false;
	} else {
		ex.textContent = str;
		if (pos > str.length || pos == 0) {
			$statusCursor(buf.id).style.display= 'none';
			buf.focusStatusBar = false;
			return;
		}

		var range = document.createRange();
		range.setStart(ex.firstChild, pos-1);
		range.setEnd(ex.firstChild, pos);
		var rect = range.getBoundingClientRect();
		var cursor = $statusCursor(buf.id)
		cursor.style.display = 'block';
		cursor.style.left = (rect.left+rect.width)+'px';
		buf.focusStatusBar = true;
	}
});

watchLocalbufChange('mode', function(buf) {
	renderMode(buf);
	keymap = keymaps[buf.mode];
	buf.focusStatusBar = false;
});

function renderMode(buf) {
	var MODES = ['NORMAL', 'INSERT'];
	var SUBMODES = ['', '(insert)'];
	var VISUAL_MODES = ['', 'VISUAL', 'VISUAL LINE', 'VISUAL BLOCK']

	var mode = buf.mode;
	if (mode >= MODES.length) {
		return;
	}

	var submode = buf.submode;
	var visualtype = buf.visual.type;
	var text = '';

	$statusCursor(buf.id).style.display = 'none';

	if (submode == 0 && visualtype == 0) {
		text = MODES[mode];
	} else if (submode == 0 && visualtype > 0) {
		text = VISUAL_MODES[visualtype];
	} else if (submode > 0) {
		text = SUBMODES[submode];
		if (visualtype > 0) {
			text += ' ' + VISUAL_MODES[visualtype];
		}
	}
	
	var ex = $statusBuf(buf.id);
	ex.textContent = '-- '+text+' --';
}

