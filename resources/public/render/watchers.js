watchLocalbufChange('str', function(buf) {
    renderLines(buf);
    setSize(buf.id);
    console.log(buf.mode);
    renderHiddenInput(buf.mode);
})

watchLocalbufChange('changes', function(buf) {
    renderChanges(buf.changes);
});

watchLocalbufChange('tabsize', function(buf) {
    $tabsize(buf.tabsize);
});

watchLocalbufChange('message', function(buf) {
    $hide($statusCursor(buf.id));
    $hide($statusCursorSecond(buf.id));

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

watchLocalbufChange('showkeys', function(buf) {
    var keys = buf['showkeys'];
    if (keys && keys.length > 0) {
        $statusKeys(buf.id).textContent = keys.reverse().join('');
        if (keys[0] === null) {
            setTimeout(function() {
                if (buf.id == buffers.active.id) {
                    $statusKeys(buf.id).innerHTML = '';
                }
            }, 100);
        }
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
        ranges[0][1] = ranges[0][1] - 1;
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

function renderWindowTitle(buf, win) {
    document.title = buf.name + ' - ' + win.cwd;
}

watchLocalbufChange('name', function(buf) {
    renderWindowTitle(buf, buffers.window);
    $statusName(buf.id).textContent = buf.name;
});

watchLocalWindowChange('cwd', function(win) {
    if (buffers.active) {
        renderWindowTitle(buffers.active, win);
    }
});

watchLocalbufChange('line-buffer', function(buf) {
    var linebuf = buf['line-buffer'];
    var str = linebuf.str;
    var pos = linebuf.pos;
    var pos2 = linebuf.pos2;
    var ex = $statusBuf(buf.id);

    //cursor
    if (str[str.length - 1] == '\n') {
        ex.textContent = str.substring(0, str.length - 1);

        $hide($statusCursor(buf.id));
        $hide($statusCursorSecond(buf.id));
        buf.focusStatusBar = false;
    } else {
        ex.textContent = str;
        if (pos > str.length || pos == 0) {
            $hide($statusCursor(buf.id));
            $hide($statusCursorSecond(buf.id));
            buf.focusStatusBar = false;
            return;
        }

        function renderCursor($cursor, pos) {
            var range = document.createRange();
            range.setStart(ex.firstChild, pos - 1);
            range.setEnd(ex.firstChild, pos);
            var rect = range.getBoundingClientRect();
            $show($cursor);
            $cursor.style.left = (rect.left + rect.width) + 'px';
        }

        renderCursor($statusCursor(buf.id), pos);
        if (pos2) {
            renderCursor($statusCursorSecond(buf.id), pos2);
        } else {
            $hide($statusCursorSecond(buf.id))
        }

        buf.focusStatusBar = true;
    }
});

watchLocalbufChange('mode', function(buf) {
    renderMode(buf);
    keymap = keymaps[buf.mode];
    buf.focusStatusBar = false;
});

function renderHiddenInput(mode) {
    var NORMAL = 0,
        INSERT = 1;

    var input = $hiddenInput();
    if (mode == NORMAL) {
        input.blur();
        console.log("set blur");
        input.disabled = true;
    }

    if (mode == INSERT) {
        input.disabled = false;
        console.log("set focus");
        input.focus();
    }
}

function renderMode(buf) {
    var MODES = ['NORMAL', 'INSERT'];
    var SUBMODES = ['', '(insert)'];
    var VISUAL_MODES = ['', 'VISUAL', 'VISUAL LINE', 'VISUAL BLOCK']

    var mode = buf.mode;
    renderHiddenInput(mode);

    if (mode >= MODES.length) {
        return;
    }

    var submode = buf.submode;
    var visualtype = buf.visual.type;
    var text = '';

    $hide($statusCursor(buf.id));
    $hide($statusCursorSecond(buf.id));

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
    ex.textContent = '-- ' + text + ' --';
}
