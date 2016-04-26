function escapseKeys(keys) {
    return keys.replace(/([\\<>])/, '\\$1');
}

function imePreview(input) {
    var previewNode;
    var previewCursor;

    function removePreviewNode() {
        if (previewNode != null) {
            $remove(previewNode);
            previewNode = null;
            previewCursor = null;
        }
    }

    function setPreviewContent(buf, text) {
        if (previewCursor) {
            $remove(previewCursor);
        } else {
            previewCursor = createSpan();
            addClass(previewCursor, 'ime-cursor');
        }

        if (previewNode == null) {
            previewNode = createSpan();
            addClass(previewNode, 'ime-preview');
            previewNode.textContent = text;

            insertNodeAtPos(buf, previewNode, buf.cursor);
        } else {
            previewNode.textContent = text;
        }

        var pos = getCaret(input);
        var range = document.createRange();
        range.setStart(previewNode.firstChild, pos);
        range.setEnd(previewNode.firstChild, pos);

        range.insertNode(previewCursor);
        return previewNode;
    }

    return {
        onTyping: function() {
            var text = input.value;
            var buf = buffers.active;
            if (text.length > 0) {
                setPreviewContent(buf, text);
                addClass($cursor(buf.id), 'ime');
            } else {
                removePreviewNode();
                removeClass($cursor(buf.id), 'ime');
            }
        },
        onInput: function() {
            input.value = '';

            var buf = buffers.active;
            removePreviewNode();
            removeClass($cursor(buf.id), 'ime');
        }
    }
}

function keyboardInit() {
    var channel = connect('/socket');

    var terminalAlias = {
        '<c-[>': '<esc>',
        '<c-h>': '<bs>',
        '<c-i>': '<tab>',
        '<c-m>': '<cr>',
    };

    function isSpecialKey(ch) {
        if (ch.length <= 1) {
            return false;
        } else if (ch.length == 2) {
            return !is32BitsChar(ch);
        } else {
            return true;
        }
    }

    function handleKey(key) {
        if (key.length == 0) return;
        //console.log(key);

        if (isSpecialKey(key)) {
            key = '<' + escapseKeys(key) + '>';
        } else {
            key = escapseKeys(key);
        }

        keymap(key, function(k) {
            channel.send(terminalAlias[k] || k);
        });
    }


    var input = $hiddenInput();
    var imeHandler = imePreview(input);

    watchLocalbufChange('mode', function(buf) {
        var NORMAL = 0,
            INSERT = 1;

        var mode = buf.mode;
        if (mode == NORMAL) {
            console.log("set blur");
            input.blur();
            input.disabled = true;
        }

        if (mode == INSERT) {
            input.disabled = false;
            console.log("set focus");
            input.focus();
        }
    });

    function onkeydown(event) {
        var key = KEYCODE_KEYDOWN[event.keyCode];
        if (key || event.ctrlKey || event.altKey) {
            //console.log(event);

            if (event.ctrlKey) {
                if (event.keyCode != 0 && event.keyCode != 16 && event.keyCode != 17 && event.keyCode != 18) {
                    var prefix = 'c-';
                    if (event.shiftKey) {
                        prefix += 's-'
                    }
                    if (event.altKey) {
                        prefix += 'a-';
                    }

                    if (KEYCODE_DIC[event.keyCode]) {
                        key = prefix + KEYCODE_DIC[event.keyCode];
                    }
                }
            } else if (event.shiftKey) {
                if (KEYCODE_DIC[event.keyCode]) {
                    key = 's-' + KEYCODE_DIC[event.keyCode];
                }
            }

            if (key) {
                event.preventDefault();
                handleKey(key);
            }
        }
    }


    var iOS =
        (function() {
            var ua = navigator.userAgent;
            return (ua.indexOf('iPad') >= 0 || ua.indexOf('iPhone') >= 0) && ua.indexOf('Safari') >= 0;
        })();

    function onkeypress(event) {
        if (iOS) {
            var code = event.keyCode || event.charCode || event.which;;
            //FIXME: is this correctly??
            if (code == 85 && event.keyIdentifier == '') {
                handleKey('esc');
                event.preventDefault();
                return;
            }
        }
    }

    document.body.onclick = input.focus;

    document.addEventListener('keydown', onkeydown);
    document.addEventListener('keypress', function(event) {
        onkeypress(event);
        if (!event.defaultPrevented) {
            handleKey(String.fromCharCode(event.keyCode));
        }
    });

    input.addEventListener('keydown', function(event) {
        event.stopPropagation();
        onkeydown(event);
    });

    input.addEventListener('keypress', function(event) {
        event.stopPropagation();
        onkeypress(event);
    });

    input.addEventListener('input', function(event) {
        //FIXME: without setTimeout here, getCaret() will always return 0, not sure why. 
        setTimeout(imeHandler.onTyping, 0);
    });

    input.addEventListener('textInput', function(event) {
        var key = escapseKeys(event.data);
        keymap(key, function(k) {
            channel.send(terminalAlias[k] || k);
        });

        //FIXME: without setTimeout here, input.value = '' will not work, not sure why. 
        setTimeout(imeHandler.onInput, 0);
    });
}
