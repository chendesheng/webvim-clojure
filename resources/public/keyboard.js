function escapseKeys(keys) {
    return keys.replace(/([\\<>])/, '\\$1');
}

var imeHandler = (function() {
    var input = $hiddenInput();
    var previewNode;

    function removePreviewNode() {
        if (previewNode != null) {
            $remove(previewNode);
            previewNode = null;
        }
    }

    function getPreviewNode(buf) {
        if (previewNode == null) {
            previewNode = document.createElement('SPAN');
            previewNode.id = 'ime-preview';
            insertNodeAtPos(buf, previewNode, buf.cursor);
        }
        return previewNode;
    }

    return {
        onTyping: function(text) {
            var buf = buffers.active;
            if (text.length > 0) {
                var pn = getPreviewNode(buf);
                pn.textContent = text;

                addClass($cursor(buf.id), 'ime');
            } else {
                removePreviewNode();
                removeClass($cursor(buf.id), 'ime');
            }
        },
        onInput: function() {
            var buf = buffers.active;
            removePreviewNode();
            removeClass($cursor(buf.id), 'ime');
        }
    }
})();

function keyboardInit() {
    var channel = connect("/socket");

    var terminalAlias = {
        'c-[': 'esc',
        'c-h': 'bs',
        'c-i': 'tab',
        'c-m': 'cr',
    };

    function handleKey(key) {
        //console.log(key);

        if (key.length > 1) {
            key = '<' + escapseKeys(key) + '>';
        } else {
            key = escapseKeys(key);
        }

        keymap(key, function(k) {
            channel.send(terminalAlias[k] || k);
        });
    }


    var input = $hiddenInput();
    input.focus();

    function onkeydown(event) {
        event.stopPropagation();

        var key = KEYCODE_KEYDOWN[event.keyCode];
        if (key || event.ctrlKey || event.altKey) {
            //console.log(event);

            if (event.ctrlKey) {
                if (event.keyCode != 0 && event.keyCode != 16 && event.keyCode != 17 && event.keyCode != 18) {
                    var prefix = "c-";
                    if (event.shiftKey) {
                        prefix += "s-"
                    }
                    if (event.altKey) {
                        prefix += "a-";
                    }
                    key = prefix + KEYCODE_DIC[event.keyCode];
                }
            } else if (event.shiftKey) {
                key = "s-" + KEYCODE_DIC[event.keyCode];
            }

            if (key) {
                event.preventDefault();
                handleKey(key);
            }
        }
    }


    function iOS() {
        var ua = navigator.userAgent;
        return (ua.indexOf('iPad') >= 0 || ua.indexOf('iPhone') >= 0) && ua.indexOf('Safari') >= 0;
    }

    function onkeypress(event) {
        event.stopPropagation();
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

    document.body.onclick = function() {
        input.focus();
    };

    document.addEventListener('keydown', function(event) {
        onkeydown(event);
    });

    document.addEventListener('keypress', function(event) {
        onkeypress(event);
        if (!event.defaultPrevented) {
            handleKey(String.fromCharCode(event.keyCode));
        }
    });

    input.addEventListener('keydown', function(event) {
        onkeydown(event);
        console.log('textdarea keydown:' + input.value);

        imeHandler.onTyping(input.value);

        if (input.value.length > 0) {
            addClass(input, 'ime');
        } else {
            removeClass(input, 'ime');
        }
    });

    input.addEventListener('keypress', function(event) {
        onkeypress(event);
        //console.log('textdarea keypress');
    });

    input.addEventListener('textInput', function(event) {
        console.log(event);
        channel.send(event.data);
        setTimeout(function() {
            input.value = '';
            imeHandler.onInput();
            removeClass(input, 'ime');
        }, 10);
    });
}
