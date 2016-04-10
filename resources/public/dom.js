function $newBuffer(bufid) {
    var ele = document.createElement('DIV');
    ele.id = 'buffer-' + bufid;
    ele.className = 'buffer';
    var tmpl = '<div id="gutter-{id}" class="gutter">' + '</div>' +
        '<div id="content-{id}" class="content">' +
        '<div id="cursor-{id}" class="cursor"></div>' +
        '<div id="selections-{id}" class="selections"></div>' +
        '<div id="highlights-{id}" class="highlights"></div>' +
        '<div id="autocompl-{id}" class="autocompl"></div>' +
        '<div id="cursor-bracket-{id}" class="cursor-bracket"></div>' +
        '<div id="lines-{id}" class="lines"></div>' +
        '</div>' +
        '<div id="status-bar-{id}" class="status-bar">' +
        '<span id="status-bar-buf-{id}" class="ex"></span>' +
        '<span id="status-bar-cursor-{id}" class="cursor"></span>' +
        '<span id="status-bar-cursor-second-{id}" class="cursor second-cursor"></span>' +
        '<span id="status-bar-keys-{id}" class="ongoing-keys"></span>' +
        '<span id="status-bar-name-{id}" class="buf-name"></span>' +
        '</div>' +
        '<div id="ex-autocompl-{id}" class="autocompl ex-autocompl"></div>';
    ele.innerHTML = replaceBinding(tmpl, {
        'id': bufid
    });
    document.body.appendChild(ele);
    return ele;
}

function $buffer(bufid) {
    var id = 'buffer-' + bufid;
    var $buf = document.getElementById(id);
    if (!$buf) {
        $buf = $newBuffer(bufid);

        var input = $hiddenInput();
        var content = $content(bufid);
        if (input.parent != content) {
            $remove(input);
            content.appendChild(input);
        }
    }

    return $buf;
}

function _$bufid(prefix, bufid) {
    var id = prefix + bufid;
    return document.getElementById(id);
}

function $lines(bufid) {
    return _$bufid('lines-', bufid);
}

function $content(bufid) {
    return _$bufid('content-', bufid);
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

function $cursorBracket(bufid) {
    return _$bufid('cursor-bracket-', bufid);
}

function $cursor(bufid) {
    return _$bufid('cursor-', bufid);
}

function $hiddenInput(bufid) {
    return document.getElementById('hidden-input');
}

function $statusBuf(bufid) {
    return _$bufid('status-bar-buf-', bufid);
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

function $statusCursorSecond(bufid) {
    return _$bufid('status-bar-cursor-second-', bufid);
}

function $autocompl(bufid) {
    return _$bufid('autocompl-', bufid);
}

function $autocomplHeight(bufid) {
    return document.getElementById('autocompl-' + bufid + '-highlight');
}

function $exAutocompl(bufid) {
    return _$bufid('ex-autocompl-', bufid);
}

function $exAutocomplHeight(bufid) {
    return document.getElementById('ex-autocompl-' + bufid + '-highlight');
}

function $remove(ele) {
    if (typeof ele.remove === 'function') {
        ele.remove();
    } else {
        if (!ele || !ele.parentNode) return;
        ele.parentNode.removeChild(ele);
    }
}

function $lineNumber(bufid, linenum) {
    return document.getElementById('line-' + bufid + '-' + linenum);
}

window.requestAnimationFrame = window.requestAnimationFrame ||
    function(fn) {
        return setTimeout(fn, 1000 / 60);
    };

function _timerScroll(ele, scrollto, i) {
    if (i == 0) {
        ele.scrollTop = scrollto;
    } else {
        ele.scrollTop = (ele.scrollTop + scrollto) / 2;
        window.requestAnimationFrame(function() {
            _timerScroll(ele, scrollto, --i);
        });
    }
}

function $animateScroll(ele, scrollto) {
    _timerScroll(ele, scrollto, 5);
}

function $tabsize(tabsize) {
    if (tabsize) {
        document.body.style.tabSize = tabsize;
    } else {
        return document.body.style.tabSize;
    }
}

function $hide(ele) {
    if (ele) ele.style.display = 'none';
}

function $show(ele, display) {
    if (ele) ele.style.display = display || '';
}

function insertAfter(p, newElement, targetElement) {
    if (!targetElement) {
        p.insertBefore(newElement, p.firstChild);
        return;
    }

    if (p.lastChild == targetElement) {
        p.appendChild(newElement);
    } else {
        p.insertBefore(newElement, targetElement.nextSibling);
    }
}

function addClass(ele, cls) {
    if (ele.classList) {
        ele.classList.add(cls);
    } else if (ele.className.indexOf(cls) < 0) {
        ele.className += ' ' + cls;
    }
}

function removeClass(ele, cls) {
    if (ele.classList) {
        ele.classList.remove(cls);
    } else if (ele.className.indexOf(cls) >= 0) {
        ele.className = ele.className.replace(cls, '');
    }
}


function $empty(ele) {
    while (ele.firstChild) {
        $remove(ele.firstChild)
    }
}

function removeUnused($p, usedIds) {
    var i = $p.firstChild;
    while (i) {
        var prev = i;
        i = i.nextSibling;
        if (!usedIds[prev.id]) {
            $remove(prev);
        }
    }
}
