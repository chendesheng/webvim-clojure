function $newBuffer(bufid) {
    var ele = document.createElement('DIV');
    ele.id = 'buffer-' + bufid;
    ele.className = 'buffer';
    var tmpl = '<div id="gutter-{id}" class="gutter">' + '</div>' +
        '<div id="content-{id}" class="content">' +
        '<div id="cursor-{id}" class="cursor">&nbsp;</div>' +
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
        var input = $hiddenInput();

        $buf = $newBuffer(bufid);

        var cursor = $cursor(bufid);
        if (input.parentNode != cursor) {
            $remove(input);
            cursor.appendChild(input);
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

function $hiddenInput() {
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

function hasClass(ele, cls) {
    return (new RegExp('\b' + cls + '\b')).test(ele.className);
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

function getCaret($txt) {
    return $txt.selectionStart;
}

function createSpan() {
    return document.createElement('SPAN');
}

var beep = (function() {
    //convert from /System/Library/Sounds/Funk.aiff
    var snd = new Audio("data:audio/mpeg;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU1LjIyLjEwMgAAAAAAAAAAAAAA//uUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAAcAAAAMAAATgAAnJycnJycnJzs7Ozs7Ozs7Tk5OTk5OTk5iYmJiYmJiYmJ2dnZ2dnZ2domJiYmJiYmJnZ2dnZ2dnZ2dsbGxsbGxsbHExMTExMTExNjY2NjY2NjY2Ozs7Ozs7Ozs//////////9MYXZmNTUuMjIuMTAyAAAAAAAAAAAkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA//uUZAAO4plkLxHhU3AmC6aCAAJvCHWOyCkA7cD8MdiAECm4FEWF5ZFvSzJSmKXv/9xuQAf/8CAAAFx5//1PPP/////Z//t8/+3zKn/7oY3PPP9T7ZmYZ3QxXVzDB+RiLFsfiLEWIsRYNgXhYQYN4hxcAoFIKhIAANf//////////V//tnfO/kIxGPdGO5zn0IEABDIQDAzqBucoyHhlwtHA0g1L3f1///3009tvdN/rru/dE9asb0pehisxprNz0ZHcxh+Yq2Y4ojErjImkSIoIlXEQiOkQEikfEYHxIDQ8gWmK6c/6//9f9f/7t6/fsfN30Wnr0dqJQw5mMKkBjnCqcxWdQic4xSyEY0GBE7A8NAuHFsYjoCgFAejM8PR1Cqs0qIfeGECGEBJ8a5n//3//////+///T+////9WajrtojyJphNxoaRLnDLnlI0IGikSTDyrge4ORCQHC4sBIUoAxR3GT6/////X//6r527GvvrRTrzTjFnKiUmHmnXQ//uUZDAP4glkMgNAO3BF7IYxBAduCRWQyA2A7cD1MZlIEB2585nTMNHXMKE7jhQYjIsHR0bDYJTgLiQIwORcNB0QsecE4nEAEBYWMGlTErYDF5hCwY8As1pcv//5/////1///+v/Zr99/S7PRHmV1pVZhQk9UKDU0w9kMMKj5wtQdJwjKiwSxwKgJOBYKhwdJCggaNHL35f////79f+vzLtWd06JaZNm3nMrnFt2mPNJs42Hx4xmqcZONSeOzRuSCNmHxsOuXJA8HA2BAbgGlEQAGoBA8esdAY3Ah8YsWgZuNAIDHTkyyDpPBDgKRA0YUGRpAQZIAiQAAFFUzxbFCdbkWxhMg/2ZCF2tqx6zw53kV/JEhzv7RMgQBiMgfCI4clja6SBx9XHOX/7///xyhb+h4qoXAJ4+GQWAAZKElhz//////6bd116ZOzZrWqlHosm/KyO6IUy0uy1GVVyKggVxBBo8VAosKqKMQg5hEBREUMgiLF8YOwaBg7CVmQuW//uUZFeO80oYtxN5ecA6rHZSBAVuE9x04lXtAAhQAFtCgCAA2Yn5whpxEmGNORWY+4cJi0CjGCSCmYGAKAQAkLBaOsYKYGJgKgsmByBeYKoE5gUgamDKgWWc1acMmbSMEUE7mRr1VtdRdz4uq4UXcmOQ7EqOM1JdMzsqwprtmtfpcLVbtXAqDJosooq2ixlua2x+v4VQoppTSTfNSDSTaKWY41pLubzmP///jerf/+t/6P2bEMqI7//////////////7EvtEJQTiziBCMPaEXDKiBMMzWknGMy6QrTN/JKk+89zxMKsADTAYgT8xWsClMAKA/jAniA4xfsDTMDEAbzAuwA0wN4ASKAJYweIEyMB0BLQMgBwQMow0wM1gxwMQYxgNKo1wMpA1wbwYXU4WMBv4sz8kBShT/83IOkgaf/mZum8w//zSmnTL///uoihUVo///5Nm6i42ym9PSS/XqXebLNrU1vUTBUWbm5odV////+7N////+imYIAqAygOF//uUZFoABXGBNoZ+wABPMCZywJwADixFDrn9AAB1AF8PACACJ/6////76f///rZ/1XuqL////6c5D2R0tTmGo7bLrNPdDLTEHBWZGk6lDDCaBEUDXMSVViomYIwwA4AP////8WhKS/////iMBEDQAQAkxbofwMeaEkjJszWY2DRlxMTFcxz480WowFcGhMApANjHzwDIwEoAZMcXBeDAdQAIHAGqa5gNYCeVQCgwHcAhEQAuhPFgRzxIYkAwEFGZZWvcwqUUQvWwXSBA6GTjsaKmU////sVkU/d7v///SAAKJALLGh/////6CaP1Xo/0B59/igjUWc81/0/VMdsOMzMpFns+BGxoMeO/8TfewmIyKskAMPvB2TB1AKgwacCOMFtCPDBxQEgwOICXMBLAngUBJGALgFhgGIAOBgCEu4HEXOckp6E6IMSYlKjRqma1a9XSFKVmjQYuN7riu/C2XAQCq//v/16TNRL///7/XrAOKP/////9qv//9H/avv16//uUZEIP45wZQQd94AAcYAbS4IgACXAxBg/15MCMMNpEEAm5dPNwE0egKhEEBhEkYZWEbGRTG+J8uhwkaBIygHHriIBiZwNacFlYaaBKZShOYOiyYDiMAAVR2RfakEMPs3wW6bH2q1a9EQ4OiUYeSv///69X+jWS///9TJv///////6p6ft/1//t3+n+u/r6s6TmsidYRZxRDDChTWGKMFSDezC5zXYy49zbMlD+qDEAirAwIMIdMymAoXiw8DAROsMHCgbMMBwaLAVosA2iUCFjsce6xUpMCgIOB8/W/xT////5fkv////VqlAAAAACthSf///////jC8balTLPbq+LataHp9fJCjxa0XEAwwjYGeMSRRAT7Ly/Yzv2t+OYgB2DEqQdswdwC0MEtAozhjcMcHgwOGzFAOCoRdtkbNmAGAgMo+PA8AgIMCzKKkjLLU06KAFQ7Ytujfv/1f/VrJf/3u/3euBP//////ttRt/////pPf/6ajCWQ5YxS5LT//uUZHCP4l4NQYP7wTAjwBbqBAIACvhBBg/xBwBEAB5IAAgGP0eawDGANd05e4VjMRjBWD2CfNJhUzgIDEQUMFCUAhqzAqfDQXSHmEc2Fl43N7+tazv5hfv+N//+v/6GShL/+v/X8qBmCX//////6f9F/dvprW/pvp//pb7e9KWS1FUYRpmGnsVbggsLCjApAyswohG2NsKcnzMbuE8xbUg9MGIChDUtpNGNIzmkzLY5MkBYSKaIkHvS3EuLQdxuCiwDQNHv//////p6P////VKGiX////9r/VP/Stv1//66////6f2bvSyFOjEBqUSKQYlHakxBTUUzLjk5LjWqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqjB6gZkxdMdoP6jI0DKNFTs42MMhMOCAujuZ3NbFAzOSzE45MHBowGB30Q5oxo8gwvjt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VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV//uUZOQA9MlVuotsG3IjjAezAAJvjNSSfySwyUgGAEAA8AAEVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV");
    return function() {
        snd.play();
    };
})();
