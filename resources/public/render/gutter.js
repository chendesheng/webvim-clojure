var __gutterWidth;

function gutterWidth(bufid, linenum) {
    if (linenum) {
        var w = 0;
        while (linenum > 0) {
            linenum = Math.floor(linenum / 10);
            w++;
        }

        __gutterWidth = w;
        $gutter(bufid).style.width = (__gutterWidth + 2) + 'ch';
    } else {
        return __gutterWidth + 2; //left padding 1ch, right padding 1ch
    }
}

function renderGutter(buf, from, visibleLines) {
    var $g = $gutter(buf.id);
    $g.style.top = from * lineHeight + 'px';

    if (visibleLines == $g.childNodes.length && visibleLines > 0 && from == parseInt($g.firstChild.textContent) - 1
        //linenum does not change or out of screen
        && (buf.lastlinecnt == buf.linecnt ||
            (from + visibleLines < buf.lastlinecnt && from + visibleLines < buf.linecnt))) {
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
                $remove($g.firstChild);
            }
        }
    }

    //console.log("equal? ", visibleLines == $g.childNodes.length, visibleLines, $g.childNodes.length);

    Array.prototype.forEach.call($g.childNodes, function(e, i) {
        var linenum = i + from + 1;
        //console.log(linenum);
        if (linenum > buf.linecnt) {
            $hide(e);
        } else {
            e.textContent = linenum;
            if (linenum == buf.cursorLine) {
                addClass(e, 'highlight');
            } else {
                removeClass(e, 'highlight');
            }
            $show(e);
        }
    });

    if (buf.lastlinecnt != buf.linecnt) {
        buf.lastlinecnt = buf.linecnt;
        gutterWidth(buf.id, buf.linecnt);
    }
}
