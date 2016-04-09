function renderCursor(buf, from, visibleLines) {
    var res = getScreenXYByPos(buf, buf.cursor);
    //out of screen
    if (res.e == null) return;

    //highlight current line number
    var currentLine = buf.currentLineNumber + 1;
    var $g = $gutter(buf.id);
    if (buf.cursorLine != currentLine) {
        buf.cursorLine = currentLine;

        Array.prototype.forEach.call($g.childNodes, function(e) {
            if (parseInt(e.textContent) == currentLine) {
                addClass(e, 'highlight');
            } else {
                removeClass(e, 'highlight');
            }
        });
    }

    var alignright = (res.ch == '\t') && (buf.mode != 1); //insert mode always align left
    if (/\r|\n|\t/.test(res.ch)) {
        res.ch = ' ';
    }
    //console.log(res);
    var style = getComputedStyle(res.e.parentNode, null);
    var color = style.color;
    var fontStyle = style.fontStyle;
    var fontWeight = style.fontWeight;

    var background = getComputedStyle(document.body, null).backgroundColor;
    //console.log(color);
    //console.log(background);
    var cursor = $cursor(buf.id);
    if (cursor.textContent != res.ch) {
        cursor.textContent = res.ch;
    }

    function colorStyle() {
        if (buf.focusStatusBar) {
            return 'border:solid 1px ' + color + ';' + 'color:rgba(0,0,0,0);';
        } else {
            return 'background-color:' + color + ';' + 'color:' + background + ';' + 'font-style:' + fontStyle + ';' + 'font-weight:' + fontWeight + ';';
        }
    }
    var x = res.left + (alignright ? res.width : 0);
    var y = res.top;
    var marginLeft = (((alignright ? -1 : 0) - gutterWidth(buf.id)));

    cursor.style.cssText = 'left:' + (x + 'px;' +
            (buf.focusStatusBar ? ('width:' + cursor.offsetWidth - 4 + 'px;' + 'height:' + cursor.offsetHeight - 4 + 'px;') : '') +
            'margin-left:' + marginLeft + 'ch') + ';' +
        'top:' + y + 'px;' + colorStyle();

    if (buf.focusStatusBar) {
        cursor.style.width = cursor.offsetWidth - 4 + 'px';
        cursor.style.height = cursor.offsetHeight - 4 + 'px';
    }

    var input = $hiddenInput();
    input.style.left = x + 'px';
    input.style.top = y + 'px';
    input.style.marginLeft = marginLeft + 'ch';
}
