function outOfRange(ele) {
    return ele == null || !/\bcode-block\b/.test(ele.className);
}

function endCode(ele) {
    return ele != null && /\bend\-code\-block\b/.test(ele.className);
}

function getCodeBlockByPos(buf, pos) {
    var i = buf.pos;
    var ele = buf.currentBlock;
    var num = buf.currentBlockNumber;
    var linenum = buf.currentLineNumber;
    if (i <= pos) {
        while (!endCode(ele)) {
            var j = i + ele.textContent.length;
            if (i <= pos && pos < j) {
                break;
            }

            i = j;
            num++;
            linenum += ele.textContent.count('\n');
            ele = ele.nextSibling;
        }

        buf.currentBlock = ele;
        buf.pos = i;
        buf.currentBlockNumber = num;
        buf.currentLineNumber = linenum;
        return {
            e: ele,
            pos: i,
            num: num,
            linenum: linenum
        }
    } else {
        while (!outOfRange(ele)) {
            num--;
            ele = ele.previousSibling;
            linenum -= ele.textContent.count('\n');
            var j = i - ele.textContent.length;

            if (j <= pos && pos < i) {
                buf.currentBlock = ele;
                buf.pos = j;
                buf.currentBlockNumber = num;
                buf.currentLineNumber = linenum;
                return {
                    e: ele,
                    pos: j,
                    num: num,
                    linenum: linenum
                }
            }

            i = j;
        }
    }

    return null;
}

function getElementByPos(buf, pos) {
    var res = getCodeBlockByPos(buf, pos);
    if (res == null) return null;
    if (endCode(res.e)) return {
        e: res.e,
        offset: 0
    };

    var ele = res.e.firstChild;
    var i = res.pos;
    var j = 0;
    while (true) {
        var l = ele.textContent.length;
        if (i <= pos && pos < i + l) {
            break;
        }

        i += l;
        ele = ele.nextSibling;
        j++;
    }

    if (ele.nodeType != 3) {
        ele = ele.firstChild;
    }

    return {
        e: ele,
        offset: pos - i,
        index: j
    };
}

function getLine(bufid, linenum) {
    var lines = $lines(bufid)
    var line;
    var e = lines.firstChild;
    var i = parseInt(lines.dataset.from) || 0;
    i = i < 0 ? 0 : i;
    while (e) {
        if (i == linenum) {
            line = e;
            break;
        }
        e = e.nextSibling;
        i++;
    }
    return line;
}

function getNextLineBreakElement(buf, pos) {
    var res = getCodeBlockByPos(buf, pos);
    if (res == null) return null;
    if (endCode(res.e)) return {
        e: res.e,
        offset: 0
    };

    var ele = res.e.firstChild;
    var i = 0;
    var offset;
    while (true) {
        var l = ele.textContent.length;
        offset = ele.textContent.indexOf('\n');
        if (offset >= 0) {
            break;
        }

        ele = ele.nextSibling;
        i++;
    }

    if (ele.nodeType != 3) {
        ele = ele.firstChild;
    }

    return {
        e: ele,
        offset: offset,
        index: i
    };
}

function getScreenXYByPos(buf, pos) {
    var res = getElementByPos(buf, pos);
    return getScreenXYInner(buf, res);
}

function getNextLineBreakScreenXY(buf) {
    var res = getNextLineBreakElement(buf, buf.pos);
    return getScreenXYInner(buf, res);
}

function getScreenXYInner(buf, res) {
    var linenum = buf.currentLineNumber;
    var ch = res.e.textContent[res.offset];

    var line = getLine(buf.id, linenum)
    if (!line) return {
        linenum: linenum
    };

    var node = line.childNodes[res.index];
    var e = (node.nodeType == 3) ? node : node.firstChild;

    var range = document.createRange();
    range.setStart(e, res.offset);
    range.setEnd(e, res.offset + 1);

    var rect = range.getBoundingClientRect();
    var left = rect.left;
    var width = rect.width;
    //When a range contains only \n getBoundingClientRect returns empty rect. 
    //Perhaps because these chars are invisible. getClientRects still work.
    if (width == 0) {
        if (res.offset > 0) {
            range = document.createRange();
            range.setStart(e, res.offset - 1);
            range.setEnd(e, res.offset);
            rect = range.getBoundingClientRect();
            left = rect.left + rect.width;
        } else {
            range = document.createRange();
            range.setStart(e, res.offset);
            range.setEnd(e, res.offset);

            var list = range.getClientRects();
            rect = list[0];
            if (list.length > 1 && list[0].top != list[1].top) {
                //line break;
                rect = list[1];
            }
            left = rect.left;
        }
    }

    return {
        top: linenum * lineHeight,
        left: left + $content(buf.id).scrollLeft,
        ch: ch,
        e: e,
        width: width
    };
}

function createDOMItem(className, text) {
    var node;
    if (className) {
        node = createSpan();
        node.className = className;
        node.textContent = text;
    } else {
        node = document.createTextNode(text);
    }
    return node;
}

var __id = 0;

function uniqueId() {
    __id++;
    return (new Date).getTime() + __id;
}

//extract text from DOM: [a, b)
function substring(buf, a, b) {
    if (a == b) return '';
    if (a > b) throw "a must smaller or equal than b";

    var res = getCodeBlockByPos(buf, a);
    var ele = res.e;

    var txt = '';
    var start = res.pos; //pos of block
    //keep [a, b)
    while (true) {
        if (a >= b) {
            break;
        }

        txt += ele.textContent.substring(a - start, b - start);
        ele = ele.nextSibling;
        if (ele == null)
            break;

        start = start + ele.textContent.length;
        a = start;
    }

    return txt;
}

function insertNodeAtPos(buf, node, pos) {
    var res = getElementByPos(buf, pos);
    var linenum = buf.currentLineNumber;
    var line = getLine(buf.id, linenum)
    if (!line) return;

    var nd = line.childNodes[res.index];
    var e = (nd.nodeType == 3) ? nd : nd.firstChild;
    //console.log(e);

    var range = document.createRange();
    range.setStart(e, res.offset);
    range.setEnd(e, res.offset);
    range.insertNode(node);
}
