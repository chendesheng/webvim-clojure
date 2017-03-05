var renderLines = wrapTime(function renderLines(buf) {
    var $buf = $buffer(buf.id);
    if (!$buf.onscroll) {
        $buf.onscroll = onscrollRender;
    }

    var hl = newHighlight(buf.lang);
    var offscreenLines = document.createElement('DIV');

    hl.states = [];
    var linecnt = 0;
    var cnt = buf.str.eachLine(function(block, i) {
        hl.states.push(null);
        if (block) {
            offscreenLines.appendChild(renderLine(hl.parseBlock(block, i)));
            linecnt += block.count('\n');
        }
    });

    $content(buf.id).style.height = cnt * lineHeight + 'px';

    //put a pivot in the end
    var pivot = createSpan();
    pivot.className = 'code-block end-code-block';
    offscreenLines.appendChild(pivot);

    buf.hl = hl;
    buf.id = buf.id;
    buf.offscreenLines = offscreenLines;
    buf.currentBlock = offscreenLines.firstChild;
    buf.currentBlockNumber = 0;
    buf.currentLineNumber = 0;
    buf.pos = 0;
    //track linecnt change
    buf.linecnt = linecnt;
    buf.lastlinecnt = -1;
    buf.dirtyLines = {};

    hl.states.push(null);
});

function renderLine(items) {
    var block = createSpan();
    block.className = 'code-block';
    block.id = uniqueId();

    items.each(function(item) {
        var className = item[0];
        var text = item[1];
        block.appendChild(createDOMItem(className, text));
    });

    return block;
}

//return true means already updated
function refreshLine(items, ele, dirtyLines) {
    function nodetype(className) {
        if (className) return Node.ELEMENT_NODE;
        else return Node.TEXT_NODE;
    }

    if (ele.childNodes.length == items.length) {
        var changed = false;
        for (var i = 0; i < items.length; i++) {
            var item = items[i];
            var className = item[0];
            var text = item[1];

            var oldele = ele.childNodes[i];
            if (text != oldele.textContent) {
                ele.replaceChild(createDOMItem(className, text), oldele);
                changed = true;
            } else {
                var nodeType = nodetype(className);
                if (nodeType == oldele.nodeType) {
                    if (nodeType == Node.ELEMENT_NODE && className != oldele.className) {
                        oldele.className = className;
                        changed = true;
                    }
                } else {
                    ele.replaceChild(createDOMItem(className, text), oldele);
                    changed = true;
                }
            }

        }
        if (changed) {
            dirtyLines[ele.id] = true;
        }
        return true;
    } else {
        return false;
    }
}

function refreshIter(index, buf, states, parentNode) {
    var i = index;
    var ele = buf.currentBlock;
    return {
        text: function() {
            return ele.textContent;
        },
        index: function() {
            return i;
        },
        render: function(items) {
            if (!refreshLine(items, ele, buf.dirtyLines)) {
                delete buf.dirtyLines[ele.id];
                var newele = renderLine(items);
                parentNode.replaceChild(newele, ele);
                ele = newele;
            }
        },
        next: function() {
            ele = ele.nextSibling;
            i++;
            return !endCode(ele);
        }
    };
}
