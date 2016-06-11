function renderChanges(changes) {
    var buf = buffers.active;
    var offscreenLines = buf.offscreenLines;
    var hl = buf.hl;
    changes.each(function(c) {
        var resa = getCodeBlockByPos(buf, c.pos);
        var resb = getCodeBlockByPos(buf, c.pos + c.len);
        var nextblock = getCodeBlockByPos(buf, resb.pos + resb.e.textContent.length);
        var prefix = resa.e.textContent.substring(0, c.pos - resa.pos);
        var suffix = resb.e.textContent.substring(c.pos + c.len - resb.pos);
        var newtxt = prefix + c.to + suffix;

        //delete [resa.e, resb.e] both inclusive
        //don't delete "end-code" pivot
        var ele = resa.e;
        var blocknumdeleted = 0;
        while (!endCode(ele)) {
            var toremove = ele;
            ele = ele.nextSibling;
            toremove.remove();
            blocknumdeleted++;

            if (toremove == resb.e) {
                break;
            }
        }

        //delete hl.states [resa.num+1, resb.num+1]
        var deletedstates = hl.states.splice(resa.num + 1, blocknumdeleted);
        var savedstate = deletedstates.pop();

        var parseFail = false;
        //insert and keep track hl.states
        var blocknuminserted = newtxt.eachLine(function(block, i) {
            var num = i + resa.num;
            if (!parseFail) {
                var res = hl.parse(block, hl.states[num]);
                hl.states.splice(num + 1, 0, res.modes);
                offscreenLines.insertBefore(renderLine(res.output), ele);
                parseFail = res.fail;
            } else {
                hl.states.splice(num + 1, 0, []);
                offscreenLines.insertBefore(renderLine([
                    [null, block]
                ]), ele);
            }
        });


        //update local buffer
        var linenuminserted = newtxt.count('\n');
        var linenumdeleted = nextblock.linenum - resa.linenum;

        var linenumdiff = linenuminserted - linenumdeleted;
        var blocknumdiff = blocknuminserted - blocknumdeleted;
        var posdiff = c.to.length - c.len;

        buf.pos = nextblock.pos + posdiff;
        buf.currentBlock = nextblock.e;
        buf.currentLineNumber = nextblock.linenum + linenumdiff;
        buf.currentBlockNumber = nextblock.num + blocknumdiff;

        //update syntax highlight
        if (!parseFail && !endCode(ele) && !savedstate.equal(hl.states[resa.num + blocknuminserted])) {
            //currentBlock will change
            var saved = buf.currentBlock.previousSibling;
            hl.refresh(refreshIter(resa.num + blocknuminserted, buf, hl.states, offscreenLines));
            buf.currentBlock = saved.nextSibling;
        }

        buf.linecnt += linenumdiff;
    });
}
