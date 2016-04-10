function $hideAutocompl(bufid) {
    buffers[bufid].suggestions = null;
    $hide($autocompl(bufid));
    $hide($exAutocompl(bufid));
}

function autocomplItem(subject, word) {
    if (subject.length == 0) {
        var ele = document.createElement('PRE');
        ele.appendChild(document.createTextNode(word));
        return ele;
    }

    var lowercaseWord = word.toLowerCase();
    subject = subject.toLowerCase();

    var j = 0;
    for (var i = 0; i < subject.length; i++) {
        var ch = subject[i];
        j = lowercaseWord.indexOf(ch, j) + 1;
    }

    var splits = [j - 1];
    j--;
    for (var i = subject.length - 2; i >= 0; i--) {
        var ch = subject[i];
        j = lowercaseWord.substring(0, j).lastIndexOf(ch);
        splits.unshift(j)
    }
    splits.unshift(-1);
    //console.log(splits);

    var ele = document.createElement('PRE');
    for (var i = 1; i < splits.length; i++) {
        ele.appendChild(document.createTextNode(word.substring(splits[i - 1] + 1, splits[i])));
        var matched = createSpan();
        matched.className = 'matched';
        matched.textContent = word[splits[i]];
        ele.appendChild(matched);
    }
    ele.appendChild(document.createTextNode(word.substring(splits[splits.length - 1] + 1)));
    return ele;
}

function appendAutocomplItems(bufid, suggestions, $a, selectedIndex) {
    $hide($a);
    $a.innerHTML = '';

    var subject = suggestions[0];
    suggestions.each(function(word, i) {
        if (i > 0) {
            var ele = autocomplItem(subject.name, word.name);
            if (word.type) {
                var eletyp = createSpan();
                eletyp.className = 'info'; //can't use "type" here conflict with class
                eletyp.textContent = word.type;
                ele.appendChild(eletyp);
            }
            if (word.class) {
                addClass(ele, 'with-class');
                addClass(ele, word.class);
            }
            $a.appendChild(ele);
            if (i == selectedIndex) {
                addClass(ele, 'highlight');
                ele.id = 'autocompl-' + bufid + '-highlight';
            }
        }
    });
    $show($a);
}


function renderAutocompl(buf) {
    var bufid = buf.id;
    var autocompl = buf.autocompl;
    var suggestions;
    if (!autocompl) {
        $hideAutocompl(bufid);
        return;
    } else if (!autocompl.suggestions) {
        //use local cache
        suggestions = buf.suggestions;
    } else {
        //update local cache
        suggestions = autocompl.suggestions;
        buf.suggestions = suggestions;
    }

    if (
        /*This should never happen, server problem*/
        suggestions == null ||
        suggestions.length < 1 ||
        /*no choice*/
        suggestions.length == 1 ||
        /*selected the only choice:*/
        (suggestions.length == 2 &&
            (autocompl.index == 1 || suggestions[0] == suggestions[1]))
    ) {
        $hideAutocompl(bufid);
        return;
    }

    var selectedIndex = autocompl.index;
    var lastScrollTop;
    var popupHeight;
    if (buf.mode == 2) {
        $a = $exAutocompl(bufid);
        lastScrollTop = $a.scrollTop;

        appendAutocomplItems(bufid, suggestions, $a, selectedIndex);
        itemHeight = $a.firstChild.offsetHeight;
        popupHeight = $a.offsetHeight;
    } else {
        $a = $autocompl(bufid);
        lastScrollTop = $a.scrollTop;

        var h = $cursor(bufid).offsetHeight + 3;
        var currentWord = suggestions[selectedIndex];
        var res = getScreenXYByPos(buf, buf.cursor - currentWord.name.length);
        if (!res.e) return;

        $a.style.left = res.left + $content(bufid).scrollLeft - 10 + 'px';
        var top = res.top;

        appendAutocomplItems(bufid, suggestions, $a, selectedIndex);
        itemHeight = $a.firstChild.offsetHeight;
        popupHeight = $a.offsetHeight;

        var $buf = $buffer(bufid);
        if (top + h + popupHeight < $buf.scrollTop + $buf.offsetHeight - $statusBar(bufid).offsetHeight) {
            $a.style.top = top + h + 'px';
        } else {
            $a.style.top = top - popupHeight - 3 + 'px';
        }

        $a.style.marginLeft = -gutterWidth() + 'ch';
    }

    var viewportTop = lastScrollTop;
    var viewportBottom = lastScrollTop + popupHeight;
    var pos = (selectedIndex - 1) * itemHeight;
    if (pos < viewportTop) {
        $a.scrollTop = pos;
    } else if (pos >= viewportBottom) {
        $a.scrollTop = pos - 9 * itemHeight;
    }
}
