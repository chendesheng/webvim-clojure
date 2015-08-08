$(document).ready(function() {
	$.getJSON('buf', function(resp) {
		render(resp);

		setSize();
		$(window).resize(function() {
			waitForFinalEvent(setSize, 500, "resize window");
		});
	});

	//scroll position
	$('.lines').scroll(function() {
		var current = $(this).scrollTop();
		$('.gutter').scrollTop(current);
	});
});

//https://stackoverflow.com/questions/2854407/javascript-jquery-window-resize-how-to-fire-after-the-resize-is-completed/4541963#4541963
var waitForFinalEvent = (function () {
  var timers = {};
  return function (callback, ms, uniqueId) {
    if (!uniqueId) {
      uniqueId = "Don't call this twice without a uniqueId";
    }
    if (timers[uniqueId]) {
      clearTimeout (timers[uniqueId]);
    }
    timers[uniqueId] = setTimeout(callback, ms);
  };
})();

function setSize() {
	var view = $('.lines')[0];
	var w = Math.round(view.offsetWidth/9.57), h = Math.round(view.offsetHeight/21);
	$.getJSON('resize/'+w+'/'+h);
}

function isChinese(c) {
	return /[\ufe30-\uffa0\u4e00-\u9eff\u3000-\u303F]/.test(c);
}

function textWidth(txt, a, b) {
	var x = 0;
	for (var i = a; i < b; i++) {
		if (txt[i] == '\t') {
			x += (4-x%4)*9.57
		} else if (isChinese(txt[i])) {
			x+=16;
		} else {
			x+=9.57;
		}
	}
	return x;
}

function htmlEncode(value) {
    return $('<div/>').text(value).html();
}

function replaceBinding(html, data, ifEncode) {
	for (var p in data) {
		if (data.hasOwnProperty(p)) {
			var v = data[p];
			html = html.replace(new RegExp('{' + p + '}', 'g'), ifEncode ? htmlEncode(v) : v);
		}
	}
	return html;
}

var lineTemplate = '<div id="line-{row}" class="line"><pre>{line}</pre></div>';
var gutterLineTemplate = '<div id="line-num-{row}" class="line-num">{incrow}</div>';

function lineid(i) {
	return '#line-'+i;
}

function render(buf) {
	if (!buf) return;

	//render lines
	if (buf.lines) {
		$('.lines').empty();
		$('.gutter').empty();
		var lines = buf.lines;
		$(lines).each(function(i, line) {
			if (line) {
				$('.lines').append(replaceBinding(lineTemplate, {row:i, line:line}, true));
				$('.gutter').append(replaceBinding(gutterLineTemplate, {row:i,incrow:i+1}));
			}
		});
		
	}

	if (buf.difflines) {
		var diff = buf.difflines;
		var row = diff.row;
		var add = diff.add;
		var sub = diff.sub;
		var oldcnt = parseInt($('.gutter :last-child').text());
		var newcnt = oldcnt+add.length-sub;

		//update gutter
		if (newcnt > oldcnt) {
			for (var i = oldcnt; i < newcnt; i++) {
				$('.gutter').append(replaceBinding(gutterLineTemplate, {row:i,incrow:i+1}));
			}
		} else if (newcnt < oldcnt) {
			for (var i = 0; i < oldcnt-newcnt; i++) {
				$('.gutter :last-child').remove();
			}
		}

		//setup space for adding
		if (add.length < sub) {
			//shift left
			var shiftcnt = sub-add.length;
			for (var i = row+shiftcnt; i < oldcnt; i++) {
				var line = $(lineid(i)+' :first-child').remove();
				$(lineid(i-shiftcnt)).empty().append(line);
			}
		} else if (add.length > sub) {
			//shift right
			var shiftcnt = add.length-sub;
			//make sure lines enough
			for (var i = 0; i < shiftcnt; i++) {
				$('.lines').append(replaceBinding(lineTemplate, {row:i+oldcnt, line:""}, false));
			}
			for (var i = oldcnt; i >= row; i--) {
				var line = $(lineid(i)+' :first-child').remove();
				$(lineid(i+shiftcnt)).empty().append(line);
			}
		}
		
		//apply add lines
		$(add).each(function(i, line) {
			var i1 = i+row;
			$(lineid(i1)).empty().append('<pre>'+htmlEncode(line)+'<pre>');
		});
	} 

	
	//render cursor
	if (typeof buf.cursor != 'undefined') {
		var cr = parseInt(buf.cursor.row);
		var cc = parseInt(buf.cursor.col);

		var cline = $('#line-'+cr)[0].textContent;
		if (!$('.lines .cursor').get(0)) {
			$('.lines').append('<div class="cursor"></div>');
		}

		var x = 0;
		for (var i = 0; i < cc; i++) {
			if (cline[i] == '\t') {
				x += (4-x%4)*9.57
			} else if (isChinese(cline[i])) {
				x+=16;
			} else {
				x+=9.57;
			}
		}
		var y = cr*21+1;
		var w = 9.57;
		if (isChinese(cline[cc])) {
			w = 16;
		}
		$('.cursor').attr('style', 'left:'+x+'px;top:'+y+'px;width:'+w+'px;');
	}

	//render ex
	if (buf.ex && buf.ex.length > 0) {
		$('.status-bar .ex').empty().text(buf.ex);
		$('.status-bar .ex').append('<span class="cursor"> </span>');
	} else if (buf.message) {
		$('.status-bar .ex').empty().text(buf.message);
	} else {
		if (typeof buf.mode != 'undefined' && buf.mode < MODES.length) {
			$('.status-bar .ex').empty().text(MODES[buf.mode]);
		}
	}

	//render unsaved
	$('.status-bar .buf-name').toggleClass('buf-unsaved', !!buf.unsaved);

	//render ongoing keys
	if (typeof buf.keys != 'undefined') {
		if (buf.keys && buf.keys.length > 0) {
			var keysstr = '';
			for (var i = 0; i < buf.keys.length; i++) {
				var k = buf.keys[i];
				if (k.length > 1) k = '<'+k+'>';
				keysstr += k;
			}
			$('.status-bar .ongoing-keys').text(keysstr);
		} else {
			$('.status-bar .ongoing-keys').empty();
		}
	}

	//render visual
	$('.lines .selections').empty();
	if (buf.visual) {
		if (!$('.lines .selections').get(0)) {
			$('.lines').append('<div class="selections"></div>');
		}
		renderSelections($('.lines .selections'), buf.visual.ranges)
	}

	//render hlsearch
	$('.lines .highlights').empty();
	if (buf.highlights) {
		if (!$('.lines .highlights').get(0)) {
			$('.lines').append('<div class="highlights"></div>');
		}
		renderSelections($('.lines .highlights'), buf.highlights, true)
	}

	//render matched brace pair
	$('.lines .highlight-brace-pair').empty();
	if (buf.braces) {
		if (!$('.lines .highlight-brace-pair').get(0)) {
			$('.lines').append('<div class="highlight-brace-pair"></div>');
		}
		var cr, cc;
		if (buf.cursor) {
			cr = parseInt(buf.cursor.row);
			cc = parseInt(buf.cursor.col);
		}

		var ranges = [];
		for (var i = 0; i < buf.braces.length; i++) {
			var pt = buf.braces[i];
			//skip cursor, don't draw twice in same point
			if (buf.cursor && pt.row == cr && pt.col == cc) continue; 

			ranges.push(pt, {row:pt.row, col:pt.col+1});
		}
		renderSelections($('.lines .highlight-brace-pair'), ranges);
	}

	scrollToCursor(buf['scroll-top'] || 0);

	//render autocompl suggestions
	if (buf.autocompl && buf.autocompl.suggestions && buf.autocompl.suggestions.length > 1) {
		var lastSelectedIndex = 0;
		var lastScrollTop = 0;
		var $autocompl = $('.lines .autocompl');
		if (!$autocompl[0]) {
			$('.lines').append('<div class="autocompl"></div>');
			$autocompl = $('.lines .autocompl');
		} else if ($('.lines .autocompl .highlight')[0]) {
			lastSelectedIndex = parseInt($('.lines .autocompl .highlight').attr('id').split('-')[1]);
			lastScrollTop = $autocompl.scrollTop();
		}

		var selectedIndex = parseInt(buf.autocompl['suggestions-index']);
		var currentWord = buf.autocompl.suggestions[selectedIndex];
		$autocompl.empty().css('left', x-currentWord.length*9.57-10+'px')
		$(buf.autocompl.suggestions).each(function(i, word) {
			if (i > 0) {
				var ele = $('<pre id="suggestion-'+i+'"></pre>').text(word)
					.appendTo('.lines .autocompl');
				if (i == selectedIndex) {
					ele.addClass('highlight');
				}
			}
		});

		if (y+21+$autocompl.height()-$('.lines').scrollTop() < $('.lines').height()) {
			$autocompl.css('top', y+21+'px');
		} else {
			$autocompl.css('top', y-$autocompl.height()+'px');
		}

		var viewportTop = lastScrollTop;
		var viewportBottom = lastScrollTop+240;
		var currentPos = (selectedIndex-1) * 24;
		if (currentPos < viewportTop) {
			$autocompl.scrollTop(currentPos);
		} else if (currentPos+24 >= viewportBottom) {
			$autocompl.scrollTop(currentPos-216);
		}
	} else {
		$('.lines .autocompl').empty();
	}

	//title
	if (buf.name) {
		document.title = buf.name;
		$('.status-bar .buf-name').text(buf.name);
	}
}

var aligntop = true;
function scrollToCursor(scrollTopRow) {
	var el = $('.cursor')[0];
	var lines = $('.lines');
	var scrleft = lines.scrollLeft();
	var width = lines.width();
	var height = lines.height();

	//lines.scrollTop(scrollTopRow * 21);
	var oldst = lines.scrollTop();
	var newst = scrollTopRow * 21;
	if (Math.abs(oldst - newst) > 3*21) {
		lines.animate({ scrollTop: newst}, 60);
	} else {
		lines.scrollTop(newst);
	}

	if (el.offsetLeft+el.offsetWidth > scrleft+width) {
		lines.scrollLeft(el.offsetLeft+el.offsetWidth-width);
	} else if (el.offsetLeft < scrleft) {
		lines.scrollLeft(el.offsetLeft);
	}
}

function renderSelections($p, ranges, reverseTextColor) {
	for (var i = 0; i < ranges.length; i+=2) {
		var s = ranges[i];
		var e = ranges[i+1];
		renderSelection($p, s, e, reverseTextColor);
	}
}

//if reverseTextColor==true copy text from lines append to line-selected
function renderSelection($p, s, e, reverseTextColor) {
	//sort
	if (s.row > e.row || (s.row == e.row && s.col > e.col)) {
		var t = s;
		s = e;
		e = t;
	}

	for (var i = s.row; i <= e.row; i++) {
		var line = $('<div class="line-selected"></div>');
		var w = 0;
		var l = 0;
		var cline = $('#line-'+i)[0].textContent;

		if (i == s.row) {
			w = textWidth(cline, s.col, (e.row==i)?e.col:cline.length);
			l = textWidth(cline, 0, s.col);
		} else if (i == e.row) {
			w = textWidth(cline, 0, e.col);
			l = 0;
		} else if (cline.length==0) {
			//empty line always contains '\n'
			w = 9.57;
			l = 0;
		} else {
			w = textWidth(cline, 0, cline.length);
			l = 0;
		}

		line.css('left', l+'px')
			.css('top', (i*21+1)+'px')
			.css('width', w+'px');

		if (reverseTextColor) {
			var txt;
			if (i == s.row) {
				txt = cline.substring(s.col, (e.row==i)?e.col:cline.length);
			} else if (i == e.row) {
				txt = cline.substring(0, e.col);
			} else if (cline.length==0) {
				txt = '\n';
			} else {
				txt = cline;
			}
			line.append(txt);
		}

		$p.append(line);
	}
}

var MODES = ['-- NORMAL --', '-- INSERT --', '-- VISUAL --'];

