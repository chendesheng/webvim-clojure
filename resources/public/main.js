$(document).ready(function() {
	$.getJSON('buf', function(resp) {
		render(resp);

		setSize();
		$(window).resize(function() {
			waitForFinalEvent(setSize, 500, "resize window");
		});
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
	var w = Math.round(view.offsetWidth/9.57), h = Math.round(view.offsetHeight/20);
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
function render(buf) {
	if (!buf) return;

	var cr = parseInt(buf.cursor.row)
	var cc = parseInt(buf.cursor.col)

	var cline = "";
	if (buf.lines) {
		var lines = buf.lines;
		$('.lines').empty();
		$('.gutter').empty();
		$(lines).each(function(i, line) {
			$('.lines').append('<div id="line-'+i+'" class="line"><pre>'+htmlEncode(line)+'</pre></div>');
			$('.gutter').append('<div id="line-num-'+i+'" class="line-num">'+(i+1)+'</div>');
		});
		cline = lines[cr];
	} else {
		cline = $('#line-'+cr)[0].textContent;
	}
	
	//render cursor
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
	var y = cr*20+1;
	var w = 9.57;
	if (isChinese(cline[cc])) {
		w = 16;
	}
	$('.cursor').attr('style', 'left:'+x+'px;top:'+y+'px;width:'+w+'px;');
	

	//render status bar
	if (buf.ex && buf.ex.length > 0) {
		$('.status-bar pre').empty().text(buf.ex);
		$('.status-bar pre').append('<span class="cursor"> </span>');
	} else if (buf.message) {
		$('.status-bar pre').empty().text(buf.message);
	} else {
		var strkeys = ""
		if (buf.keys && buf.keys.length > 0) {
			for (var i = 0; i < buf.keys.length; i++) {
				strkeys += buf.keys[i];
			}
		}

		$('.status-bar pre').empty().text(MODES[buf.mode]+" "+strkeys);
	}

	//render visual
	$('.lines .selections').empty();
	if (buf.mode == 2) {
		if (!$('.lines .selections').get(0)) {
			$('.lines').append('<div class="selections"></div>');
		}
		var rangs = buf.visual.ranges;
		var s = rangs[0];
		var e = rangs[1];
		//var lines = buf.lines;
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
				w = textWidth(cline, s.col, (e.row==i)?e.col+1:cline.length);
				l = textWidth(cline, 0, s.col);
			} else if (i == e.row) {
				w = textWidth(cline, 0, e.col+1);
				l = 0;
			} else {
				w = textWidth(cline, 0, cline.length);
				l = 0;
			}

			line.css('left', l+'px')
				.css('top', i*20+1+'px')
				.css('width', w+'px')
				.css('height', '20px')
			$('.selections').append(line);
		}
	}

	//render autocompl suggestions
	$('.lines .autocompl').empty();
	if (buf.autocompl && buf.autocompl.suggestions && buf.autocompl.suggestions.length > 1) {
		if (!$('.lines .autocompl')[0]) {
			$('.lines').append('<div class="autocompl"></div>');
		}

		var currentWord = buf.autocompl.suggestions[parseInt(buf.autocompl['suggestions-index'])];

		$('.lines .autocompl').empty()
			.css('left', x-currentWord.length*9.57-10+'px')
			.css('top', y+20+'px');

		$(buf.autocompl.suggestions).each(function(i, word) {
			if (i > 0) {
				var ele = $('<pre></pre>').text(word).appendTo('.lines .autocompl');
				if (i == parseInt(buf.autocompl['suggestions-index'])) {
					ele.addClass('highlight');
				}
			}
		});
	}

	//title
	if (buf.name) {
		document.title = buf.name;
	}

	//scroll position
	$('.lines').scroll(function() {
		var current = $(this).scrollTop();
		$('.gutter').scrollTop(current);
	});

	scrollToCursor(buf.cursor);
}

var aligntop = true;
function scrollToCursor(cursor) {
	var el = $('.cursor')[0];
	var lines = $('.lines');
	var scrtop = lines.scrollTop();
	var scrleft = lines.scrollLeft();
	var width = lines.width();
	var height = lines.height();

	var vpheight = Math.round(height/20);
	//var aligntop = cursor.vprow < vpheight/2; //true from top or false from bottom
	if (cursor.vprow == vpheight-1)
		aligntop = false;
	if (cursor.vprow == 0)
		aligntop = true;
	if (aligntop) {
		var srctop = 20*(cursor.row - cursor.vprow);
		lines.scrollTop(srctop+1);
	} else {
		var srctop = 20*cursor.row - (height-20*(vpheight - cursor.vprow));
		console.log(srctop);
		lines.scrollTop(srctop+1);
	}

	if (el.offsetLeft+el.offsetWidth > scrleft+width) {
		lines.scrollLeft(el.offsetLeft+el.offsetWidth-width);
	} else if (el.offsetLeft < scrleft) {
		lines.scrollLeft(el.offsetLeft);
	}
}

var MODES = ['-- NORMAL --', '-- INSERT --', '-- VISUAL --'];

