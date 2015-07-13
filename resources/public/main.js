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
			$('.lines').append('<div id="line-'+i+'" class="line"><pre>'+line+'</pre></div>');
			$('.gutter').append('<div id="line-num-'+i+'" class="line-num">'+(i+1)+'</div>');
		});
		cline = lines[cr];
	} else {
		cline = $('#line-'+cr)[0].textContent;
	}
	
	//$('.lines').append('<div class="cursor"></div>');
	if (!$('.lines .cursor').get(0)) {
		$('.lines').append('<div class="cursor"></div>');
	}

	function isChinese(c) {
		return /[\ufe30-\uffa0\u4e00-\u9eff\u3000-\u303F]/.test(c);
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
	

	if (buf.mode == 2) {
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

	if (buf.name) {
		document.title = buf.name;
	}

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

var MODES = ['-- NORMAL --', '-- INSERT --'];

