function renderSelection($p, a, b, buf, usedIds) {
	if (b < a) return;

	var __e = $p.firstChild;
	function getOrCreate() {
		while(__e) {
			if (!usedIds[__e.id]) {
				return __e;
			}
			__e = __e.nextSibling;
		}
		__e = document.createElement('SPAN');
		__e.className = 'line-selected';
		__e.id = uniqueId();
		return __e;
	}

	function append(x, y, w, h) {
		var sp = getOrCreate();
		usedIds[sp.id] = true;

		var styles = [['left', x+'px'],
				['top', y+'px'],
				['marginLeft', (-gutterWidth(buf.id)+'ch')],
				['width', w+'px'],
				['paddingBottom', '0px'],
				['height', h+'px']];

		styles.each(function(item) {
			var k = item[0];
			var v = item[1];
			if (sp.style[k] != v) {
				sp.style[k] = v;
			}
		});

		if (!sp.parentNode) {
			$p.appendChild(sp);
		}

		return sp;
	}

	var resa = getScreenXYByPos(buf, a);
	var resb = getScreenXYByPos(buf, b);
				

	if (resa.e == null && resb.e == null) {
		//render nothing if resa and resb on "same side"
		var from = parseInt($lines(buf.id).dataset.from);
		if (resa.linenum < from &&  resb.linenum < from) {
			return;
		}

		var to = from+$lines(buf.id).childNodes.length;
		if (resa.linenum >= to &&  resb.linenum >= to) {
			return;
		}
	}

	if (resa.e == null) {
		var from = parseInt($lines(buf.id).dataset.from);
		resa = {
			top:from*lineHeight,
			left:0,
			width:0
		};
	}

	if (resb.e == null) {
		var from = parseInt($lines(buf.id).dataset.from);
		var to = from+$lines(buf.id).childNodes.length;
		resb = {
			top:(to-1)*lineHeight,
			left:0,
			width:0
		};
	}

	if (resa.top != resb.top) {
		var w = $buffer(buf.id).offsetWidth;
		append(resa.left, resa.top, (w-resa.left), lineHeight);
		var mh = resb.top-resa.top-lineHeight;
		if (mh > 0) {
			append(0, resa.top+lineHeight, w, mh);
		}
		append(0, resb.top, resb.left+resb.width, lineHeight);
	} else {
		append(resa.left, resa.top, resb.left+resb.width-resa.left, lineHeight);
	}
}

function renderSelections($p, buf, ranges) {
	var usedIds = {};
	for (var i = 0; i < ranges.length; i++) {
		renderSelection($p, ranges[i][0], ranges[i][1], buf, usedIds);
	}
	removeUnused($p, usedIds);
}


