$(document).keydown(function(event) {
	var key = KEYCODE_KEYDOWN[event.keyCode];
	if (key || event.ctrlKey || event.altKey) {
		event.preventDefault();

		if (event.ctrlKey) {
			if (event.keyCode != 16 && event.keyCode != 17 && event.keyCode != 18) {
				var prefix = "c+";
				if (event.shiftKey) {
					prefix += "s+"
				}
				if (event.altKey) {
					prefix += "a+";
				}
				key = prefix+KEYCODE_DIC[event.keyCode];
			}
		}

		if (key == 'c+[') {
			key = 'esc';
		}

		if (key) {
			$.getJSON('key?code='+encodeURIComponent(key), function(resp) {
				render(resp);
			});
		}
	}
});

$(document).keypress(function(event) {
	event.preventDefault();
	var code = event.keyCode || event.charCode || event.which;;

	var ch = String.fromCharCode(code)
	if (ch) {
		$.getJSON('key?code='+encodeURIComponent(ch), function(resp) {
			render(resp);
		});
	}
});
