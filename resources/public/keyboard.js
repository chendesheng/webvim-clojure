$(document).keydown(function(event) {
	console.log(event.keyCode);
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
			$.getJSON('key/'+key, function(resp) {
				render(resp);
			});
		}
	}
});

$(document).keypress(function(event) {
	event.preventDefault();
	var code = event.keyCode || event.charCode || event.which;;
	console.log(code);

	var ch = String.fromCharCode(code)
	if (ch) {
		console.log(ch);

		$.getJSON('key/'+encodeURIComponent(ch), function(resp) {
			render(resp);
		});
	}
});
