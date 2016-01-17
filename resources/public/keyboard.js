function escapseKeys(keys) {
	return keys.replace(/([\\<>])/, '\\$1');
}

function keyboardInit() {
	var channel = connect("/socket/");

	function handleKey(key) {
		if (key.length > 1) {
			key = '<' + escapseKeys(key) + '>';
		} else {
			key = escapseKeys(key);
		}

		keymap(key, function(k) {
			channel.send(k);
		});
	}


	$(document).keydown(function(event) {
		var key = KEYCODE_KEYDOWN[event.keyCode];
		if (key || event.ctrlKey || event.altKey) {
			event.preventDefault();

			if (event.ctrlKey) {
				if (event.keyCode != 16 && event.keyCode != 17 && event.keyCode != 18) {
					var prefix = "c-";
					if (event.shiftKey) {
						prefix += "s-"
					}
					if (event.altKey) {
						prefix += "a-";
					}
					key = prefix+KEYCODE_DIC[event.keyCode];
				}
			} else if (event.shiftKey) {
				if (event.keyCode == 9) {
					key = "s-"+KEYCODE_DIC[event.keyCode];
				}
			}

			if (key) {
				handleKey(key);
			}
		}
	});

	$(document).keypress(function(event) {
		event.preventDefault();
		var code = event.keyCode || event.charCode || event.which;;

		var ch = String.fromCharCode(code);
		handleKey(ch);
	});

}
