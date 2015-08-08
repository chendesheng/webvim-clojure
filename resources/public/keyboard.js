var inputQueue = [];
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
			inputQueue.push(key);
			if (inputQueue.length == 1) {
				sendQueue();
			}
		}
	}
});

$(document).keypress(function(event) {
	event.preventDefault();
	var code = event.keyCode || event.charCode || event.which;;

	var ch = String.fromCharCode(code)
	if (ch) {
		inputQueue.push(ch);
		if (inputQueue.length == 1) {
			sendQueue();
		}
	}
});

function sendQueue() {
	if (inputQueue.length > 0) {
		var key = inputQueue[0];
		$.getJSON('key?code='+encodeURIComponent(key), function(resp) {
			render(resp);

			inputQueue.shift();
			sendQueue();
		});
	}
}
