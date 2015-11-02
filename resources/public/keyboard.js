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

function handleKey(key) {
	if (key.length > 1) {
		key = '<' + escapseKeys(key) + '>';
	} else {
		key = escapseKeys(key);
	}
	keymap(key, function(k) {
		inputQueue.push(k);
		if (inputQueue.length == 1) {
			sendQueue();
		}
	});
}

function escapseKeys(keys) {
	return keys.replace(/([\\<>])/, '\\$1');
}


function sendQueue() {
	if (inputQueue.length > 0) {
		var keys = inputQueue.join('');
		inputQueue = [keys];
		var url = 'key?code='+encodeURIComponent(keys);
		url = addViewportParams(url);

		$.getJSON(url, function(resp) {
			resp.each(function(change) {
				render(change);
			});

			inputQueue.shift();
			sendQueue();
		}).fail(function() {
			//some keys do nothing returns empty string
			inputQueue = [];  
		});
	}
}


var ongoingkeys = [];
var ongoingkeysTimer;

function showOngoingKey(key) {
	if (key.length == 1) {
		var cursor = $cursor(buffers.active.id);
		cursor.textContent = key;
		cursor.className = 'cursor ongoing-key';
	}
}

function hideOngoingKey() {
	$cursor(buffers.active.id).className = 'cursor';
}

//map key to another get return from callback
function imap(key, callback) {
	if (!key) {
		return;
	}

	if (key == '<c+[>') {
		callback('<esc>');
		return;
	}

	if (key == 'j') {
		if (ongoingkeys.length == 0) {
			ongoingkeysTimer = setTimeout(function() {
				hideOngoingKey();
				callback('j');
				ongoingkeys = [];
			}, 1000);

			ongoingkeys.push(key);
			showOngoingKey(key);
		} else {
			hideOngoingKey();
			//press twice 'j'
			if (ongoingkeysTimer != null) {
				clearTimeout(ongoingkeysTimer);
				ongoingkeysTimer = null;
			}

			ongoingkeys = [];
			callback('<esc>');
		}
	} else {
		if (ongoingkeysTimer != null) {
			clearTimeout(ongoingkeysTimer);
			ongoingkeysTimer = null;
		}
		hideOngoingKey();

		ongoingkeys.push(key);
		ongoingkeys.each(function(key){
			callback(key);
		});

		ongoingkeys = [];
	}
}

function nmap(key, callback) {
	if (key == '<c+l>') {
		callback(':nohlsearch<cr>');
	} else if (key == '<c+[>') {
		callback('<esc>');
	} else {
		callback(key);
	}
}

var vmap = nmap;

var keymaps = [nmap, imap, vmap];

var keymap = nmap;
