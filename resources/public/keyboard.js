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
	keymap(key, function(k) {
		inputQueue.push(k);
		if (inputQueue.length == 1) {
			sendQueue();
		}
	});
}

function sendQueue() {
	if (inputQueue.length > 0) {
		var key = inputQueue[0];
		$.getJSON('key?code='+encodeURIComponent(key), function(resp) {
			render(resp);

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
		$('.lines .cursor').text(key).removeClass().addClass('cursor ongoing-key');
	}
}

function hideOngoingKey() {
	$('.lines .cursor').text('').removeClass('ongoing-key');
}

//map key to another get return from callback
function imap(key, callback) {
	if (!key) {
		return;
	}

	if (key == 'c+[') {
		callback('esc');
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
			callback('esc');
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
	callback(key);
}

var vmap = nmap;

var keymaps = [nmap, imap, vmap];

var keymap = nmap;
