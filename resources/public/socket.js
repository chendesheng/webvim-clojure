function connect(path) {
	var _inputBuffer = '';
	var _conn;

	function _connect() {
		_conn = new WebSocket(window.location.href.replace(/^http(s?:\/\/[^/]*)(\/.*)?/i, "ws$1")+path);
		_conn.onmessage = function(message) {
			var changes = JSON.parse(message.data);

			changes.each(function(ch) {
				render(ch);
			});
		};
		_conn.onopen = function() {
			_flushBuffer();
		};
	}

	function _flushBuffer() {
		if (_inputBuffer.length > 0) {
			updateViewportSize(function() {
				if (_conn.readyState == 1) { //OPEN https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#Ready_state_constants
					_conn.send(wrapActiveId(_inputBuffer));
					_inputBuffer = '';
				} else if (_conn.readyState != 0) { //not CONNECTING
					//reconnect
					_connect();
				}
			});
		}
	}

	_connect();

	return {
		'send': function (key) {
			_inputBuffer += key;
			_flushBuffer();
		}
	};
}
