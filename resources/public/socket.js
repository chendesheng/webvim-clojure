function connect(path) {
    var _inputBuffer = '';
    var _conn;
    var _lastSentTime = (new Date).getTime();

    function _idleSeconds() {
        return ((new Date).getTime() - _lastSentTime) / 1000;
    }

    function _connect(query) {
        _conn = new WebSocket(window.location.href.replace(/^http(s?:\/\/[^/]*)(\/.*)?/i, "ws$1") + path + (query || ''));
        _conn.onmessage = function(message) {
            console.log(message.data);
            JSON.parse(message.data).each(render);
        };
        _conn.onopen = function() {
            _flushBuffer();
        };
        _conn.onclose = function() {
            //don't auto re-connect if idle for a long time
            if (_idleSeconds() < 2) {
                _connect();
            }
        };
    }

    function _flushBuffer() {
        if (_inputBuffer.length > 0) {
            updateViewportSize(function() {
                if (_conn.readyState == 1) { //OPEN https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#Ready_state_constants
                    _conn.send(wrapActiveId(_inputBuffer));
                    console.log(_inputBuffer);
                    _inputBuffer = '';
                } else if (_conn.readyState != 0) { //not CONNECTING
                    //reconnect
                    _connect();
                }
            });
        }
    }

    _connect('?init=true');

    return {
        'send': function(key) {
            _lastSentTime = (new Date).getTime();
            _inputBuffer += key;
            _flushBuffer();
        }
    };
}
