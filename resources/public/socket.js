function connect(path, ondata, onclose, onerror) {
	var websocket = new WebSocket(window.location.href.replace(/^http(s?:\/\/[^/]*)(\/.*)?/i, "ws$1")+path);

	websocket.onopen = onOpen;
	websocket.onclose = onClose;
	websocket.onmessage = onMessage;
	websocket.onerror = onError;

	function onOpen(evt) {
	}

	function onClose(evt) {
		if (onclose) {
			onclose(evt);
		}
	}

	function onMessage(evt) {
		ondata(JSON.parse(evt.data));
	}

	function onError(evt) {
		if (onerror) {
			onerror(evt.data);
		}
	}

	return websocket;
}

