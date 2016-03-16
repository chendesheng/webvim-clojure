//local states for each buffer, doesn't contains text content since it might be too large.
var buffers = {};
var viewport = {};
var lineHeight = 21;

function wrapActiveId(keys) {
    //minimize overhead of add id to every input keys
    return buffers.active.id + "!" + keys;
}

window.onload = function() { //use window.onload, not sure if stylesheet is loaded in document.ready
    var d = document.createElement('SPAN');
    d.className = 'line-num';
    d.style.opacity = 0;
    d.style.position = 'absolute';
    d.style.right = 0;
    d.style.bottom = 0;
    d.textContent = 'M';
    document.body.appendChild(d);
    lineHeight = d.offsetHeight;
    document.body.removeChild(d);

    keyboardInit();
    viewport.height = Math.floor((window.innerHeight - lineHeight * 1.5) / lineHeight);
};

var localbufWatchers = {};

function watchLocalbufChange(p, f) {
    var watchers = localbufWatchers[p] || [];
    watchers.push(f);
    localbufWatchers[p] = watchers;
}

function mergeToLocal(buf) {
    var watchers = [];

    if (buffers.active == null) {
        buffers[buf.id] = buf;
        buffers.active = buf;
    } else if (buf.id != buffers.active.id) {
        $remove($buffer(buffers.active.id))
        buffers[buf.id] = buf;
        buffers.active = buf;
    }

    for (var p in buf) {
        //TODO: client and server should use same name
        if (p == 'pos') {
            buffers.active.cursor = buf.pos;
            watchers = watchers.concat(localbufWatchers.cursor || []);
        } else if (buf.hasOwnProperty(p)) {
            buffers.active[p] = buf[p];
            watchers = watchers.concat(localbufWatchers[p] || []);
        }
    }

    (watchers || []).forEach(function(f) {
        f(buffers.active);
    });

    delete buffers.active.str;
    delete buffers.active.changes;
}

function render(buf) {
    if (!buf) return;
    if (typeof buf.id == 'undefined') {
        buf.id = buffers.active.id;
    }

    $buffer(buf.id);

    var newbuf = !!buf.str;
    mergeToLocal(buf);

    window.requestAnimationFrame(function() {
        renderViewport();
        scrollToCursor(buffers.active, newbuf);
    });
}
