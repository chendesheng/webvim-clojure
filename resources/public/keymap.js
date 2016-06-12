//map key to another get return from callback
function imap(key, callback) {
    if (!key) {
        return;
    }
    if (key == '<c-j>') {
        //can't capture ctrl-n and ctrl-p on Windows's Chrome
        key = '<c-n>';
    } else if (key == '<c-k>') {
        key = '<c-p>';
    } else if (key == '<c-s>') {
        callback('<esc>:w<cr>');
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
        ongoingkeys.each(function(key) {
            callback(key);
        });

        ongoingkeys = [];
    }
}

function nmap(key, callback) {
    if (key == '<c-l>') {
        callback(':nohl<cr>');
    } else if (key == '<c-[>') {
        callback('<esc>');
    } else if (key == '<c-s>') {
        callback(':w<cr>');
    } else if (key == '<f6>') {
        callback(':reload<cr>');
    } else {
        callback(key);
    }
}

var vmap = nmap;
var keymaps = [nmap, imap, vmap];
var keymap = nmap;

var ongoingkeys = [];
var ongoingkeysTimer;

function showOngoingKey(key) {
    if (key.length == 1) {
        var cursor = $cursor(buffers.active.id);
        cursor.firstChild.textContent = key;
        cursor.className = 'cursor ongoing-key';
    }
}

function hideOngoingKey() {
    $cursor(buffers.active.id).className = 'cursor';
}

watchLocalbufChange('mode', function(buf) {
    if (buf.mode == 0) {
        if (buf.visual.type == 0) {
            keymap = nmap;
        } else {
            keymap = vmap;
        }
    }

    if (buf.mode == 1) {
        keymap = imap;
    }
});
