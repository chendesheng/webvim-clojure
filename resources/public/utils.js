String.prototype.contains = function(str) {
    return this.indexOf(str) >= 0;
}
String.prototype.eachLine = function(fn) {
    var str = this;
    var num = 0;
    while (true) {
        var i = str.indexOf('\n') + 1;
        if (i == 0) {
            break;
        } else {
            fn(str.substring(0, i), num);
            str = str.substring(i);
        }
        num++;
    }
    return num;
};

String.prototype.count = function(ch) {
    var i = 0;
    var cnt = 0;
    while (true) {
        var nexti = this.indexOf(ch, i);
        if (nexti == -1)
            break;
        i = nexti + ch.length;
        cnt++;
    }

    return cnt;
};

String.prototype.eachBlock = function(fn, n) {
    n = n || 100;
    var str = this;
    var num = 0;
    while (str.length > 0) {
        //ensure callback string length (str.length) >= 0.5n, avoid fragment code block
        if (str.length < 1.5 * n) {
            fn(str, num);
            num++;
            break;
        } else {
            var i = str.substring(n).search(/\s/);

            if (i == -1) {
                fn(str, num);
                num++;
                break;
            } else {
                i = i + n + 1;
                fn(str.substring(0, i), num);
                num++;
                str = str.substring(i);
            }
        }
    }

    return num;
};

Array.prototype.peek = function() {
    return this[this.length - 1];
};

Array.prototype.each = function(fn) {
    for (var i = 0; i < this.length; i++) {
        fn(this[i], i);
    }
};

Array.prototype.contains = function(item) {
    for (var i = 0; i < this.length; i++) {
        if (this[i] == item) {
            return true;
        }
    }

    return false;
}

Array.prototype.equal = function(arr) {
    if (arr == this) return true;
    if (arr.length != this.length) return false;

    for (var i = 0; i < arr.length; i++) {
        if (this[i] != arr[i])
            return false;
    }

    return true;
};

Array.prototype.count = function(pred) {
    var n = 0;
    for (var i = this.length - 1; i >= 0; i--) {
        if (pred(this[i])) {
            n++;
        }
    }
    return n;
}

Array.prototype.toSet = function() {
    var set = {};
    this.each(function(s) {
        set[s] = true;
    });
    return set;
};

RegExp.quote = function(str) {
    return (str + '').replace(/[.?*+^$[\]\\(){}|-]/g, "\\$&");
};

Array.prototype.reverse = function() {
    var arr = [];
    for (var i = this.length - 1; i >= 0; i--) {
        arr.push(this[i]);
    }
    return arr;
};

//Returns the first logical true value of pred(x) for any x in array, else nil.
Array.prototype.some = function(pred) {
    for (var i = this.length - 1; i >= 0; i--) {
        var item = this[i];
        if (pred(item)) {
            return item;
        }
    }
};


//http://stackoverflow.com/questions/4775722/check-if-object-is-array
function isArray(arr) {
    return Object.prototype.toString.call(arr) === '[object Array]';
}



function isChinese(c) {
    return /[\ufe30-\uffa0\u4e00-\u9eff\u3000-\u303F]/.test(c);
}

function replaceBinding(html, data) {
    for (var p in data) {
        if (data.hasOwnProperty(p)) {
            var v = data[p];
            html = html.replace(new RegExp('{' + p + '}', 'g'), v);
        }
    }
    return html;
}

//https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign#Polyfill
if (typeof Object.assign != 'function') {
    (function() {
        Object.assign = function(target) {
            'use strict';
            if (target === undefined || target === null) {
                throw new TypeError('Cannot convert undefined or null to object');
            }

            var output = Object(target);
            for (var index = 1; index < arguments.length; index++) {
                var source = arguments[index];
                if (source !== undefined && source !== null) {
                    for (var nextKey in source) {
                        if (source.hasOwnProperty(nextKey)) {
                            output[nextKey] = source[nextKey];
                        }
                    }
                }
            }
            return output;
        };
    })();
}
