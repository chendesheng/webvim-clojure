String.prototype.eachLine = function(fn) {
	var str = this;
	var num = 0;
	while(true) {
		var i = str.indexOf('\n')+1;
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
	while(true) {
		var nexti = this.indexOf(ch, i);
		if (nexti == -1)
			break;
		i = nexti+ch.length;
		cnt++;
	}

	return cnt;
};

String.prototype.eachBlock = function(fn, n) {
	n = n || 100;
	var str = this;
	var num = 0;
	while(str.length > 0) {
		//ensure callback string length (str.length) >= 0.5n, avoid fragment code block
		if (str.length < 1.5*n) {  
			fn(str, num);
			num++;
			break;
		} else {
			var i = str.substring(n).search(/\s/);

			if (i==-1) {
				fn(str, num);
				num++;
				break;
			} else {
				i = i+n+1;
				fn(str.substring(0, i), num);
				num++;
				str = str.substring(i);
			}
		}
	}

	return num;
};

Array.prototype.peek = function() {
	return this[this.length-1];
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

Array.prototype.map = function(fn) {
	var cp = new Array(this.length);
	this.each(function(item, i) {
		var res = fn(item);
		cp[i] = res;
	});
	return cp;
};

Array.prototype.filter = function(fn) {
	var cp = [];
	this.each(function(item, i) {
		if (fn(item)) {
			cp.push(item);
		}
	});
	return cp;
};

Array.prototype.toSet = function () {
	var set = {};
	this.each(function(s) {
		set[s] = true;
	});
	return set;
};

RegExp.quote = function(str) {
	    return (str+'').replace(/[.?*+^$[\]\\(){}|-]/g, "\\$&");
};

Array.prototype.reverse = function () {
	var arr = [];
	for (var i = this.length-1; i >= 0; i--) {
		arr.push(this[i]);
	}
	return arr;
};



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

