Array.prototype.peek = function() {
	return this[this.length-1];
};

Array.prototype.equal = function(arr) {
	if (arr.length != this.length)
		return false;

	for (var i = 0; i < arr.length; i++) {
		if (this[i] != arr[i])
			return false;
	}

	return true;
};

function hlclojure() {
	function replaceBracesClass(ele, cls) {
		var oldcls = ele.className;
		if (/brace-(\d|miss)/.test(oldcls)) {
			ele.className = oldcls.replace(/brace-(\d|miss)/, cls);
		} else {
			ele.className += ' ' + cls;
		}
	}

	//each line has a start state
	//each state is a stack of open braces before this line
	//TODO Expand this strategy to general increment syntax parsing. Save syntax parser state to each line, if one line changes start parse syntax from that line until meet equal state or EOF (or stop parse when pass screen boundary)
	var states = [];

	var rightBraces = {'(':')', '[':']', '{':'}'};
	var leftBraces = {')':'(', ']':'[', '}':'{'};
	var keywords = /(;.*|#"[^"]*"|:[\w-]+|@[\w-]+|"[^"]*"|\(|\)|\[|\]|\{|\}|%\d+|-[\d.]+(?![\w-])|\b[\d.]+(?![\w-])|->|->>|\+|>=|<=|>|<|\*|-|\b\/|==|=|%|\bprint(?![\w-])|\bprintln(?![\w-])|\bdefonce(?![\w-])|\batom(?![\w-])|\bref(?![\w-])|\bagent(?![\w-])|\bdec(?![\w-])|\binc(?![\w-])|\bfn(?![\w-])|\bdefn(?![\w-])|\bif(?![\w-])|\blet(?![\w-])|\bcond(?![\w-])|\bcount(?![\w-])|\bdef(?![\w-])|\bdo(?![\w-])|\bdoseq(?![\w-])|\bloop(?![\w-])|\brecur(?![\w-])|\bstr(?![\w-])|\btry(?![\w-])|\bcatch(?![\w-])|\bmerge(?![\w-])|\bupdate(?![\w-])|\bassoc(?![\w-])|\bupdate-in(?![\w-])|\bdissoc(?![\w-])|\bnil\?(?![\w-])|\bnil(?![\w-])|\btrue(?![\w-])|\bswap!(?![\w-])|\bfalse(?![\w-])|\bsubs(?![\w-])|\bsubvec(?![\w-])|\bvec(?![\w-])|\bvec\?(?![\w-])|\bmap\?(?![\w-])|\bfn\?(?![\w-])|\btrue\?(?![\w-])|\bmap(?![\w-]))/g;
	var hl = {};	

	hl.states = states;
	hl.parseLine = function parseLine(line, row) {
		var lasti = 0;
		var html = '';
	
		if (!states[row]) {
			states[row] = [];
		}

		var state = states[row].slice();
		while((result = keywords.exec(line)) != null) {
			result = result[1];
			//[lasti...][i...]keywords.lastIndex
			var i = keywords.lastIndex-result.length;
	
			html+= htmlEncode(line.substring(lasti, i));
	
			var cls='';
			var firstchar = result[0]
			if (':' == firstchar || '@' == firstchar) {
				cls = 'colon-keyword';
			} else if (';' == firstchar) { 
				cls = 'comment';
			} else if ('#' == firstchar) {
				cls = 'regexp';
			} else if ('"' == firstchar) {
				cls = 'string';
			} else if (/^%\d+$/.test(result)) {
				cls = 'colon-keyword';
			} else if (/^-?[\d.]+$/.test(result)) {
				cls = 'number';
			} else if (/\(|\[|\{/.test(firstchar)) {
				cls = 'brace brace-'+state.length%6;
				state.push(firstchar);
			} else if (/\)|\]|\}/.test(firstchar)) {
				var ch = leftBraces[firstchar];
				if (state.peek() == ch) {
					state.pop();
					cls = 'brace brace-'+state.length%6;
				} else {
					cls = 'brace brace-miss';
				}
			} else {
				cls = 'keyword';
			}
			html += '<span class="'+cls+'">'+htmlEncode(result)+'</span>';
	
			lasti = keywords.lastIndex;
		}
	
		html += htmlEncode(line.substring(lasti));
	
		states[row+1] = state;
		return html;
	}


	hl.refreshLines = function updateBraces(row, cnt) {
		if (row < 0) row = 0;
		for (var i = row; i < cnt; i++) {
			var state = states[i].slice();
			//var line = $(lineid(i))[0].textContent;
			var braces = $(lineid(i)+' .brace');
			for (var j = 0; j < braces.length; j++) {
				var ch = braces[j].textContent[0];
				if (/\(|\[|\{/.test(ch)) {
					replaceBracesClass(braces[j],'brace-'+state.length%6);
					state.push(ch);
				} else if (/\)|\]|\}/.test(ch)) {
					if (state.peek() == leftBraces[ch]) {
						state.pop();
						replaceBracesClass(braces[j],'brace-'+state.length%6);
					} else {
						replaceBracesClass(braces[j],'brace-miss');
					}
				}
			}
	
	
			//If start state and text content are both not changed then parse result will be same
			//There is no need to continue if next line's last start state is equal to current
			if (states[i+1].equal(state)) {
				//console.log('break:'+(i+1));
				break;
			} else {
				states[i+1] = state;
			}
		}
	}
	
	return hl;
}

function hljavascript() {
	function replaceBracesClass(ele, cls) {
		var oldcls = ele.className;
		if (/brace-(\d|miss)/.test(oldcls)) {
			ele.className = oldcls.replace(/brace-(\d|miss)/, cls);
		} else {
			ele.className += ' ' + cls;
		}
	}

	var rightBraces = {'{':'}'};
	var leftBraces = {'}':'{'};
	var keywords = /(\/\/.*|\/[^/]+\/|"[^"]*"|'[^']*'|\{|\}|-[\d.]+\b|\b[\d.]+\b|&&|\|\||\+|>=|<=|>|<|\*|-|\b\/|==|=|%|\bfunction\b|\bthis\b|\bconsole\b|\bfor\b|\belse\b|\breturn\b|\bArray\b|\bvar\b|\btrue\b|\bcase\b|\bswitch\b|\bfalse\b|\bdocument\b|\bwindow\b|\bif\b|\bwhile\b|\bnull\b|\bbreak\b)/g;
	var states = [];
	var hl = {};

	hl.states = states;

	hl.parseLine = function parseLine(line, row) {
		var lasti = 0;
		var html = '';
	
		if (!states[row]) {
			states[row] = [];
		}

		var state = states[row].slice();
		while((result = keywords.exec(line)) != null) {
			result = result[1];
			//[lasti...][i...]keywords.lastIndex
			var i = keywords.lastIndex-result.length;
	
			html+= htmlEncode(line.substring(lasti, i));
	
			var cls='';
			var firstchar = result[0];
			if (':' == firstchar || '@' == firstchar) {
				cls = 'colon-keyword';
			} else if (/^\/\//.test(result)) { 
				cls = 'comment';
			} else if (/\/[^/]+\//.test(result)) {
				cls = 'regexp';
			} else if ('"' == firstchar || '\'' == firstchar) {
				cls = 'string';
			} else if (/^%\d+$/.test(result)) {
				cls = 'colon-keyword';
			} else if (/^-?[\d.]+$/.test(result)) {
				cls = 'number';
			} else if (/\{/.test(firstchar)) {
				cls = 'brace brace-'+state.length%6;
				state.push(firstchar);
			} else if (/\}/.test(firstchar)) {
				var ch = leftBraces[firstchar];
				if (state.peek() == ch) {
					state.pop();
					cls = 'brace brace-'+state.length%6;
				} else {
					cls = 'brace brace-miss';
				}
			} else {
				cls = 'keyword';
			}
			html += '<span class="'+cls+'">'+htmlEncode(result)+'</span>';
	
			lasti = keywords.lastIndex;
		}
	
		html += htmlEncode(line.substring(lasti));
	
		states[row+1] = state;
		return html;
	}


	hl.refreshLines = function updateBraces(row, cnt) {
		if (row < 0) row = 0;
		for (var i = row; i < cnt; i++) {
			var state = states[i].slice();
			//var line = $(lineid(i))[0].textContent;
			var braces = $(lineid(i)+' .brace');
			for (var j = 0; j < braces.length; j++) {
				var ch = braces[j].textContent[0];
				if (/\{/.test(ch)) {
					replaceBracesClass(braces[j],'brace-'+state.length%6);
					state.push(ch);
				} else if (/|\}/.test(ch)) {
					if (state.peek() == leftBraces[ch]) {
						state.pop();
						replaceBracesClass(braces[j],'brace-'+state.length%6);
					} else {
						replaceBracesClass(braces[j],'brace-miss');
					}
				}
			}
	
	
			//If start state and text content are both not changed then parse result will be same
			//There is no need to continue if next line's last start state is equal to current
			if (states[i+1].equal(state)) {
				//console.log('break:'+(i+1));
				break;
			} else {
				states[i+1] = state;
			}
		}
	}
	
	return hl;
}

function hldefault() {
	var hl = {};
	hl.parseLine = function(line, row) {
		return htmlEncode(line);
	};
	hl.refreshLines = function() {
	};
	return hl;
}

var highlights = {
	'Clojure': hlclojure,
	'JavaScript': hljavascript,
	'CSS': hljavascript,
	'default': hldefault
};

function newHighlight(lang) {
	var fn = highlights[lang] || highlights['default'];
	return fn();
}
