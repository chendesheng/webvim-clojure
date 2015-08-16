Array.prototype.peek = function() {
	return this[this.length-1];
};

Array.prototype.each = function(fn) {
	for (var i = 0; i < this.length; i++) {
		fn(this[i], i);
	}
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


var highlights = (function() {
	function inherit(parent, obj) {
		var result = {}, key;
		for (key in parent)
			result[key] = parent[key];
		if (obj)
			for (key in obj)
				result[key] = obj[key];
		return result;
	}

	var hljs = {};
	hljs.toSet = function(str) { return str.split(/\s+/).toSet(); }
	hljs.inherit = inherit;
	// Common regexps
	hljs.IDENT_RE = '[a-zA-Z]\\w*';
	hljs.UNDERSCORE_IDENT_RE = '[a-zA-Z_]\\w*';
	hljs.NUMBER_RE = '\\b\\d+(\\.\\d+)?';
	hljs.C_NUMBER_RE = '(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)'; // 0x..., 0..., decimal, float
	hljs.BINARY_NUMBER_RE = '\\b(0b[01]+)'; // 0b...
	hljs.RE_STARTERS_RE = '!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~';

	// Common modes
	hljs.BACKSLASH_ESCAPE = {
		className: 'escape',
		begin: '\\\\[\\s\\S]', relevance: 0
	};
	hljs.APOS_STRING_MODE = {
		className: 'string',
		begin: '\'', end: '\'',
		illegal: '\\n',
		contains: [hljs.BACKSLASH_ESCAPE]
	};
	hljs.QUOTE_STRING_MODE = {
		className: 'string',
		begin: '"', end: '"',
		illegal: '\\n',
		contains: [hljs.BACKSLASH_ESCAPE]
	};
	hljs.PHRASAL_WORDS_MODE = {
		begin: /\b(a|an|the|are|I|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such)\b/
	};
	hljs.COMMENT = function (begin, end, inherits) {
		var mode = hljs.inherit(
			{
				className: 'comment',
				begin: begin, end: end,
				contains: []
			},
			inherits || {}
		);
		mode.contains.push(hljs.PHRASAL_WORDS_MODE);
		mode.contains.push({
		className: 'doctag',
		begin: "(?:TODO|FIXME|NOTE|BUG|XXX):",
			relevance: 0
		});
		return mode;
	};
	hljs.C_LINE_COMMENT_MODE = hljs.COMMENT('//', '$');
	hljs.C_BLOCK_COMMENT_MODE = hljs.COMMENT('/\\*', '\\*/');
	hljs.HASH_COMMENT_MODE = hljs.COMMENT('#', '$');
	hljs.NUMBER_MODE = {
		className: 'number',
		begin: hljs.NUMBER_RE,
		relevance: 0
	};
	hljs.C_NUMBER_MODE = {
		className: 'number',
		begin: hljs.C_NUMBER_RE,
		relevance: 0
	};

	hljs.BINARY_NUMBER_MODE = {
		className: 'number',
		begin: hljs.BINARY_NUMBER_RE,
		relevance: 0
	};

	hljs.CSS_NUMBER_MODE = {
		className: 'number',
		begin: hljs.NUMBER_RE + '(' +
		'%|em|ex|ch|rem'  +
		'|vw|vh|vmin|vmax' +
		'|cm|mm|in|pt|pc|px' +
		'|deg|grad|rad|turn' +
		'|s|ms' +
		'|Hz|kHz' +
		'|dpi|dpcm|dppx' +
		')?',
		relevance: 0
	};

	hljs.REGEXP_MODE = {
		className: 'regexp',
		begin: /\//, end: /\/[gimuy]*/,
		illegal: /\n/,
		contains: [
			hljs.BACKSLASH_ESCAPE,
			{
				begin: /\[/, end: /\]/,
				relevance: 0,
				contains: [hljs.BACKSLASH_ESCAPE]
			}
		]
	};

	hljs.TITLE_MODE = {
		className: 'title',
		begin: hljs.IDENT_RE,
		relevance: 0
	};

	hljs.UNDERSCORE_TITLE_MODE = {
		className: 'title',
		begin: hljs.UNDERSCORE_IDENT_RE,
		relevance: 0
	};


	return {
		'Clojure': hlclojure(hljs),
		'JavaScript': hljavascript(hljs),
		'CSS': hlcss(hljs),
		'XML': hlxml(hljs),
	}
})();

function newHighlight(lang) {
	return hlcompile(highlights[lang]);
}

function hlcompile(ROOT) {
	if (!ROOT) {
		return {
			parseLine: function(line, row) {
				return htmlEncode(line);
			},
			refreshLines: function() {}
		};
	}

	function reStr(re) {
		return (re && re.source) || re;
	}

	function contains2re(mode) {
		var all = [];
		//illegal is similar with end with higher priority and can be used as an exit pattern
		//nested language use illegal to back to parent when match "language end pattern" (like </script>)
		if (mode.illegal) {
			if (!(mode.illegal instanceof Array)) {
				mode.illegal = [mode.illegal];
			}

			var illegals = []
			mode.illegal.each(function(re) {
				illegals.push(reStr(re));
			});
			mode.reIllegal = new RegExp(illegals.join('|'));

			all = all.concat(illegals);
		}

		if (mode.contains) {
			mode.contains.each(function(c) {
				if(c.begin) {
					all.push(reStr(c.begin));
				}
			});
		}
		//it's greedy so end is the last one
		if (mode.end) {
			all.push(reStr(mode.end));
		}

		return new RegExp(all.join('|'), 'g');
	}

	function compileError(msg) {
		return 'Compile Error: ' + msg;
	}

	//return new array
	function concatArray(a, b) {
		a = a || [];
		b = b || [];
		a=(a instanceof Array) ? a : [a];
		b=(b instanceof Array) ? b : [b];
		return a.concat(b);
	}

	//generate some regexps first.
	function compile(mode) {
		//prevent circle reference
		if (mode.compiled) return mode;
		mode.compiled = true;

		if (mode.begin) {
			mode.rebegin = new RegExp('^'+reStr(mode.begin));
		}

		if (mode.starts) {
			mode.starts = compile(mode.starts);
		}

		if (mode.contains) {
			mode.contains.each(function(c) {
				compile(c);
			});
		} else if (mode.subLanguage) {
			var lang = highlights[mode.subLanguage];
			if (!lang) {
				throw compileError('Refer an unexsits language "'+mode.subLanguage+'".');
			}
			var submode = compile(lang);
			mode.contains = submode.contains;

			//merge sub language illegal into current mode
			if (submode.illegal) {
				mode.illegal = concatArray(mode.illegal, submode.illegal);
			}
		}

		if (mode.contains || mode.end || mode.illegal) {
			//recontains is stateful and will be reused in next parsing over and over
			//so it MUST reset lastIndex before use it.
			mode.recontains = contains2re(mode);
		}

		if (mode.end) {
			mode.reend = new RegExp('^'+reStr(mode.end)+'$');
		}

		return mode;
	}

	var rootCompiled = compile(ROOT);

	function buildSPAN(className, text) {
		if (text.length == 0) return '';
		else if (!className) return htmlEncode(text);
		return '<span class="'+className+'">'+htmlEncode(text)+'</span>';
	}

	//Pesudo code:
	//fun parse(ctx) {
	//	if (ctx.needRestore()) {
	//		ctx.depth++
	//		parse(ctx)
	//		ctx.depth--
	//	}
	//
	//	while (captured=ctx.matchcontains_end()) {
	//		if (ctx.matchIllegal) {
	//			ctx.output += buildSPAN(illegal)
	//			var lastmode = ctx.pop()
	//			return
	//		} else if (ctx.matchend) {
	//			ctx.output += buildSPAN(end)
	//			var lastmode = ctx.pop()
	//			parseNext(lastmode.starts)
	//			return
	//		} else {
	//			for submode in ctx.submodes {
	//				if (submode.matchbegin(captured) {
	//					ctx.output += buildSPAN(submode.begin)
	//					parseNext(submode)
	//				}
	//			}
	//		}
	//	}
	//	ctx.output += rest
	//}
	//TODO: Convert recur to loop, this should be easy because ctx.modes stack is already there
	//TODO: Separent parse and html generate, parse only outputs array of nodes
	function parse(ctx) {
		var line = ctx.line;
		var mode = ctx.mode;
		if (ctx.modes.length > ctx.depth) {
			//recover modes back
			ctx.mode = ctx.modes[ctx.depth];
			ctx.depth++;
			parse(ctx);
			ctx.depth--;
		}

		var recontains = mode.recontains;
		recontains.lastIndex = ctx.index;
		while((result = recontains.exec(line)) != null) {
			var captured = result[0];
			if (mode.reIllegal && mode.reIllegal.test(captured)) {
				var className = mode.illegalCapture?mode.illegalCapture(ctx, captured):mode.className;
				ctx.output += buildSPAN(className, line.substring(ctx.index, recontains.lastIndex));
				ctx.index = recontains.lastIndex;
				ctx.modes.pop();
				return;
			} else if (mode.reend && mode.reend.test(captured)) {
				var className = mode.endCapture?mode.endCapture(ctx, captured):mode.className;
				ctx.output += buildSPAN(className, line.substring(ctx.index, recontains.lastIndex));
				ctx.index = recontains.lastIndex;
				var nextmode = ctx.modes.pop().starts;
				//starts next mode after pop()
				if (nextmode) {
					parseNext(ctx, nextmode);
				}
				return;
			} else {
				var matched = false;
				for (var i = 0; i < mode.contains.length; i++) {
					var c = mode.contains[i];
					if (c.rebegin.test(captured)) {
						matched = true;
						ctx.output += buildSPAN(mode.className, line.substring(ctx.index, recontains.lastIndex-captured.length));
						if (c.beginCapture) {
							ctx.output += buildSPAN(c.beginCapture(ctx, captured), captured);
						} else {
							ctx.output += buildSPAN(c.className, captured);
						}
						if (c.recontains || c.reend) { 
							ctx.index = recontains.lastIndex;
							parseNext(ctx, c);
						} else {
							ctx.index = recontains.lastIndex;
						}
						break;
					}
				}

				if (!matched) {
					throw 'Something wrong inside contains2re, should never reach here';
				}

				recontains.lastIndex = ctx.index;
			}
		}

		if (ctx.index < line.length) {
			ctx.output += buildSPAN(mode.className, line.substring(ctx.index));
			ctx.index = line.length;
		}
	}

	function parseNext(ctx, mode) {
		var saved = ctx.mode;
		ctx.modes.push(mode);
		ctx.mode = mode;
		ctx.depth++;
		parse(ctx);
		ctx.depth--;
		ctx.mode = saved;
	}

	function doParse(line, rootMode, lastModes) {
		var ctx = {
			line: line, 
			index: 0,
			mode: rootMode,
			modes: lastModes,
			output: '',
			depth:0
		};
		parse(ctx);
		return ctx;
	}

	var hl = {};
	hl.states; //caller should init states before call parseLine

	//parse one line and save end state to next line
	//call this function when line changes
	hl.parseLine = function(line, row) {
		var states = hl.states;
		if (!states[row]) {
			states[row] = [];
		}
		//logmodes(row);
		var ctx = doParse(line, rootCompiled, states[row].slice());
		states[row+1] = ctx.modes;
		return ctx.output;
	};

	//These lines' text are not changed but syntax affected by previous lines change
	hl.refreshLines = function(row, cnt) {
		var states = hl.states;
		for (var i = row; i < cnt; i++) {
			var $line = $('#line-'+i+' pre');
			var line = $line[0].textContent;

			//logmodes(i);
			var ctx = doParse(line, rootCompiled, states[i].slice());
			$line.html(ctx.output);
			if (states[i+1].equal(ctx.modes)) {
				//doParse has 3 arguments, in next lines none of them changed
				//so no syntax will be changed in next lines
				return;
			} else {
				states[i+1] = ctx.modes;
			}
		}
	};

	function logmodes(row) {
		var out = [row];
		hl.states[row].each(function(c) {
			out.push(c.begin);
		});
		console.log(out);
	}
	return hl;
}
