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
	hljs.C_LINE_COMMENT_MODE = hljs.COMMENT('//', '\n');
	hljs.C_BLOCK_COMMENT_MODE = hljs.COMMENT('/\\*', '\\*/');
	hljs.HASH_COMMENT_MODE = hljs.COMMENT('#', '\n');
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
			parseBlock: function(block, row) {
				return [[null, block]];
			},
			refreshLines: function() {}
		};
	}

	function reStr(re) {
		return (re && re.source) || re;
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
		if (mode.contains) {
			mode.contains.each(function(c) {
				compile(c);
			});
		} else if (mode.subLanguage) {
			var lang = highlights[mode.subLanguage];
			if (!lang) {
				throw compileError('Refer an unexsits language "'+mode.subLanguage+'".');
			}
			lang.root = true;
			var submode = compile(lang);
			mode.contains = submode.contains;

			//merge sub language illegal into current mode
			if (submode.illegal) {
				mode.illegal = concatArray(mode.illegal, submode.illegal);
			}
		} else {
			mode.contains = [];
		}

		if (mode.begin) {
			mode.rebegin = new RegExp('^'+reStr(mode.begin));
		}

		if (mode.starts) {
			mode.starts = compile(mode.starts);
		}

		//merge illegal and end into contains set terminator=true
		if (mode.illegal) {
			if (!(mode.illegal instanceof Array)) {
				mode.illegal = [mode.illegal];
			}
			var begin = mode.illegal.map(reStr).join('|');
			mode.contains.unshift(compile({
				className: mode.className,
				begin: begin,
				beginCapture: mode.illegalCapture,
				terminator: true
			}));
		}

		if (mode.end) {
			var endmode = compile({
				className: mode.className,
				begin: mode.end,
				beginCapture: mode.endCapture,
				terminator: true
			});

			mode.contains.push(endmode)
		}

		if (mode.contains.length > 0) {
			//recontains is stateful and will be reused in next parsing over and over
			//so it MUST reset lastIndex before use it.
			var str = mode.contains
				.filter(function(c) {return c.begin})
				.map(function(c) { return reStr(c.begin)})
				.join('|');
			mode.recontains = new RegExp(str, 'g');

			//language root doesn't have begin but submodes must have one
			if (!mode.root && !mode.begin) {
				mode.begin = str;
				mode.rebegin = new RegExp('^'+str);
			}
		}

		return mode;
	}

	ROOT.root = true;
	var rootCompiled = compile(ROOT);

	function writeOutput(ctx, className, text) {
		if (!text) {
			return;
		}

		var prev = ctx.output.peek();
		if (prev && prev[0] == className) {
			//merge to previous item if same className (this can reduce about 100 nodes on testfile.clj file)
			prev[1] += text;
		} else {
			ctx.output.push([className, text]);
		}
	}

	//Pesudo code:
	//fun parse(ctx) {
	//while(not EOF) {
	//	mode = ctx.peek()
	//	if (captured=ctx.matchcontains_end()) {
	//		for submode in ctx.submodes {
	//			if (submode.matchbegin(captured) {
	//				if (termintor) {
	//					ctx.pop()
	//					if (mode.starts) ctx.push(mode.starts)
	//					break
	//				}
	//				writeOutput
	//				ctx.push(submode)
	//				break
	//			}
	//		}
	//	} else {
	//		writeOutput
	//		set EOF
	//	}
	//}
	function modeReContains(mode, index) {
		if (!mode) return null;

		var recontains = mode.recontains;
		if (!recontains) return null;

		recontains.lastIndex = index;
		return recontains;
	}
	function parse(ctx) {
		var block = ctx.block;
		while(ctx.index < block.length) {
			var mode = ctx.modes.peek();
//			console.log(mode.begin);
//			console.log(mode.block);
//			console.log(mode.rebegin);
//			console.log(mode.recontains);

			var recontains = modeReContains(mode, ctx.index);

			if(recontains && (result = recontains.exec(block)) != null) {
				var captured = result[0];
				var matched = false;
				for (var i = 0; i < mode.contains.length; i++) {
					var c = mode.contains[i];
					if (c.rebegin.test(captured)) {
						matched = true;
						writeOutput(ctx, mode.className, block.substring(ctx.index, recontains.lastIndex-captured.length));
						ctx.index = recontains.lastIndex;
						if (c.beginCapture) {
						      writeOutput(ctx, c.beginCapture(ctx, captured), captured);
						} else {
						      writeOutput(ctx, c.className, captured);
						}

						if (c.terminator) {
							ctx.modes.pop();
							if (mode.starts) {
								ctx.modes.push(mode.starts);
							}
						} else if (c.recontains) { 
							ctx.modes.push(c);
						} else {
							if (c.starts) {
								ctx.modes.push(c.starts);
							}
						}
						break;
					}
				}

				//match recontains MUST match one of contains[i].rebegin too
				if (!matched) {
					throw 'Something wrong with syntax descriptor, should never reach here';
				}

			} else {
				if (ctx.index < block.length) {
					writeOutput(ctx, (mode||{}).className, block.substring(ctx.index));
					ctx.index = block.length;
				}
			}
		}
	}

	function doParse(block, startModes) {
		var ctx = {
			block: block, 
			index: 0,
			modes: startModes.slice(),
			output: [] //[[className, text], [className, text]...]
		};
		parse(ctx);
		return ctx;
	}

	var hl = {};
	hl.states; //caller should init states before call parseBlock

	//var nodeCount = 0;
	//parse one block and save end state to next block
	//call this function when block changes
	hl.parseBlock = function(block, row) {
		var states = hl.states;
		if (states[row]==null) {
			states[row] = [rootCompiled];
		}
		var ctx = doParse(block, states[row]);
		states[row+1] = ctx.modes;
		//logmodes(row+1);
		
		//nodeCount += ctx.output.length;
		//console.log('output count:' + nodeCount);
		return ctx.output;
	};

	hl.parse = function(block, state) {
		if (state==null) {
			state = [rootCompiled];
		}
		var ctx = doParse(block, state);
		return [ctx.modes, ctx.output];
	};

	hl.refresh = function(iter) {
		var states = hl.states;
		while(true) {
			var i = iter.index();
			var ctx = doParse(iter.text(), states[i]);
			iter.render(ctx.output);
			if (states[i+1].equal(ctx.modes)) {
				//doParse has 3 arguments, in next lines none of them changed
				//so no syntax will be changed in next lines
				return;
			} else {
				states[i+1] = ctx.modes;
			}

			if (!iter.next()) {
				break;
			}
		}
	}

	function logmodes(index) {
		var out = [index];
		hl.states[index].each(function(c) {
			out.push(c.begin);
		});
		console.log(out);
	}
	return hl;
}
