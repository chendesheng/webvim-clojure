function testRe(re, lexeme) {
	var match = re && re.exec(lexeme);
	return match && match.index == 0;
}

function inherit(parent, obj) {
	var result = {}, key;
	for (key in parent)
		result[key] = parent[key];
	if (obj)
		for (key in obj)
			result[key] = obj[key];
	return result;
}

var highlights = (function() {

	var hljs = {};
	hljs.inherit = inherit;
	
	// Common regexps
	hljs.IDENT_RE = '[a-zA-Z]\\w*';
	hljs.UNDERSCORE_IDENT_RE = '[a-zA-Z_]\\w*';
	hljs.NUMBER_RE = '\\b\\d+(\\.\\d+)?';
	hljs.C_NUMBER_RE = '(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)'; // 0x..., 0..., decimal, float
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
		begin: /\b(a|an|the|are|I|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|like)\b/
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

function hlcompile(language) {
	if (!language) {
		var hl = {
			parse: function(block, state) {
				return [[], [[null, block]]];
			},
			parseBlock: function(block, row) {
				if (hl.states[row] == null)
					hl.states[row]=[];
				return [[null, block]];
			},
			refresh: function(iter) {
				iter.render([[null, iter.text()]]);
			}
		};
		return hl;
	}

	function reStr(re) {
		return (re && re.source) || re;
	}

	function langRe(value, global) {
      return new RegExp(
        reStr(value),
        'm' + (language.case_insensitive ? 'i' : '') + (global ? 'g' : '')
      );
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
	function compile(mode, parent) {
		//prevent circle reference
		if (mode.compiled) return mode;
		mode.compiled = true;

		mode.keywords = mode.keywords || mode.beginKeywords;
		
		if (mode.keywords) {
			var compiled_keywords = {};

			var flatten = function(className, str) {
				if (language.case_insensitive) {
					str = str.toLowerCase();
				}
				str.split(' ').forEach(function(kw) {
					var pair = kw.split('|');
					compiled_keywords[pair[0]] = [className, pair[1] ? Number(pair[1]) : 1];
				});
			};

			if (typeof mode.keywords == 'string') { // string
				flatten('keyword', mode.keywords);
			} else {
				Object.keys(mode.keywords).forEach(function (className) {
					flatten(className, mode.keywords[className]);
				});
			}
			mode.keywords = compiled_keywords;
		}
		
		mode.lexemesRe = langRe(mode.lexemes || /\b\w+\b/, true);
		
		if (parent) {
			if (mode.beginKeywords) {
				mode.begin = '\\b(' + mode.beginKeywords.split(' ').join('|') + ')\\b';
			}
			if (!mode.begin)
				mode.begin = /\B|\b/;
			mode.beginRe = langRe(mode.begin);
			if (!mode.end && !mode.endsWithParent)
				mode.end = /\B|\b/;
			if (mode.end)
				mode.endRe = langRe(mode.end);
			mode.terminator_end = reStr(mode.end) || '';
			if (mode.endsWithParent && parent.terminator_end)
				mode.terminator_end += (mode.end ? '|' : '') + parent.terminator_end;
      	}
      	
      	if (mode.illegal)
      		mode.illegalRe = langRe(mode.illegal);
      	if (mode.relevance === undefined)
   			mode.relevance = 1;
		if (!mode.contains)
			mode.contains = [];
		
		var expanded_contains = [];
		mode.contains.forEach(function(c) {
			if (c.variants) {
				c.variants.forEach(function(v) {expanded_contains.push(inherit(c, v));});
			} else {
				expanded_contains.push(c == 'self' ? mode : c);
			}
		});
		mode.contains = expanded_contains;

		if (mode.contains) {
			mode.contains.each(function(c) {
				compile(c, mode);
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

		if (mode.starts) {
			mode.starts = compile(mode.starts, parent);
		}

		//merge illegal and end into contains set terminator=true
//		if (mode.illegal) {
//			if (!(mode.illegal instanceof Array)) {
//				mode.illegal = [mode.illegal];
//			}
//			var begin = mode.illegal.map(reStr).join('|');
//			mode.contains.unshift(compile({
//				className: mode.className,
//				begin: begin,
//				beginCapture: mode.illegalCapture,
//				terminator: true
//			}));
//		}

//		if (mode.end) {
//			var endmode = compile({
//				className: mode.className,
//				begin: mode.end,
//				beginCapture: mode.endCapture,
//				terminator: true
//			});
//
//			mode.contains.push(endmode)
//		}

		var terminators =
		mode.contains.map(function(c) {
			return c.beginKeywords ? '\\.?(' + c.begin + ')\\.?' : c.begin;
		})
		.concat([mode.terminator_end, mode.illegal])
		.map(reStr)
		.filter(Boolean);
		mode.terminators = terminators.length ? langRe(terminators.join('|'), true) : {exec: function(/*s*/) {return null;}};
		
		return mode;
	}

	var rootCompiled = compile(language);

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
	
	function keywordMatch(mode, match) {
		var match_str = false ? match[0].toLowerCase() : match[0];
		return mode.keywords.hasOwnProperty(match_str) && mode.keywords[match_str];
	}
	
	function processKeywords(ctx, mode, mode_buffer) {
		if (!mode.keywords) {
			writeOutput(ctx, mode.className, mode_buffer);
			return;
		}
		var result = '';
		var last_index = 0;
		mode.lexemesRe.lastIndex = 0;
		var match = mode.lexemesRe.exec(mode_buffer);
		while (match) {
			result += escape();
			writeOutput(ctx, mode.className, mode_buffer.substr(last_index, match.index - last_index));
			var keyword_match = keywordMatch(mode, match);
			if (keyword_match) {
				//relevance += keyword_match[1];
				writeOutput(ctx, keyword_match[0], match[0]);
			} else {
				writeOutput(ctx, mode.className, match[0]);
			}
			last_index = mode.lexemesRe.lastIndex;
			match = mode.lexemesRe.exec(mode_buffer);
		}
		
		writeOutput(ctx, mode.className, mode_buffer.substr(last_index));
		//return result + escape(mode_buffer.substr(last_index));
		return;
	}
	
	function modeTerminators(mode, index) {
		if (!mode) return null;

		var terminators = mode.terminators;
		if (!terminators) return null;

		terminators.lastIndex = index;
		return terminators;
	}
	function parse(ctx) {
		var block = ctx.block;
		while(ctx.index < block.length) {
			var mode = ctx.modes.peek();
			debugger;
//			console.log(mode.begin);
//			console.log(mode.block);
//			console.log(mode.beginRe);
//			console.log(mode.terminators);

			var terminators = modeTerminators(mode, ctx.index);

			if(terminators && (result = terminators.exec(block)) != null) {
				var captured = result[0];
				var matched = false;
				
				processKeywords(ctx, mode, block.substring(ctx.index, terminators.lastIndex-captured.length));
				ctx.index = terminators.lastIndex;
				
				for (var i = 0; i < mode.contains.length; i++) {
					var c = mode.contains[i];
					
					if (testRe(c.beginRe, captured)) {
						matched = true;
						processKeywords(ctx, c, captured);
						
						ctx.modes.push(c);
						break;
					}
					//TODO: handle excludeBegin & returnBegin
				}
				
				if (!matched) {
					//TODO: handle endsParent & endsWithParent
					if (testRe(mode.endRe, captured)) {
						matched = true;
						processKeywords(ctx, mode, captured);
						
						ctx.modes.pop();
						
						if (mode.starts) {
							ctx.modes.push(mode.starts);
						}
					}
					//TODO: handle: returnEnd, excludeEnd
				}
				
				if (!matched) {
					if (testRe(mode.illegalRe, captured)) {
						matched = true;
						writeOutput(ctx, "illegal", captured);
					}
				}

				if (!matched) {
					throw 'Something wrong with syntax descriptor, should never reach here';
				}

			} else {
				if (ctx.index < block.length) {
					processKeywords(ctx, (mode||{}), block.substring(ctx.index));
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
