function testRe(re, lexeme) {
    var match = re && re.exec(lexeme);
    return match && match.index == 0 && match[0] == lexeme;
}

function inherit(parent, obj) {
    var result = {},
        key;
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
        begin: '\\\\[\\s\\S]',
        relevance: 0
    };
    hljs.APOS_STRING_MODE = {
        className: 'string',
        begin: '\'',
        end: '\'',
        illegal: '\\n',
        contains: [hljs.BACKSLASH_ESCAPE]
    };
    hljs.QUOTE_STRING_MODE = {
        className: 'string',
        begin: '"',
        end: '"',
        illegal: '\\n',
        contains: [hljs.BACKSLASH_ESCAPE]
    };
    hljs.PHRASAL_WORDS_MODE = {
        begin: /\b(a|an|the|are|I|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|like)\b/
    };
    hljs.COMMENT = function(begin, end, inherits) {
        var mode = hljs.inherit({
                className: 'comment',
                begin: begin,
                end: end,
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
            '%|em|ex|ch|rem' +
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
        begin: /\//,
        end: /\/[gimuy]*/,
        illegal: /\n/,
        contains: [
            hljs.BACKSLASH_ESCAPE, {
                begin: /\[/,
                end: /\]/,
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

    function rainbowColor(text, ctx) {
        var pred = function(mode) {
            return mode.className === rainbowColor;
        };

        if ('([{'.indexOf(text) != -1) {
            return 'bracket-' + (ctx.modes.count(pred) - 1) % 7;
        } else if (')]}'.indexOf(text) != -1) {
            return 'bracket-' + ctx.modes.count(pred) % 7;
        } else {
            return '';
        }
    }

    hljs.rainbowColor = rainbowColor;
    return {
        'Clojure': hlclojure(hljs),
        'JavaScript': hljavascript(hljs),
        'CSS': hlcss(hljs),
        'XML': hlxml(hljs),
        'SQL': hlsql(hljs),
        'Go': hlgo(hljs),
        'C#': hlcs(hljs),
    }
})();

function newHighlight(lang) {
    return hlcompile(highlights[lang]);
}

function hlcompile(language) {
    if (!language) {
        var hl = {
            parse: function(block, state) {
                return [
                    [],
                    [
                        [null, block]
                    ]
                ];
            },
            parseBlock: function(block, row) {
                if (hl.states[row] == null)
                    hl.states[row] = [];
                return [
                    [null, block]
                ];
            },
            refresh: function(iter) {
                iter.render([
                    [null, iter.text()]
                ]);
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
        a = (a instanceof Array) ? a : [a];
        b = (b instanceof Array) ? b : [b];
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
                Object.keys(mode.keywords).forEach(function(className) {
                    flatten(className, mode.keywords[className]);
                });
            }
            mode.keywords = compiled_keywords;
        }

        mode.lexemesRe = langRe(mode.lexemes || /\b\w+\b/, true);

        if (parent) {
            if (!mode.className) {
                mode.className = parent.className;
            }

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
                c.variants.forEach(function(v) {
                    expanded_contains.push(inherit(c, v));
                });
            } else {
                expanded_contains.push(c == 'self' ? mode : c);
            }
        });
        mode.contains = expanded_contains;

        if (mode.contains.length > 0) {
            mode.contains = mode.contains.map(function(c) {
                return compile(c, mode);
            });
        } else if (mode.subLanguage) {
            var subLanguage = highlights[mode.subLanguage];
            if (subLanguage) {
                delete mode.contains;
                delete mode.keywords;
                mode = inherit(compile(subLanguage), mode)
            }
            var terminators = [mode.terminator_end].concat(
                    mode.contains.map(function(c) {
                        return c.beginKeywords ? '\\.?(' + c.begin + ')\\.?' : c.begin;
                    })
                )
                .map(reStr)
                .filter(Boolean);
            mode.terminators = terminators.length ? langRe(terminators.join('|'), true) : {
                exec: function( /*s*/ ) {
                    return null;
                }
            };
            return mode;
        }

        if (mode.starts) {
            mode.starts = compile(mode.starts, parent);
        }

        var terminators =
            mode.contains.map(function(c) {
                return c.beginKeywords ? '\\.?(' + c.begin + ')\\.?' : c.begin;
            })
            .concat([mode.terminator_end, mode.illegal])
            .map(reStr)
            .filter(Boolean);
        mode.terminators = terminators.length ? langRe(terminators.join('|'), true) : {
            exec: function( /*s*/ ) {
                return null;
            }
        };

        return mode;
    }

    var rootCompiled = compile(language);

    function handleDescendantClassName(ctx, className) {
        var modeDescendantClassName = ctx.modes.some(function(m) {
            return !!m.descendantClassName
        });

        if (modeDescendantClassName) {
            return modeDescendantClassName.descendantClassName;
        }

        return className;
    }

    function writeOutput(ctx, className, text) {
        if (!text) {
            return;
        }

        className = handleDescendantClassName(ctx, className);

        if (typeof className == 'function') {
            className = className(text, ctx);
        }

        var prev = ctx.output.peek();
        if (prev && prev[0] == className) {
            //merge to previous item if same className (this can reduce about 100 nodes on testfile.clj file)
            prev[1] += text;
        } else {
            ctx.output.push([className, text]);
        }
    }

    function keywordMatch(mode, match) {
        var match_str = language.case_insensitive ? match[0].toLowerCase() : match[0];
        return mode.keywords.hasOwnProperty(match_str) && mode.keywords[match_str];
    }

    function processBuffer(ctx, mode, className, mode_buffer) {
        if (mode.subLanguage)
            processSubLanguage(ctx, mode, className, mode_buffer)
        else
            processKeywords(ctx, mode, className, mode_buffer)
    }

    function processSubLanguage(ctx, mode, className, mode_buffer) {
        var subLanguage = mode.subLanguage;
        var hl = newHighlight(subLanguage);
        hl.parse(ctx)
    }

    function processKeywords(ctx, mode, className, mode_buffer) {
        if (!mode.keywords) {
            writeOutput(ctx, className, mode_buffer);
            return;
        }
        var result = '';
        var last_index = 0;
        mode.lexemesRe.lastIndex = 0;
        var match = mode.lexemesRe.exec(mode_buffer);
        while (match) {
            result += escape();
            writeOutput(ctx, className, mode_buffer.substr(last_index, match.index - last_index));
            var keyword_match = keywordMatch(mode, match);
            if (keyword_match) {
                //relevance += keyword_match[1];
                writeOutput(ctx, keyword_match[0], match[0]);
            } else {
                writeOutput(ctx, className, match[0]);
            }
            last_index = mode.lexemesRe.lastIndex;
            match = mode.lexemesRe.exec(mode_buffer);
        }

        writeOutput(ctx, className, mode_buffer.substr(last_index));
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
        //debugger;
        while (ctx.index < block.length) {
            var mode = ctx.modes.peek();
            var terminators = modeTerminators(mode, ctx.index);

            if (terminators && (result = terminators.exec(block)) != null) {
                var captured = result[0];
                var startIndex = result.index;
                var matched = false;

                processKeywords(ctx, mode, mode.className, block.substring(ctx.index, startIndex));
                ctx.index = terminators.lastIndex;

                for (var i = 0; i < mode.contains.length; i++) {
                    var c = mode.contains[i];

                    if (testRe(c.beginRe, captured)) {
                        //if (testRe(new RegExp("^"+reStr(c.beginRe)+"$"), captured)) {
                        matched = true;

                        ctx.modes.push(c);
                        if (c.returnBegin) {
                            ctx.index = startIndex;
                        } else {
                            processKeywords(ctx, c,
                                (c.excludeBegin ? mode.className : c.className), captured);
                        }
                        break;
                    }
                }

                if (!matched) {
                    var endmode;
                    while (true) {
                        endmode = ctx.modes.peek();
                        if (testRe(endmode.endRe, captured)) {
                            while (endmode.endsParent && ctx.modes.length > 0) {
                                ctx.modes.pop();
                                endmode = ctx.modes.peek();
                            }
                            ctx.modes.pop();
                            var parent = ctx.modes.peek();

                            if (endmode.returnEnd) {
                                ctx.index = startIndex;
                            } else {
                                processKeywords(ctx, endmode, (endmode.excludeEnd ? (parent || {}).className : endmode.className), captured);
                            }
                            matched = true;
                            break;
                        }

                        if (!endmode.endsWithParent) {
                            break;
                        } else {
                            ctx.modes.pop();
                        }
                    }

                    if (endmode) {
                        if (endmode.starts) {
                            ctx.modes.push(endmode.starts);
                        }
                    }
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
                    mode = (mode || {});
                    processKeywords(ctx, mode, mode.className, block.substring(ctx.index));
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
        if (states[row] == null) {
            states[row] = [rootCompiled];
        }
        var ctx = doParse(block, states[row]);
        states[row + 1] = ctx.modes;
        //logmodes(row+1);

        //nodeCount += ctx.output.length;
        //console.log('output count:' + nodeCount);
        return ctx.output;
    };

    hl.parse = function(block, state) {
        if (state == null) {
            state = [rootCompiled];
        }
        var ctx = doParse(block, state);
        return [ctx.modes, ctx.output];
    };

    hl.refresh = function(iter) {
        var states = hl.states;
        while (true) {
            var i = iter.index();
            var ctx = doParse(iter.text(), states[i]);
            iter.render(ctx.output);
            if (states[i + 1].equal(ctx.modes)) {
                //doParse has 3 arguments, in next lines none of them changed
                //so no syntax will be changed in next lines
                return;
            } else {
                states[i + 1] = ctx.modes;
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
