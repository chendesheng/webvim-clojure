/*
Language: JavaScript
Category: common, scripting
*/

function hljavascript(hljs) {
    var keywords = hljs.toSet(
        'in of if for while finally var new function do return void else break catch ' +
        'instanceof with throw case default try this switch continue typeof delete ' +
        'let yield const export super debugger as async await'+
        'true false null undefined NaN Infinity'+
        'eval isFinite isNaN parseFloat parseInt decodeURI decodeURIComponent ' +
        'encodeURI encodeURIComponent escape unescape Object Function Boolean Error ' +
        'EvalError InternalError RangeError ReferenceError StopIteration SyntaxError ' +
        'TypeError URIError Number Math Date String RegExp Array Float32Array ' +
        'Float64Array Int16Array Int32Array Int8Array Uint16Array Uint32Array ' +
        'Uint8Array Uint8ClampedArray ArrayBuffer DataView JSON Intl arguments require ' +
        'module console window document Symbol Set Map WeakSet WeakMap Proxy Reflect ' +
        'Promise'+
	' + - * / = == <= >= < > || && & | ? :'
    );

  return {
    aliases: ['js'],
    contains: [
      //{
      //  className: 'pi',
      //  relevance: 10,
      //  begin: /^\s*['"]use (strict|asm)['"]/
      //},
      hljs.APOS_STRING_MODE,
      hljs.QUOTE_STRING_MODE,
      { // template string
        className: 'string',
        begin: '`', end: '`',
        contains: [
          hljs.BACKSLASH_ESCAPE,
          {
            begin: '\\$\\{', end: '\\}'
          }
        ]
      },
      hljs.C_LINE_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
	{	
		className: 'number',
		begin: hljs.C_NUMBER_RE
	},
	{
		className: 'regexp',
		begin: /\/[^\/]*\/(?![\d\s])[ig]*/,
	},
      //{
      //  begin: '\\b(0[bB][01]+)', //TODO: support variants
      //  className: 'number',
      //  variants: [
      //    { begin: '\\b(0[bB][01]+)' },
      //    { begin: '\\b(0[oO][0-7]+)' },
      //    { begin: hljs.C_NUMBER_RE }
      //  ],
      //  relevance: 0
      //},
      //{ // "value" container
      //  begin: '(' + hljs.RE_STARTERS_RE + '|\\b(case|return|throw)\\b)\\s*',
      //  keywords: 'return throw case',
      //  contains: [
      //    hljs.C_LINE_COMMENT_MODE,
      //    hljs.C_BLOCK_COMMENT_MODE,
      //    hljs.REGEXP_MODE,
      //    { // E4X / JSX
      //      begin: /</, end: />\s*[);\]]/,
      //      relevance: 0,
      //      subLanguage: 'xml'
      //    }
      //  ],
      //  relevance: 0
      //},
	      {
		      className: 'brace-3',
		      begin: /[\+\-\*\/\|\?\:\^&%!~<>=]|\|\||&&|>=|<=|==|===/
	      },
      {
        begin: /\bfunction\b/, end: /\{/,
	beginCapture: function() {return 'keyword'},
        contains: [
          hljs.inherit(hljs.TITLE_MODE, {begin: /[A-Za-z$_][0-9A-Za-z$_]*/}),
          {
            begin: /\(/, end: /\)/,
            excludeBegin: true,
            excludeEnd: true,
            contains: [
              hljs.C_LINE_COMMENT_MODE,
              hljs.C_BLOCK_COMMENT_MODE
            ]
          }
        ],
        illegal: /\[|%/
      },
	{
		begin: /[A-Za-z$_][0-9A-Za-z$_]*/,
		beginCapture: function(ctx, captured) {
			if (keywords[captured]) {
				return 'keyword';
			} else {
				var i = ctx.mode.recontains.lastIndex;
				var line = ctx.line.substr(i);
				if (ctx.line[i-captured.length-1]=='.'){
					return 'title';
				} else if (/^\s*\(/.test(line)) {
					return 'title';
				}
				return null;
			}
		}
	},
      //{
      //  begin: /\$[(.]/ // relevance booster for a pattern common to JS libs: `$(something)` and `$.something`
      //},
      //{
      //  begin: '\\.' + hljs.IDENT_RE, relevance: 0 // hack: prevents detection of keywords after dots
      //}
      //,
      // ECMAScript 6 modules import
      //{
      //  beginKeywords: 'import', end: '[;$]',
      //  keywords: 'import from as',
      //  contains: [
      //    hljs.APOS_STRING_MODE,
      //    hljs.QUOTE_STRING_MODE
      //  ]
      //},
      //{ // ES6 class
      //  className: 'class',
      //  beginKeywords: 'class', end: /[{;=]/, excludeEnd: true,
      //  illegal: /[:"\[\]]/,
      //  contains: [
      //    {beginKeywords: 'extends'},
      //    hljs.UNDERSCORE_TITLE_MODE
      //  ]
      //}
    ],
    illegal: /#/
  };
}
