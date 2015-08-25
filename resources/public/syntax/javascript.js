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
	'Promise import class'+
	' + - * / = == <= >= < > || && & | ? : $');

	function openBrace(ctx) {
		return 'brace-'+(ctx.modes.length-1)%7;
	}

	function closeBrace(ctx) {
		return 'brace-'+(ctx.modes.length-2)%7;
	}

	function openBrace2(ctx) {
		return 'brace-'+(ctx.modes.length-2)%7;
	}

	var BRACE = {
		begin: '\\{',
		end: '\\}',
		beginCapture: openBrace,
		endCapture: closeBrace
	};

	var BRACE_CLOSE = {
		end: /\}/,
		endCapture: closeBrace
	};

	var CONTAINS = [
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
		hljs.inherit(hljs.QUOTE_STRING_MODE,
		{
			className: 'regexp',
			begin: /\//,
			end:/\//
		}),
		BRACE,
		{
			begin: /\bfunction\b/,
			beginCapture: function() {return 'keyword'},
			end: /\{/,
			endCapture: openBrace2,
			contains: [
				hljs.inherit(hljs.TITLE_MODE, {begin: /[A-Za-z$_][0-9A-Za-z$_]*/}),
				{
					begin: /\(/,
					end: /\)/,
					contains: [
						hljs.C_LINE_COMMENT_MODE,
						hljs.C_BLOCK_COMMENT_MODE
					]
				}
			],
			starts: BRACE_CLOSE,
			illegal: /\[|%/
		},
		{
			begin: /[A-Za-z$_][0-9A-Za-z$_]*/,
			beginCapture: function(ctx, captured) {
				if (keywords[captured]) {
					return 'keyword';
				} else {
					var i = ctx.index;
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
	];

	BRACE_CLOSE.contains = CONTAINS;
	BRACE.contains = CONTAINS;

return {
	aliases: ['js'],
	contains: CONTAINS,
	illegal: /#/
};
}
