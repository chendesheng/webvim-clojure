/*
Language: CSS
Category: common, css
*/

function hlcss(hljs) {
	var IDENT_RE = '[a-zA-Z-][a-zA-Z0-9_-]*';
	var RULE = {
		begin: /[A-Za-z\_\.\-]+\s*:/,
		beginCapture: function(ctx) { return 'keyword'; },
		end: '\n',
		contains: [
			hljs.CSS_NUMBER_MODE,
			hljs.QUOTE_STRING_MODE,
			hljs.APOS_STRING_MODE,
			hljs.C_BLOCK_COMMENT_MODE,
			{
				className: 'number', begin: '#[0-9A-Fa-f]+'
			}
		]
	};

	var TITLE = {
		begin: /[^\{\n,]+/,
		beginCapture: function() {return 'title'},
	};


	var BLOCK = {
		begin: /\{/, 
		end: /\}/,
		contains: [
			hljs.C_BLOCK_COMMENT_MODE,
			RULE,
		]
	};

	BLOCK.contains.push(BLOCK);

	return {
		case_insensitive: true,
		contains: [
			hljs.C_BLOCK_COMMENT_MODE,
			TITLE,
			BLOCK
		]
	};
}
