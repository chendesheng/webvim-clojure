/*
Language: HTML, XML
Category: common
*/
function hlxml(hljs) {
var XML_IDENT_RE = '[A-Za-z0-9\\._:-]+';
var TAG_INTERNALS = [
	{
		className: 'keyword',
		begin: /[A-Za-z0-9\\._:-]+/
	},
	hljs.APOS_STRING_MODE,
	hljs.QUOTE_STRING_MODE
    //endsWithParent: true,
    //illegal: /</,
    //relevance: 0,
//    contains: [
//      {
//        className: 'keyword',
//        begin: XML_IDENT_RE,
//        relevance: 0
//      }
    //,
//      {
//        begin: '=',
//        contains: [
//		hljs.QUOTE_STRING_MODE
        //  {
        //    className: 'string',
	//    begin: /"/, 
	//    end: /"/,
            //variants: [
            //  {begin: /"/, end: /"/},
            //  {begin: /'/, end: /'/},
            //  {begin: /[^\s\/>]+/}
            //]
          //}
      //  ]
      //}
    ];
  //};
return {
    aliases: ['html', 'xhtml', 'rss', 'atom', 'xsl', 'plist'],
    case_insensitive: true,
    contains: [
      {
        className: 'comment',
        begin: '<!doctype', end: '>',
        relevance: 10,
        contains: [{begin: '\\[', end: '\\]'}]
      },
      hljs.COMMENT( '<!--', '-->', { relevance: 10 }),
      {
        className: 'comment',
        begin: '<\\!\\[CDATA\\[', end: '\\]\\]>',
        relevance: 10
      },
      {
        //className: 'tag',
/*
        The lookahead pattern (?=...) ensures that 'begin' only matches
        '<style' as a single word, followed by a whitespace or an
        ending braket. The '$' is needed for the lexeme to be recognized
        by hljs.subMode() that tests lexemes outside the stream.
        */
        begin: '<style[^>]*>', //end: '>',
	beginCapture: function() {
		return 'title';
	},
        //keywords: {title: 'style'},
        //contains: TAG_INTERNALS,
        //starts: {
	end: /<\/style>/, 
	endCapture: function() {
		return 'title';
	},
	subLanguage: 'CSS'
        //}
      },
      {
        className: 'title',
        begin: /<script(?=\s|>|$)/, 
	end: />/,
	contains:TAG_INTERNALS,

        //className: 'title',
// See the comment in the <style tag about the lookahead pattern
        //begin: '<script[^>]*>', //end: '>',
	//beginCapture: function() {
	//	return 'title';
	//},
        //keywords: {title: 'script'},
        //contains: TAG_INTERNALS,
	//end: /<\/script>/,
	//endCapture: function() {
	//	return 'title';
	//},
	starts: {
		subLanguage: 'JavaScript',
		illegal: /<\/script>/,
		illegalCapture: function() {return 'title'}
	}
      },
//PHP,
      //{
      //  className: 'title',
      //  begin: /<\?\w+/,
      //  end: /\?>/,
      //}
    //,
      {
        className: 'title',
        begin: /<\/?[^ \/><\n\t]+/, 
	end: /\/?>/,
	contains:TAG_INTERNALS 
      }
    ]
  };
}
