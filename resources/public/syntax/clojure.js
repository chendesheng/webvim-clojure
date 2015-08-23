/*
Language: Clojure
Description: Clojure syntax (based on lisp.js)
Author: mfornos
Category: lisp
*/

function hlclojure(hljs) {
	var keywords = hljs.toSet
('def defonce cond apply if-not if-let if not not= = < > <= >= == + / * - rem '+
'quot neg? pos? delay? symbol? keyword? true? false? integer? empty? coll? list? '+
'set? ifn? fn? associative? sequential? sorted? counted? reversible? number? decimal? '+
'class? distinct? isa? float? rational? reduced? ratio? odd? even? char? seq? vector? '+
'string? map? nil? contains? zero? instance? not-every? not-any? libspec? -> ->> .. . '+
'inc compare do dotimes mapcat take remove take-while drop letfn drop-last take-last '+
'drop-while while intern condp case reduced cycle split-at split-with repeat replicate '+
'iterate range merge zipmap declare line-seq sort comparator sort-by dorun doall nthnext '+
'nthrest partition eval doseq await await-for let agent atom send send-off release-pending-sends '+
'add-watch mapv filterv remove-watch agent-error restart-agent set-error-handler error-handler '+
'set-error-mode! error-mode shutdown-agents quote var fn loop recur throw try catch monitor-enter '+
'monitor-exit defmacro defn defn- macroexpand macroexpand-1 for dosync and or '+
'when when-not when-let comp juxt partial sequence memoize constantly complement identity assert '+
'peek pop doto proxy defstruct first rest cons defprotocol cast coll deftype defrecord last butlast '+
'sigs reify second ffirst fnext nfirst nnext defmulti defmethod meta with-meta ns in-ns create-ns import '+
'refer keys select-keys vals key val rseq name namespace promise into transient persistent! conj! '+
'assoc! dissoc! pop! disj! use class type num float double short byte boolean bigint biginteger '+
'bigdec print-method print-dup throw-if printf format load compile get-in update-in pr pr-on newline '+
'flush read slurp read-line subvec with-open memfn time re-find re-groups rand-int rand mod locking '+
'assert-valid-fdecl alias resolve ref deref refset swap! reset! set-validator! compare-and-set! alter-meta! '+
'reset-meta! commute get-validator alter ref-set ref-history-count ref-min-history ref-max-history ensure sync io! '+
'new next conj set! to-array future future-call into-array aset gen-class reduce map filter find empty '+
'hash-map hash-set sorted-map sorted-map-by sorted-set sorted-set-by vec vector seq flatten reverse assoc dissoc list '+
'disj get union difference intersection extend extend-type extend-protocol int nth delay count concat chunk chunk-buffer '+
'chunk-append chunk-first chunk-rest max min dec unchecked-inc-int unchecked-inc unchecked-dec-inc unchecked-dec unchecked-negate '+
'unchecked-add-int unchecked-add unchecked-subtract-int unchecked-subtract chunk-next chunk-cons chunked-seq? prn vary-meta '+
'lazy-seq spread list* str find-keyword keyword symbol gensym force rationalize');

	var SYMBOLSTART = 'a-zA-Z_\\-!.?+*=<>&#\'';
	var SYMBOL_RE = '[' + SYMBOLSTART + '][' + SYMBOLSTART + '0-9/;:]*';
	var SIMPLE_NUMBER_RE = '[-+]?\\d+(\\.\\d+)?';

	function iskeywords(ctx, capture) {
		if (keywords[capture]) {
			return 'brace-4';
		}
	}

	var SYMBOL = {
		begin: SYMBOL_RE,
		beginCapture: iskeywords
	};

	var LITERAL = {
		className: 'number',
		begin: /\\\(|\\\[|\\\{|\\\}|\\\]|\\\)|\\\w|\b(true|false|nil)\b/
	};

	var NUMBER = {
		className: 'number', 
		begin: SIMPLE_NUMBER_RE,
	};

	var REGEXP = hljs.inherit(hljs.QUOTE_STRING_MODE, {
		begin: '#"',
		illegal:null
	});

	var STRING = hljs.inherit(hljs.QUOTE_STRING_MODE, {illegal:null});

	var COMMENT = hljs.COMMENT(';', '$',{illegal:/\n/});

	var KEY = {
		className: 'keyword',
		begin: '[:@]' + SYMBOL_RE,
	};

	var HINT = {
		className: 'comment',
		begin: '\\^' + SYMBOL_RE
	};

	var HINT_COL = hljs.COMMENT('\\^\\{', '\\}');

	function openBrace(ctx) {
		return 'brace-'+(ctx.modes.length-1)%7;
	}

	function closeBrace(ctx) {
		return 'brace-'+(ctx.modes.length-2)%7;
	}

	var VECTOR = {
		begin: '\\[',
		end: '\\]',
		beginCapture: openBrace,
		endCapture: closeBrace
	};

	var MAP = {
		begin: '\\{',
		end: '\\}',
		beginCapture: openBrace,
		endCapture: closeBrace
	};

	var NAME = {
		begin: SYMBOL_RE,
		beginCapture: function(ctx, capture) {
			return iskeywords(ctx, capture) || 'brace-3';
		}
	};


	var BODY = {
		end: /\)/,
		endCapture: closeBrace,
		relevance: 0
	};

	var LIST = {
		begin: /\(\s*/,
		contains: [NAME],
		end: /\s|,|\b|\B/,
		beginCapture: openBrace,
		starts:BODY
	};

	var DEFAULT_CONTAINS = [LIST, STRING, REGEXP, HINT, HINT_COL, COMMENT, KEY, MAP, VECTOR, NUMBER, LITERAL, SYMBOL];
	VECTOR.contains = DEFAULT_CONTAINS;
	MAP.contains = DEFAULT_CONTAINS;
	BODY.contains = DEFAULT_CONTAINS;

	return { 
		contains: [LIST, STRING, REGEXP, HINT, HINT_COL, COMMENT, KEY, MAP, VECTOR, NUMBER, LITERAL]
	};
}
