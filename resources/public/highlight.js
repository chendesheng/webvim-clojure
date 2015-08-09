//TODO syntax highlight and rainbow color braces
var keywords = /(;.*|#"[^"]*"|:[\w-]+|@[\w-]+|"[^"]*"|%\d+|-[\d.]+|\b[\d.]+|->|->>|\+|>=|<=|>|<|\*|-|\b\/|==|=|%|\bprint|\bprintln|\bdefonce|\batom|\bref|\bagent|\bdec|\binc|\bfn|\bdefn|\bif|\blet|\bcond|\bcount|\bdef|\bdo|\bdoseq|\bloop|\brecur|\bstr|\btry|\bcatch|\bmerge|\bupdate|\bassoc|\bupdate-in|\bdissoc|\bnil\?|\bnil|\btrue|\bswap!|\bfalse|\bsubs|\bsubvec|\bvec|\bvec\?|\bmap\?|\bfn\?|\btrue\?|\bmap)(?![\w-])/g;
function parseLine(line) {
	var lasti = 0;
	var html = '';
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
		} else {
			cls = 'keyword';
		}
		html += '<span class="'+cls+'">'+result+'</span>';

		lasti = keywords.lastIndex;
	}

	html += htmlEncode(line.substring(lasti));

	return html;
}
