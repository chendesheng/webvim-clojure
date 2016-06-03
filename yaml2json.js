yaml = require('js-yaml');
fs = require('fs');

// Get document, or throw exception on error
try {
    var doc = yaml.safeLoad(fs.readFileSync('go.yaml', 'utf8'));
    console.log(JSON.stringify(doc));
} catch (e) {
    console.log(e);
}
