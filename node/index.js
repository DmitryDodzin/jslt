var jslt = null;

try {
  jslt = require(`./bin/${process.platform}-${process.arch}.node`);
} catch (_) {
  jslt = require('./bin/index.node');
}

exports.compile = jslt.compile;
exports.transform = jslt.transform;
exports.transformStr = jslt.transformStr;
exports.transformParse = jslt.transformParse;
