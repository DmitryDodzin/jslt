var jslt = null;

try {
  jslt = require(`jslt-node-${process.platform}-${process.arch}/jslt.node`);
} catch (_) {
  jslt = require('./bin/index.node');
}

exports.compile = jslt.compile;
exports.transform = jslt.transform;
exports.transformStr = jslt.transformStr;
exports.transformParse = jslt.transformParse;
