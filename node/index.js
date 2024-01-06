var jslt = null;

const detectLibc = require('detect-libc');

const runtimeLibc = () => 
  detectLibc.isNonGlibcLinuxSync()
    ? detectLibc.familySync() : '';

try {
  jslt = require(`jslt-node-${process.platform}${runtimeLibc()}-${process.arch}/jslt.node`);
} catch (_) {
  jslt = require('./bin/index.node');
}

exports.compile = jslt.compile;
exports.transform = jslt.transform;
exports.transformStr = jslt.transformStr;
exports.transformParse = jslt.transformParse;
