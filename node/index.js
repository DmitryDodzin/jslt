var jslt = null;

const detectLibc = require('detect-libc');

const runtimeLibc = () => 
  detectLibc.isNonGlibcLinuxSync()
    ? detectLibc.familySync() ?? '' : '';

try {
  jslt = require(`jslt-node-${process.platform}${runtimeLibc()}-${process.arch}/jslt.node`);
} catch (_) {
  jslt = require('./bin/index.node');
}


const nativeschema = Symbol('NativeSchema');

function Jslt(schema) {
  this[nativeschema] = jslt.compile(schema);
}

Jslt.prototype.transform = function(input) {
  return jslt.transform(this[nativeschema], input);
};

Jslt.prototype.transformStr = function(input) {
  return jslt.transformStr(this[nativeschema], input);
};

Jslt.prototype.transformParse = function(input) {
  return jslt.transformParse(this[nativeschema], input);
};

Jslt.prototype.transformStringify = function(input) {
  return jslt.transformStringify(this[nativeschema], input);
};

exports.Jslt = Jslt;

exports.compile = jslt.compile;
exports.transform = jslt.transform;
exports.transformStr = jslt.transformStr;
exports.transformParse = jslt.transformParse;
exports.transformStringify = jslt.transformStringify;
