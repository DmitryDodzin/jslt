#! /bin/env node

const detectLibc = require('detect-libc');

const runtimeLibc = () => 
  detectLibc.isNonGlibcLinuxSync()
    ? detectLibc.familySync() : '';

try {
  require(`jslt-node-${process.platform}${runtimeLibc()}-${process.arch}/jslt.node`);
} catch (e) {
  const fs = require("fs");

  if (!fs.existsSync(`./bin/index.node`)) {
    process.exit(1);
  }
}
