#! /bin/env node
const fs = require("fs");

if (!fs.existsSync(`./bin/${process.platform}-${process.arch}.node`) && !fs.existsSync(`./bin/index.node`)) {
	process.exit(1);
}
