#! /bin/env node

try {
	require(`jslt-node-${process.platform}-${process.arch}/jslt.node`);
} catch (e) {
	const fs = require("fs");

	if (!fs.existsSync(`./bin/index.node`)) {
		process.exit(1);
	}
}
