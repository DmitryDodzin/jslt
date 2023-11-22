#! /bin/sh

if [ ! -e ./bin/index.node ]; then
	echo 'require("fs").existsSync(`./bin/${process.platform}-${process.arch}.node`) || process.exit(1)' | node -
fi
