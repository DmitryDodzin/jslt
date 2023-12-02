install:
	pnpm i

bin: bin/darwin-arm64.node bin/darwin-x64.node bin/linux-x64.node

bin/darwin-arm64.node:
	ARTIFACT_PATH=./bin/darwin-arm64.node pnpm zigbuild --release --target aarch64-apple-darwin

bin/darwin-x64.node:
	ARTIFACT_PATH=./bin/darwin-x64.node pnpm zigbuild --release --target x86_64-apple-darwin

bin/linux-x64.node:
	ARTIFACT_PATH=./bin/linux-x64.node pnpm zigbuild --release --target x86_64-unknown-linux-gnu
