#!/usr/bin/env sh
set -o errexit -o nounset -o xtrace
cd "$( dirname "$0" )"
npm ci
npx tsc --noEmit
mkdir -p wasm
cp ../wasm/dist/ghc_wasm_jsffi.js wasm/ghc_wasm_jsffi.mjs
cp ../wasm/dist/scrod-wasm.wasm wasm/
cp ../github-pages/vendor/browser_wasi_shim.js wasm/browser_wasi_shim.mjs
scrod_version=$(awk '/^version:/ {print $2}' ../../scrod.cabal)
npm version --no-git-tag-version "1.0.0-${scrod_version}"
exec npx vsce package
