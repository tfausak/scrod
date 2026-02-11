#!/usr/bin/env sh
set -o errexit -o nounset -o xtrace

dist=extra/github-pages/dist

rm -rf "$dist"
mkdir -p "$dist"

cp extra/github-pages/index.html "$dist/"
cp extra/github-pages/index.js "$dist/"
cp extra/github-pages/worker.js "$dist/"
cp extra/github-pages/style.css "$dist/"
cp -r extra/github-pages/vendor "$dist/"
cp extra/bootstrap/bootstrap.min.css "$dist/vendor/"
cp extra/wasm/dist/scrod-wasm.wasm "$dist/"
cp extra/wasm/dist/ghc_wasm_jsffi.js "$dist/"
