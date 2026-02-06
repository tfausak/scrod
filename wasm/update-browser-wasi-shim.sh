#!/usr/bin/env bash
set -euo pipefail

ref="${1:-2f86b49dce50916e2984029c535321e34b234229}"
url="https://esm.sh/gh/haskell-wasm/browser_wasi_shim@$ref/es2022/browser_wasi_shim.mjs"
dest="$(dirname "$0")/www/vendor/browser_wasi_shim"
mkdir -p "$dest"

rm -f "$dest"/*.js
curl -fsSL "$url" -o "$dest/index.js"

echo "Updated browser_wasi_shim to $ref"
