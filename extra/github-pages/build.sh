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

# Make assets content addressable by embedding a hash in each filename.
# Files are processed bottom-up so that a parent's hash reflects the
# hashed names it references.

# hash_asset FILE REF_FILES...
# Renames dist/FILE to include a content hash and updates all references
# in the given REF_FILES (also relative to dist).
hash_asset() {
  file=$1
  shift
  hash=$(sha256sum "$dist/$file" | head -c 16)
  base=${file%.*}
  ext=${file##*.}
  new="${base}.${hash}.${ext}"
  mv "$dist/$file" "$dist/$new"
  for ref in "$@"; do
    sed -i "s|$file|$new|g" "$dist/$ref"
  done
}

# Phase 1: leaf assets (no outgoing references to other hashed assets)
hash_asset vendor/browser_wasi_shim.js worker.js
hash_asset ghc_wasm_jsffi.js           worker.js
hash_asset scrod-wasm.wasm              worker.js
hash_asset vendor/bootstrap.min.css     index.html index.js
hash_asset style.css                    index.html

# Phase 2: files that reference leaf assets (content now includes hashed names)
hash_asset worker.js                    index.js
hash_asset index.js                     index.html
