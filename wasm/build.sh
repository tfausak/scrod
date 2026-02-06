#!/usr/bin/env sh
set -o errexit -o xtrace

cd "$(dirname "$0")/.."

echo "Building WASM executable..."
wasm32-wasi-cabal build \
  --project-file=wasm/cabal.project \
  exe:scrod-wasm

echo "Locating WASM binary..."
wasm=$(find dist-newstyle -name "scrod-wasm.wasm" -type f | head -n 1)
if [ -z "$wasm" ]; then
  echo "Error: could not find scrod-wasm.wasm" >&2
  exit 1
fi
echo "Found: $wasm"

echo "Assembling dist directory..."
mkdir -p wasm/dist

echo "Running GHC JS post-linker..."
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  -i "$wasm" \
  -o wasm/dist/ghc_wasm_jsffi.js

cp "$wasm" wasm/dist/scrod-wasm.wasm

if command -v wasm-strip > /dev/null 2>&1; then
  echo "Running wasm-strip..."
  wasm-strip wasm/dist/scrod-wasm.wasm
fi

cp -r wasm/www/* wasm/dist/

echo "Build complete."
echo "Serve with: python3 -m http.server -d wasm/dist"
