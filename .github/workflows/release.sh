#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail -o xtrace

name="$1"
version="$2"
sha="$3"
artifact_prefix="${name}-${sha}"

tar --extract --file "${artifact_prefix}-Linux/artifact.tar" --strip-components=1 --wildcards '*.tar.gz'

shopt -s nullglob
dirs=("${artifact_prefix}"-*/)
if [ ${#dirs[@]} -eq 0 ]; then
  echo "no artifact directories found for prefix: $artifact_prefix" >&2
  exit 1
fi

for dir in "${dirs[@]}"
do
  platform="${dir%/}"
  platform="${platform##*-}"
  case "$platform" in
    macOS) platform=darwin; file="${name}" ;;
    Linux) platform=linux; file="${name}" ;;
    Windows) platform=win32; file="${name}.exe" ;;
    wasm)
      tar --extract --file "$dir/artifact.tar"
      tar --create --gzip --file "${name}-${version}-wasm.tar.gz" -C artifact "${name}-wasm.wasm" ghc_wasm_jsffi.js browser_wasi_shim.js
      tar --create --gzip --file "${name}-${version}-wasi.tar.gz" -C artifact "${name}-wasi.wasm"
      rm -rf artifact
      continue
      ;;
    vscode)
      cp "$dir"/*.vsix .
      continue
      ;;
    *) echo "unknown platform: $platform" >&2; exit 1 ;;
  esac
  tar --extract --file "$dir/artifact.tar"
  tar --create --gzip --file "${name}-${version}-${platform}.tar.gz" -C artifact "$file"
  rm -rf artifact
done
