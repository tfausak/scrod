#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

name="$1"
version="$2"
sha="$3"
artifact_prefix="${name}-${sha}"

tar --extract --file "${artifact_prefix}-Linux/artifact.tar" --strip-components=1 --wildcards '*.tar.gz'

for dir in ${artifact_prefix}-*/
do
  platform="${dir%/}"
  platform="${platform##*-}"
  case "$platform" in
    macOS) platform=darwin; files="${name}" ;;
    Linux) platform=linux; files="${name}" ;;
    Windows) platform=win32; files="${name}.exe" ;;
    wasm) files="${name}-wasm.wasm ${name}-wasi.wasm" ;;
    *) echo "unknown platform: $platform" >&2; exit 1 ;;
  esac
  tar --extract --file "$dir/artifact.tar"
  tar --create --gzip --file "${name}-${version}-${platform}.tar.gz" -C artifact $files
  rm -rf artifact
done
