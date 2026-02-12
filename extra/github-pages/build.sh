#!/usr/bin/env sh
set -o errexit -o nounset -o xtrace
cd "$( dirname "$0" )"
npm ci
node build.mjs
