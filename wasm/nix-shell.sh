#!/usr/bin/env bash
set -euo pipefail

exec nix shell \
  'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org' \
  'nixpkgs#wabt' \
  -c "$@"
