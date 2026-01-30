#! /usr/bin/env sh
cabal test --test-options=--hide-successes "$@"
