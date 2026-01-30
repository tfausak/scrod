#! /usr/bin/env sh
exec find source -name '*.hs' -exec ormolu --mode=inplace "$@" {} +
