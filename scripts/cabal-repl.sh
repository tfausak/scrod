#!/usr/bin/env sh

# Example usage:
#
#     printf 'import qualified Data.Text as Text\nText.pack "foo"' | scripts/cabal-repl.sh

exec cabal repl --repl-options=-Wwarn "$@"
