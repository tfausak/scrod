# Scrod

[![CI](https://github.com/tfausak/scrod/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/tfausak/scrod/actions/workflows/ci.yml)

:fish: Worse Haskell documentation.

## Summary

Scrod is a documentation generator for Haskell modules, similar to [Haddock](https://haskell-haddock.readthedocs.io).

- Produces JSON or HTML output
- Supports Literate Haskell (Bird and LaTeX style), Backpack signatures, and CPP
- Uses GHC and Haddock (the library) behind the scenes
- Only parses the module; it doesn't rename or type check, so it's fast but limited

You can try it out in the browser at [scrod.fyi](https://scrod.fyi).

## Installation

Download the latest release from [GitHub Releases](https://github.com/tfausak/scrod/releases). Each release includes:

| Asset | Description |
| --- | --- |
| `scrod-*-linux.tar.gz` | Linux binary |
| `scrod-*-darwin.tar.gz` | macOS binary |
| `scrod-*-win32.tar.gz` | Windows binary |
| `scrod-*.vsix` | VSCode extension for live documentation preview |
| `scrod-*-wasm.tar.gz` | WASM build (for use in browsers) |
| `scrod-*-wasi.tar.gz` | WASI build (for use with runtimes like Wasmtime) |
| `scrod-*-schema.json` | JSON schema for the JSON output format |
| `scrod-*.tar.gz` | Source tarball |

Or build from source with [GHC](https://www.haskell.org/ghc/) 9.14 and [Cabal](https://www.haskell.org/cabal/):

```sh
cabal install scrod
```

## Usage

Scrod reads from stdin and writes to stdout:

```sh
scrod < MyModule.hs                # JSON output (default)
scrod --format html < MyModule.hs  # HTML output
```

### Options

```
$ scrod --help
scrod version 0.2026.2.18
<https://scrod.fyi>

  -h[BOOL]  --help[=BOOL]           Shows the help.
            --version[=BOOL]        Shows the version.
            --format=FORMAT         Sets the output format (json or html).
            --ghc-option=OPTION     Sets a GHC option (e.g. -XOverloadedStrings).
            --literate[=BOOL]       Treats the input as Literate Haskell.
            --schema[=BOOL]         Shows the JSON output schema.
            --signature[=BOOL]      Treats the input as a Backpack signature.
```

- **`-h`, `--help`**: Prints the help then exits.
- **`--version`**: Prints the version then exits.
- **`--format`**: Sets the output format. Either `json` (the default) or `html`.
- **`--ghc-option`**: Passes an option to the GHC parser (e.g. `-XOverloadedStrings`). Can be given multiple times.
- **`--literate`**: Treats the input as literate Haskell, either Bird or LaTeX style.
- **`--schema`**: Prints the JSON output schema then exits.
- **`--signature`**: Treats the input as a Backpack signature.

Boolean flags accept an optional `=BOOL` argument (`True` or `False`). Without the argument, the flag is set to `True`. This lets you override earlier flags, e.g. `--literate --literate=False`.
