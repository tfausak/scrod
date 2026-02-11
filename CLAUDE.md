# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Scrod?

Scrod is a Haskell documentation tool that parses Haskell source code using the GHC API and renders documentation as HTML or JSON. It has a CLI tool (reads from stdin) and a WASM-based web app deployed to GitHub Pages with a live split-pane editor.

**Pipeline:** Haskell Source → GHC Parser → GHC AST → Scrod Core Types → HTML/JSON

## Building, Testing, and Conventions

See [CONTRIBUTING.md](CONTRIBUTING.md) for build commands, test filtering, linting, code style, and git conventions.

## Architecture

### Source layout

- `source/library/` — all library code under `Scrod.*` modules
- `source/executables/` — CLI (`scrod`), WASM (`scrod-wasm`), and WASI (`scrod-wasi`) entry points
- `source/test-suite/` — test suite entry point (uses Tasty/HUnit via custom `Scrod.Spec` abstraction)
- `extra/github-pages/` — web app frontend source (HTML, JS, CSS) deployed to GitHub Pages
- `extra/wasm/` — WASM cross-compilation build script and cabal project file
- `extra/vscode/` — VSCode extension (TypeScript + esbuild) for live documentation preview

### Key module groups

- **`Scrod.Core.*`** — Data types for the intermediate representation (Module, Item, Doc, Export, etc.). Each type lives in its own module with a `Mk`-prefixed constructor (e.g., `MkModule`, `MkItem`).
- **`Scrod.Convert.FromGhc`** — Converts GHC AST into Scrod Core types. Largest and most complex module. Uses a `ConvertM` state monad that assigns auto-incrementing `ItemKey`s.
- **`Scrod.Convert.FromHaddock`** — Converts Haddock doc strings into Scrod's `Doc` type.
- **`Scrod.Convert.ToHtml`** / **`ToJson`** — Render Core types to HTML/JSON output.
- **`Scrod.Ghc.*`** — Wrappers around GHC API internals to set up parsing without a full GHC session.
- **`Scrod.Xml.*`**, **`Scrod.Json.*`**, **`Scrod.Css.*`** — Custom implementations for XML/HTML, JSON, and CSS generation (no external libraries).

### Testing

Tests use `Scrod.Spec`, a custom test specification DSL (`MkSpec` record with `assertFailure`/`describe`/`it`) that abstracts over the test framework. The test-suite entry point (`source/test-suite/Main.hs`) wires the DSL into Tasty/HUnit.

There are two kinds of tests:

- **Unit tests** — defined inline in library modules via a `spec` function (e.g., `Scrod.Decimal.spec`). These test individual functions directly.
- **Integration tests** — in `Scrod.TestSuite.Integration`. These run the full pipeline (parse → convert → JSON) and assert on JSON output using JSON Pointer paths. The `check` helper takes a Haskell source string and a list of `(pointer, expected JSON)` pairs.

`Scrod.TestSuite.All` aggregates all module specs (both unit and integration) in one place. When adding a new module with a `spec` function, it must be registered here.

### WASM web app

The WASM build exports a `scrod` function via JavaScript FFI. The web frontend source lives in `extra/github-pages/` and runs it in a Web Worker (`worker.js`) to avoid blocking the UI. Shareable URLs use base64-encoded hash fragments.
