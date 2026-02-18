# Design: VSCode Extension Gets Language Extensions from Cabal File

**Issue:** #248
**Date:** 2026-02-18

## Problem

Scrod parses Haskell source using only the LANGUAGE pragmas and cabal script
headers found in the source file itself. When a project specifies
`default-language`, `default-extensions`, or `ghc-options` in its `.cabal`
file, Scrod doesn't know about them. This causes incorrect parsing when the
source relies on extensions enabled by the build system rather than per-file
pragmas.

The VSCode extension has access to the workspace and can find `.cabal` files,
but has no way to pass their settings to Scrod.

## Solution

Two layers:

1. **Scrod CLI**: Add `--ghc-option` and `--cabal` flags so extensions,
   language, and cabal file content can be passed externally.
2. **VSCode extension**: Find and read `.cabal` files from the workspace,
   pass their content to the WASM engine via `--cabal`.

## Detailed Design

### Part 1: New CLI Flags

Add to `Scrod.Executable.Flag`:

- `--ghc-option OPTION` (repeatable) — accepts any GHC option string
  (e.g. `-XOverloadedStrings`, `-XGHC2021`). Threaded through as extra
  options to `Session.parseDynamicFilePragma`.
- `--cabal CONTENT` — accepts raw `.cabal` file content as a string.
  Scrod parses it in Haskell to extract `default-extensions` and
  `default-language`, converting them to `-X` flags.

Add to `Scrod.Executable.Config`:

- `ghcOptions :: [String]` — collected from `--ghc-option` flags.
- `cabal :: Maybe String` — content from `--cabal` flag.

### Part 2: Extend Scrod.Cabal

Add `parseCabalFile :: String -> ([String], Maybe String)`:

- Parses a full `.cabal` file using `Distribution.Fields.readFields`.
- Recurses into `Section` nodes (library, common, executable, etc.) to
  find `default-extensions` and `default-language` fields.
- Collects from all stanzas (union of extensions).
- Returns `(extensionNames, maybeLanguage)`.

### Part 3: Thread Options Through Parse Pipeline

Modify `Scrod.Ghc.Parse.parse` to accept an additional
`[SrcLoc.Located String]` parameter for extra options. In
`Scrod.Executable.Main.mainWith`:

1. Convert `ghcOptions` to `[SrcLoc.Located String]`.
2. Parse `cabal` content via `Scrod.Cabal.parseCabalFile` and convert
   extensions to `-X` flags and language to `-X<Language>` flag.
3. Merge both lists and pass to `parse`.

### Part 4: VSCode Extension

In `extension.ts`:

- On activation: find `.cabal` files via
  `vscode.workspace.findFiles('**/*.cabal')`.
- Read the first `.cabal` file found and cache its content.
- Watch for cabal file changes (via `FileSystemWatcher`) to refresh cache.
- Pass cached content when invoking the WASM engine.

In `wasmEngine.ts`:

- Extend `Process` type to accept optional cabal content string.
- Pass `--cabal <content>` when cabal content is available.

### Testing

- Unit tests in `Scrod.Cabal.spec` for `parseCabalFile` with full `.cabal`
  file structures (sections, nested fields, multiple stanzas).
- Unit test in `Scrod.Ghc.Parse.spec` for `parse` with extra options.
- Integration test verifying `--ghc-option -XOverloadedStrings` affects
  parsing output.
- Integration test verifying `--cabal` with realistic cabal content works.
