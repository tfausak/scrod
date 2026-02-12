# Design: JSON-only Haskell + Shared JS Renderer

## Problem

HTML generation is duplicated across Haskell, GitHub Pages, and the VSCode extension, making it hard to keep in sync. The Haskell HTML pipeline is ~3,100 lines (ToHtml.hs + Xml library + Css library) and is difficult to iterate on.

## Decision

Move HTML rendering from Haskell to a shared TypeScript renderer. Haskell emits only JSON. Both GitHub Pages and the VSCode extension import the same renderer.

## Haskell Changes

**Delete entirely:**
- `Scrod.Convert.ToHtml` (1,030 lines)
- `Scrod.Xml.*` (10 modules, ~1,078 lines)
- `Scrod.Css.*` (9 modules, ~1,039 lines)
- `Scrod.Executable.Format` (the `Html | Json` type)

**Simplify:**
- `Config.hs` — remove `format` field and `--format` flag
- `Main.hs` — remove format switch; always produce JSON
- `Flag.hs` — remove the `Format` flag
- Cabal file — remove deleted modules from `exposed-modules`
- `TestSuite/All.hs` — remove specs for deleted modules

**Keep:** `Schema.hs` and `--schema` flag (used to generate TypeScript types).

CLI becomes JSON-only. The `--schema` output feeds type generation.

## TypeScript Renderer (`extra/renderer/`)

```
extra/renderer/
  package.json
  tsconfig.json
  src/
    types.ts            # Generated from scrod --schema
    render.ts           # Main: renderModule(module: Module): string
    sections/
      header.ts
      metadata.ts
      exports.ts
      imports.ts
      extensions.ts     # Includes extension URL map
      items.ts
      footer.ts
      doc.ts            # Recursive Doc -> HTML
  dist/
    renderer.js         # Bundled output (esbuild)
```

Key properties:
- Takes a parsed JSON object, returns an HTML string
- Pure string concatenation (no DOM dependency) — works in browser and Node.js
- Reproduces the same Bootstrap 5 classes, KaTeX delimiters, badge colors, and `data-line`/`data-col` attributes as the current Haskell output
- `types.ts` generated from `scrod --schema` and checked in
- Single bundled `dist/renderer.js` for consumers

## Consumer Integration

### GitHub Pages (`extra/github-pages/`)
- `worker.js`: Always sends `--format json` to WASM (or omits flag since JSON is now default)
- `index.js`: Imports renderer. On JSON result, calls `renderModule(JSON.parse(json))`, injects HTML into shadow DOM. Format dropdown removed or repurposed for "Rendered" vs "Raw JSON"
- KaTeX loaded after rendering (same as today)
- Bootstrap CSS still from CDN `<link>`

### VSCode Extension (`extra/vscode/`)
- `wasmEngine.ts`: Removes `--format html` (JSON is default)
- `extension.ts`: Imports bundled renderer. Parses JSON, calls `renderModule()`, passes HTML to webview
- Build step extended to copy renderer alongside WASM artifacts

## What This Achieves

- Removes ~3,100 lines of Haskell
- Single source of truth for HTML rendering (the TypeScript renderer)
- Faster iteration on styling (JS vs Haskell rebuild)
- Type-safe renderer via schema-generated TypeScript types
- Both frontends share identical rendering code
