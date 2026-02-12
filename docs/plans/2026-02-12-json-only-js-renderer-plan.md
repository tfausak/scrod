# JSON-Only Haskell + Shared JS Renderer — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move HTML rendering from Haskell to a shared TypeScript renderer so both GitHub Pages and the VSCode extension use the same code, then remove ~3,100 lines of Haskell.

**Architecture:** Haskell emits only JSON. A new `extra/renderer/` TypeScript package takes parsed JSON and returns an HTML string. Both frontends import the bundled renderer. The Haskell ToHtml, Xml, and Css modules are deleted.

**Tech Stack:** TypeScript, esbuild, existing Bootstrap 5 CDN, existing KaTeX CDN

**Design doc:** `docs/plans/2026-02-12-json-only-js-renderer-design.md`

---

## Phase 1: Create the TypeScript Renderer

### Task 1: Scaffold the renderer project

**Files:**
- Create: `extra/renderer/package.json`
- Create: `extra/renderer/tsconfig.json`

**Step 1: Create package.json**

```json
{
  "name": "scrod-renderer",
  "version": "0.0.0",
  "private": true,
  "type": "module",
  "scripts": {
    "build": "esbuild --bundle --format=esm --outfile=dist/renderer.js src/render.ts",
    "check": "tsc --noEmit"
  },
  "devDependencies": {
    "esbuild": "^0.25.0",
    "typescript": "^5.7.0"
  }
}
```

**Step 2: Create tsconfig.json**

```json
{
  "compilerOptions": {
    "strict": true,
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "bundler",
    "outDir": "dist",
    "rootDir": "src",
    "declaration": true,
    "esModuleInterop": true
  },
  "include": ["src"]
}
```

**Step 3: Install dependencies**

Run: `cd extra/renderer && npm install`

**Step 4: Commit**

```bash
git add extra/renderer/package.json extra/renderer/tsconfig.json extra/renderer/package-lock.json
git commit -m "Scaffold extra/renderer TypeScript project"
```

### Task 2: Generate TypeScript types from JSON Schema

The Haskell CLI has a `--schema` flag that emits a JSON Schema 2020-12 document describing the module JSON output. We need to convert this into TypeScript types.

**Files:**
- Create: `extra/renderer/src/types.ts`

**Step 1: Generate the schema JSON**

Run: `cabal run scrod -- --schema > extra/renderer/schema.json`

Examine the output to understand the type structure. Key types to extract:
- `Module` (root object)
- `Item` (declaration)
- `Doc` (documentation, 22-variant tagged union)
- `Export`, `Import`, `Extension`
- `ItemKind`, `Namespace`, `Level` (enums)
- `Location`, `Located<T>`, etc.

**Step 2: Write types.ts by hand from the schema**

Write TypeScript types matching the JSON structure. The JSON uses a tagged union pattern: `{"type": "ConstructorName", "value": ...}` for sum types, and flat objects for record types. Null values are omitted from objects.

Key patterns in the JSON:
- Record types → `{ field1: Type1, field2?: Type2 }` (optional fields for Maybe)
- Sum types → `{ type: "Variant1" } | { type: "Variant2", value: Inner }`
- Located → `{ location: Location, value: T }`
- Lists → `T[]`

The `Module` root type has fields: `version`, `name?`, `language?`, `documentation`, `warning?`, `since?`, `extensions`, `exports?`, `imports`, `items`, `signature`.

**Step 3: Verify types compile**

Run: `cd extra/renderer && npx tsc --noEmit`
Expected: no errors

**Step 4: Commit**

```bash
git add extra/renderer/schema.json extra/renderer/src/types.ts
git commit -m "Add TypeScript types generated from Scrod JSON Schema"
```

### Task 3: Write the Doc renderer

The `Doc` type is a 22-variant tagged union and is the most complex piece. Implement it first since everything else depends on it.

**Files:**
- Create: `extra/renderer/src/sections/doc.ts`

**Step 1: Write doc.ts**

Translate the logic from `source/library/Scrod/Convert/ToHtml.hs` lines 852-975 (`docToContents` and helpers). The function signature:

```typescript
export function renderDoc(doc: Doc): string
```

Map each Doc variant to HTML:
- `Empty` → `""`
- `Append` → `renderDoc(d1) + renderDoc(d2)`
- `String` → escaped text
- `Paragraph` → `<p>...</p>`
- `Identifier` → `<code class="font-monospace text-success">...</code>`
- `Module` → `<code class="font-monospace text-info">...</code>`
- `Emphasis` → `<em>...</em>`
- `Monospaced` → `<code>...</code>`
- `Bold` → `<strong>...</strong>`
- `UnorderedList` → `<ul><li>...</li></ul>`
- `OrderedList` → `<ol><li value="N">...</li></ol>`
- `DefList` → `<dl><dt>...</dt><dd>...</dd></dl>`
- `CodeBlock` → `<pre class="bg-body-secondary rounded p-3 my-3"><code>...</code></pre>`
- `Hyperlink` → `<a href="...">...</a>`
- `Pic` → `<img src="..." alt="..." title="...">`
- `MathInline` → `\(...\)` (KaTeX delimiters)
- `MathDisplay` → `\[...\]`
- `AName` → `<a id="..."></a>`
- `Property` → `<pre class="border-start border-4 border-primary ...">...</pre>`
- `Examples` → styled div with `>>> expr` and results
- `Header` → `<h1>` through `<h6>` based on level
- `Table` → Bootstrap table with thead/tbody

Include a helper `escapeHtml(text: string): string` that escapes `&`, `<`, `>`, `"`.

**Step 2: Verify it compiles**

Run: `cd extra/renderer && npx tsc --noEmit`

**Step 3: Commit**

```bash
git add extra/renderer/src/sections/doc.ts
git commit -m "Add Doc-to-HTML renderer"
```

### Task 4: Write the section renderers

**Files:**
- Create: `extra/renderer/src/sections/header.ts`
- Create: `extra/renderer/src/sections/metadata.ts`
- Create: `extra/renderer/src/sections/exports.ts`
- Create: `extra/renderer/src/sections/imports.ts`
- Create: `extra/renderer/src/sections/extensions.ts`
- Create: `extra/renderer/src/sections/items.ts`
- Create: `extra/renderer/src/sections/footer.ts`

**Step 1: Write header.ts**

Translate `source/library/Scrod/Convert/ToHtml.hs` lines 168-178. Renders module name as `<h1>`, warning as alert, and module doc.

```typescript
export function renderHeader(module: Module): string
```

**Step 2: Write metadata.ts**

Translate lines 204-247. Renders language and since fields as a card with `<dl>`.

```typescript
export function renderMetadata(module: Module): string
```

**Step 3: Write exports.ts**

Translate lines 267-377. Renders export list with sections, identifiers, subordinates.

```typescript
export function renderExports(exports: Export[] | undefined): string
```

**Step 4: Write imports.ts**

Translate lines 382-425. Renders imports as a collapsible `<details>` with module count.

```typescript
export function renderImports(imports: Import[]): string
```

**Step 5: Write extensions.ts**

Translate lines 429-619. Renders extensions as badges linking to GHC docs. Includes the 136-entry `extensionUrlPaths` map.

```typescript
export function renderExtensions(extensions: Record<string, boolean>): string
```

**Step 6: Write items.ts**

Translate lines 623-848. Renders declarations as Bootstrap cards with:
- Color-coded border and badge per `ItemKind`
- Name in monospace
- Type signature
- Location button with `data-line` / `data-col`
- Nested children indented with border-start
- Parent/child hierarchy via `parentKey`

```typescript
export function renderItems(items: Located<Item>[]): string
```

This is the most complex section renderer. Key details:
- `kindToText`: maps each ItemKind to display text
- `kindBadgeClass` / `kindBorderStyle`: maps ItemKind → Bootstrap color class
- Type-variable signatures (data, newtype, type, class) show sig before the kind badge; others show `:: sig` after
- Children are found by `parentKey` matching parent's `key`

**Step 7: Write footer.ts**

Translate lines 250-263. Simple "Generated by Scrod version X.Y.Z" footer.

```typescript
export function renderFooter(module: Module): string
```

**Step 8: Verify all compile**

Run: `cd extra/renderer && npx tsc --noEmit`

**Step 9: Commit**

```bash
git add extra/renderer/src/sections/
git commit -m "Add section renderers (header, metadata, exports, imports, extensions, items, footer)"
```

### Task 5: Write the main render entry point

**Files:**
- Create: `extra/renderer/src/render.ts`

**Step 1: Write render.ts**

This assembles all sections into a complete `<html>` document, matching `source/library/Scrod/Convert/ToHtml.hs` lines 60-164.

```typescript
import { renderHeader } from "./sections/header.js";
import { renderMetadata } from "./sections/metadata.js";
import { renderExports } from "./sections/exports.js";
import { renderImports } from "./sections/imports.js";
import { renderExtensions } from "./sections/extensions.js";
import { renderItems } from "./sections/items.js";
import { renderFooter } from "./sections/footer.js";
import type { Module } from "./types.js";

export function renderModule(module: Module): string {
  // Assemble head (meta, title, Bootstrap CSS, KaTeX CSS, theme script)
  // and body (container with all sections)
  // Match the exact same structure as ToHtml.toHtml
}

export type { Module } from "./types.js";
```

The `<head>` includes:
- `<meta charset="utf-8">`
- `<meta name="viewport" content="width=device-width, initial-scale=1">`
- `<title>` from module name (or "Documentation")
- Bootstrap 5.3.8 CSS `<link>` with SRI hash
- KaTeX 0.16.22 CSS `<link>` with SRI hash
- KaTeX auto-render `<link rel="modulepreload">` with SRI hash
- Dark mode + KaTeX `<script>`

The `<body>` is a `<div class="container py-4 text-break">` containing all sections in order.

**Step 2: Build the bundle**

Run: `cd extra/renderer && npm run build`
Expected: creates `dist/renderer.js`

**Step 3: Verify the bundle works**

Quick smoke test: run scrod to get JSON, then use Node.js to render it:

```bash
echo "module Foo where\nx :: Int\nx = 1" | cabal run scrod | node -e "
  import('./extra/renderer/dist/renderer.js').then(r => {
    const json = require('fs').readFileSync('/dev/stdin', 'utf8');
    console.log(r.renderModule(JSON.parse(json)));
  });
"
```

Compare output against:
```bash
echo "module Foo where\nx :: Int\nx = 1" | cabal run scrod -- --format html
```

They should produce equivalent HTML.

**Step 4: Commit**

```bash
git add extra/renderer/src/render.ts extra/renderer/dist/
git commit -m "Add main renderModule entry point and bundle"
```

### Task 6: Verify output parity

**Step 1: Write a comparison test script**

Create `extra/renderer/test-parity.sh` that:
1. Takes a Haskell source file (or uses a default inline snippet)
2. Runs `cabal run scrod -- --format html` to get Haskell HTML
3. Runs `cabal run scrod -- --format json` piped through the Node.js renderer
4. Diffs the two outputs
5. Reports mismatches

Test with several inputs:
- Simple module with a function
- Module with data types, classes, instances
- Module with Haddock documentation (paragraphs, code blocks, lists, links)
- Module with extensions and imports

**Step 2: Fix any discrepancies**

Iterate until the outputs match (or differ only in acceptable whitespace).

**Step 3: Commit**

```bash
git add extra/renderer/test-parity.sh
git commit -m "Add HTML parity test script"
```

---

## Phase 2: Integrate with Consumers

### Task 7: Integrate with GitHub Pages

**Files:**
- Modify: `extra/github-pages/worker.js` (lines 56-60)
- Modify: `extra/github-pages/index.js` (lines 93-121)
- Modify: `extra/github-pages/index.html` (lines 14-18)
- Copy: `extra/renderer/dist/renderer.js` → `extra/github-pages/renderer.js`

**Step 1: Copy renderer to github-pages**

```bash
cp extra/renderer/dist/renderer.js extra/github-pages/renderer.js
```

**Step 2: Update worker.js**

Remove the `msg.format` parameter from the WASM call — always use JSON (the default). Change lines 56-60:

Before:
```javascript
const args = ['--format', msg.format];
```

After:
```javascript
const args = [];
```

Keep literate/signature flags as-is.

**Step 3: Update index.js**

Import the renderer at the top of the file. In `worker.onmessage` (lines 104-121), change the result handler:

Before:
```javascript
if (msg.format === 'json') {
  showJson(msg.value);
} else {
  shadow.innerHTML = msg.value;
  syncShadowTheme();
  renderMath();
}
```

After: always get JSON, render with `renderModule`. Repurpose the format dropdown to toggle between "Rendered" (HTML via renderer) and "Raw JSON" (pretty-printed JSON). When "Rendered" is selected, use the renderer; when "Raw JSON", show pretty-printed JSON.

**Step 4: Update index.html**

Change the format dropdown options from "HTML/JSON" to "Rendered/JSON":

```html
<select id='format' class='form-select form-select-sm w-auto'>
  <option value='rendered' selected>Rendered</option>
  <option value='json'>JSON</option>
</select>
```

**Step 5: Update hash encoding**

In `updateHash()` and the hash-reading code, update format values from `'html'` to `'rendered'` (or keep backward compat by treating `'html'` as `'rendered'`).

**Step 6: Test locally**

Open `extra/github-pages/index.html` in a browser (or use a local server). Verify the rendered output matches the old HTML output.

**Step 7: Commit**

```bash
git add extra/github-pages/
git commit -m "Integrate shared renderer into GitHub Pages"
```

### Task 8: Integrate with VSCode Extension

**Files:**
- Modify: `extra/vscode/src/wasmEngine.ts` (line 56)
- Modify: `extra/vscode/src/extension.ts`
- Modify: `extra/vscode/package.json` (build scripts)
- Copy: `extra/renderer/dist/renderer.js` → `extra/vscode/renderer/renderer.mjs`

**Step 1: Copy renderer**

```bash
mkdir -p extra/vscode/renderer
cp extra/renderer/dist/renderer.js extra/vscode/renderer/renderer.mjs
```

Add this to the `copy-wasm` script in the VSCode build so it stays in sync.

**Step 2: Update wasmEngine.ts**

Change line 56 from `["--format", "html"]` to `[]` (JSON is default). The return type changes from HTML string to JSON string.

**Step 3: Update extension.ts**

After getting JSON from WASM, parse it and call `renderModule()` before passing to the webview. Import the renderer using dynamic `import()` (same ESM pattern used for WASI shim).

**Step 4: Build and test**

Run: `cd extra/vscode && npm run build && npx vsce package --allow-missing-repository`

Install the `.vsix` and verify preview rendering works.

**Step 5: Commit**

```bash
git add extra/vscode/
git commit -m "Integrate shared renderer into VSCode extension"
```

---

## Phase 3: Remove Haskell HTML Code

### Task 9: Remove Format type and --format flag

**Files:**
- Delete: `source/library/Scrod/Executable/Format.hs`
- Modify: `source/library/Scrod/Executable/Flag.hs` — remove `Format String` constructor (line 12), remove `--format` option descriptor (line 32), remove format tests (lines 50-58)
- Modify: `source/library/Scrod/Executable/Config.hs` — remove `format` field (line 16), remove `Format.Format` import, remove `Flag.Format` handler (lines 29-31), remove format tests (lines 76-86)
- Modify: `source/library/Scrod/Executable/Main.hs` — remove `Format` import (line 20), simplify convert to always use JSON (lines 69-71)
- Modify: `source/library/Scrod/TestSuite/All.hs` — remove `Scrod.Executable.Format` import (line 21) and spec call (line 78)
- Modify: `scrod.cabal` — remove `Scrod.Executable.Format` from exposed-modules (line 155)

**Step 1: Delete Format.hs**

```bash
rm source/library/Scrod/Executable/Format.hs
```

**Step 2: Update Flag.hs**

Remove the `Format String` constructor and its option descriptor and tests.

**Step 3: Update Config.hs**

Remove the `format` field, its import of `Format`, and the `Flag.Format` case in `applyFlag`. Remove format-related tests.

**Step 4: Update Main.hs**

Remove imports for `ToHtml`, `Format`, and `Xml.Document`. Change lines 69-72 from the format switch to:

```haskell
pure $ (Json.encode . ToJson.toJson) module_ <> Builder.charUtf8 '\n'
```

Also update the module doc comment (line 3) to remove "HTML or".

**Step 5: Update All.hs**

Remove the `Scrod.Executable.Format` import and spec call.

**Step 6: Update scrod.cabal**

Remove `Scrod.Executable.Format` from exposed-modules.

**Step 7: Build and test**

Run: `cabal build && cabal test --test-options='--hide-successes'`
Expected: all tests pass

**Step 8: Commit**

```bash
git add -u
git commit -m "Remove --format flag and Format type (JSON is now the only output)"
```

### Task 10: Delete ToHtml.hs

**Files:**
- Delete: `source/library/Scrod/Convert/ToHtml.hs`
- Modify: `scrod.cabal` — remove `Scrod.Convert.ToHtml` from exposed-modules (line 103)

**Step 1: Delete the file**

```bash
rm source/library/Scrod/Convert/ToHtml.hs
```

**Step 2: Update scrod.cabal**

Remove `Scrod.Convert.ToHtml` from exposed-modules.

**Step 3: Build and test**

Run: `cabal build && cabal test --test-options='--hide-successes'`

**Step 4: Commit**

```bash
git add -u
git commit -m "Delete Scrod.Convert.ToHtml (HTML rendering moved to TypeScript)"
```

### Task 11: Delete the Xml library

**Files:**
- Delete: `source/library/Scrod/Xml/Attribute.hs`
- Delete: `source/library/Scrod/Xml/Comment.hs`
- Delete: `source/library/Scrod/Xml/Content.hs`
- Delete: `source/library/Scrod/Xml/Declaration.hs`
- Delete: `source/library/Scrod/Xml/Document.hs`
- Delete: `source/library/Scrod/Xml/Element.hs`
- Delete: `source/library/Scrod/Xml/Instruction.hs`
- Delete: `source/library/Scrod/Xml/Misc.hs`
- Delete: `source/library/Scrod/Xml/Name.hs`
- Delete: `source/library/Scrod/Xml/Text.hs`
- Modify: `scrod.cabal` — remove all 10 `Scrod.Xml.*` modules (lines 197-206)
- Modify: `source/library/Scrod/TestSuite/All.hs` — remove all `Scrod.Xml.*` imports (lines 48-57) and spec calls (lines 105-114)

**Step 1: Delete all Xml module files**

```bash
rm -r source/library/Scrod/Xml/
```

**Step 2: Update scrod.cabal**

Remove all 10 `Scrod.Xml.*` entries from exposed-modules.

**Step 3: Update All.hs**

Remove all `Scrod.Xml.*` imports and spec calls.

**Step 4: Build and test**

Run: `cabal build && cabal test --test-options='--hide-successes'`

**Step 5: Commit**

```bash
git add -u
git commit -m "Delete Scrod.Xml library (no longer needed)"
```

### Task 12: Delete the Css library

**Files:**
- Delete: `source/library/Scrod/Css/AtRule.hs`
- Delete: `source/library/Scrod/Css/Block.hs`
- Delete: `source/library/Scrod/Css/BlockContent.hs`
- Delete: `source/library/Scrod/Css/Declaration.hs`
- Delete: `source/library/Scrod/Css/Item.hs`
- Delete: `source/library/Scrod/Css/Name.hs`
- Delete: `source/library/Scrod/Css/Rule.hs`
- Delete: `source/library/Scrod/Css/Selector.hs`
- Delete: `source/library/Scrod/Css/Stylesheet.hs`
- Modify: `scrod.cabal` — remove all 9 `Scrod.Css.*` modules (lines 143-151)
- Modify: `source/library/Scrod/TestSuite/All.hs` — remove all `Scrod.Css.*` imports (lines 9-17) and spec calls (lines 67-75)

**Step 1: Delete all Css module files**

```bash
rm -r source/library/Scrod/Css/
```

**Step 2: Update scrod.cabal**

Remove all 9 `Scrod.Css.*` entries from exposed-modules.

**Step 3: Update All.hs**

Remove all `Scrod.Css.*` imports and spec calls.

**Step 4: Build and test**

Run: `cabal build && cabal test --test-options='--hide-successes'`

**Step 5: Commit**

```bash
git add -u
git commit -m "Delete Scrod.Css library (was already unused)"
```

---

## Phase 4: Cleanup

### Task 13: Update documentation

**Files:**
- Modify: `CONTRIBUTING.md` — remove `--format html` from example (line 9), update code conventions to remove mention of XML/CSS (line 55)
- Modify: `CLAUDE.md` — update architecture description to reflect JSON-only output and shared renderer

**Step 1: Update CONTRIBUTING.md**

Change line 9 from:
```
cabal run scrod -- --format html     # run CLI (reads stdin)
```
to:
```
cabal run scrod                      # run CLI (reads stdin, outputs JSON)
```

Remove "No external dependencies for XML, JSON, and CSS generation" from line 55, update to reflect the new architecture.

**Step 2: Update CLAUDE.md**

Update the pipeline description, module groups, and architecture sections to reflect:
- CLI outputs JSON only
- ToHtml, Xml, Css modules are gone
- `extra/renderer/` exists as the shared TypeScript renderer
- GitHub Pages and VSCode both import the renderer

**Step 3: Commit**

```bash
git add CONTRIBUTING.md CLAUDE.md
git commit -m "Update documentation for JSON-only output and shared renderer"
```

### Task 14: Final verification

**Step 1: Full Haskell build with pedantic flag**

Run: `cabal build --flags=pedantic`
Expected: builds with no warnings

**Step 2: Full test suite**

Run: `cabal test --test-options='--hide-successes'`
Expected: all tests pass

**Step 3: Lint and format check**

Run:
```bash
hlint source/
ormolu --mode check $(find source -name "*.hs")
cabal-gild --input scrod.cabal --mode check
cabal check
```
Expected: all pass

**Step 4: Renderer build**

Run: `cd extra/renderer && npm run build && npm run check`
Expected: builds and type-checks with no errors

**Step 5: Commit any final fixes**

If any issues found, fix and commit.
