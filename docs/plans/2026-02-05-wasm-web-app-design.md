# WASM Web App Design

Offline web app for rendering Haskell documentation in the browser. Left pane is a textarea for Haskell source input, right pane shows rendered HTML output. Compiled to WASM using GHC's wasm32-wasi backend.

## Architecture

```
┌─────────────────────────────────────────────────┐
│  Browser                                        │
│                                                 │
│  ┌──────────────┐        ┌──────────────┐       │
│  │  index.html  │        │  worker.js   │       │
│  │              │  msg    │              │       │
│  │  textarea ───┼───────►│  WASM module │       │
│  │  (input)     │◄───────┤  (lc.wasm)   │       │
│  │              │  html   │              │       │
│  │  <div>  ◄────┤        └──────────────┘       │
│  │  (output)    │                               │
│  └──────────────┘                               │
└─────────────────────────────────────────────────┘
```

- **Main thread**: Static HTML page with a left/right split layout. Left pane has a `<textarea>` for Haskell source. Right pane is a `<div>` that renders the HTML output. A small `index.js` script manages the Web Worker and updates the DOM.
- **Web Worker** (`worker.js`): Loads the WASM module compiled with GHC's wasm32-wasi backend. Receives Haskell source strings via `postMessage`, runs the parse/convert/HTML pipeline, and posts back the resulting HTML string.
- **WASM module**: The core Haskell library compiled to wasm32-wasi. Exposes a single function: source in, HTML out. This is the entire `Parse.parse -> FromGhc.fromGhc -> ToHtml.toHtml -> Xml.encode` pipeline.

The whole thing is a handful of static files that can be served from anywhere or wrapped as a PWA with a service worker for true offline use.

## Project Structure

```
legendary-chainsaw/
├── source/                          # existing
│   ├── library/
│   ├── executable/
│   └── test-suite/
├── wasm/                            # new
│   ├── Main.hs                      # WASM entry point (Haskell, JSFFI exports)
│   ├── build.sh                     # Build script (wasm32-wasi-cabal + post-linker)
│   └── www/                         # Static web assets
│       ├── index.html               # Split-pane layout
│       ├── index.js                 # Main thread: manages worker, updates DOM
│       ├── worker.js                # Loads WASM, handles messages
│       └── style.css                # Minimal layout styles
├── legendary-chainsaw.cabal         # add a new wasm-specific executable stanza
└── cabal.project.wasm               # separate cabal project file for WASM builds
```

- **Separate `cabal.project.wasm`** rather than modifying the existing `cabal.project.local`. The WASM build needs different flags (`--with-compiler=wasm32-wasi-ghc`, etc.) and shouldn't interfere with normal development.
- **`wasm/Main.hs`** is a thin entry point that imports the existing library and exposes a JSFFI foreign export. The actual logic stays in the library.
- **New executable stanza** in the cabal file (`executable legendary-chainsaw-wasm`) that depends on the library and builds `wasm/Main.hs`.
- **`build.sh`** orchestrates: `wasm32-wasi-cabal build`, runs the GHC JS post-linker, copies artifacts into a `dist/` directory ready to serve.

## Haskell WASM Entry Point

`wasm/Main.hs` is small glue between JSFFI and the existing library:

```haskell
module Main where

import qualified GHC.Wasm.Prim as Wasm
import qualified LegendaryChainsaw.Convert.FromGhc as FromGhc
import qualified LegendaryChainsaw.Convert.ToHtml as ToHtml
import qualified LegendaryChainsaw.Ghc.Parse as Parse
import qualified LegendaryChainsaw.Xml.Document as Xml
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString

foreign export javascript "processHaskell"
  processHaskell :: Wasm.JSString -> IO Wasm.JSString

processHaskell :: Wasm.JSString -> IO Wasm.JSString
processHaskell input = do
  let source = Wasm.fromJSString input
  pure . Wasm.toJSString $ case pipeline source of
    Left err -> "<pre class=\"error\">" <> err <> "</pre>"
    Right html -> html

pipeline :: String -> Either String String
pipeline source = do
  parsed <- Parse.parse source
  module_ <- FromGhc.fromGhc parsed
  let builder = Xml.encode (ToHtml.toHtml module_)
  pure
    . LazyByteString.toString
    . Builder.toLazyByteString
    $ builder
```

- `GHC.Wasm.Prim` provides `JSString`, `toJSString`, `fromJSString`.
- `foreign export javascript` is GHC WASM JSFFI syntax making `processHaskell` callable from JS.
- Errors are returned as HTML (wrapped in `<pre>`) so the output pane always receives renderable content.
- Compiled with `-no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=processHaskell`.

## Web Frontend

**`index.html`**:

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Legendary Chainsaw</title>
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <div class="pane input-pane">
    <textarea id="source" placeholder="Enter Haskell source..."></textarea>
  </div>
  <div class="pane output-pane" id="output"></div>
  <script src="index.js"></script>
</body>
</html>
```

**`index.js`** (main thread, manages the worker):

```javascript
const worker = new Worker("worker.js");
const source = document.getElementById("source");
const output = document.getElementById("output");

let debounceTimer;
source.addEventListener("input", () => {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    worker.postMessage(source.value);
  }, 300);
});

worker.onmessage = (e) => {
  output.innerHTML = e.data;
};
```

**`worker.js`** (loads WASM, processes messages):

```javascript
// Import the GHC post-linker generated glue
importScripts("ghc_wasm_jsffi.js");

let processHaskell;

async function init() {
  const mod = await WebAssembly.compileStreaming(fetch("lc.wasm"));
  const instance = await init_legendary_chainsaw_wasm(mod);
  processHaskell = instance.processHaskell;
  postMessage(""); // signal ready
}

init();

onmessage = (e) => {
  if (processHaskell) {
    const result = processHaskell(e.data);
    postMessage(result);
  }
};
```

The exact WASM loading code depends on what the GHC JS post-linker generates. Ormolu-live's `worker.js` is a good reference for the real incantation.

**`style.css`**: Two panes side by side, each 50% width, full viewport height. The output pane inherits the CSS that `ToHtml` already produces (the existing `Css/` modules generate a stylesheet as part of the HTML output).

## Build Process

Prerequisites: a wasm32-wasi GHC toolchain from ghc-wasm-meta.

**`cabal.project.wasm`**:

```
packages: .
with-compiler: wasm32-wasi-ghc
with-hc-pkg: wasm32-wasi-ghc-pkg

package legendary-chainsaw
  ghc-options: -no-hs-main -optl-mexec-model=reactor
```

**Cabal file addition**:

```cabal
executable legendary-chainsaw-wasm
  import: library
  build-depends: legendary-chainsaw
  hs-source-dirs: wasm
  main-is: Main.hs
  ghc-options: -no-hs-main
```

**`build.sh`**:

```bash
#!/bin/bash
set -euo pipefail

# 1. Build the WASM executable
wasm32-wasi-cabal build \
  --project-file=cabal.project.wasm \
  exe:legendary-chainsaw-wasm

# 2. Find the built .wasm file
WASM=$(find dist-newstyle -name "legendary-chainsaw-wasm.wasm" -type f)

# 3. Run the GHC JS post-linker to generate JSFFI glue
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
  --input "$WASM" \
  --output wasm/dist/lc.wasm

# 4. Assemble dist directory
mkdir -p wasm/dist
cp wasm/www/* wasm/dist/
# The post-linker also produces ghc_wasm_jsffi.js alongside lc.wasm

echo "Build complete. Serve wasm/dist/ to run the app."
```

Serve locally with `python3 -m http.server -d wasm/dist`.

## Risks & Open Questions

1. **GHC 9.14.1 WASM support**: The WASM backend is still a tech preview. The project pins `ghc ^>=9.14.1`. A matching `wasm32-wasi-ghc` 9.14.x must be available from ghc-wasm-meta. If nightly artifacts don't have 9.14 yet, it may need to be built manually or the GHC version adjusted.
2. **Template Haskell**: The project uses `template-haskell` (for QQ). TH works with wasm32-wasi as of GHC 9.10+, but requires `cabal-3.14+` and `wasm32-wasi-cabal`. Should be fine but worth verifying early.
3. **Binary size**: Expect 20-40MB gzipped due to the GHC library dependency. Acceptable for an offline tool that loads once and is cached by the browser.
4. **`haddock-library` under WASM**: Needs to compile with wasm32-wasi-ghc. Pure Haskell, so should work, but needs verification.

## Implementation Order

1. Get ghc-wasm-meta set up and verify `wasm32-wasi-ghc --version` matches the project GHC.
2. Try `wasm32-wasi-cabal build` on the existing library — this is the real feasibility test.
3. Add the WASM executable stanza and `wasm/Main.hs` with a minimal JSFFI export (echo a string back).
4. Wire up the web worker and verify round-trip JS -> WASM -> JS works.
5. Connect the real pipeline (Parse -> FromGhc -> ToHtml).
6. Build the HTML/CSS/JS frontend with the split-pane layout.
7. Polish: debounce, loading indicator, error display, example snippets.

## References

- [ormolu-live](https://github.com/tweag/ormolu/tree/master/ormolu-live) — primary inspiration
- [GHC WASM backend docs (9.14.1)](https://downloads.haskell.org/ghc/latest/docs/users_guide/wasm.html)
- [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) — toolchain setup
- [jsaddle-wasm](https://hackage.haskell.org/package/jsaddle-wasm) — reference for JSFFI patterns
