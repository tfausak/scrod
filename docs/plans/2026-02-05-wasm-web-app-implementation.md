# WASM Web App Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create an offline web app that compiles legendary-chainsaw to WASM, with a split-pane UI for Haskell source input and rendered HTML output.

**Architecture:** Thin WASM entry point calls existing library pipeline (Parse -> FromGhc -> ToHtml -> Xml.encode). Web Worker runs the WASM module off the main thread. Plain HTML/JS/CSS frontend with no framework.

**Tech Stack:** GHC WASM backend (wasm32-wasi), JSFFI, plain HTML/JS/CSS, Web Workers.

**Constraint:** `wasm32-wasi-ghc` is not available in this dev environment. We write all code and verify non-WASM parts build with native GHC. The WASM build itself must be tested separately once the toolchain is set up.

---

### Task 1: Extract shared pipeline function into the library

The existing pipeline lives inline in `Executable/Main.hs`. Extract it into a reusable function so both the CLI and WASM entry points can use it without duplication.

**Files:**
- Create: `source/library/LegendaryChainsaw/Pipeline.hs`
- Modify: `source/library/LegendaryChainsaw/Executable/Main.hs`

**Step 1: Create `Pipeline.hs` with the shared pipeline**

```haskell
module LegendaryChainsaw.Pipeline where

import qualified Data.ByteString.Builder as Builder
import qualified LegendaryChainsaw.Convert.FromGhc as FromGhc
import qualified LegendaryChainsaw.Convert.ToHtml as ToHtml
import qualified LegendaryChainsaw.Convert.ToJson as ToJson
import qualified LegendaryChainsaw.Executable.Format as Format
import qualified LegendaryChainsaw.Ghc.Parse as Parse
import qualified LegendaryChainsaw.Json.Value as Json
import qualified LegendaryChainsaw.Xml.Document as Xml

toHtmlBuilder :: String -> Either String Builder.Builder
toHtmlBuilder source = do
  (_, lHsModule) <- Parse.parse source
  module_ <- FromGhc.fromGhc (_, lHsModule)  -- needs full parse result
  pure . Xml.encode $ ToHtml.toHtml module_

toJsonBuilder :: String -> Either String Builder.Builder
toJsonBuilder source = do
  (_, lHsModule) <- Parse.parse source
  module_ <- FromGhc.fromGhc (_, lHsModule)
  pure . Json.encode . ToJson.toJson $ module_
```

Wait -- looking at the actual code more carefully:

```haskell
-- In Main.hs:
result <- either fail pure $ Parse.parse input
module_ <- either fail pure $ FromGhc.fromGhc result
```

`Parse.parse` returns the full tuple, `FromGhc.fromGhc` takes that tuple. So the pipeline is:

```haskell
toBuilder :: Format -> String -> Either String Builder.Builder
toBuilder fmt source = do
  result <- Parse.parse source
  module_ <- FromGhc.fromGhc result
  pure $ case fmt of
    Format.Json -> Json.encode (ToJson.toJson module_)
    Format.Html -> Xml.encode (ToHtml.toHtml module_)
```

Actually, let's keep this simpler. The WASM entry point only needs HTML output. And the existing `Main.hs` is already small. Rather than over-abstracting, just let both entry points call the same library functions directly. The WASM `Main.hs` is glue code that won't be compiled by native GHC anyway.

**Revised approach:** Skip extraction. The duplication is minimal (3 lines), and adding an abstraction layer for two call sites violates YAGNI. Both entry points just call `Parse.parse`, `FromGhc.fromGhc`, and the encoder directly.

### Task 1: Add WASM executable stanza to cabal file

**Files:**
- Modify: `legendary-chainsaw.cabal`

**Step 1: Add the executable stanza**

Add after the existing `executable legendary-chainsaw` stanza at the end of the file:

```cabal
executable legendary-chainsaw-wasm
  import: library
  build-depends: legendary-chainsaw
  hs-source-dirs: wasm
  main-is: Main.hs
  ghc-options: -no-hs-main
```

**Step 2: Run `cabal check` to verify**

Run: `cabal check`
Expected: No new errors (there may be pre-existing warnings about the TODO synopsis).

**Step 3: Commit**

```
git add legendary-chainsaw.cabal
git commit -m "Add WASM executable stanza to cabal file"
```

### Task 2: Create `wasm/Main.hs` (JSFFI entry point)

**Files:**
- Create: `wasm/Main.hs`

**Step 1: Write the WASM entry point**

```haskell
module Main where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified GHC.Wasm.Prim as Wasm
import qualified LegendaryChainsaw.Convert.FromGhc as FromGhc
import qualified LegendaryChainsaw.Convert.ToHtml as ToHtml
import qualified LegendaryChainsaw.Ghc.Parse as Parse
import qualified LegendaryChainsaw.Xml.Document as Xml

foreign export javascript "processHaskell"
  processHaskell :: Wasm.JSString -> IO Wasm.JSString

processHaskell :: Wasm.JSString -> IO Wasm.JSString
processHaskell input = do
  let source = Wasm.fromJSString input
  pure . Wasm.toJSString $ case pipeline source of
    Left err -> "<pre class=\"error\">" <> escapeHtml err <> "</pre>"
    Right html -> html

pipeline :: String -> Either String String
pipeline source = do
  result <- Parse.parse source
  module_ <- FromGhc.fromGhc result
  pure
    . map (Char.chr . fromIntegral)
    . LazyByteString.unpack
    . Builder.toLazyByteString
    . Xml.encode
    $ ToHtml.toHtml module_

escapeHtml :: String -> String
escapeHtml = concatMap $ \c -> case c of
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  _ -> [c]
```

Note: This file uses `GHC.Wasm.Prim` and `foreign export javascript` which are only available with `wasm32-wasi-ghc`. It will NOT compile with native GHC. That's expected -- it's only built via `cabal.project.wasm`.

**Step 2: Verify directory exists**

Run: `ls wasm/Main.hs`
Expected: File exists.

**Step 3: Commit**

```
git add wasm/Main.hs
git commit -m "Add WASM entry point with JSFFI export"
```

### Task 3: Create `cabal.project.wasm`

**Files:**
- Create: `cabal.project.wasm`

**Step 1: Write the WASM project file**

```
packages: .

with-compiler: wasm32-wasi-ghc
with-hc-pkg: wasm32-wasi-ghc-pkg

package legendary-chainsaw
  ghc-options: -no-hs-main -optl-mexec-model=reactor
```

**Step 2: Commit**

```
git add cabal.project.wasm
git commit -m "Add cabal project file for WASM builds"
```

### Task 4: Create `wasm/www/index.html`

**Files:**
- Create: `wasm/www/index.html`

**Step 1: Write the HTML**

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Legendary Chainsaw</title>
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <div class="pane input-pane">
    <textarea id="source" placeholder="Enter Haskell source..." spellcheck="false"></textarea>
  </div>
  <div class="pane output-pane" id="output">
    <p class="loading">Loading WASM module...</p>
  </div>
  <script src="index.js"></script>
</body>
</html>
```

**Step 2: Commit**

```
git add wasm/www/index.html
git commit -m "Add HTML page for WASM web app"
```

### Task 5: Create `wasm/www/style.css`

**Files:**
- Create: `wasm/www/style.css`

**Step 1: Write the CSS**

```css
* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  display: flex;
  height: 100vh;
  font-family: system-ui, sans-serif;
}

.pane {
  width: 50%;
  height: 100%;
  overflow: auto;
}

.input-pane {
  border-right: 1px solid #ccc;
}

.input-pane textarea {
  width: 100%;
  height: 100%;
  padding: 1rem;
  border: none;
  outline: none;
  resize: none;
  font-family: monospace;
  font-size: 14px;
  tab-size: 2;
}

.output-pane {
  padding: 1rem;
}

.loading {
  color: #888;
  font-style: italic;
}

.error {
  color: #c00;
  white-space: pre-wrap;
  font-family: monospace;
  font-size: 14px;
}
```

**Step 2: Commit**

```
git add wasm/www/style.css
git commit -m "Add CSS for WASM web app"
```

### Task 6: Create `wasm/www/index.js`

**Files:**
- Create: `wasm/www/index.js`

**Step 1: Write the main thread script**

```javascript
"use strict";

var worker = new Worker("worker.js");
var source = document.getElementById("source");
var output = document.getElementById("output");
var debounceTimer;
var ready = false;

worker.onmessage = function (e) {
  if (!ready) {
    ready = true;
    output.innerHTML = "";
    return;
  }
  output.innerHTML = e.data;
};

worker.onerror = function (e) {
  output.innerHTML = "<pre class=\"error\">Worker error: " + e.message + "</pre>";
};

source.addEventListener("input", function () {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(function () {
    if (ready) {
      worker.postMessage(source.value);
    }
  }, 300);
});
```

**Step 2: Commit**

```
git add wasm/www/index.js
git commit -m "Add main thread script for WASM web app"
```

### Task 7: Create `wasm/www/worker.js`

**Files:**
- Create: `wasm/www/worker.js`

**Step 1: Write the worker script**

```javascript
"use strict";

// The GHC WASM post-linker generates ghc_wasm_jsffi.js alongside the .wasm file.
// It exports an initialization function. The exact API depends on the post-linker
// version. This will need adjustment once the WASM build is working.
// See: https://github.com/tweag/ormolu/blob/master/ormolu-live/www/worker.js

importScripts("ghc_wasm_jsffi.js");

var processHaskell;

async function init() {
  try {
    var mod = await WebAssembly.compileStreaming(fetch("legendary-chainsaw-wasm.wasm"));
    var instance = await init(mod);
    processHaskell = instance.exports.processHaskell;
    postMessage("");
  } catch (e) {
    postMessage("<pre class=\"error\">Failed to load WASM module: " + e.message + "</pre>");
  }
}

init();

onmessage = function (e) {
  if (processHaskell) {
    try {
      var result = processHaskell(e.data);
      postMessage(result);
    } catch (err) {
      postMessage("<pre class=\"error\">" + err.message + "</pre>");
    }
  }
};
```

Note: The exact WASM loading API depends on what the GHC post-linker generates. This is a best-guess scaffold that will need adjustment once a real WASM build is produced. The ormolu-live `worker.js` should be consulted at that point.

**Step 2: Commit**

```
git add wasm/www/worker.js
git commit -m "Add web worker script for WASM web app"
```

### Task 8: Create `wasm/build.sh`

**Files:**
- Create: `wasm/build.sh`

**Step 1: Write the build script**

```bash
#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Building WASM executable..."
wasm32-wasi-cabal build \
  --project-file=cabal.project.wasm \
  exe:legendary-chainsaw-wasm

echo "Locating WASM binary..."
wasm=$(find dist-newstyle -name "legendary-chainsaw-wasm.wasm" -type f | head -n 1)
if [ -z "$wasm" ]; then
  echo "Error: could not find legendary-chainsaw-wasm.wasm" >&2
  exit 1
fi
echo "Found: $wasm"

echo "Assembling dist directory..."
mkdir -p wasm/dist

echo "Running GHC JS post-linker..."
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  --input "$wasm" \
  --output wasm/dist/legendary-chainsaw-wasm.wasm

cp wasm/www/* wasm/dist/

echo "Build complete."
echo "Serve with: python3 -m http.server -d wasm/dist"
```

**Step 2: Make it executable**

Run: `chmod +x wasm/build.sh`

**Step 3: Commit**

```
git add wasm/build.sh
git commit -m "Add build script for WASM web app"
```

### Task 9: Update cabal-gild and verify native build

**Step 1: Run cabal-gild to update exposed-modules**

Run: `.vscode/gild.sh --io=legendary-chainsaw.cabal`

(This shouldn't change anything since `wasm/Main.hs` is under `hs-source-dirs: wasm`, not the library's `source/library`.)

**Step 2: Run formatting on any Haskell files we touched**

Run: `find source -name '*.hs' -exec .vscode/ormolu.sh --mode=inplace {} +`

**Step 3: Verify native build still works**

Run: `cabal build`
Expected: Success. The `legendary-chainsaw-wasm` executable will fail to build with native GHC (missing `GHC.Wasm.Prim`), but the library and native executable should build fine.

Note: If `cabal build` tries to build the WASM executable and fails, we may need to gate it behind a cabal flag. Handle this if it occurs.

**Step 4: Run tests**

Run: `cabal test`
Expected: All tests pass.

**Step 5: Commit any formatting/gild changes**

```
git add -A
git commit -m "Run cabal-gild and ormolu"
```

### Task 10: Gate WASM executable behind a cabal flag (if needed)

This task only applies if Task 9 Step 3 fails because native GHC tries to build the WASM executable.

**Files:**
- Modify: `legendary-chainsaw.cabal`

**Step 1: Add a `wasm` flag**

```cabal
flag wasm
  default: False
  manual: True
```

**Step 2: Gate the executable**

```cabal
executable legendary-chainsaw-wasm
  import: library
  build-depends: legendary-chainsaw
  hs-source-dirs: wasm
  main-is: Main.hs
  ghc-options: -no-hs-main

  if !flag(wasm)
    buildable: False
```

Then update `cabal.project.wasm` to enable it:

```
packages: .

with-compiler: wasm32-wasi-ghc
with-hc-pkg: wasm32-wasi-ghc-pkg

flags: +wasm

package legendary-chainsaw
  ghc-options: -no-hs-main -optl-mexec-model=reactor
```

**Step 3: Verify native build succeeds**

Run: `cabal build`
Expected: Builds library + native executable only. WASM executable is skipped.

**Step 4: Commit**

```
git add legendary-chainsaw.cabal cabal.project.wasm
git commit -m "Gate WASM executable behind a cabal flag"
```
