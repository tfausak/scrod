"use strict";

// The GHC WASM post-linker generates ghc_wasm_jsffi.js alongside the .wasm
// file. It exports an initialization function. The exact API depends on the
// post-linker version. This will need adjustment once the WASM build is
// working.
// See: https://github.com/tweag/ormolu/blob/master/ormolu-live/www/worker.js

importScripts("ghc_wasm_jsffi.js");

var processHaskell;

async function initialize() {
  try {
    var mod = await WebAssembly.compileStreaming(
      fetch("legendary-chainsaw-wasm.wasm")
    );
    var instance = await initLegendaryChainsaw(mod);
    processHaskell = instance.exports.processHaskell;
    postMessage("");
  } catch (e) {
    postMessage(
      '<pre class="error">Failed to load WASM module: ' + e.message + "</pre>"
    );
  }
}

initialize();

onmessage = function (e) {
  if (processHaskell) {
    try {
      var result = processHaskell(e.data);
      postMessage(result);
    } catch (err) {
      postMessage('<pre class="error">' + err.message + "</pre>");
    }
  }
};
