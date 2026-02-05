import ghcWasmJsffi from "./ghc_wasm_jsffi.js";
import { WASI, File, OpenFile, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.4.2/dist/index.js";

var processHaskell;

async function initialize() {
  var fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered(function (msg) { console.log(msg); }),
    ConsoleStdout.lineBuffered(function (msg) { console.error(msg); }),
  ];
  var wasi = new WASI([], [], fds);

  var exports = null;
  var exportsProxy = new Proxy(
    {},
    {
      get: function (_, property) {
        if (!exports) {
          throw new Error("WASM exports not initialized");
        }
        return exports[property];
      },
    }
  );

  var jsffi = ghcWasmJsffi(exportsProxy);

  var wasmBuffer = await (
    await fetch("legendary-chainsaw-wasm.wasm")
  ).arrayBuffer();
  var result = await WebAssembly.instantiate(wasmBuffer, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffi,
  });

  exports = result.instance.exports;
  wasi.initialize(result.instance);

  processHaskell = result.instance.exports.processHaskell;
  postMessage({ tag: "ready" });
}

initialize().catch(function (e) {
  postMessage({ tag: "error", html: "Failed to load WASM module: " + e.message });
});

onmessage = async function (e) {
  if (processHaskell) {
    try {
      var result = await processHaskell(e.data);
      postMessage({ tag: "result", html: result });
    } catch (err) {
      postMessage({ tag: "error", html: err.message });
    }
  }
};
