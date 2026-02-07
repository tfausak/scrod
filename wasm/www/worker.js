import ghcWasmJsffi from "./ghc_wasm_jsffi.js";
import { WASI, File, OpenFile, ConsoleStdout } from "./vendor/browser_wasi_shim/index.js";

let processHaskell;

async function initialize() {
  const fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered(function (msg) { console.log(msg); }),
    ConsoleStdout.lineBuffered(function (msg) { console.error(msg); }),
  ];
  const wasi = new WASI([], [], fds);

  let exports = null;
  const exportsProxy = new Proxy(
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

  const jsffi = ghcWasmJsffi(exportsProxy);

  const response = await fetch("scrod-wasm.wasm");
  if (!response.ok) {
    throw new Error(
      "Failed to fetch WASM module (status " +
        response.status +
        " " +
        response.statusText +
        ")"
    );
  }
  const wasmBuffer = await response.arrayBuffer();
  const result = await WebAssembly.instantiate(wasmBuffer, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffi,
  });

  exports = result.instance.exports;
  wasi.initialize(result.instance);

  processHaskell = result.instance.exports.processHaskell;
  postMessage({ tag: "ready" });
}

initialize().catch(function (e) {
  postMessage({ tag: "error", message: "Failed to load WASM module: " + e.message });
});

onmessage = async function (e) {
  if (processHaskell) {
    try {
      const msg = e.data;
      const result = await processHaskell(msg.format, msg.source);
      postMessage({ tag: "result", value: result, format: msg.format });
    } catch (err) {
      postMessage({ tag: "error", message: err.message });
    }
  } else {
    postMessage({ tag: "error", message: "WASM module is not initialized yet." });
  }
};
