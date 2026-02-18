import ghcWasmJsffi from '../wasm/dist/ghc_wasm_jsffi.js';
import { WASI, File, OpenFile, ConsoleStdout } from './vendor/browser_wasi_shim.js';

let scrod;

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
          throw new Error('WASM exports not initialized');
        }
        return exports[property];
      },
    }
  );

  const jsffi = ghcWasmJsffi(exportsProxy);

  const response = await fetch(WASM_URL);
  if (!response.ok) {
    throw new Error(
      `Failed to fetch WASM module (status ${response.status} ${response.statusText})`
    );
  }
  const wasmBuffer = await response.arrayBuffer();
  const result = await WebAssembly.instantiate(wasmBuffer, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffi,
  });

  exports = result.instance.exports;
  wasi.initialize(result.instance);

  scrod = result.instance.exports.scrod;
  postMessage({ tag: 'ready' });
}

initialize().catch(function (e) {
  postMessage({ tag: 'error', message: 'Failed to load WASM module: ' + e.message });
});

onmessage = async function (e) {
  if (scrod) {
    try {
      const msg = e.data;
      const args = ['--format', msg.format];
      if (msg.literate) args.push('--literate');
      if (msg.signature) args.push('--signature');
      const start = performance.now();
      const result = await scrod(args, msg.source);
      const elapsed = performance.now() - start;
      console.log('scrod: finished in ' + Math.round(elapsed) + ' ms');
      postMessage({ tag: 'result', value: result, format: msg.format });
    } catch (err) {
      postMessage({ tag: 'error', message: err.message });
    }
  } else {
    postMessage({ tag: 'error', message: 'WASM module is not initialized yet.' });
  }
};
