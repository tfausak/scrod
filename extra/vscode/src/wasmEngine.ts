import * as fs from "fs";
import * as path from "path";
import { pathToFileURL } from "url";

type Process = (source: string, literate: boolean, signature: boolean) => Promise<string>;

export async function loadWasmEngine(extensionPath: string): Promise<Process> {
  const wasmDir = path.join(extensionPath, "wasm");

  const [ghcWasmJsffiModule, wasiModule] = await Promise.all([
    import(pathToFileURL(path.join(wasmDir, "ghc_wasm_jsffi.mjs")).href),
    import(pathToFileURL(path.join(wasmDir, "browser_wasi_shim.mjs")).href),
  ]);

  const { WASI, File, OpenFile, ConsoleStdout } = wasiModule;
  const wasi = new WASI(
    [],
    [],
    [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered(() => {}),
      ConsoleStdout.lineBuffered(() => {}),
    ]
  );

  let exports: WebAssembly.Exports | null = null;
  const jsffi = ghcWasmJsffiModule.default(
    new Proxy(
      {},
      {
        get: (_: unknown, property: string | symbol) => {
          if (!exports) throw new Error("WASM exports not initialized");
          return exports[property as string];
        },
      }
    )
  );

  const wasmBuffer = await fs.promises.readFile(
    path.join(wasmDir, "scrod-wasm.wasm")
  );
  const result = await WebAssembly.instantiate(wasmBuffer, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffi,
  });

  exports = result.instance.exports;
  wasi.initialize(result.instance);

  const scrod = result.instance.exports["scrod"] as (
    args: string[],
    source: string
  ) => Promise<string>;

  return async (source, literate, signature) => {
    const args = ["--format", "html"];
    if (literate) args.push("--literate");
    if (signature) args.push("--signature");
    return scrod(args, source);
  };
}
