import * as fs from "fs";
import * as path from "path";
import { pathToFileURL } from "url";

export class WasmEngine {
  private scrod:
    | ((args: string[], source: string) => Promise<string>)
    | null = null;
  private initPromise: Promise<void>;

  constructor(extensionPath: string) {
    this.initPromise = this.initialize(extensionPath);
  }

  private async initialize(extensionPath: string): Promise<void> {
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

    const wasmBuffer = fs.readFileSync(path.join(wasmDir, "scrod-wasm.wasm"));
    const result = await WebAssembly.instantiate(wasmBuffer, {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: jsffi,
    });

    exports = result.instance.exports;
    wasi.initialize(result.instance);

    this.scrod = result.instance.exports.scrod as (
      args: string[],
      source: string
    ) => Promise<string>;
  }

  async process(source: string, literate: boolean): Promise<string> {
    await this.initPromise;
    if (!this.scrod) throw new Error("WASM module not initialized");
    const args = ["--format", "html"];
    if (literate) args.push("--literate");
    return this.scrod(args, source);
  }
}
