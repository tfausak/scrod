import { build } from 'esbuild';
import { copyFile, readFile, writeFile, mkdir, rm } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { basename, dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = dirname(fileURLToPath(import.meta.url));
const dist = join(root, 'dist');

// Find the entry-point output path from an esbuild metafile.
function entryOutput(metafile) {
  const entry = Object.entries(metafile.outputs).find(
    ([, meta]) => meta.entryPoint
  );
  return basename(entry[0]);
}

await rm(dist, { recursive: true, force: true });
await mkdir(dist, { recursive: true });

// Phase 1: Hash and copy the WASM binary (esbuild cannot trace fetch() targets)

const wasmSource = join(root, '..', 'wasm', 'dist', 'scrod-wasm.wasm');
const wasmBytes = await readFile(wasmSource);
const wasmHash = createHash('sha256').update(wasmBytes).digest('hex').slice(0, 16);
const wasmFilename = `scrod-wasm.${wasmHash}.wasm`;
await copyFile(wasmSource, join(dist, wasmFilename));

// Phase 2: Bundle worker.js (inlines ghc_wasm_jsffi.js and browser_wasi_shim.js)

const workerResult = await build({
  entryPoints: [join(root, 'worker.js')],
  bundle: true,
  format: 'esm',
  outdir: dist,
  entryNames: '[name]-[hash]',
  metafile: true,
  define: {
    WASM_URL: JSON.stringify(wasmFilename),
  },
});

const workerFilename = entryOutput(workerResult.metafile);

// Phase 3: Bundle index.js (inlines renderer.js)

const indexResult = await build({
  entryPoints: [join(root, 'index.js')],
  bundle: true,
  format: 'esm',
  outdir: dist,
  entryNames: '[name]-[hash]',
  metafile: true,
  define: {
    WORKER_URL: JSON.stringify(workerFilename),
  },
});

const indexFilename = entryOutput(indexResult.metafile);

// Phase 4: Process style.css

const styleResult = await build({
  entryPoints: [join(root, 'style.css')],
  bundle: true,
  outdir: dist,
  entryNames: '[name]-[hash]',
  metafile: true,
});

const styleFilename = entryOutput(styleResult.metafile);

// Phase 5: Generate index.html with hashed references

const html = await readFile(join(root, 'index.html'), 'utf8');
const outputHtml = html
  .replace('style.css', styleFilename)
  .replace('index.js', indexFilename);
await writeFile(join(dist, 'index.html'), outputHtml);
