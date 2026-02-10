const esbuild = require('esbuild');
const isWatch = process.argv.includes('--watch');

const config = {
  entryPoints: ['src/extension.ts'],
  bundle: true,
  outfile: 'out/extension.js',
  external: ['vscode'],
  format: 'cjs',
  platform: 'node',
  sourcemap: true,
};

if (isWatch) {
  esbuild.context(config).then(function (ctx) { ctx.watch(); });
} else {
  esbuild.build(config);
}
