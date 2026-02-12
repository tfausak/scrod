// Main entry point for the Scrod TypeScript HTML renderer.
// Assembles the complete HTML document from section renderers,
// matching the output of source/library/Scrod/Convert/ToHtml.hs.

import { renderHeader } from "./sections/header.js";
import { renderMetadata } from "./sections/metadata.js";
import { renderExports } from "./sections/exports.js";
import { renderImports } from "./sections/imports.js";
import { renderExtensions } from "./sections/extensions.js";
import { renderItems } from "./sections/items.js";
import { renderFooter } from "./sections/footer.js";
import type { Module } from "./types.js";

/**
 * Render a complete HTML document for a Scrod Module.
 *
 * The output matches the Haskell `toHtml` function in ToHtml.hs:
 * - `<!doctype html>` prolog followed by a newline
 * - `<html>` root element containing `<head>` and `<body>`
 */
export function renderModule(module: Module): string {
  const title =
    module.name != null ? module.name.value : "Documentation";

  const head =
    `<head>` +
    `<meta charset="utf-8" />` +
    `<meta name="viewport" content="width=device-width, initial-scale=1" />` +
    `<title>${escapeHtml(title)}</title>` +
    `<link rel="stylesheet" href="https://esm.sh/bootstrap@5.3.8/dist/css/bootstrap.min.css" integrity="sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB" crossorigin="anonymous" />` +
    `<link rel="stylesheet" href="https://esm.sh/katex@0.16.22/dist/katex.min.css" integrity="sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP" crossorigin="anonymous" />` +
    `<link rel="modulepreload" href="https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js" integrity="sha384-PV5j9Y/tL/HYr0HSxUY3afWRVHizeuTKLWTR+OwVlGHOBcN8jOZvCAS79+ULHoEU" crossorigin="anonymous" />` +
    `<script>` +
    `const dark = matchMedia('(prefers-color-scheme: dark)');\n` +
    `const setTheme = (e) =>\n` +
    `  document.documentElement.dataset.bsTheme = e.matches ? 'dark' : 'light';\n` +
    `setTheme(dark);\n` +
    `dark.addEventListener('change', setTheme);\n` +
    `import('https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js')\n` +
    `  .then((m) => m.default(document.body, { delimiters: [\n` +
    `    { left: '\\\\(', right: '\\\\)', display: false },\n` +
    `    { left: '\\\\[', right: '\\\\]', display: true }\n` +
    `  ]}));` +
    `</script>` +
    `</head>`;

  const body =
    `<body>` +
    `<div class="container py-4 text-break">` +
    renderHeader(module) +
    renderMetadata(module) +
    renderExports(module.exports) +
    renderImports(module.imports) +
    renderExtensions(module.extensions) +
    renderItems(module.items) +
    renderFooter(module) +
    `</div>` +
    `</body>`;

  return `<!doctype html>\n<html>${head}${body}</html>`;
}

/**
 * Escape special HTML characters in text content.
 * Uses the same entities as the Haskell XML encoder.
 */
function escapeHtml(text: string): string {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

export type { Module } from "./types.js";
