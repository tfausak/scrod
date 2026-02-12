// Renders the imports section of a module page.
// TypeScript translation of `importsContents` from
// source/library/Scrod/Convert/ToHtml.hs (lines 382-425).

import type { Import } from "../types.js";
import { escapeHtml } from "./doc.js";

/**
 * Render the imports section as a collapsible details element.
 * Returns an empty string if imports is empty.
 */
export function renderImports(imports: Import[]): string {
  if (imports.length === 0) {
    return "";
  }

  const uniqueNames = new Set(imports.map((i) => i.name));
  const uniqueCount = uniqueNames.size;
  const noun = uniqueCount === 1 ? "module" : "modules";
  const summary = `Imports (${uniqueCount} ${noun})`;

  const items = imports.map(renderImport).join("");

  return (
    `<details class="my-4">` +
    `<summary class="fs-4 fw-bold">${escapeHtml(summary)}</summary>` +
    `<ul class="list-group list-group-flush font-monospace small">${items}</ul>` +
    `</details>`
  );
}

function renderImport(imp: Import): string {
  const pkg =
    imp.package != null
      ? escapeHtml(`"${imp.package}" `)
      : "";
  const name = escapeHtml(imp.name);
  const alias =
    imp.alias != null
      ? escapeHtml(` as ${imp.alias}`)
      : "";

  return `<li class="list-group-item bg-transparent py-1 px-2">${pkg}${name}${alias}</li>`;
}
