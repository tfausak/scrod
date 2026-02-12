// Renders the header section of a module page.
// TypeScript translation of `headerSection` from
// source/library/Scrod/Convert/ToHtml.hs (lines 168-200).

import type { Module } from "../types.js";
import { escapeHtml, renderDoc } from "./doc.js";

/**
 * Render the module header: title, optional warning, and module documentation.
 */
export function renderHeader(module: Module): string {
  const title =
    module.name != null ? module.name.value : "Documentation";

  const dataLine =
    module.name != null
      ? ` data-line="${module.name.location.line}"`
      : "";

  const warning =
    module.warning != null
      ? `<div class="alert alert-warning"><span class="fw-bold">${escapeHtml(module.warning.category)}</span>: ${escapeHtml(module.warning.value)}</div>`
      : "";

  const doc =
    module.documentation.type !== "Empty"
      ? `<div class="my-3">${renderDoc(module.documentation)}</div>`
      : "";

  return (
    `<header class="mb-4"${dataLine}>` +
    `<h1 class="border-bottom border-2 pb-2 mt-0">${escapeHtml(title)}</h1>` +
    warning +
    doc +
    `</header>`
  );
}
