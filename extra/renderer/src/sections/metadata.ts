// Renders the metadata section of a module page.
// TypeScript translation of `metadataContents` from
// source/library/Scrod/Convert/ToHtml.hs (lines 204-247).

import type { Module } from "../types.js";
import { escapeHtml } from "./doc.js";

/**
 * Render the metadata card with language and since information.
 * Returns an empty string if neither language nor since is present.
 */
export function renderMetadata(module: Module): string {
  let items = "";

  if (module.language != null) {
    items += `<dt>Language</dt><dd>${escapeHtml(module.language)}</dd>`;
  }

  if (module.since != null) {
    const packagePrefix =
      module.since.package != null
        ? escapeHtml(module.since.package) + "-"
        : "";
    const version = module.since.version.join(".");
    items += `<dt>Since</dt><dd class="text-body-secondary small">${packagePrefix}${escapeHtml(version)}</dd>`;
  }

  if (items === "") {
    return "";
  }

  return (
    `<section class="card border-start border-primary border-4 mb-3">` +
    `<dl class="card-body mb-0">${items}</dl>` +
    `</section>`
  );
}
