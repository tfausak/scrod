// Renders the exports section of a module page.
// TypeScript translation of `exportsContents` from
// source/library/Scrod/Convert/ToHtml.hs (lines 267-377).

import type { Export, ExportIdentifier, ExportName, Section, Subordinates } from "../types.js";
import { escapeHtml, renderDoc } from "./doc.js";

/**
 * Render the exports section.
 * Returns an empty string if exports is undefined or empty.
 */
export function renderExports(exports: Export[] | undefined): string {
  if (exports == null || exports.length === 0) {
    return "";
  }

  const items = exports.map(renderExport).join("");

  return (
    `<section class="my-4">` +
    `<h2 class="border-bottom pb-1 mt-4">Exports</h2>` +
    `<ul class="list-group list-group-flush">${items}</ul>` +
    `</section>`
  );
}

function renderExport(exp: Export): string {
  const liClass = "list-group-item bg-transparent py-1 px-2";

  switch (exp.type) {
    case "Identifier":
      return `<li class="${liClass}">${renderExportIdentifier(exp.value)}</li>`;
    case "Group":
      return `<li class="${liClass}">${renderSection(exp.value)}</li>`;
    case "Doc":
      return `<li class="${liClass}"><div class="mt-1">${renderDoc(exp.value)}</div></li>`;
    case "DocNamed":
      return `<li class="${liClass}"><div class="mt-1">${escapeHtml("\u00a7" + exp.value)}</div></li>`;
  }
}

function renderExportIdentifier(ident: ExportIdentifier): string {
  const warning =
    ident.warning != null
      ? `<div class="alert alert-warning"><span class="fw-bold">${escapeHtml(ident.warning.category)}</span>: ${escapeHtml(ident.warning.value)}</div>`
      : "";

  const nameText = exportNameToText(ident.name);
  const subs = renderSubordinates(ident.subordinates);

  const doc =
    ident.doc != null
      ? `<div class="mt-1">${renderDoc(ident.doc)}</div>`
      : "";

  return (
    `<div class="py-1">` +
    warning +
    `<code class="font-monospace">${escapeHtml(nameText)}${subs}</code>` +
    doc +
    `</div>`
  );
}

function exportNameToText(name: ExportName): string {
  let prefix = "";
  if (name.kind != null) {
    switch (name.kind.type) {
      case "Pattern":
        prefix = "pattern ";
        break;
      case "Type":
        prefix = "type ";
        break;
      case "Module":
        prefix = "module ";
        break;
    }
  }
  return prefix + name.name;
}

function renderSubordinates(subs: Subordinates | undefined): string {
  if (subs == null) {
    return "";
  }

  const wildcardText = "..";
  const explicitTexts = subs.explicit.map((en) => en.name);
  const allTexts = subs.wildcard
    ? [wildcardText, ...explicitTexts]
    : explicitTexts;
  const combined = allTexts.join(", ");

  return escapeHtml("(" + combined + ")");
}

function renderSection(section: Section): string {
  const tag = sectionLevelToName(section.level);
  return `<div class="my-3"><${tag} class="fw-bold">${renderDoc(section.title)}</${tag}></div>`;
}

function sectionLevelToName(level: number): string {
  switch (level) {
    case 1:
      return "h3";
    case 2:
      return "h4";
    case 3:
      return "h5";
    default:
      return "h6";
  }
}
