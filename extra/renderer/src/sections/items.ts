// Renders the items (declarations) section of a module page.
// TypeScript translation of `itemsContents` from
// source/library/Scrod/Convert/ToHtml.hs (lines 623-848).

import type { Item, ItemKind, Located } from "../types.js";
import { escapeHtml, renderDoc } from "./doc.js";

// ---------------------------------------------------------------------------
// Kind text mapping (ToHtml.hs lines 761-792)
// ---------------------------------------------------------------------------

function kindToText(kind: ItemKind): string {
  switch (kind.type) {
    case "Function":
      return "function";
    case "PatternBinding":
      return "pattern binding";
    case "PatternSynonym":
      return "pattern";
    case "DataType":
      return "data";
    case "Newtype":
      return "newtype";
    case "TypeData":
      return "type data";
    case "TypeSynonym":
      return "type";
    case "DataConstructor":
      return "constructor";
    case "GADTConstructor":
      return "GADT constructor";
    case "RecordField":
      return "field";
    case "Class":
      return "class";
    case "ClassMethod":
      return "method";
    case "ClassInstance":
      return "instance";
    case "StandaloneDeriving":
      return "standalone deriving";
    case "DerivedInstance":
      return "deriving";
    case "OpenTypeFamily":
      return "type family";
    case "ClosedTypeFamily":
      return "type family";
    case "DataFamily":
      return "data family";
    case "TypeFamilyInstance":
      return "type instance";
    case "DataFamilyInstance":
      return "data instance";
    case "ForeignImport":
      return "foreign import";
    case "ForeignExport":
      return "foreign export";
    case "FixitySignature":
      return "fixity";
    case "InlineSignature":
      return "inline";
    case "SpecialiseSignature":
      return "specialise";
    case "StandaloneKindSig":
      return "kind";
    case "Rule":
      return "rule";
    case "Default":
      return "default";
    case "Annotation":
      return "annotation";
    case "Splice":
      return "splice";
  }
}

// ---------------------------------------------------------------------------
// Kind colors (ToHtml.hs lines 794-848)
// ---------------------------------------------------------------------------

type KindColor = "success" | "info" | "secondary" | "primary" | "warning";

function kindColor(kind: ItemKind): KindColor {
  switch (kind.type) {
    case "Function":
    case "PatternBinding":
    case "PatternSynonym":
      return "success";

    case "DataType":
    case "Newtype":
    case "TypeData":
    case "TypeSynonym":
    case "OpenTypeFamily":
    case "ClosedTypeFamily":
    case "DataFamily":
    case "TypeFamilyInstance":
    case "DataFamilyInstance":
    case "StandaloneKindSig":
      return "info";

    case "DataConstructor":
    case "GADTConstructor":
    case "RecordField":
    case "FixitySignature":
    case "InlineSignature":
    case "SpecialiseSignature":
    case "Rule":
    case "Default":
    case "Annotation":
    case "Splice":
      return "secondary";

    case "Class":
    case "ClassMethod":
    case "ClassInstance":
    case "StandaloneDeriving":
    case "DerivedInstance":
      return "primary";

    case "ForeignImport":
    case "ForeignExport":
      return "warning";
  }
}

function kindBadgeClass(kind: ItemKind): string {
  switch (kindColor(kind)) {
    case "success":
      return "bg-success-subtle text-success-emphasis";
    case "info":
      return "bg-info-subtle text-info-emphasis";
    case "secondary":
      return "bg-secondary-subtle text-body";
    case "primary":
      return "bg-primary-subtle text-primary-emphasis";
    case "warning":
      return "bg-warning-subtle text-warning-emphasis";
  }
}

function kindBorderStyle(kind: ItemKind): string {
  switch (kindColor(kind)) {
    case "success":
      return "border-left-color: var(--bs-success)";
    case "info":
      return "border-left-color: var(--bs-info)";
    case "secondary":
      return "border-left-color: var(--bs-secondary)";
    case "primary":
      return "border-left-color: var(--bs-primary)";
    case "warning":
      return "border-left-color: var(--bs-warning)";
  }
}

// ---------------------------------------------------------------------------
// Signature placement
// ---------------------------------------------------------------------------

function isTypeVarSignature(kind: ItemKind): boolean {
  switch (kind.type) {
    case "DataType":
    case "Newtype":
    case "TypeData":
    case "TypeSynonym":
    case "Class":
      return true;
    default:
      return false;
  }
}

// ---------------------------------------------------------------------------
// Individual item rendering
// ---------------------------------------------------------------------------

function renderItem(located: Located<Item>): string {
  const loc = located.location;
  const item = located.value;

  // Name span
  const nameHtml =
    item.name != null
      ? `<span class="font-monospace fw-bold text-success">${escapeHtml(item.name)}</span>`
      : "";

  // Kind badge
  const badgeCls = `badge ${kindBadgeClass(item.kind)} ms-2`;
  const kindElement = `<span class="${badgeCls}">${escapeHtml(kindToText(item.kind))}</span>`;

  // Signature
  let sigBeforeKind = "";
  let sigAfterKind = "";
  if (item.signature != null) {
    const typeVar = isTypeVarSignature(item.kind);
    const prefix = typeVar ? " " : " :: ";
    const sigHtml = `<span class="font-monospace text-body-secondary">${escapeHtml(prefix + item.signature)}</span>`;
    if (typeVar) {
      sigBeforeKind = sigHtml;
    } else {
      sigAfterKind = sigHtml;
    }
  }

  // Location button
  const lineNum = loc.line;
  const locationButton =
    `<button type="button" class="item-location ms-auto text-body-tertiary small bg-transparent border-0 p-0"` +
    ` aria-label="Go to line ${lineNum}" data-line="${lineNum}" data-col="${loc.column}">` +
    `line ${lineNum}</button>`;

  // Card header
  const cardHeader =
    `<div class="card-header bg-transparent d-flex align-items-center py-2">` +
    nameHtml +
    sigBeforeKind +
    kindElement +
    sigAfterKind +
    locationButton +
    `</div>`;

  // Card body (only if doc is not Empty)
  const cardBody =
    item.documentation.type !== "Empty"
      ? `<div class="card-body">${renderDoc(item.documentation)}</div>`
      : "";

  return (
    `<div class="card mb-3 border-start border-4"` +
    ` style="${kindBorderStyle(item.kind)}"` +
    ` id="item-${item.key}"` +
    ` data-line="${lineNum}">` +
    cardHeader +
    cardBody +
    `</div>`
  );
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/**
 * Render all items (declarations) with parent/child hierarchy.
 * Returns an empty string if items is empty.
 */
export function renderItems(items: Located<Item>[]): string {
  if (items.length === 0) {
    return "";
  }

  // Build children map: parentKey -> list of child items
  const childrenMap = new Map<number, Located<Item>[]>();
  for (const li of items) {
    const pk = li.value.parentKey;
    if (pk != null) {
      const existing = childrenMap.get(pk);
      if (existing != null) {
        existing.push(li);
      } else {
        childrenMap.set(pk, [li]);
      }
    }
  }

  // Top-level items have no parentKey
  const topLevelItems = items.filter((li) => li.value.parentKey == null);

  function renderItemWithChildren(li: Located<Item>): string {
    const k = li.value.key;
    const children = childrenMap.get(k) ?? [];
    let result = renderItem(li);

    if (children.length > 0) {
      const childrenHtml = children
        .map(renderItemWithChildren)
        .join("");
      result += `<div class="ms-4 mt-2 border-start border-2 ps-3">${childrenHtml}</div>`;
    }

    return result;
  }

  const itemsHtml = topLevelItems.map(renderItemWithChildren).join("");

  return (
    `<section class="my-4">` +
    `<h2 class="border-bottom pb-1 mt-4">Declarations</h2>` +
    itemsHtml +
    `</section>`
  );
}
