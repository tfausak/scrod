// Renders a Doc value to an HTML string.
// This is a TypeScript translation of `docToContents` from
// source/library/Scrod/Convert/ToHtml.hs (lines 852-1031).

import type { Doc } from "../types.js";

/**
 * Escape special HTML characters to prevent injection.
 */
export function escapeHtml(text: string): string {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

/**
 * Render a Doc value to an HTML string, matching the Haskell `docToContents`
 * output exactly.
 */
export function renderDoc(doc: Doc): string {
  switch (doc.type) {
    case "Empty":
      return "";

    case "Append":
      return doc.value.map(renderDoc).join("");

    case "String":
      return escapeHtml(doc.value);

    case "Paragraph":
      return `<p>${renderDoc(doc.value)}</p>`;

    case "Identifier": {
      const id = doc.value;
      let prefix = "";
      if (id.namespace != null) {
        prefix = id.namespace.type === "Value" ? "v'" : "t'";
      }
      return `<code class="font-monospace text-success">${escapeHtml(prefix + id.value)}</code>`;
    }

    case "Module": {
      const mod = doc.value;
      const inner =
        mod.label != null ? renderDoc(mod.label) : escapeHtml(mod.name);
      return `<code class="font-monospace text-info">${inner}</code>`;
    }

    case "Emphasis":
      return `<em>${renderDoc(doc.value)}</em>`;

    case "Monospaced":
      return `<code>${renderDoc(doc.value)}</code>`;

    case "Bold":
      return `<strong>${renderDoc(doc.value)}</strong>`;

    case "UnorderedList": {
      const items = doc.value
        .map((item) => `<li>${renderDoc(item)}</li>`)
        .join("");
      return `<ul>${items}</ul>`;
    }

    case "OrderedList": {
      const items = doc.value
        .map((e) => `<li value="${e.index}">${renderDoc(e.item)}</li>`)
        .join("");
      return `<ol>${items}</ol>`;
    }

    case "DefList": {
      const entries = doc.value
        .map(
          (e) =>
            `<dt>${renderDoc(e.term)}</dt><dd>${renderDoc(e.definition)}</dd>`
        )
        .join("");
      return `<dl>${entries}</dl>`;
    }

    case "CodeBlock":
      return `<pre class="bg-body-secondary rounded p-3 my-3"><code>${renderDoc(doc.value)}</code></pre>`;

    case "Hyperlink": {
      const h = doc.value;
      const inner =
        h.label != null ? renderDoc(h.label) : escapeHtml(h.url);
      return `<a href="${escapeHtml(h.url)}">${inner}</a>`;
    }

    case "Pic": {
      const p = doc.value;
      const alt = p.title != null ? escapeHtml(p.title) : "";
      const titleAttr =
        p.title != null ? ` title="${escapeHtml(p.title)}"` : "";
      return `<img src="${escapeHtml(p.uri)}" alt="${alt}"${titleAttr}>`;
    }

    case "MathInline":
      return `\\(${escapeHtml(doc.value)}\\)`;

    case "MathDisplay":
      return `\\[${escapeHtml(doc.value)}\\]`;

    case "AName":
      return `<a id="${escapeHtml(doc.value)}"></a>`;

    case "Property":
      return `<pre class="border-start border-4 border-primary bg-primary-subtle rounded-end p-3 my-3 font-monospace">${escapeHtml(doc.value)}</pre>`;

    case "Examples": {
      const examples = doc.value
        .map((ex) => {
          const resultLines = ex.result
            .map(
              (r) =>
                `<div class="font-monospace text-body-secondary ps-3">${escapeHtml(r)}</div>`
            )
            .join("");
          return (
            `<div class="my-1">` +
            `<div class="font-monospace">` +
            `<span class="text-warning-emphasis user-select-none">&gt;&gt;&gt; </span>${escapeHtml(ex.expression)}` +
            `</div>` +
            resultLines +
            `</div>`
          );
        })
        .join("");
      return `<div class="border-start border-4 border-warning bg-warning-subtle rounded-end p-3 my-3">${examples}</div>`;
    }

    case "Header": {
      const h = doc.value;
      const level = Math.max(1, Math.min(6, h.level));
      return `<h${level}>${renderDoc(h.title)}</h${level}>`;
    }

    case "Table": {
      const t = doc.value;
      let thead = "";
      if (t.headerRows.length > 0) {
        const rows = t.headerRows
          .map((row) => {
            const cells = row
              .map(
                (cell) =>
                  `<th colspan="${cell.colspan}" rowspan="${cell.rowspan}">${renderDoc(cell.contents)}</th>`
              )
              .join("");
            return `<tr>${cells}</tr>`;
          })
          .join("");
        thead = `<thead>${rows}</thead>`;
      }
      let tbody = "";
      if (t.bodyRows.length > 0) {
        const rows = t.bodyRows
          .map((row) => {
            const cells = row
              .map(
                (cell) =>
                  `<td colspan="${cell.colspan}" rowspan="${cell.rowspan}">${renderDoc(cell.contents)}</td>`
              )
              .join("");
            return `<tr>${cells}</tr>`;
          })
          .join("");
        tbody = `<tbody>${rows}</tbody>`;
      }
      return `<table class="table table-bordered table-sm table-striped my-3">${thead}${tbody}</table>`;
    }
  }
}
