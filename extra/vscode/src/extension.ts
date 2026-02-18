import * as crypto from "crypto";
import * as vscode from "vscode";
import * as path from "path";
import { loadWasmEngine } from "./wasmEngine";

let engine: Promise<(source: string, literate: boolean, signature: boolean, cabalContent: string | null) => Promise<string>>;
let panel: vscode.WebviewPanel | undefined;
let debounceTimer: ReturnType<typeof setTimeout> | undefined;
let webviewReady = false;
let pendingHtml: string | undefined;
let previewDocumentUri: vscode.Uri | undefined;
let cabalFileCache: vscode.Uri[] | null = null;
let cabalContentCache: Map<string, string> = new Map();
const log = vscode.window.createOutputChannel("Scrod", { log: true });

export function activate(context: vscode.ExtensionContext): void {
  engine = loadWasmEngine(context.extensionPath);

  const cabalWatcher = vscode.workspace.createFileSystemWatcher("**/*.cabal");
  const invalidateAndRefresh = () => {
    cabalFileCache = null;
    cabalContentCache.clear();
    if (panel && previewDocumentUri) {
      vscode.workspace.openTextDocument(previewDocumentUri).then((doc) => {
        immediateUpdate(doc);
      });
    }
  };
  cabalWatcher.onDidChange(invalidateAndRefresh);
  cabalWatcher.onDidCreate(invalidateAndRefresh);
  cabalWatcher.onDidDelete(invalidateAndRefresh);
  context.subscriptions.push(cabalWatcher, log);

  context.subscriptions.push(
    vscode.commands.registerCommand("scrod.openPreview", () => {
      if (panel) {
        panel.reveal(vscode.ViewColumn.Beside, true);
      } else {
        panel = vscode.window.createWebviewPanel(
          "scrodPreview",
          "Scrod Preview",
          { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
          { enableScripts: true, retainContextWhenHidden: true }
        );
        webviewReady = false;
        panel.webview.html = wrapperHtml();
        panel.webview.onDidReceiveMessage((msg) => {
          if (msg.type === "ready") {
            webviewReady = true;
            if (pendingHtml !== undefined) {
              panel?.webview.postMessage({ type: "update", html: pendingHtml });
              pendingHtml = undefined;
            }
          } else if (msg.type === "goto") {
            gotoLocation(msg.line, msg.col);
          }
        });
        panel.onDidDispose(() => {
          panel = undefined;
          webviewReady = false;
          pendingHtml = undefined;
          previewDocumentUri = undefined;
          clearTimeout(debounceTimer);
        });
      }
      immediateUpdate(vscode.window.activeTextEditor?.document);
    }),
    vscode.window.onDidChangeActiveTextEditor((editor) =>
      immediateUpdate(editor?.document)
    ),
    vscode.workspace.onDidChangeTextDocument((event) =>
      scheduleUpdate(event.document)
    ),
    vscode.workspace.onDidSaveTextDocument((document) =>
      immediateUpdate(document)
    )
  );
}

export function deactivate(): void { }

function extname(doc: vscode.TextDocument): string {
  return path.extname(doc.fileName).toLowerCase();
}

function isHaskell(doc: vscode.TextDocument): boolean {
  const ext = extname(doc);
  return (
    doc.languageId === "haskell" ||
    doc.languageId === "literate haskell" ||
    ext === ".hs" ||
    ext === ".lhs" ||
    ext === ".hsig" ||
    ext === ".lhsig"
  );
}

async function findNearestCabalContent(documentUri: vscode.Uri): Promise<string | null> {
  if (cabalFileCache === null) {
    const t0 = performance.now();
    cabalFileCache = await vscode.workspace.findFiles("**/*.cabal", "{**/dist-newstyle/**,**/.stack-work/**}");
    log.info(`findFiles **/*.cabal: ${(performance.now() - t0).toFixed(1)} ms, found ${cabalFileCache.length}`);
  }
  if (cabalFileCache.length === 0) return null;
  const docDir = path.dirname(documentUri.fsPath);
  const closest = cabalFileCache.reduce((a, b) => {
    const aDist = directoryDistance(docDir, path.dirname(a.fsPath));
    const bDist = directoryDistance(docDir, path.dirname(b.fsPath));
    return aDist <= bDist ? a : b;
  });
  const key = closest.toString();
  const cached = cabalContentCache.get(key);
  if (cached !== undefined) return cached;
  try {
    const bytes = await vscode.workspace.fs.readFile(closest);
    const content = Buffer.from(bytes).toString("utf-8");
    cabalContentCache.set(key, content);
    return content;
  } catch {
    return null;
  }
}

function directoryDistance(from: string, to: string): number {
  const fromParts = from.split(path.sep);
  const toParts = to.split(path.sep);
  let common = 0;
  while (common < fromParts.length && common < toParts.length && fromParts[common] === toParts[common]) {
    common++;
  }
  return (fromParts.length - common) + (toParts.length - common);
}

function immediateUpdate(document: vscode.TextDocument | undefined): void {
  if (!panel) return;
  clearTimeout(debounceTimer);
  if (!document || !isHaskell(document)) return;
  update(document);
}

function scheduleUpdate(document: vscode.TextDocument): void {
  if (!panel) return;
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    if (!isHaskell(document)) return;
    update(document);
  }, 300);
}

async function update(document: vscode.TextDocument): Promise<void> {
  if (!panel) return;
  previewDocumentUri = document.uri;
  panel.title = `Preview: ${path.basename(document.fileName)}`;
  const ext = extname(document);
  const literate =
    document.languageId === "literate haskell" || ext === ".lhs" || ext === ".lhsig";
  const isSignature = ext === ".hsig" || ext === ".lhsig";
  const t0 = performance.now();
  const cabalContent = await findNearestCabalContent(document.uri);
  const t1 = performance.now();
  let html: string;
  try {
    const process = await engine;
    html = await process(document.getText(), literate, isSignature, cabalContent);
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    const escaped = message
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
    html = `<pre style="color: var(--vscode-errorForeground, #c00); white-space: pre-wrap; padding: 1rem;">${escaped}</pre>`;
  }
  const t2 = performance.now();
  log.info(`update ${path.basename(document.fileName)}: cabal ${(t1 - t0).toFixed(1)} ms, render ${(t2 - t1).toFixed(1)} ms`);
  if (!webviewReady) {
    pendingHtml = html;
    return;
  }
  panel.webview.postMessage({ type: "update", html });
}

async function gotoLocation(line: number, col: number): Promise<void> {
  if (!previewDocumentUri) return;
  const existing = vscode.window.visibleTextEditors.find(
    (e) => e.document.uri.toString() === previewDocumentUri?.toString()
  );
  const document = existing
    ? existing.document
    : await vscode.workspace.openTextDocument(previewDocumentUri);
  const editor = await vscode.window.showTextDocument(
    document,
    existing?.viewColumn
  );
  const position = new vscode.Position(line - 1, col - 1);
  editor.selection = new vscode.Selection(position, position);
  editor.revealRange(
    new vscode.Range(position, position),
    vscode.TextEditorRevealType.InCenterIfOutsideViewport
  );
}

function getNonce(): string {
  return crypto.randomBytes(16).toString("hex");
}

function wrapperHtml(): string {
  const nonce = getNonce();
  return `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src https:; style-src 'unsafe-inline' https://esm.sh; font-src https://esm.sh; script-src 'nonce-${nonce}' https://esm.sh;">
  <style id="vscode-theme">
    body {
      background-color: var(--vscode-editor-background) !important;
      color: var(--vscode-foreground) !important;
    }
    a:not(.btn) {
      color: var(--vscode-textLink-foreground) !important;
    }
    a:not(.btn):hover {
      color: var(--vscode-textLink-activeForeground) !important;
    }
    .link-underline {
      color: var(--vscode-textLink-foreground) !important;
    }
    .text-secondary, .text-body-secondary {
      color: var(--vscode-descriptionForeground) !important;
    }
    .card {
      background-color: var(--vscode-editorWidget-background) !important;
      border-color: var(--vscode-widget-border) !important;
    }
    .card-header {
      border-color: var(--vscode-widget-border) !important;
    }
    .badge.text-bg-secondary {
      background-color: var(--vscode-badge-background) !important;
      color: var(--vscode-badge-foreground) !important;
    }
    .badge.text-bg-info {
      background-color: var(--vscode-badge-background) !important;
      color: var(--vscode-badge-foreground) !important;
    }
    .badge.text-bg-warning {
      background-color: var(--vscode-editorWarning-foreground) !important;
      color: var(--vscode-editor-background) !important;
    }
    .btn-outline-secondary {
      color: var(--vscode-foreground) !important;
      border-color: var(--vscode-widget-border) !important;
    }
    .btn-outline-secondary:hover {
      background-color: var(--vscode-button-secondaryBackground) !important;
      color: var(--vscode-button-secondaryForeground) !important;
    }
    .alert-warning {
      background-color: var(--vscode-inputValidation-warningBackground) !important;
      border-color: var(--vscode-inputValidation-warningBorder) !important;
      color: var(--vscode-foreground) !important;
    }
    .alert-info {
      background-color: var(--vscode-inputValidation-infoBackground) !important;
      border-color: var(--vscode-inputValidation-infoBorder) !important;
      color: var(--vscode-foreground) !important;
    }
    .text-warning {
      color: var(--vscode-editorWarning-foreground) !important;
    }
    .table {
      --bs-table-color: var(--vscode-foreground);
      --bs-table-bg: transparent;
      --bs-table-border-color: var(--vscode-widget-border);
    }
    pre, code {
      color: inherit !important;
    }
    h1, h2, h3, h4, h5, h6 {
      color: var(--vscode-foreground) !important;
    }
  </style>
</head>
<body>
  <script nonce="${nonce}">
    (function() {
      var vscode = acquireVsCodeApi();

      window.addEventListener('message', function(event) {
        var msg = event.data;
        if (msg.type === 'update') {
          applyUpdate(msg.html);
        }
      });

      function applyUpdate(html) {
        var parser = new DOMParser();
        var doc = parser.parseFromString(html, 'text/html');

        var newStyle = doc.querySelector('style');
        var style = document.getElementById('scrod-style');
        if (newStyle) {
          if (!style) {
            style = document.createElement('style');
            style.id = 'scrod-style';
            document.head.appendChild(style);
          }
          style.textContent = newStyle.textContent;
        }

        var newLinks = doc.querySelectorAll('link[rel="stylesheet"]');
        for (var i = 0; i < newLinks.length; i++) {
          var href = newLinks[i].getAttribute('href');
          if (href && !document.querySelector('link[href="' + href + '"]')) {
            document.head.appendChild(newLinks[i].cloneNode());
          }
        }

        // Re-append the VS Code theme <style> to the end of <head> so its
        // rules cascade after any dynamically-loaded stylesheets (Bootstrap).
        var themeStyle = document.getElementById('vscode-theme');
        if (themeStyle) {
          document.head.appendChild(themeStyle);
        }

        var scrollTop = document.documentElement.scrollTop;
        document.body.innerHTML = doc.body.innerHTML;
        document.documentElement.scrollTop = scrollTop;
        syncTheme();
        renderMath();
      }

      var katexPromise = import('https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js')
        .catch(function (e) { console.error('Failed to load KaTeX:', e); });

      function renderMath() {
        katexPromise.then(function (m) {
          if (m) m.default(document.body, { delimiters: [
            { left: '\\\\(', right: '\\\\)', display: false },
            { left: '\\\\[', right: '\\\\]', display: true }
          ]});
        });
      }

      function syncTheme() {
        var isDark = document.body.classList.contains('vscode-dark')
          || document.body.classList.contains('vscode-high-contrast');
        document.documentElement.setAttribute('data-bs-theme', isDark ? 'dark' : 'light');
      }

      syncTheme();

      new MutationObserver(function() { syncTheme(); })
        .observe(document.body, { attributes: true, attributeFilter: ['class'] });

      document.addEventListener('click', function(event) {
        var target = event.target;
        while (target && target !== document.body) {
          var line = parseInt(target.getAttribute('data-line'), 10);
          var col = parseInt(target.getAttribute('data-col'), 10);
          if (line && col) {
            event.preventDefault();
            vscode.postMessage({ type: 'goto', line: line, col: col });
            return;
          }
          target = target.parentElement;
        }
      });

      vscode.postMessage({ type: 'ready' });
    })();
  </script>
</body>
</html>`;
}
