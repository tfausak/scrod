import * as crypto from "crypto";
import * as vscode from "vscode";
import * as path from "path";
import { loadWasmEngine } from "./wasmEngine";

let engine: Promise<(source: string, literate: boolean, signature: boolean) => Promise<string>>;
let panel: vscode.WebviewPanel | undefined;
let debounceTimer: ReturnType<typeof setTimeout> | undefined;
let webviewReady = false;
let pendingHtml: string | undefined;
let previewDocumentUri: vscode.Uri | undefined;

export function activate(context: vscode.ExtensionContext): void {
  engine = loadWasmEngine(context.extensionPath);

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
              const editor = vscode.window.activeTextEditor;
              if (editor && isHaskell(editor.document)) {
                syncScroll(editor);
              }
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
    ),
    vscode.window.onDidChangeTextEditorVisibleRanges((event) => {
      if (
        panel &&
        event.textEditor === vscode.window.activeTextEditor &&
        isHaskell(event.textEditor.document)
      ) {
        syncScroll(event.textEditor);
      }
    })
  );
}

export function deactivate(): void {}

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
  let html: string;
  try {
    const process = await engine;
    html = await process(document.getText(), literate, isSignature);
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    const escaped = message
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
    html = `<pre style="color: #c00; white-space: pre-wrap; padding: 1rem;">${escaped}</pre>`;
  }
  if (!webviewReady) {
    pendingHtml = html;
    return;
  }
  panel.webview.postMessage({ type: "update", html });
  const editor = vscode.window.activeTextEditor;
  if (editor && isHaskell(editor.document)) {
    syncScroll(editor);
  }
}

function syncScroll(editor: vscode.TextEditor): void {
  if (!panel || !webviewReady) return;
  const topLine = (editor.visibleRanges[0]?.start.line ?? 0) + 1;
  panel.webview.postMessage({ type: "scroll", line: topLine });
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
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline' https://esm.sh; font-src https://esm.sh; script-src 'nonce-${nonce}' https://esm.sh;">
  <style>.item-location { cursor: pointer; } .item-location:hover { text-decoration: underline; }</style>
</head>
<body>
  <script nonce="${nonce}">
    (function() {
      var vscode = acquireVsCodeApi();

      window.addEventListener('message', function(event) {
        var msg = event.data;
        if (msg.type === 'update') {
          applyUpdate(msg.html);
        } else if (msg.type === 'scroll') {
          scrollToLine(msg.line);
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

        var scrollTop = document.documentElement.scrollTop;
        document.body.innerHTML = doc.body.innerHTML;
        document.documentElement.scrollTop = scrollTop;
        syncTheme();
        renderMath();
      }

      function renderMath() {
        import('https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js')
          .then(function (m) {
            m.default(document.body, { delimiters: [
              { left: '\\\\(', right: '\\\\)', display: false },
              { left: '\\\\[', right: '\\\\]', display: true }
            ]});
          })
          .catch(function () {});
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
          if (target.classList && target.classList.contains('item-location')) {
            var line = parseInt(target.getAttribute('data-line'), 10);
            var col = parseInt(target.getAttribute('data-col'), 10);
            if (line && col) {
              event.preventDefault();
              vscode.postMessage({ type: 'goto', line: line, col: col });
            }
            return;
          }
          target = target.parentElement;
        }
      });

      function scrollToLine(line) {
        var elements = document.querySelectorAll('[data-line]');
        var best = null;
        var bestLine = 0;
        for (var i = 0; i < elements.length; i++) {
          var el = elements[i];
          var elLine = parseInt(el.getAttribute('data-line'), 10);
          if (elLine <= line && elLine > bestLine) {
            best = el;
            bestLine = elLine;
          }
        }
        if (best) {
          best.scrollIntoView({ block: 'start' });
        } else {
          window.scrollTo(0, 0);
        }
      }

      vscode.postMessage({ type: 'ready' });
    })();
  </script>
</body>
</html>`;
}
