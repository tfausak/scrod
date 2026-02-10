import * as crypto from "crypto";
import * as vscode from "vscode";
import * as path from "path";
import { loadWasmEngine } from "./wasmEngine";

let engine: Promise<(source: string, literate: boolean) => Promise<string>>;
let panel: vscode.WebviewPanel | undefined;
let debounceTimer: ReturnType<typeof setTimeout> | undefined;
let webviewReady = false;
let pendingHtml: string | undefined;

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
          }
        });
        panel.onDidDispose(() => {
          panel = undefined;
          webviewReady = false;
          pendingHtml = undefined;
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
    ext === ".lhs"
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
  panel.title = `Preview: ${path.basename(document.fileName)}`;
  const literate =
    document.languageId === "literate haskell" || extname(document) === ".lhs";
  let html: string;
  try {
    const process = await engine;
    html = await process(document.getText(), literate);
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
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
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

        var scrollTop = document.documentElement.scrollTop;
        document.body.innerHTML = doc.body.innerHTML;
        document.documentElement.scrollTop = scrollTop;
      }

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
