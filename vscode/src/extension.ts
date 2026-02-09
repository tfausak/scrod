import * as vscode from "vscode";
import * as path from "path";
import { WasmEngine } from "./wasmEngine";

const panels = new Map<string, vscode.WebviewPanel>();
const timers = new Map<string, ReturnType<typeof setTimeout>>();
let engine: WasmEngine;

export function activate(context: vscode.ExtensionContext): void {
  engine = new WasmEngine(context.extensionPath);

  context.subscriptions.push(
    vscode.commands.registerCommand("scrod.openPreview", () => {
      const editor = vscode.window.activeTextEditor;
      if (editor) openPreview(editor.document);
    }),
    vscode.workspace.onDidChangeTextDocument((e) =>
      scheduleUpdate(e.document)
    ),
    vscode.workspace.onDidSaveTextDocument((doc) => scheduleUpdate(doc))
  );
}

export function deactivate(): void {}

function openPreview(document: vscode.TextDocument): void {
  const key = document.uri.toString();
  const existing = panels.get(key);
  if (existing) {
    existing.reveal(vscode.ViewColumn.Beside, true);
    return;
  }

  const panel = vscode.window.createWebviewPanel(
    "scrodPreview",
    `Preview: ${path.basename(document.fileName)}`,
    vscode.ViewColumn.Beside,
    { retainContextWhenHidden: true }
  );
  panels.set(key, panel);
  panel.onDidDispose(() => {
    panels.delete(key);
    clearTimeout(timers.get(key));
    timers.delete(key);
  });

  update(panel, document);
}

function scheduleUpdate(document: vscode.TextDocument): void {
  const key = document.uri.toString();
  const panel = panels.get(key);
  if (!panel) return;

  clearTimeout(timers.get(key));
  timers.set(key, setTimeout(() => update(panel, document), 300));
}

async function update(
  panel: vscode.WebviewPanel,
  document: vscode.TextDocument
): Promise<void> {
  const literate = document.fileName.endsWith(".lhs");
  try {
    panel.webview.html = await engine.process(document.getText(), literate);
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    const escaped = message
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
    panel.webview.html = `<!DOCTYPE html>
<html><body>
<pre style="color: #c00; white-space: pre-wrap; padding: 1rem;">${escaped}</pre>
</body></html>`;
  }
}
