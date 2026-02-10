import * as vscode from "vscode";
import * as path from "path";
import { loadWasmEngine } from "./wasmEngine";

let engine: Promise<(source: string, literate: boolean) => Promise<string>>;
let panel: vscode.WebviewPanel | undefined;
let debounceTimer: ReturnType<typeof setTimeout> | undefined;

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
          { retainContextWhenHidden: true }
        );
        panel.onDidDispose(() => {
          panel = undefined;
          clearTimeout(debounceTimer);
        });
      }
      immediateUpdate();
    }),
    vscode.window.onDidChangeActiveTextEditor(() => immediateUpdate()),
    vscode.workspace.onDidChangeTextDocument(() => scheduleUpdate()),
    vscode.workspace.onDidSaveTextDocument(() => immediateUpdate())
  );
}

export function deactivate(): void {}

function isHaskell(doc: vscode.TextDocument): boolean {
  return doc.languageId === "haskell" || doc.languageId === "literate haskell";
}

function immediateUpdate(): void {
  if (!panel) return;
  clearTimeout(debounceTimer);
  const editor = vscode.window.activeTextEditor;
  if (!editor || !isHaskell(editor.document)) return;
  update(editor.document);
}

function scheduleUpdate(): void {
  if (!panel) return;
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    const editor = vscode.window.activeTextEditor;
    if (!editor || !isHaskell(editor.document)) return;
    update(editor.document);
  }, 300);
}

async function update(document: vscode.TextDocument): Promise<void> {
  if (!panel) return;
  panel.title = `Preview: ${path.basename(document.fileName)}`;
  const literate = document.fileName.endsWith(".lhs");
  try {
    const process = await engine;
    panel.webview.html = await process(document.getText(), literate);
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
