"use strict";

const worker = new Worker("worker.js", { type: "module" });
const source = document.getElementById("source");
const output = document.getElementById("output");
const shadow = output.attachShadow({ mode: "open" });
shadow.innerHTML = '<p style="color: #888; font-style: italic">Loading WASM module...</p>';
let debounceTimer;
let ready = false;

function showError(message) {
  shadow.textContent = "";
  const pre = document.createElement("pre");
  pre.style.cssText = "color: #c00; white-space: pre-wrap; font-family: monospace; font-size: 14px";
  pre.textContent = message;
  shadow.appendChild(pre);
}

worker.onmessage = function (e) {
  const msg = e.data;
  if (msg.tag === "ready") {
    ready = true;
    shadow.innerHTML = "";
  } else if (msg.tag === "result") {
    shadow.innerHTML = msg.html;
  } else if (msg.tag === "error") {
    showError(msg.html);
  }
};

worker.onerror = function (e) {
  showError("Worker error: " + e.message);
};

source.addEventListener("input", function () {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(function () {
    if (ready) {
      worker.postMessage(source.value);
    }
  }, 300);
});
