"use strict";

const worker = new Worker("worker.js", { type: "module" });
const source = document.getElementById("source");
const output = document.getElementById("output");
let debounceTimer;
let ready = false;

function showError(message) {
  output.textContent = "";
  const pre = document.createElement("pre");
  pre.className = "error";
  pre.textContent = message;
  output.appendChild(pre);
}

worker.onmessage = function (e) {
  const msg = e.data;
  if (msg.tag === "ready") {
    ready = true;
    output.innerHTML = "";
  } else if (msg.tag === "result") {
    output.innerHTML = msg.html;
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
