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
    if (source.value) {
      worker.postMessage(source.value);
    }
  } else if (msg.tag === "result") {
    shadow.innerHTML = msg.html;
  } else if (msg.tag === "error") {
    showError(msg.html);
  }
};

worker.onerror = function (e) {
  showError("Worker error: " + e.message);
};

function encodeHash(text) {
  return btoa(new TextEncoder().encode(text).reduce(function (s, b) {
    return s + String.fromCharCode(b);
  }, ""));
}

function decodeHash(hash) {
  return new TextDecoder().decode(
    Uint8Array.from(atob(hash), function (c) { return c.charCodeAt(0); })
  );
}

function updateHash() {
  if (source.value) {
    history.replaceState(null, "", "#" + encodeHash(source.value));
  } else {
    history.replaceState(null, "", location.pathname);
  }
}

source.addEventListener("input", function () {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(function () {
    updateHash();
    if (ready) {
      worker.postMessage(source.value);
    }
  }, 300);
});

// Load content from URL hash on startup
if (location.hash.length > 1) {
  try {
    source.value = decodeHash(location.hash.slice(1));
  } catch (e) {
    // ignore invalid hash
  }
}
