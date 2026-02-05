"use strict";

var worker = new Worker("worker.js", { type: "module" });
var source = document.getElementById("source");
var output = document.getElementById("output");
var debounceTimer;
var ready = false;

worker.onmessage = function (e) {
  var msg = e.data;
  if (msg.tag === "ready") {
    ready = true;
    output.innerHTML = "";
  } else if (msg.tag === "result") {
    output.innerHTML = msg.html;
  } else if (msg.tag === "error") {
    output.innerHTML = '<pre class="error">' + msg.html + "</pre>";
  }
};

worker.onerror = function (e) {
  output.innerHTML =
    '<pre class="error">Worker error: ' + e.message + "</pre>";
};

source.addEventListener("input", function () {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(function () {
    if (ready) {
      worker.postMessage(source.value);
    }
  }, 300);
});
