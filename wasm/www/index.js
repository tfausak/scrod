"use strict";

var worker = new Worker("worker.js");
var source = document.getElementById("source");
var output = document.getElementById("output");
var debounceTimer;
var ready = false;

worker.onmessage = function (e) {
  if (!ready) {
    ready = true;
    output.innerHTML = "";
    return;
  }
  output.innerHTML = e.data;
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
