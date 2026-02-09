'use strict';

const worker = new Worker('worker.js', { type: 'module' });
const source = document.getElementById('source');
const output = document.getElementById('output');
const format = document.getElementById('format');
const literate = document.getElementById('literate');
const shadow = output.attachShadow({ mode: 'open' });
shadow.innerHTML = '<p style="color: #888; font-style: italic">Loading WASM module...</p>';
let debounceTimer;
let ready = false;

function showError(message) {
  shadow.textContent = '';
  const pre = document.createElement('pre');
  pre.style.cssText = 'color: #c00; white-space: pre-wrap; font-family: monospace; font-size: 14px';
  pre.textContent = message;
  shadow.appendChild(pre);
}

function showJson(json) {
  shadow.textContent = '';
  const pre = document.createElement('pre');
  pre.style.cssText = 'white-space: pre-wrap; font-family: monospace; font-size: 14px';
  try {
    pre.textContent = JSON.stringify(JSON.parse(json), null, 2);
  } catch (e) {
    pre.textContent = json;
  }
  shadow.appendChild(pre);
}

worker.onmessage = function (e) {
  const msg = e.data;
  if (msg.tag === 'ready') {
    ready = true;
    shadow.innerHTML = '';
    if (source.value) {
      process();
    }
  } else if (msg.tag === 'result') {
    if (msg.format === 'json') {
      showJson(msg.value);
    } else {
      shadow.innerHTML = msg.value;
    }
  } else if (msg.tag === 'error') {
    showError(msg.message);
  }
};

worker.onerror = function (e) {
  showError(`Worker error: ${e.message}`);
};

function encodeHash(text) {
  return btoa(new TextEncoder().encode(text).reduce(function (s, b) {
    return s + String.fromCharCode(b);
  }, ''));
}

function decodeHash(hash) {
  return new TextDecoder().decode(
    Uint8Array.from(atob(hash), function (c) { return c.charCodeAt(0); })
  );
}

function updateHash() {
  if (source.value) {
    var params = new URLSearchParams();
    if (format.value !== 'html') {
      params.set('format', format.value);
    }
    if (literate.checked) {
      params.set('literate', 'true');
    }
    params.set('input', encodeHash(source.value));
    history.replaceState(null, '', `#${params.toString()}`);
  } else {
    history.replaceState(null, '', location.pathname);
  }
}

function isUrl(text) {
  try {
    var url = new URL(text);
    return url.protocol === 'http:' || url.protocol === 'https:';
  } catch (e) {
    return false;
  }
}

let fetchController = null;

function fetchUrl(url) {
  if (fetchController) {
    fetchController.abort();
  }
  fetchController = new AbortController();
  shadow.innerHTML = '<p style="color: #888; font-style: italic">Fetching URL...</p>';
  fetch(url, { signal: fetchController.signal }).then(function (response) {
    if (!response.ok) {
      throw new Error(`HTTP ${response.status} ${response.statusText}`);
    }
    return response.text();
  }).then(function (text) {
    source.value = text;
    updateHash();
    process(true);
  }).catch(function (err) {
    if (err.name !== 'AbortError') {
      showError(`Failed to fetch URL: ${err.message}`);
    }
  });
}

function process(skipUrlDetection) {
  if (ready) {
    var trimmed = source.value.trim();
    if (!skipUrlDetection && isUrl(trimmed)) {
      fetchUrl(trimmed);
      return;
    }
    worker.postMessage({ source: source.value, format: format.value, literate: literate.checked });
  }
}

source.addEventListener('input', function () {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(function () {
    updateHash();
    process();
  }, 300);
});

format.addEventListener('change', function () {
  updateHash();
  process();
});

literate.addEventListener('change', function () {
  updateHash();
  process();
});

// Load content from URL hash on startup
if (location.hash.length > 1) {
  try {
    var hash = location.hash.slice(1);
    var params = new URLSearchParams(hash);
    if (params.has('input')) {
      source.value = decodeHash(params.get('input'));
      if (params.has('format')) {
        var hashFormat = params.get('format');
        if (hashFormat === 'html' || hashFormat === 'json') {
          format.value = hashFormat;
        }
      }
      if (params.get('literate') === 'true') {
        literate.checked = true;
      }
    } else {
      // Legacy format: bare base64-encoded source
      source.value = decodeHash(hash);
    }
  } catch (e) {
    // ignore invalid hash
  }
}
