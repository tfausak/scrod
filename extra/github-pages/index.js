'use strict';

var worker = new Worker('worker.js', { type: 'module' });
var source = document.getElementById('source');
var output = document.getElementById('output');
var format = document.getElementById('format');
var literate = document.getElementById('literate');
var signature = document.getElementById('signature');
var theme = document.getElementById('theme');
var fileButton = document.getElementById('file-button');
var fileInput = document.getElementById('file-input');
var dropOverlay = document.getElementById('drop-overlay');
var shadow = output.attachShadow({ mode: 'open' });
var debounceTimer;
var ready = false;

function bootstrapLink() {
  var link = document.createElement('link');
  link.rel = 'stylesheet';
  link.href = 'vendor/bootstrap.min.css';
  return link;
}

function effectiveTheme() {
  if (theme.value !== 'auto') return theme.value;
  return matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

function setShadowMessage(element) {
  shadow.textContent = '';
  shadow.appendChild(bootstrapLink());
  var wrapper = document.createElement('div');
  wrapper.setAttribute('data-bs-theme', effectiveTheme());
  wrapper.appendChild(element);
  shadow.appendChild(wrapper);
}

function syncShadowTheme() {
  var t = effectiveTheme();
  var els = shadow.querySelectorAll('html, [data-bs-theme]');
  for (var i = 0; i < els.length; i++) {
    els[i].setAttribute('data-bs-theme', t);
  }
}

(function () {
  var p = document.createElement('p');
  p.className = 'text-body-secondary fst-italic';
  p.textContent = 'Loading WASM module...';
  setShadowMessage(p);
})();

function applyTheme(value) {
  document.documentElement.setAttribute('data-bs-theme', effectiveTheme());
  syncShadowTheme();
  if (value === 'auto') {
    localStorage.removeItem('scrod-theme');
  } else {
    localStorage.setItem('scrod-theme', value);
  }
}

matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function () {
  if (theme.value === 'auto') {
    document.documentElement.setAttribute('data-bs-theme', effectiveTheme());
    syncShadowTheme();
  }
});

function showError(message) {
  var pre = document.createElement('pre');
  pre.className = 'text-danger font-monospace';
  pre.textContent = message;
  setShadowMessage(pre);
}

function showJson(json) {
  var pre = document.createElement('pre');
  pre.className = 'font-monospace';
  try {
    pre.textContent = JSON.stringify(JSON.parse(json), null, 2);
  } catch (e) {
    pre.textContent = json;
  }
  setShadowMessage(pre);
}

worker.onmessage = function (e) {
  var msg = e.data;
  if (msg.tag === 'ready') {
    ready = true;
    shadow.innerHTML = '';
    process();
  } else if (msg.tag === 'result') {
    if (msg.format === 'json') {
      showJson(msg.value);
    } else {
      shadow.innerHTML = msg.value;
      syncShadowTheme();
    }
  } else if (msg.tag === 'error') {
    showError(msg.message);
  }
};

worker.onerror = function (e) {
  showError('Worker error: ' + e.message);
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
    if (signature.checked) {
      params.set('signature', 'true');
    }
    params.set('input', encodeHash(source.value));
    history.replaceState(null, '', '#' + params.toString());
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

var fetchController = null;

function fetchUrl(url) {
  if (fetchController) {
    fetchController.abort();
  }
  fetchController = new AbortController();
  var p = document.createElement('p');
  p.className = 'text-body-secondary fst-italic';
  p.textContent = 'Fetching URL...';
  setShadowMessage(p);
  fetch(url, { signal: fetchController.signal }).then(function (response) {
    if (!response.ok) {
      throw new Error('HTTP ' + response.status + ' ' + response.statusText);
    }
    return response.text();
  }).then(function (text) {
    source.value = text;
    updateHash();
    process(true);
  }).catch(function (err) {
    if (err.name !== 'AbortError') {
      if (err instanceof TypeError) {
        showError('Failed to fetch URL (possible CORS issue). Try a proxy: https://corsproxy.io/?url=' + encodeURIComponent(url));
      } else {
        showError('Failed to fetch URL: ' + err.message);
      }
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
    worker.postMessage({ source: source.value, format: format.value, literate: literate.checked, signature: signature.checked });
  }
}

function loadFile(file) {
  var reader = new FileReader();
  reader.onload = function () {
    source.value = reader.result;
    literate.checked = file.name.endsWith('.lhs') || file.name.endsWith('.lhsig');
    signature.checked = file.name.endsWith('.hsig') || file.name.endsWith('.lhsig');
    updateHash();
    process(true);
  };
  reader.onerror = function () {
    showError('Failed to read file: ' + (reader.error ? reader.error.message : file.name));
  };
  reader.readAsText(file);
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

signature.addEventListener('change', function () {
  updateHash();
  process();
});

theme.addEventListener('change', function () {
  applyTheme(theme.value);
});

fileButton.addEventListener('click', function () {
  fileInput.click();
});

fileInput.addEventListener('change', function () {
  if (fileInput.files.length > 0) {
    loadFile(fileInput.files[0]);
    fileInput.value = '';
  }
});

// Drag and drop support
var dragCounter = 0;

function hasFiles(e) {
  return e.dataTransfer && e.dataTransfer.types && e.dataTransfer.types.indexOf('Files') !== -1;
}

function hideOverlay() {
  dragCounter = 0;
  dropOverlay.classList.add('d-none');
  dropOverlay.classList.remove('d-flex');
}

document.addEventListener('dragenter', function (e) {
  if (!hasFiles(e)) return;
  e.preventDefault();
  dragCounter++;
  if (dragCounter === 1) {
    dropOverlay.classList.remove('d-none');
    dropOverlay.classList.add('d-flex');
  }
});

document.addEventListener('dragleave', function (e) {
  if (!hasFiles(e)) return;
  e.preventDefault();
  dragCounter = Math.max(0, dragCounter - 1);
  if (dragCounter === 0) {
    dropOverlay.classList.add('d-none');
    dropOverlay.classList.remove('d-flex');
  }
});

document.addEventListener('dragover', function (e) {
  if (!hasFiles(e)) return;
  e.preventDefault();
});

document.addEventListener('drop', function (e) {
  e.preventDefault();
  hideOverlay();
  if (e.dataTransfer.files.length > 0) {
    loadFile(e.dataTransfer.files[0]);
  }
});

document.addEventListener('dragend', function () {
  hideOverlay();
});

// Load saved theme preference
var savedTheme = localStorage.getItem('scrod-theme');
if (savedTheme === 'light' || savedTheme === 'dark') {
  theme.value = savedTheme;
}
applyTheme(theme.value);

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
      if (params.get('signature') === 'true') {
        signature.checked = true;
      }
    } else {
      // Legacy format: bare base64-encoded source
      source.value = decodeHash(hash);
    }
  } catch (e) {
    // ignore invalid hash
  }
}
