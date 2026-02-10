'use strict';

var worker = new Worker('worker.js', { type: 'module' });
var source = document.getElementById('source');
var output = document.getElementById('output');
var format = document.getElementById('format');
var literate = document.getElementById('literate');
var theme = document.getElementById('theme');
var fileInput = document.getElementById('file-input');
var dropOverlay = document.getElementById('drop-overlay');
var shadow = output.attachShadow({ mode: 'open' });
shadow.innerHTML = '<p style="color: #888; font-style: italic">Loading WASM module...</p>';
var debounceTimer;
var ready = false;

// Dark mode CSS variable overrides for the shadow DOM.
// These use :host() selectors to override the generated CSS variables
// when the user manually picks a theme (instead of "auto").
var darkVars = [
  '--scrod-text: #ddd',
  '--scrod-text-secondary: #aaa',
  '--scrod-text-muted: #888',
  '--scrod-bg: #121212',
  '--scrod-bg-subtle: #2a2a2a',
  '--scrod-metadata-bg: #1e1e1e',
  '--scrod-item-bg: #1a1a1a',
  '--scrod-border: #444',
  '--scrod-accent: #4da6ff',
  '--scrod-code-color: #66cc66',
  '--scrod-module-color: #cc66cc',
  '--scrod-warning-bg: #3d3000',
  '--scrod-warning-border: #ffc107',
  '--scrod-warning-text: #ffd75e',
  '--scrod-examples-bg: #2a2800',
  '--scrod-examples-border: #e6db74',
  '--scrod-property-bg: #1a2233',
  '--scrod-property-border: #6b8ef0',
  '--scrod-extension-bg: #3a3a3a',
  '--scrod-extension-disabled-bg: #3d1a1a',
  'color-scheme: dark'
].join('; ');
var lightVars = [
  '--scrod-text: #333',
  '--scrod-text-secondary: #666',
  '--scrod-text-muted: #999',
  '--scrod-bg: white',
  '--scrod-bg-subtle: #f4f4f4',
  '--scrod-metadata-bg: #f9f9f9',
  '--scrod-item-bg: #fafafa',
  '--scrod-border: #ddd',
  '--scrod-accent: #0066cc',
  '--scrod-code-color: #006600',
  '--scrod-module-color: #660066',
  '--scrod-warning-bg: #fff3cd',
  '--scrod-warning-border: #ffc107',
  '--scrod-warning-text: #856404',
  '--scrod-examples-bg: #fffef0',
  '--scrod-examples-border: #e6db74',
  '--scrod-property-bg: #f0f8ff',
  '--scrod-property-border: #4169e1',
  '--scrod-extension-bg: #e8e8e8',
  '--scrod-extension-disabled-bg: #ffebeb',
  'color-scheme: light'
].join('; ');

function ensureThemeStyle() {
  if (!shadow.querySelector('#theme-override')) {
    var style = document.createElement('style');
    style.id = 'theme-override';
    style.textContent =
      ':host(.theme-dark) body { ' + darkVars + ' } ' +
      ':host(.theme-light) body { ' + lightVars + ' }';
    shadow.appendChild(style);
  }
}

function applyTheme(value) {
  if (value === 'auto') {
    document.documentElement.removeAttribute('data-theme');
  } else {
    document.documentElement.setAttribute('data-theme', value);
  }
  output.classList.remove('theme-dark', 'theme-light');
  if (value !== 'auto') {
    output.classList.add('theme-' + value);
  }
  ensureThemeStyle();
  if (value === 'auto') {
    localStorage.removeItem('scrod-theme');
  } else {
    localStorage.setItem('scrod-theme', value);
  }
}

function showError(message) {
  shadow.textContent = '';
  var pre = document.createElement('pre');
  pre.style.cssText = 'color: #c00; white-space: pre-wrap; font-family: monospace; font-size: 14px';
  pre.textContent = message;
  shadow.appendChild(pre);
  ensureThemeStyle();
}

function showJson(json) {
  shadow.textContent = '';
  var pre = document.createElement('pre');
  pre.style.cssText = 'white-space: pre-wrap; font-family: monospace; font-size: 14px';
  try {
    pre.textContent = JSON.stringify(JSON.parse(json), null, 2);
  } catch (e) {
    pre.textContent = json;
  }
  shadow.appendChild(pre);
  ensureThemeStyle();
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
      ensureThemeStyle();
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
  shadow.innerHTML = '<p style="color: #888; font-style: italic">Fetching URL...</p>';
  ensureThemeStyle();
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
      showError('Failed to fetch URL: ' + err.message);
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

function loadFile(file) {
  var reader = new FileReader();
  reader.onload = function () {
    source.value = reader.result;
    literate.checked = file.name.endsWith('.lhs');
    updateHash();
    process(true);
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

theme.addEventListener('change', function () {
  applyTheme(theme.value);
});

fileInput.addEventListener('change', function () {
  if (fileInput.files.length > 0) {
    loadFile(fileInput.files[0]);
    fileInput.value = '';
  }
});

// Drag and drop support
var dragCounter = 0;

document.addEventListener('dragenter', function (e) {
  e.preventDefault();
  dragCounter++;
  if (dragCounter === 1) {
    dropOverlay.classList.add('active');
  }
});

document.addEventListener('dragleave', function (e) {
  e.preventDefault();
  dragCounter--;
  if (dragCounter === 0) {
    dropOverlay.classList.remove('active');
  }
});

document.addEventListener('dragover', function (e) {
  e.preventDefault();
});

document.addEventListener('drop', function (e) {
  e.preventDefault();
  dragCounter = 0;
  dropOverlay.classList.remove('active');
  if (e.dataTransfer.files.length > 0) {
    loadFile(e.dataTransfer.files[0]);
  }
});

// Load saved theme preference
var savedTheme = localStorage.getItem('scrod-theme');
if (savedTheme === 'light' || savedTheme === 'dark') {
  theme.value = savedTheme;
  applyTheme(savedTheme);
}

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
