/**
 * Mirror Shiny's built-in fileInput upload progress (#file_progress)
 * into the EasyEyes shinyalert modal (#file-progress-*).
 *
 * Also bridges the gap after the browser POST finishes but before R
 * starts read_files, which previously left the modal on "Starting...".
 */
(function () {
  var state = {
    phase: "idle", // idle | uploading | waiting_server | reading
    totalBytes: 0,
    fileCount: 0,
    observer: null,
    rafId: null,
    lastPct: -1,
    pending: null
  };

  function formatBytes(n) {
    if (!isFinite(n) || n < 0) return "";
    if (n < 1024) return Math.round(n) + " B";
    if (n < 1024 * 1024) return (n / 1024).toFixed(1) + " KB";
    if (n < 1024 * 1024 * 1024) {
      return (n / (1024 * 1024)).toFixed(1) + " MB";
    }
    return (n / (1024 * 1024 * 1024)).toFixed(2) + " GB";
  }

  function setModalTitle(title) {
    if (!title) return;
    var el =
      document.querySelector(".sweet-alert h2") ||
      document.querySelector(".swal-title") ||
      document.querySelector(".swal2-title");
    if (el) el.textContent = title;
  }

  function setModalProgress(pct, detail, title) {
    var bar = document.getElementById("file-progress-bar");
    var pctEl = document.getElementById("file-progress-pct");
    var detailEl = document.getElementById("file-progress-detail");

    if (!bar && !detailEl) {
      state.pending = { pct: pct, detail: detail, title: title };
      return false;
    }
    state.pending = null;

    if (typeof pct === "number" && isFinite(pct)) {
      pct = Math.max(0, Math.min(100, pct));
      if (bar) bar.style.width = pct + "%";
      if (pctEl) pctEl.innerText = Math.round(pct) + "%";
    }
    if (detailEl && typeof detail === "string") {
      detailEl.innerText = detail;
    }
    if (title) setModalTitle(title);
    return true;
  }

  function flushPending() {
    if (!state.pending) return;
    var p = state.pending;
    setModalProgress(p.pct, p.detail, p.title);
  }

  function shinyProgressBar() {
    return document.querySelector(
      "#file_progress.shiny-file-input-progress .progress-bar"
    );
  }

  function shinyProgressActive() {
    var box = document.querySelector(
      "#file_progress.shiny-file-input-progress"
    );
    return !!(box && box.classList.contains("active"));
  }

  function readShinyUploadPct() {
    var bar = shinyProgressBar();
    if (!bar) return null;
    var width = bar.style.width || "";
    var pct = parseFloat(width);
    if (!isFinite(pct)) {
      var parent = bar.parentElement;
      if (parent) {
        var parentW = parent.getBoundingClientRect().width;
        var barW = bar.getBoundingClientRect().width;
        if (parentW > 0) pct = (barW / parentW) * 100;
      }
    }
    if (!isFinite(pct)) return null;
    return {
      pct: pct,
      fileName: (bar.textContent || "").trim()
    };
  }

  function buildUploadDetail(pct, fileName) {
    var parts = [];
    if (state.fileCount > 0) {
      parts.push(
        state.fileCount === 1
          ? "Uploading 1 file"
          : "Uploading " + state.fileCount + " files"
      );
    } else {
      parts.push("Uploading to server");
    }
    if (state.totalBytes > 0 && isFinite(pct)) {
      var loaded = (pct / 100) * state.totalBytes;
      parts.push(formatBytes(loaded) + " / " + formatBytes(state.totalBytes));
    }
    if (fileName) parts.push(fileName);
    return parts.join(" — ");
  }

  function markWaitingForServer() {
    if (state.phase === "reading") return;
    state.phase = "waiting_server";
    stopWatchingUpload();
    setModalProgress(
      100,
      "Upload complete. Waiting for server to read files...",
      "Upload complete"
    );
  }

  function syncFromShinyBar() {
    if (state.phase === "reading" || state.phase === "waiting_server") {
      return;
    }
    var info = readShinyUploadPct();
    if (!info) return;
    if (info.pct <= 0 && state.totalBytes <= 0 && !shinyProgressActive()) {
      return;
    }

    state.phase = "uploading";

    // Shiny's FileUploader calls onProgress(file, 0) at the start of EVERY
    // file, which sets the native bar to 0% even when earlier files already
    // finished. Ignore that regression so the modal doesn't jump around.
    if (
      state.lastPct > 1 &&
      info.pct < state.lastPct - 1 &&
      info.pct < 5
    ) {
      // Keep prior %; refresh detail with the new filename if present.
      setModalProgress(
        state.lastPct,
        buildUploadDetail(state.lastPct, info.fileName),
        "Uploading file(s)..."
      );
      return;
    }

    // Keep progress monotonic across files.
    var pct = info.pct;
    if (pct < state.lastPct) {
      pct = state.lastPct;
    }

    // Do NOT treat mid-batch peaks as "upload complete". Shiny may briefly
    // report ~100% for overall bytes while still active, or we may be
    // between files. Wait until the progress widget deactivates, or until
    // shiny:inputchanged (handled elsewhere).
    if (pct >= 99.5 && !shinyProgressActive()) {
      state.lastPct = 100;
      markWaitingForServer();
      return;
    }

    if (Math.abs(pct - state.lastPct) < 0.05 && pct < 99.5) return;
    state.lastPct = pct;
    setModalProgress(
      pct,
      buildUploadDetail(pct, info.fileName),
      "Uploading file(s)..."
    );
  }

  function startWatchingUpload() {
    stopWatchingUpload();
    var bar = shinyProgressBar();
    if (bar && typeof MutationObserver !== "undefined") {
      state.observer = new MutationObserver(function () {
        syncFromShinyBar();
      });
      state.observer.observe(bar, {
        attributes: true,
        attributeFilter: ["style", "class"],
        childList: true,
        characterData: true,
        subtree: true
      });
      var box = document.querySelector(
        "#file_progress.shiny-file-input-progress"
      );
      if (box) {
        state.observer.observe(box, {
          attributes: true,
          attributeFilter: ["class", "style"]
        });
      }
    }
    (function tick() {
      syncFromShinyBar();
      if (state.phase === "uploading" || state.phase === "idle") {
        state.rafId = window.setTimeout(tick, 100);
      }
    })();
  }

  function stopWatchingUpload() {
    if (state.observer) {
      state.observer.disconnect();
      state.observer = null;
    }
    if (state.rafId) {
      window.clearTimeout(state.rafId);
      state.rafId = null;
    }
  }

  function onFilesChosen(fileList) {
    var files = fileList || [];
    var total = 0;
    for (var i = 0; i < files.length; i++) {
      total += files[i].size || 0;
    }
    state.phase = "uploading";
    state.totalBytes = total;
    state.fileCount = files.length;
    state.lastPct = -1;
    setModalProgress(
      0,
      buildUploadDetail(0, files.length ? files[0].name : ""),
      "Uploading file(s)..."
    );
    startWatchingUpload();
  }

  window.EasyEyesFileProgress = {
    setReading: function (pct, detail) {
      state.phase = "reading";
      stopWatchingUpload();
      setModalProgress(
        pct,
        detail || "Reading file(s)...",
        "Reading file(s)..."
      );
    },
    setUploading: function (pct, detail) {
      state.phase = "uploading";
      setModalProgress(pct, detail, "Uploading file(s)...");
    },
    markWaitingForServer: markWaitingForServer,
    reset: function () {
      state.phase = "idle";
      state.totalBytes = 0;
      state.fileCount = 0;
      state.lastPct = -1;
      state.pending = null;
      stopWatchingUpload();
    },
    getPhase: function () {
      return state.phase;
    }
  };

  function bindFileInput() {
    var input = document.getElementById("file");
    if (!input) return false;
    if (input.dataset.easyeyesUploadBound === "1") return true;
    input.dataset.easyeyesUploadBound = "1";

    // Capture phase so we can compress BEFORE Shiny's uploader runs.
    input.addEventListener(
      "change",
      function (event) {
        var el = this;
        if (!el.files || el.files.length === 0) return;

        // Second change after we assigned a compressed File — let Shiny run.
        if (el.dataset.easyeyesPrecompressed === "1") {
          onFilesChosen(el.files);
          return;
        }

        event.stopImmediatePropagation();
        event.preventDefault();

        var files = el.files;
        var runUploadAsIs = function () {
          // Re-dispatch so Shiny uploads the original selection.
          el.dataset.easyeyesPrecompressed = "1";
          el.dispatchEvent(new Event("change", { bubbles: true }));
          window.setTimeout(function () {
            delete el.dataset.easyeyesPrecompressed;
          }, 0);
        };

        if (
          !window.EasyEyesCompressUpload ||
          !EasyEyesCompressUpload.maybeCompressBeforeUpload
        ) {
          onFilesChosen(files);
          runUploadAsIs();
          return;
        }

        EasyEyesCompressUpload.maybeCompressBeforeUpload(el, files).then(
          function (handled) {
            if (handled) {
              // Compress path already re-dispatched change with new files.
              return;
            }
            onFilesChosen(files);
            runUploadAsIs();
          }
        );
      },
      true
    );
    return true;
  }

  function ready(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn);
    } else {
      fn();
    }
  }

  ready(function () {
    if (!bindFileInput()) {
      var tries = 0;
      var timer = window.setInterval(function () {
        tries += 1;
        if (bindFileInput() || tries > 50) {
          window.clearInterval(timer);
        }
      }, 100);
    }

    if (typeof MutationObserver !== "undefined") {
      var modalSeen = false;
      var bodyObserver = new MutationObserver(function () {
        if (!document.getElementById("file-progress-bar")) {
          modalSeen = false;
          return;
        }
        flushPending();
        if (modalSeen) return;
        modalSeen = true;
        var input = document.getElementById("file");
        if (
          input &&
          input.files &&
          input.files.length > 0 &&
          (state.phase === "idle" || state.phase === "uploading")
        ) {
          if (state.phase === "idle") {
            onFilesChosen(input.files);
          } else {
            startWatchingUpload();
            syncFromShinyBar();
            flushPending();
          }
        } else if (state.phase !== "reading") {
          startWatchingUpload();
          syncFromShinyBar();
          flushPending();
        }
      });
      bodyObserver.observe(document.body, {
        childList: true,
        subtree: true
      });
    }

    // When Shiny registers the uploaded file, leave "Starting..." immediately
    // even if the heavy files() reactive is slow to send progress.
    if (window.jQuery) {
      $(document).on("shiny:inputchanged", function (event) {
        if (event.name !== "file") return;
        if (!event.value) return;
        if (state.phase === "reading") return;
        markWaitingForServer();
        window.EasyEyesFileProgress.setReading(
          0,
          "Upload complete. Reading file(s)..."
        );
      });
    }
  });
})();
