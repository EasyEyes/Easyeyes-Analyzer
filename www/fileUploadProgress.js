/**
 * Mirror Shiny's built-in fileInput upload progress (#file_progress)
 * into the EasyEyes shinyalert modal (#file-progress-*).
 *
 * Phases: idle → uploading → waiting_server → reading → processing → done
 *
 * After Reading reaches 100%, the modal switches to an animated
 * "Processing…" state (summary table / thresholds) so the UI does not look
 * frozen at "Reading … 100%". On close we enter "done" (not idle) so a
 * late shiny:inputchanged for `file` cannot reopen Reading at 0%.
 */
(function () {
  var state = {
    phase: "idle", // idle | uploading | waiting_server | reading | processing | done
    totalBytes: 0,
    fileCount: 0,
    observer: null,
    rafId: null,
    lastPct: -1,
    pending: null,
    processingDotsTimer: null,
    processingWatchdog: null,
    closeTimer: null
  };

  function clearCloseTimer() {
    if (state.closeTimer) {
      window.clearTimeout(state.closeTimer);
      state.closeTimer = null;
    }
  }

  function clearProcessingWatchdog() {
    if (state.processingWatchdog) {
      window.clearTimeout(state.processingWatchdog);
      state.processingWatchdog = null;
    }
  }

  function isPastReading() {
    return state.phase === "processing" || state.phase === "done";
  }

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

  function setBarIndeterminate(on) {
    var bar = document.getElementById("file-progress-bar");
    var wrap = bar && bar.parentElement;
    if (bar) {
      if (on) {
        bar.classList.add("is-indeterminate");
        bar.style.width = "40%";
      } else {
        bar.classList.remove("is-indeterminate");
      }
    }
    if (wrap) {
      if (on) wrap.classList.add("is-indeterminate");
      else wrap.classList.remove("is-indeterminate");
    }
  }

  function stopProcessingDots() {
    if (state.processingDotsTimer) {
      window.clearInterval(state.processingDotsTimer);
      state.processingDotsTimer = null;
    }
  }

  function startProcessingDots() {
    stopProcessingDots();
    var frames = ["Processing", "Processing.", "Processing..", "Processing..."];
    var i = 0;
    var tick = function () {
      var detailEl = document.getElementById("file-progress-detail");
      if (detailEl) detailEl.innerText = frames[i % frames.length];
      setModalTitle("Processing...");
      i += 1;
    };
    tick();
    state.processingDotsTimer = window.setInterval(tick, 450);
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
      if (bar && !bar.classList.contains("is-indeterminate")) {
        bar.style.width = pct + "%";
      }
      if (pctEl) {
        if (state.phase === "processing") {
          pctEl.innerText = "";
        } else {
          pctEl.innerText = Math.round(pct) + "%";
        }
      }
    }
    if (detailEl && typeof detail === "string" && state.phase !== "processing") {
      detailEl.innerText = detail;
    }
    if (title && state.phase !== "processing") setModalTitle(title);
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

  function enterProcessing() {
    if (state.phase === "done") return;
    clearProcessingWatchdog();
    if (state.phase === "processing") {
      setBarIndeterminate(true);
      if (!state.processingDotsTimer) startProcessingDots();
      return;
    }
    state.phase = "processing";
    stopWatchingUpload();
    setBarIndeterminate(true);
    var pctEl = document.getElementById("file-progress-pct");
    if (pctEl) pctEl.innerText = "";
    setModalTitle("Processing...");
    startProcessingDots();
  }

  function markWaitingForServer() {
    if (
      state.phase === "reading" ||
      state.phase === "processing" ||
      state.phase === "done" ||
      state.phase === "waiting_server"
    ) {
      return;
    }
    state.phase = "waiting_server";
    stopWatchingUpload();
    setBarIndeterminate(false);
    setModalProgress(100, "Waiting...", "Reading...");
  }

  function dismissUploadProgressAlert() {
    // shinyalert owns the modal via SwalService — plain swal.close() is a no-op
    // for those instances and left the UI stuck on Processing after the table
    // rendered. Mirror shinyalert.closeAlert / closeAlert() from R.
    if (!document.getElementById("file-progress-bar")) return;
    try {
      if (
        typeof shinyalert !== "undefined" &&
        shinyalert.instances &&
        shinyalert.instances.length &&
        typeof swalService !== "undefined" &&
        typeof swalService.closeAndFireCallback === "function"
      ) {
        while (
          shinyalert.instances.length > 0 &&
          document.getElementById("file-progress-bar")
        ) {
          var item = shinyalert.instances.shift();
          swalService.closeAndFireCallback(item.swal_id, false);
        }
        return;
      }
    } catch (e) {}
    try {
      if (typeof swal !== "undefined" && typeof swal.close === "function") {
        swal.close();
      }
    } catch (e2) {}
  }

  function finishAndClose() {
    clearCloseTimer();
    clearProcessingWatchdog();
    stopWatchingUpload();
    stopProcessingDots();
    setBarIndeterminate(false);
    state.phase = "done";
    state.pending = null;
    dismissUploadProgressAlert();
  }

  function syncFromShinyBar() {
    if (
      state.phase === "reading" ||
      state.phase === "waiting_server" ||
      state.phase === "processing" ||
      state.phase === "done"
    ) {
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
      setModalProgress(
        state.lastPct,
        buildUploadDetail(state.lastPct, info.fileName),
        "Uploading..."
      );
      return;
    }

    var pct = info.pct;
    if (pct < state.lastPct) {
      pct = state.lastPct;
    }

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
      "Uploading..."
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
    clearCloseTimer();
    clearProcessingWatchdog();
    state.phase = "uploading";
    state.totalBytes = total;
    state.fileCount = files.length;
    state.lastPct = -1;
    state.pending = null;
    stopProcessingDots();
    setBarIndeterminate(false);
    setModalProgress(
      0,
      buildUploadDetail(0, files.length ? files[0].name : ""),
      "Uploading..."
    );
    startWatchingUpload();
  }

  window.EasyEyesFileProgress = {
    setReading: function (pct, detail) {
      // Never regress from Processing/Done back to Reading (stale R messages
      // or a re-fired file inputchanged after close used to stick at 0%).
      if (isPastReading()) return;

      // Post-read / late merge: switch to animated Processing… so the UI is
      // not stuck on "Reading … 100%" while R builds the Sessions table
      // (that work often runs before the next message can flush).
      if (
        (typeof pct === "number" && pct >= 99.5) ||
        (typeof detail === "string" && /processing|merging/i.test(detail))
      ) {
        enterProcessing();
        return;
      }
      state.phase = "reading";
      stopWatchingUpload();
      stopProcessingDots();
      setBarIndeterminate(false);
      setModalProgress(
        pct,
        detail || "Reading...",
        "Reading..."
      );
      // If R goes quiet near the end of reading, still show Processing…
      // rather than a frozen "Reading … 95%".
      if (typeof pct === "number" && pct >= 85) {
        clearProcessingWatchdog();
        state.processingWatchdog = window.setTimeout(function () {
          if (state.phase === "reading") enterProcessing();
        }, 1500);
      }
    },
    setProcessing: function (detail) {
      if (state.phase === "done") return;
      enterProcessing();
      if (detail && typeof detail === "string") {
        var detailEl = document.getElementById("file-progress-detail");
        // Dots animation owns the detail text; ignore static overrides.
        if (detailEl && !state.processingDotsTimer) {
          detailEl.innerText = detail;
        }
      }
    },
    setUploading: function (pct, detail) {
      if (state.phase === "done") return;
      state.phase = "uploading";
      stopProcessingDots();
      setBarIndeterminate(false);
      setModalProgress(pct, detail, "Uploading...");
    },
    markWaitingForServer: markWaitingForServer,
    finish: finishAndClose,
    reset: function () {
      // Prefer finish() on successful upload close. reset() is kept for
      // compatibility; treat it as "done" so we do not re-enter Reading.
      clearCloseTimer();
      clearProcessingWatchdog();
      state.phase = "done";
      state.totalBytes = 0;
      state.fileCount = 0;
      state.lastPct = -1;
      state.pending = null;
      stopWatchingUpload();
      stopProcessingDots();
      setBarIndeterminate(false);
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
        if (state.phase === "processing") {
          setBarIndeterminate(true);
          if (!state.processingDotsTimer) startProcessingDots();
        }
        if (modalSeen) return;
        modalSeen = true;
        // Do not revive upload UI after a finished cycle (phase === "done").
        if (state.phase === "done") return;
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
        } else if (
          state.phase !== "reading" &&
          state.phase !== "processing" &&
          state.phase !== "waiting_server"
        ) {
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

    if (window.jQuery) {
      $(document).on("shiny:inputchanged", function (event) {
        if (event.name !== "file") return;
        if (!event.value) return;
        // Only bridge into Reading from upload/idle — never after Processing
        // or Done (that caused Reading stuck at 0% with the table already up).
        if (
          state.phase === "reading" ||
          state.phase === "processing" ||
          state.phase === "done" ||
          state.phase === "waiting_server"
        ) {
          return;
        }
        markWaitingForServer();
        window.EasyEyesFileProgress.setReading(0, "Reading...");
      });

      // Safety net: if Sessions output arrives while the modal is still open,
      // dismiss it (covers a missed close message).
      $(document).on("shiny:value", function (event) {
        if (!event || event.name !== "ex1") return;
        if (state.phase !== "processing" && state.phase !== "reading") return;
        window.setTimeout(function () {
          if (state.phase === "processing" || state.phase === "reading") {
            finishAndClose();
          }
        }, 200);
      });
    }
  });
})();
