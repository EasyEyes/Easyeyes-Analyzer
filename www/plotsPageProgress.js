/**
 * Non-blocking Plots-tab progress window.
 *
 * Two sources feed the panel:
 *   - Browser: opens the panel the moment the user switches to Plots (R may
 *     still be busy with an earlier step and cannot react yet).
 *   - R server: session$sendCustomMessage("plotsPageProgress", {...}) with
 *     active, done, stage, detail, timerReset, elapsedSec, generation.
 *
 * Display (while running):
 *   Plots 0:38
 *   Plotting histograms …
 *   22/30
 *
 * The timer runs in the browser from the tab click, so the final
 * "Done. m:ss" is the wait the user actually saw (R + network + browser).
 *
 * Does NOT gray out the page or capture pointer events outside the panel,
 * so users can view/download finished plots while later stages still render.
 */
(function () {
  var POLL_MS = 250;
  // Give up waiting for images that never fire load/error after R is done.
  var BROWSER_DRAIN_MAX_MS = 15000;
  // Browser-started run with R idle this long and silent: nothing to plot.
  var AWAIT_IDLE_GIVE_UP_MS = 8000;

  var state = {
    generation: 0,
    run: 0,
    dismissedRun: -1,
    awaitingServer: false,
    idleSince: null,
    running: false,
    serverDone: false,
    serverDoneAt: null,
    serverStage: "",
    serverDetail: "",
    uploadSeq: 0,
    doneUploadSeq: -1,
    timerStartedAt: null,
    timerRaf: null,
    pollId: null,
    dragging: false,
    dragOffsetX: 0,
    dragOffsetY: 0
  };

  function formatElapsed(sec) {
    sec = Math.max(0, Math.round(Number(sec) || 0));
    var m = Math.floor(sec / 60);
    var s = sec % 60;
    return m + ":" + (s < 10 ? "0" : "") + s;
  }

  function onPlotsTab() {
    var pane = plotsPane();
    if (pane) return pane.classList.contains("active");
    var link = document.querySelector('a[data-value="Plots"]');
    return !!(link && link.parentElement &&
      link.parentElement.classList.contains("active"));
  }

  function plotsPane() {
    return document.querySelector('.tab-pane[data-value="Plots"]');
  }

  function serverBusy() {
    return document.documentElement.classList.contains("shiny-busy");
  }

  function isVisible(el) {
    return !!(el && el.offsetParent !== null);
  }

  function browserCounts() {
    var pane = plotsPane();
    var out = { waitingOutputs: 0, loadingImages: 0 };
    if (!pane) return out;
    var outputs = pane.querySelectorAll(
      ".shiny-image-output, .shiny-plot-output, .html-widget-output"
    );
    for (var i = 0; i < outputs.length; i++) {
      var o = outputs[i];
      if (isVisible(o) && o.classList.contains("recalculating")) {
        out.waitingOutputs += 1;
      }
    }
    var imgs = pane.querySelectorAll("img");
    for (var j = 0; j < imgs.length; j++) {
      var img = imgs[j];
      if (!isVisible(img) || !img.getAttribute("src")) continue;
      if (!img.complete) out.loadingImages += 1;
    }
    return out;
  }

  function plural(n, word) {
    return n + " " + word + (n === 1 ? "" : "s");
  }

  function ensurePanel() {
    var el = document.getElementById("plots-page-progress");
    if (el) return el;

    el = document.createElement("div");
    el.id = "plots-page-progress";
    el.className = "plots-page-progress";
    el.setAttribute("role", "status");
    el.setAttribute("aria-live", "polite");
    el.innerHTML =
      '<div class="plots-page-progress-header" id="plots-page-progress-header">' +
      '  <span class="plots-page-progress-title" id="plots-page-progress-title">Plots 0:00</span>' +
      '  <button type="button" class="plots-page-progress-close" id="plots-page-progress-close" ' +
      '          aria-label="Close plots progress" title="Close">&times;</button>' +
      "</div>" +
      '<div class="plots-page-progress-body" id="plots-page-progress-body">' +
      '  <div class="plots-page-progress-stage" id="plots-page-progress-stage"></div>' +
      '  <div class="plots-page-progress-detail" id="plots-page-progress-detail"></div>' +
      "</div>";

    document.body.appendChild(el);

    document
      .getElementById("plots-page-progress-close")
      .addEventListener("click", function (e) {
        e.preventDefault();
        e.stopPropagation();
        dismiss();
      });

    var header = document.getElementById("plots-page-progress-header");
    header.addEventListener("mousedown", function (e) {
      if (e.button !== 0) return;
      if (e.target && e.target.id === "plots-page-progress-close") return;
      state.dragging = true;
      var rect = el.getBoundingClientRect();
      state.dragOffsetX = e.clientX - rect.left;
      state.dragOffsetY = e.clientY - rect.top;
      el.classList.add("is-dragging");
      e.preventDefault();
    });

    document.addEventListener("mousemove", function (e) {
      if (!state.dragging) return;
      var x = e.clientX - state.dragOffsetX;
      var y = e.clientY - state.dragOffsetY;
      var maxX = Math.max(0, window.innerWidth - el.offsetWidth);
      var maxY = Math.max(0, window.innerHeight - el.offsetHeight);
      x = Math.min(Math.max(0, x), maxX);
      y = Math.min(Math.max(0, y), maxY);
      el.style.left = x + "px";
      el.style.top = y + "px";
      el.style.right = "auto";
    });

    document.addEventListener("mouseup", function () {
      if (!state.dragging) return;
      state.dragging = false;
      el.classList.remove("is-dragging");
    });

    return el;
  }

  function setText(id, text) {
    var node = document.getElementById(id);
    if (!node) return;
    node.textContent = text || "";
    node.hidden = !text;
  }

  function stopTimerLoop() {
    if (state.timerRaf != null) {
      cancelAnimationFrame(state.timerRaf);
      state.timerRaf = null;
    }
  }

  function elapsedLabel() {
    if (!state.timerStartedAt) return "0:00";
    return formatElapsed((Date.now() - state.timerStartedAt) / 1000);
  }

  function setTitleRunning() {
    var title = document.getElementById("plots-page-progress-title");
    if (title) title.textContent = "Plots " + elapsedLabel();
  }

  function tickTimer() {
    if (!state.running || !state.timerStartedAt) {
      state.timerRaf = null;
      return;
    }
    setTitleRunning();
    state.timerRaf = requestAnimationFrame(tickTimer);
  }

  function startTimer(reset, elapsedSec) {
    if (reset || !state.timerStartedAt) {
      var offset = typeof elapsedSec === "number" && isFinite(elapsedSec)
        ? elapsedSec * 1000
        : 0;
      state.timerStartedAt = Date.now() - offset;
    }
    stopTimerLoop();
    tickTimer();
  }

  function stopPoll() {
    if (state.pollId != null) {
      window.clearInterval(state.pollId);
      state.pollId = null;
    }
  }

  function startPoll() {
    if (state.pollId != null) return;
    state.pollId = window.setInterval(refresh, POLL_MS);
  }

  function dismiss() {
    state.dismissedRun = state.run;
    stopTimerLoop();
    stopPoll();
    var el = document.getElementById("plots-page-progress");
    if (el) el.hidden = true;
  }

  function beginRun(resetTimer, elapsedSec) {
    state.run += 1;
    state.running = true;
    state.serverDone = false;
    state.serverDoneAt = null;
    state.serverStage = "";
    state.serverDetail = "";
    startTimer(resetTimer !== false, elapsedSec);
  }

  // Prefer R's stage/detail; fall back to a short browser-side status.
  function displayLines(counts) {
    if (state.awaitingServer) {
      return {
        stage: serverBusy()
          ? "Waiting for R …"
          : "Starting plots …",
        detail: ""
      };
    }
    if (state.serverDone) {
      if (counts.loadingImages > 0) {
        return {
          stage: "Loading images …",
          detail: String(counts.loadingImages)
        };
      }
      if (counts.waitingOutputs > 0) {
        return {
          stage: "Finishing plots …",
          detail: String(counts.waitingOutputs)
        };
      }
      return { stage: "Finishing …", detail: "" };
    }
    if (!onPlotsTab()) {
      return {
        stage: state.serverStage || "Paused",
        detail: state.serverDetail || ""
      };
    }
    if (state.serverStage) {
      return { stage: state.serverStage, detail: state.serverDetail };
    }
    if (counts.loadingImages > 0) {
      return {
        stage: "Loading images …",
        detail: plural(counts.loadingImages, "image")
      };
    }
    if (counts.waitingOutputs > 0) {
      return {
        stage: "Waiting for plots …",
        detail: plural(counts.waitingOutputs, "plot")
      };
    }
    return { stage: "Plotting …", detail: "" };
  }

  function showDone() {
    var el = ensurePanel();
    state.running = false;
    state.doneUploadSeq = state.uploadSeq;
    stopTimerLoop();
    stopPoll();
    if (state.dismissedRun === state.run) return;
    el.hidden = false;
    el.classList.remove("is-running");
    el.classList.add("is-done");
    var body = document.getElementById("plots-page-progress-body");
    if (body) body.hidden = true;
    var title = document.getElementById("plots-page-progress-title");
    if (title) title.textContent = "Done. " + elapsedLabel();
  }

  function refresh() {
    if (!state.running) return;
    var counts = browserCounts();

    if (state.awaitingServer) {
      if (serverBusy()) {
        state.idleSince = null;
      } else if (!state.idleSince) {
        state.idleSince = Date.now();
      } else if (Date.now() - state.idleSince > AWAIT_IDLE_GIVE_UP_MS) {
        state.awaitingServer = false;
        state.running = false;
        dismiss();
        return;
      }
    }

    if (state.serverDone) {
      var drained = counts.loadingImages === 0 && counts.waitingOutputs === 0;
      var waitedTooLong = state.serverDoneAt &&
        Date.now() - state.serverDoneAt > BROWSER_DRAIN_MAX_MS;
      if (drained || waitedTooLong || !onPlotsTab()) {
        showDone();
        return;
      }
    }

    if (state.dismissedRun === state.run) return;
    var el = ensurePanel();
    el.hidden = false;
    el.classList.remove("is-done");
    el.classList.add("is-running");
    var body = document.getElementById("plots-page-progress-body");
    if (body) body.hidden = false;
    setTitleRunning();

    var lines = displayLines(counts);
    setText("plots-page-progress-stage", lines.stage);
    setText("plots-page-progress-detail", lines.detail);
    if (!state.timerRaf) startTimer(false);
  }

  function maybeStartFromBrowser() {
    if (state.uploadSeq <= 0) return;
    if (state.doneUploadSeq === state.uploadSeq) return;
    if (state.running) {
      refresh();
      startPoll();
      return;
    }
    beginRun(true);
    state.awaitingServer = true;
    state.idleSince = null;
    refresh();
    startPoll();
  }

  function onServerMessage(msg) {
    if (!msg || typeof msg !== "object") return;
    var gen = typeof msg.generation === "number" ? msg.generation : 0;
    if (gen > state.generation) {
      state.generation = gen;
      if (state.awaitingServer || (state.running && !state.serverDone)) {
        state.awaitingServer = false;
      } else {
        beginRun(!!msg.timerReset, msg.elapsedSec);
      }
    } else if (gen < state.generation || state.awaitingServer) {
      return;
    }

    if (msg.done) {
      state.serverDone = true;
      state.serverDoneAt = Date.now();
    } else if (msg.active === false) {
      return;
    } else {
      state.serverDone = false;
      state.serverDoneAt = null;
      state.serverStage = msg.stage || "Plotting …";
      state.serverDetail = msg.detail || "";
      if (!state.running) {
        state.running = true;
        startTimer(false, msg.elapsedSec);
      }
    }
    refresh();
    if (state.running) startPoll();
  }

  function onNewUpload() {
    state.uploadSeq += 1;
    state.doneUploadSeq = -1;
    state.awaitingServer = false;
    state.running = false;
    stopTimerLoop();
    stopPoll();
    var el = document.getElementById("plots-page-progress");
    if (el) el.hidden = true;
    if (onPlotsTab()) maybeStartFromBrowser();
  }

  function bindShiny() {
    Shiny.addCustomMessageHandler("plotsPageProgress", onServerMessage);
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    bindShiny();
  } else {
    document.addEventListener("shiny:connected", bindShiny);
  }

  if (window.jQuery) {
    $(document).on("shiny:inputchanged", function (event) {
      if (event.name === "file" && event.value) {
        onNewUpload();
      } else if (event.name === "navbar") {
        if (event.value === "Plots") {
          window.setTimeout(maybeStartFromBrowser, 0);
        } else if (state.running) {
          refresh();
        }
      }
    });
    $(document).on("shiny:value", function () {
      if (state.running) window.setTimeout(refresh, 0);
    });
    document.addEventListener("load", function (e) {
      if (state.running && e.target && e.target.tagName === "IMG") refresh();
    }, true);
  }
})();
