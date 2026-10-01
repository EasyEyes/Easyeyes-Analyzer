/**
 * Non-blocking Plots-tab progress window.
 *
 * Server: session$sendCustomMessage("plotsPageProgress", {...})
 * Payload: active, done, stage, detail, timerReset, elapsedSec, generation
 *
 * Does NOT gray out the page or capture pointer events outside the panel,
 * so users can view/download finished plots while later stages still render.
 */
(function () {
  var state = {
    generation: 0,
    dismissedGen: -1,
    done: false,
    timerStartedAt: null,
    timerRaf: null,
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
      '  <span class="plots-page-progress-title" id="plots-page-progress-title">Plots</span>' +
      '  <button type="button" class="plots-page-progress-close" id="plots-page-progress-close" ' +
      '          aria-label="Close plots progress" title="Close">&times;</button>' +
      "</div>" +
      '<div class="plots-page-progress-body" id="plots-page-progress-body">' +
      '  <div class="plots-page-progress-stage" id="plots-page-progress-stage"></div>' +
      '  <div class="plots-page-progress-detail" id="plots-page-progress-detail"></div>' +
      '  <div class="plots-page-progress-timer" id="plots-page-progress-timer">0:00</div>' +
      "</div>" +
      '<div class="plots-page-progress-done" id="plots-page-progress-done" hidden></div>';

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

  function stopTimerLoop() {
    if (state.timerRaf != null) {
      cancelAnimationFrame(state.timerRaf);
      state.timerRaf = null;
    }
  }

  function tickTimer() {
    var timerEl = document.getElementById("plots-page-progress-timer");
    if (!timerEl || state.done || !state.timerStartedAt) {
      state.timerRaf = null;
      return;
    }
    var sec = (Date.now() - state.timerStartedAt) / 1000;
    timerEl.textContent = formatElapsed(sec);
    state.timerRaf = requestAnimationFrame(tickTimer);
  }

  function startTimer(reset) {
    if (reset || !state.timerStartedAt) {
      state.timerStartedAt = Date.now();
    }
    stopTimerLoop();
    tickTimer();
  }

  function dismiss() {
    state.dismissedGen = state.generation;
    stopTimerLoop();
    var el = document.getElementById("plots-page-progress");
    if (el) el.hidden = true;
  }

  function showRunning(msg) {
    var el = ensurePanel();
    if (state.dismissedGen === state.generation) {
      return;
    }
    el.hidden = false;
    el.classList.remove("is-done");
    el.classList.add("is-running");

    var body = document.getElementById("plots-page-progress-body");
    var done = document.getElementById("plots-page-progress-done");
    if (body) body.hidden = false;
    if (done) done.hidden = true;

    var title = document.getElementById("plots-page-progress-title");
    if (title) title.textContent = "Plots progress";

    var stage = document.getElementById("plots-page-progress-stage");
    if (stage) stage.textContent = msg.stage || "Plotting …";

    var detail = document.getElementById("plots-page-progress-detail");
    if (detail) {
      detail.textContent = msg.detail || "";
      detail.hidden = !msg.detail;
    }

    state.done = false;
    if (msg.timerReset) {
      startTimer(true);
    } else if (!state.timerStartedAt) {
      if (typeof msg.elapsedSec === "number" && isFinite(msg.elapsedSec)) {
        state.timerStartedAt = Date.now() - msg.elapsedSec * 1000;
      }
      startTimer(false);
    } else if (!state.timerRaf) {
      startTimer(false);
    }
  }

  function showDone(msg) {
    var el = ensurePanel();
    if (state.dismissedGen === state.generation) {
      return;
    }
    el.hidden = false;
    el.classList.remove("is-running");
    el.classList.add("is-done");

    state.done = true;
    stopTimerLoop();

    var elapsed =
      typeof msg.elapsedSec === "number" && isFinite(msg.elapsedSec)
        ? msg.elapsedSec
        : state.timerStartedAt
          ? (Date.now() - state.timerStartedAt) / 1000
          : 0;

    var body = document.getElementById("plots-page-progress-body");
    var done = document.getElementById("plots-page-progress-done");
    if (body) body.hidden = true;
    if (done) done.hidden = true;

    var title = document.getElementById("plots-page-progress-title");
    if (title) {
      var stageText = msg.stage ? String(msg.stage) : "";
      title.textContent =
        stageText.indexOf("Done.") === 0
          ? stageText
          : "Done. " + formatElapsed(elapsed);
    }
  }

  function onMessage(msg) {
    if (!msg || typeof msg !== "object") return;
    var gen = typeof msg.generation === "number" ? msg.generation : 0;
    if (gen > state.generation) {
      state.generation = gen;
      // New run: allow the panel again even if user closed a previous one.
    } else if (gen < state.generation) {
      return; // stale
    }

    if (msg.done) {
      showDone(msg);
      return;
    }
    if (msg.active === false && !msg.done) {
      return;
    }
    showRunning(msg);
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("plotsPageProgress", onMessage);
  } else {
    document.addEventListener("shiny:connected", function () {
      Shiny.addCustomMessageHandler("plotsPageProgress", onMessage);
    });
  }
})();
