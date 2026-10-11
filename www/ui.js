// JavaScript function to update document title based on active tab
function updateTitleBasedOnTab(tabName) {
  document.title = "EasyEyes Analysis | " + tabName.trim();
}

// Set initial title to 'EasyEyes Analysis' with the default tab
document.addEventListener("DOMContentLoaded", function () {
  updateTitleBasedOnTab("Sessions");
});

// Update title whenever a tab is clicked
$(document).on("shown.bs.tab", 'a[data-toggle=\"tab\"]', function (e) {
  var tabName = $(e.target).text(); // Get the active tab name
  updateTitleBasedOnTab(tabName);
});

// Removed gray screen element
document.addEventListener("DOMContentLoaded", function () {
  // Function to remove the overlay if it exists
  function removeOverlayIfExists() {
    var overlay = document.getElementById("ss-overlay");
    if (overlay) {
      overlay.remove();
      return true;
    }
    return false;
  }

  // Try removing right away
  if (!removeOverlayIfExists()) {
    // If not found, observe for it
    var observer = new MutationObserver(function (mutations) {
      mutations.forEach(function (mutation) {
        if (removeOverlayIfExists()) {
          observer.disconnect(); // Stop watching once removed
        }
      });
    });

    observer.observe(document.body, { childList: true, subtree: true });
  }
});

// Timer JS
let startTime = new Date().getTime();
function updateTimer() {
  let now = new Date().getTime();
  let elapsed = now - startTime;
  let seconds = Math.floor((elapsed / 1000) % 60);
  let minutes = Math.floor((elapsed / (1000 * 60)) % 60);
  let hours = Math.floor((elapsed / (1000 * 60 * 60)) % 24);

  document.getElementById("timer").innerHTML =
    (hours < 10 ? "0" + hours : hours) +
    ":" +
    (minutes < 10 ? "0" + minutes : minutes) +
    ":" +
    (seconds < 10 ? "0" + seconds : seconds);

  // Check every second
  setTimeout(updateTimer, 1000);
}
document.addEventListener("DOMContentLoaded", (event) => {
  updateTimer();
});

// Plots flex grid: only participant-violin (etc.) slots that actually show a
// full-width conditionalPanel get .plots-flex-cell-full. Shiny leaves hidden
// conditionalPanel nodes in the DOM, so CSS :has(.marker) cannot be used.
(function () {
  function panelIsShown(el) {
    if (!el) return false;
    // Shiny conditionalPanel toggles inline display.
    if (el.style && el.style.display === "none") return false;
    var style = window.getComputedStyle(el);
    return style.display !== "none" && style.visibility !== "hidden";
  }

  function syncPlotsFlexFullWidth(root) {
    var scope = root && root.querySelectorAll ? root : document;
    var cells = scope.querySelectorAll
      ? scope.querySelectorAll(".plots-flex-cell")
      : [];
    if (!cells.length && root && root.classList && root.classList.contains("plots-flex-cell")) {
      cells = [root];
    }
    for (var i = 0; i < cells.length; i++) {
      var cell = cells[i];
      var marker = cell.querySelector(".plots-flex-full-active");
      var full = panelIsShown(marker);
      cell.classList.toggle("plots-flex-cell-full", full);
    }
  }

  function ready(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn);
    } else {
      fn();
    }
  }

  ready(function () {
    syncPlotsFlexFullWidth(document);
    if (typeof MutationObserver !== "undefined") {
      var obs = new MutationObserver(function (mutations) {
        for (var i = 0; i < mutations.length; i++) {
          var m = mutations[i];
          var target = m.target;
          if (
            target &&
            target.classList &&
            (target.classList.contains("plots-flex-full-active") ||
              target.classList.contains("plots-flex-cell") ||
              target.classList.contains("plots-flex-grid"))
          ) {
            syncPlotsFlexFullWidth(
              target.classList.contains("plots-flex-cell")
                ? target
                : target.closest
                  ? target.closest(".plots-flex-grid") || document
                  : document
            );
            continue;
          }
          if (m.type === "childList") {
            syncPlotsFlexFullWidth(document);
          }
        }
      });
      obs.observe(document.body, {
        childList: true,
        subtree: true,
        attributes: true,
        attributeFilter: ["style", "class", "hidden"]
      });
    }
    if (window.jQuery) {
      $(document).on("shiny:value shiny:visualchange shown.bs.tab", function () {
        window.setTimeout(function () {
          syncPlotsFlexFullWidth(document);
        }, 0);
      });
    }
  });
})();
