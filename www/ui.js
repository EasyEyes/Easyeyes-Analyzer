// JavaScript function to update document title based on active tab
function updateTitleBasedOnTab(tabName) {
  document.title = tabName + " | EasyEyes Analysis";
}

// Set initial title to 'EasyEyes Analysis' with the default tab
document.addEventListener("DOMContentLoaded", function () {
  updateTitleBasedOnTab("Session"); // Default tab name
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
