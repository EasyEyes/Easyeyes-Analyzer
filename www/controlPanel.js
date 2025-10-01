document.addEventListener("DOMContentLoaded", function () {
  const visibleTabs = ["Sessions", "Plots", "Staircases", "Timing", "Stats", "Quality", "Anova", "Distance"];

  function updatePanelVisibility(tabName) {
    const controlPanel = document.getElementById("controlPanel");
    const thresholdParam = document.getElementById(
      "thresholdParameterSelector",
    );

    // Toggle controlPanel
    if (controlPanel) {
      if (visibleTabs.includes(tabName)) {
        controlPanel.style.display = "block";
      } else {
        controlPanel.style.display = "none";
      }
    }

    // Toggle thresholdParameter
    if (thresholdParam) {
      if (tabName === "Staircases") {
        thresholdParam.style.display = "block";
      } else {
        thresholdParam.style.display = "none";
      }
    }
  }

  // Initial tab check
  const initialTab = document.querySelector(".nav-tabs .active a");
  if (initialTab) {
    updatePanelVisibility(initialTab.textContent.trim());
  }

  // Handle tab switches
  $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function (e) {
    const newTabName = e.target.textContent.trim();
    updatePanelVisibility(newTabName);
  });
});
