document.addEventListener("DOMContentLoaded", function () {
  const visibleTabs = ["Sessions", "Plots", "Staircases", "Timing", "Stats"];

  function updateControlPanelVisibility(tabName) {
    const panel = document.getElementById("controlPanel");
    if (!panel) return;
    if (visibleTabs.includes(tabName)) {
      panel.style.display = "block";
    } else {
      panel.style.display = "none";
    }
  }

  // Initial check (if default tab is loaded)
  const initialTab = document.querySelector(".nav-tabs .active a")?.textContent?.trim();
  if (initialTab) updateControlPanelVisibility(initialTab);

  // Watch for tab changes
  $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function (e) {
    const newTabName = $(e.target).text().trim();
    updateControlPanelVisibility(newTabName);
  });
});
