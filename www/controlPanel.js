document.addEventListener("DOMContentLoaded", function () {
  const visibleTabs = ["Sessions", "Plots", "Languages", "Staircases", "Timing", "Stats", "Quality", "Anova", "Distance"];

  function tabNameFromElement(el) {
    if (!el) return "";
    const node = el.closest ? (el.closest("a") || el) : el;
    return (
      node.getAttribute("data-value") ||
      node.getAttribute("data-bs-value") ||
      (node.textContent || "")
    ).trim();
  }

  function updatePanelVisibility(tabName) {
    const controlPanel = document.getElementById("controlPanel");
    const thresholdParam = document.getElementById(
      "thresholdParameterSelector",
    );
    const name = (tabName || "").trim();

    if (controlPanel) {
      controlPanel.style.display = visibleTabs.includes(name) ? "block" : "none";
    }

    if (thresholdParam) {
      thresholdParam.style.display = name === "Staircases" ? "block" : "none";
    }
  }

  function onTabShown(e) {
    updatePanelVisibility(tabNameFromElement(e.target));
  }

  $(document).on(
    "shown.bs.tab",
    'a[data-toggle="tab"], a[data-bs-toggle="tab"], a.nav-link',
    onTabShown,
  );

  $(document).on("shiny:inputchanged", function (event) {
    if (event.name === "navbar") {
      updatePanelVisibility(event.value);
    }
  });

  function registerControlPanelHandler() {
    if (window.Shiny && typeof Shiny.addCustomMessageHandler === "function") {
      Shiny.addCustomMessageHandler("updateControlPanel", updatePanelVisibility);
    }
  }
  registerControlPanelHandler();
  $(document).on("shiny:connected", registerControlPanelHandler);

  const initialTab = document.querySelector(
    ".nav-tabs .active a, .navbar .nav-link.active, a.nav-link.active",
  );
  if (initialTab) {
    updatePanelVisibility(tabNameFromElement(initialTab));
  }
});
