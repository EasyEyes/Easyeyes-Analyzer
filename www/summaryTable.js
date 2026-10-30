// Summary-table DataTables helpers. Wired from R via DT callback body:
//   if (window.initSummaryTableCallbacks) {
//     window.initSummaryTableCallbacks(table);
//   }
// (DT wraps that body in function(table) { ... }.)

(function (window, $) {
  "use strict";

  window.errorExplanations = window.errorExplanations || {};
  window.unmetNeedsExplanations = window.unmetNeedsExplanations || {};
  window.warningExplanations = window.warningExplanations || {};

  function redrawSummaryTables() {
    if (window.jQuery && jQuery.fn.dataTable) {
      try {
        jQuery.fn.dataTable.tables({ visible: true, api: true }).draw(false);
      } catch (e) {
        /* table may not be ready yet */
      }
    }
  }

  function setErrorExplanations(map) {
    window.errorExplanations = map && typeof map === "object" ? map : {};
    redrawSummaryTables();
  }

  function setUnmetNeedsExplanations(map) {
    window.unmetNeedsExplanations = map && typeof map === "object" ? map : {};
    redrawSummaryTables();
  }

  function setWarningExplanations(map) {
    window.warningExplanations = map && typeof map === "object" ? map : {};
    redrawSummaryTables();
  }

  var explanationHandlersRegistered = false;
  function registerExplanationHandlers() {
    if (!(window.Shiny && Shiny.addCustomMessageHandler)) return;
    if (!explanationHandlersRegistered) {
      Shiny.addCustomMessageHandler(
        "setErrorExplanations",
        setErrorExplanations,
      );
      Shiny.addCustomMessageHandler(
        "setUnmetNeedsExplanations",
        setUnmetNeedsExplanations,
      );
      Shiny.addCustomMessageHandler(
        "setWarningExplanations",
        setWarningExplanations,
      );
      explanationHandlersRegistered = true;
    }
    if (Shiny.setInputValue) {
      Shiny.setInputValue("summaryExplanationsReady", Date.now(), {
        priority: "event",
      });
    }
  }

  function onReady(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn);
    } else {
      fn();
    }
  }

  onReady(registerExplanationHandlers);
  if (window.jQuery) {
    jQuery(document).on("shiny:connected", registerExplanationHandlers);
  } else {
    document.addEventListener("shiny:connected", registerExplanationHandlers);
  }

  function showSummaryDetailModal(title, html) {
    var content =
      html == null || html === "" || html === "null" || html === "undefined"
        ? "<em>No details</em>"
        : html;
    var $modal = $("#summaryDetailModal");
    if ($modal.length === 0) {
      $("body").append(
        '<div class="modal fade" id="summaryDetailModal" tabindex="-1" role="dialog" aria-hidden="true">' +
          '<div class="modal-dialog modal-lg" role="document">' +
          '<div class="modal-content">' +
          '<div class="modal-header">' +
          '<h4 class="modal-title" id="summaryDetailModalTitle"></h4>' +
          '<button type="button" class="close btn-close" data-dismiss="modal" data-bs-dismiss="modal" aria-label="Close">' +
          '<span aria-hidden="true">&times;</span>' +
          "</button>" +
          "</div>" +
          '<div class="modal-body" id="summaryDetailModalBody" style="max-height:70vh;overflow:auto;white-space:normal;word-break:break-word;"></div>' +
          '<div class="modal-footer">' +
          '<button type="button" class="btn btn-default btn-secondary" data-dismiss="modal" data-bs-dismiss="modal">Close</button>' +
          "</div>" +
          "</div>" +
          "</div>" +
          "</div>",
      );
      $modal = $("#summaryDetailModal");
    }
    $("#summaryDetailModalTitle").text(title);
    $("#summaryDetailModalBody").html(content);
    if (typeof bootstrap !== "undefined" && bootstrap.Modal) {
      bootstrap.Modal.getOrCreateInstance($modal[0]).show();
    } else {
      $modal.modal("show");
    }
  }

  function escapeHtml(text) {
    return String(text)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function cellTextToPlain(rawHtml) {
    return String(rawHtml || "")
      .replace(/<br\s*\/?>/gi, "\n")
      .replace(/<[^>]+>/g, " ");
  }

  function summaryDetailText(raw) {
    return cellTextToPlain(raw).replace(/\s+/g, " ").trim();
  }

  function hasSummaryDetail(raw) {
    var plain = summaryDetailText(raw);
    return plain !== "" && plain !== "null" && plain !== "undefined";
  }

  function hasErrorDetail(raw) {
    var plain = summaryDetailText(raw);
    return plain !== "" && plain !== "compatibilityNotMet";
  }

  function hasProlificInfoDetail(_raw, data) {
    return [2, 3, 4].some(function (index) {
      return hasSummaryDetail(data[index]);
    });
  }

  // One content-aware cursor rule for every detail column. Recompute after
  // each draw so new DataTables nodes and whitespace-only cells stay correct.
  function watchSummaryDetailCursors(table, definitions) {
    var update = function () {
      for (var i = 0; i < definitions.length; i++) {
        var definition = definitions[i];
        if (definition.index < 0) continue;
        table
          .column(definition.index)
          .nodes()
          .to$()
          .each(function () {
            var data = table.row($(this).closest("tr")).data() || [];
            var raw = data[definition.index];
            $(this).toggleClass(
              "summary-clickable",
              definition.predicate(raw, data),
            );
          });
      }
    };
    update();
    table.on("draw.dt", update);
  }

  // Parameter Glossary `_s` keys lead with `_`. Ensure unmetNeeds tokens do too
  // before matching (e.g. needMemoryGB → _needMemoryGB).
  function withLeadingUnderscores(plain) {
    return String(plain).replace(
      /(^|[\s,;]+)(_*)([A-Za-z][\w]*)/g,
      function (_m, sep, _us, name) {
        return sep + "_" + name;
      },
    );
  }

  // Longest-name-first scan against a name→explanation map.
  // options.prefixUnderscore: normalize cell text so tokens match `_s` keys.
  function findAllNameMatches(rawHtml, map, options) {
    map = map || {};
    options = options || {};
    var plain = cellTextToPlain(rawHtml);
    if (options.prefixUnderscore) {
      plain = withLeadingUnderscores(plain);
    }
    if (!plain.trim()) return [];

    var names = Object.keys(map).sort(function (a, b) {
      return b.length - a.length;
    });
    var matched = [];
    var used = [];

    function overlaps(start, end) {
      for (var i = 0; i < used.length; i++) {
        if (start < used[i][1] && end > used[i][0]) return true;
      }
      return false;
    }

    function isTokenStart(idx) {
      if (idx <= 0) return true;
      return /[\s,;]/.test(plain.charAt(idx - 1));
    }

    var ignore = {};
    if (options.ignoreNames) {
      for (var n = 0; n < options.ignoreNames.length; n++) {
        ignore[options.ignoreNames[n]] = true;
      }
    }

    for (var i = 0; i < names.length; i++) {
      var name = names[i];
      if (!name || ignore[name]) continue;
      var from = 0;
      while (from < plain.length) {
        var idx = plain.indexOf(name, from);
        if (idx < 0) break;
        var end = idx + name.length;
        if (!isTokenStart(idx) || overlaps(idx, end)) {
          from = idx + 1;
          continue;
        }
        matched.push({
          name: name,
          explanation: map[name],
          start: idx,
          end: end,
        });
        used.push([idx, end]);
        from = end;
      }
    }

    matched.sort(function (a, b) {
      return a.start - b.start;
    });
    return matched;
  }

  function formatExplanationPopupContent(rawHtml, map, options) {
    if (rawHtml == null || rawHtml === "" || rawHtml === "null") {
      return "<em>No details</em>";
    }

    var matches = findAllNameMatches(rawHtml, map, options);
    options = options || {};
    if (!matches.length) {
      return "<p>" + rawHtml + "</p>";
    }

    var seen = {};
    var out = [];
    for (var i = 0; i < matches.length; i++) {
      var m = matches[i];
      if (seen[m.name]) continue;
      seen[m.name] = true;
      out.push(
        "<p><strong>" +
          escapeHtml(m.name) +
          "</strong><br>" +
          escapeHtml(m.explanation) +
          "</p>",
      );
    }
    var content = out.join(
      '<hr style="margin:8px 0;border:0;border-top:1px solid #ddd;">',
    );
    if (options.includeRaw) {
      return (
        "<p>" +
        rawHtml +
        '</p><hr style="margin:8px 0;border:0;border-top:1px solid #ddd;">' +
        content
      );
    }
    return content;
  }

  function formatExplainedCellDisplay(data, map, cssClass, options) {
    if (
      data == null ||
      data === "" ||
      data === "null" ||
      data === "undefined"
    ) {
      return data;
    }
    var display = String(data);
    if (display.length > 30) {
      display = display.substr(0, 30) + "...";
    }
    if (findAllNameMatches(data, map, options).length > 0) {
      return (
        '<span class="' +
        cssClass +
        '" style="text-decoration:underline;text-underline-offset:2px">' +
        display +
        "</span>"
      );
    }
    return display;
  }

  // Details live in unmetNeeds; skip sheet explanation UI for this error.
  var ERROR_EXPLAIN_IGNORE = ["compatibilityNotMet"];

  window.formatErrorCellDisplay = function (data) {
    return formatExplainedCellDisplay(
      data,
      window.errorExplanations,
      "error-has-explanation",
      { ignoreNames: ERROR_EXPLAIN_IGNORE },
    );
  };

  window.formatUnmetNeedsCellDisplay = function (data) {
    return formatExplainedCellDisplay(
      data,
      window.unmetNeedsExplanations,
      "unmetNeeds-has-explanation",
      { prefixUnderscore: true },
    );
  };

  window.formatWarningCellDisplay = function (data) {
    return formatExplainedCellDisplay(
      data,
      window.warningExplanations,
      "warning-has-explanation",
      {},
    );
  };

  function parseCameraAttempt(raw) {
    if (raw == null || raw === "") return null;
    if (typeof raw === "object" && !Array.isArray(raw)) return raw;
    try {
      var parsed = JSON.parse(String(raw));
      return parsed && typeof parsed === "object" && !Array.isArray(parsed)
        ? parsed
        : null;
    } catch (_e) {
      return null;
    }
  }

  function hasCameraAttemptDetail(raw) {
    return parseCameraAttempt(raw) !== null;
  }

  function cameraAttemptOutcomeText(attempt) {
    var outcome = attempt.outcome;
    if (outcome && outcome.reason) return String(outcome.reason);
    if (outcome && outcome.type === "selected") {
      return "selected: " + (attempt.selectedCamera?.name || "camera");
    }
    if (outcome && outcome.type) return String(outcome.type);
    return "pending: " + (attempt.phase || "unknown");
  }

  window.formatCameraAttemptCellDisplay = function (data) {
    var attempt = parseCameraAttempt(data);
    if (!attempt) {
      var plain = summaryDetailText(data);
      return plain.length > 30
        ? escapeHtml(plain.substr(0, 30)) + "..."
        : escapeHtml(plain);
    }
    return (
      '<span class="cameraAttempt-has-detail" style="text-decoration:underline;text-underline-offset:2px">' +
      escapeHtml(cameraAttemptOutcomeText(attempt)) +
      "</span>"
    );
  };

  function cameraAttemptDetailRow(label, value) {
    if (value == null || value === "") return "";
    return (
      "<tr><th style='text-align:left;vertical-align:top;padding:3px 12px 3px 0'>" +
      escapeHtml(label) +
      "</th><td style='padding:3px 0'>" +
      escapeHtml(value) +
      "</td></tr>"
    );
  }

  function cameraAttemptErrorText(error) {
    if (!error) return "";
    if (typeof error === "string") return error;
    var parts = [];
    if (error.name) parts.push(error.name);
    if (error.message && error.message !== error.name)
      parts.push(error.message);
    if (error.reason) parts.push("reason=" + error.reason);
    if (error.cause) {
      parts.push(
        "cause=" + (error.cause.name || "") + " " + (error.cause.message || ""),
      );
    }
    return parts.join("; ");
  }

  function cameraAttemptCameraList(title, cameras) {
    if (!Array.isArray(cameras) || cameras.length === 0) return "";
    var items = cameras
      .map(function (camera) {
        var name = camera.name || "Camera";
        var classification = camera.class ? " (" + camera.class + ")" : "";
        return "<li>" + escapeHtml(name + classification) + "</li>";
      })
      .join("");
    return (
      "<p><strong>" + escapeHtml(title) + "</strong></p><ul>" + items + "</ul>"
    );
  }

  function formatCameraAttemptPopupContent(raw) {
    var attempt = parseCameraAttempt(raw);
    if (!attempt) return "<em>No camera attempt details</em>";
    var rows = [];
    rows.push(
      cameraAttemptDetailRow("Outcome", cameraAttemptOutcomeText(attempt)),
    );
    rows.push(cameraAttemptDetailRow("Last phase", attempt.phase));
    rows.push(cameraAttemptDetailRow("Permission", attempt.permission));
    rows.push(
      cameraAttemptDetailRow(
        "Detected cameras",
        Array.isArray(attempt.cameras) ? attempt.cameras.length : 0,
      ),
    );
    rows.push(
      cameraAttemptDetailRow(
        "Eligible cameras",
        Array.isArray(attempt.eligibleCameras)
          ? attempt.eligibleCameras.length
          : 0,
      ),
    );
    rows.push(
      cameraAttemptDetailRow("Selected camera", attempt.selectedCamera?.name),
    );
    rows.push(
      cameraAttemptDetailRow("Unavailable reason", attempt.unavailableReason),
    );
    rows.push(
      cameraAttemptDetailRow(
        "Startup error",
        cameraAttemptErrorText(attempt.startupError),
      ),
    );
    rows.push(
      cameraAttemptDetailRow(
        "Enumeration error",
        cameraAttemptErrorText(attempt.enumerationError),
      ),
    );
    rows.push(cameraAttemptDetailRow("Last progress", attempt.lastProgressAt));
    if (attempt.timings && typeof attempt.timings === "object") {
      for (var key of Object.keys(attempt.timings)) {
        if (attempt.timings[key] != null) {
          rows.push(
            cameraAttemptDetailRow("Timing: " + key, attempt.timings[key]),
          );
        }
      }
    }
    if (
      Array.isArray(attempt.previousOutcomes) &&
      attempt.previousOutcomes.length
    ) {
      rows.push(
        cameraAttemptDetailRow(
          "Previous outcomes",
          attempt.previousOutcomes
            .map(function (outcome) {
              return (
                outcome.type + (outcome.reason ? ": " + outcome.reason : "")
              );
            })
            .join("; "),
        ),
      );
    }
    var events = "";
    if (Array.isArray(attempt.events) && attempt.events.length) {
      events =
        "<p><strong>Events</strong></p><ul>" +
        attempt.events
          .map(function (event) {
            return (
              "<li>" +
              escapeHtml(
                (event.at || "") +
                  " — " +
                  (event.type || "event") +
                  (event.reason ? " — " + event.reason : ""),
              ) +
              "</li>"
            );
          })
          .join("") +
        "</ul>";
    }
    return (
      "<table style='width:100%;border-collapse:collapse'><tbody>" +
      rows.join("") +
      "</tbody></table>" +
      cameraAttemptCameraList("Detected cameras", attempt.cameras) +
      cameraAttemptCameraList("Eligible cameras", attempt.eligibleCameras) +
      events +
      "<details style='margin-top:12px'><summary>Raw JSON</summary><pre style='white-space:pre-wrap;word-break:break-word'>" +
      escapeHtml(JSON.stringify(attempt, null, 2)) +
      "</pre></details>"
    );
  }

  function longTaskSecValues(raw) {
    var plain = cellTextToPlain(raw).replace(/\s+/g, " ").trim();
    if (!plain) return [];
    return plain
      .split(/,\s*/)
      .map(function (s) {
        return s.trim();
      })
      .filter(function (s) {
        return s.length > 0;
      });
  }

  window.formatLongTaskSecCellDisplay = function (data) {
    var values = longTaskSecValues(data);
    if (!values.length) return "";
    var preview = values.slice(0, 3).join(", ");
    if (values.length > 3) {
      preview += ", … (" + values.length + ")";
    }
    return (
      '<span class="longTaskSec-has-detail" style="text-decoration:underline;text-underline-offset:2px">' +
      escapeHtml(preview) +
      "</span>"
    );
  };

  function formatLongTaskSecPopupContent(raw) {
    var values = longTaskSecValues(raw);
    if (!values.length) return "<em>No long tasks</em>";
    var lines = values
      .map(function (v) {
        return escapeHtml(v);
      })
      .join("<br>");
    return (
      "<p><strong>" +
      values.length +
      "</strong> long task" +
      (values.length === 1 ? "" : "s") +
      " (sec)</p>" +
      "<div style='max-height:50vh;overflow:auto;white-space:nowrap'>" +
      lines +
      "</div>"
    );
  }

  function formatErrorPopupContent(rawHtml) {
    return formatExplanationPopupContent(rawHtml, window.errorExplanations, {
      ignoreNames: ERROR_EXPLAIN_IGNORE,
    });
  }

  function formatUnmetNeedsPopupContent(rawHtml) {
    return formatExplanationPopupContent(
      rawHtml,
      window.unmetNeedsExplanations,
      {
        prefixUnderscore: true,
      },
    );
  }

  function formatWarningPopupContent(rawHtml) {
    return formatExplanationPopupContent(rawHtml, window.warningExplanations, {
      includeRaw: true,
    });
  }

  function toggleChildRow(table, td, html) {
    var row = table.row($(td).closest("tr"));
    if (row.child.isShown()) {
      row.child.hide();
    } else {
      row.child(html).show();
    }
  }

  /**
   * Attach click handlers for expandable / popup columns on the Sessions
   * summary DataTable. Column indexes must stay in sync with generate_summary_table().
   */
  window.initSummaryTableCallbacks = function (table) {
    if (!table || !$) return;

    // error column: popup with explanation when name matches the sheet
    // (compatibilityNotMet is excluded — details are in unmetNeeds)
    table.on("click", "td.errorC-control", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data) return;
      var raw = data[18];
      if (!hasErrorDetail(raw)) return;
      showSummaryDetailModal("Error", formatErrorPopupContent(raw));
    });

    // unmetNeeds / longTaskSec: look up by header (indexes shift when columns change)
    var unmetIdx = -1;
    var longTaskIdx = -1;
    var cameraAttemptIdx = -1;
    table.columns().every(function () {
      var header = $(this.header()).text().trim();
      if (header === "unmetNeeds") unmetIdx = this.index();
      if (header === "longTaskSec") longTaskIdx = this.index();
      if (header === "cameraAttempt") cameraAttemptIdx = this.index();
    });
    watchSummaryDetailCursors(table, [
      { index: 18, predicate: hasErrorDetail },
      { index: 19, predicate: hasSummaryDetail },
      { index: unmetIdx, predicate: hasSummaryDetail },
      {
        index: longTaskIdx,
        predicate: function (raw) {
          return longTaskSecValues(raw).length > 0;
        },
      },
      { index: cameraAttemptIdx, predicate: hasCameraAttemptDetail },
      { index: 34, predicate: hasSummaryDetail },
      { index: 35, predicate: hasSummaryDetail },
      { index: 36, predicate: hasSummaryDetail },
      { index: 40, predicate: hasSummaryDetail },
      { index: 2, predicate: hasProlificInfoDetail },
      { index: 3, predicate: hasProlificInfoDetail },
      { index: 4, predicate: hasProlificInfoDetail },
    ]);

    if (unmetIdx >= 0 || longTaskIdx >= 0 || cameraAttemptIdx >= 0) {
      table.on("click", "tbody td", function () {
        var cell = table.cell(this);
        if (!cell || !cell.index) return;
        var info = cell.index();
        if (!info) return;
        var rowData = table.row($(this).closest("tr")).data();
        if (!rowData) return;
        if (info.column === cameraAttemptIdx) {
          if (!hasCameraAttemptDetail(rowData[cameraAttemptIdx])) return;
          showSummaryDetailModal(
            "Camera attempt",
            formatCameraAttemptPopupContent(rowData[cameraAttemptIdx]),
          );
          return;
        }
        if (info.column === unmetIdx) {
          if (!hasSummaryDetail(rowData[unmetIdx])) return;
          showSummaryDetailModal(
            "unmetNeeds",
            formatUnmetNeedsPopupContent(rowData[unmetIdx]),
          );
          return;
        }
        if (info.column === longTaskIdx) {
          var raw = rowData[longTaskIdx];
          if (!longTaskSecValues(raw).length) return;
          showSummaryDetailModal(
            "longTaskSec",
            formatLongTaskSecPopupContent(raw),
          );
        }
      });
    }

    // warning column: same modal interaction as error — raw details plus
    // Sheet explanations when available, raw details alone otherwise.
    table.on("click", "td.warnC-control", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data) return;
      var raw = data[19];
      if (!hasSummaryDetail(raw)) return;
      showSummaryDetailModal("Warning", formatWarningPopupContent(raw));
    });

    // computer51Deg
    table.on("click", "td.computer51Deg", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data || !hasSummaryDetail(data[34])) return;
      toggleChildRow(table, this, "<p>" + data[34] + "</p>");
    });

    // loudspeakerSurvey
    table.on("click", "td.loudspeakerSurvey", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data || !hasSummaryDetail(data[35])) return;
      toggleChildRow(table, this, "<p>" + data[35] + "</p>");
    });

    // microphoneSurvey
    table.on("click", "td.microphoneSurvey", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data || !hasSummaryDetail(data[36])) return;
      toggleChildRow(table, this, "<p>" + data[36] + "</p>");
    });

    // comment
    table.on("click", "td.comment", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data || !hasSummaryDetail(data[40])) return;
      toggleChildRow(table, this, "<p>" + data[40] + "</p>");
    });

    // prolific / info columns: all three open the combined details, so each
    // is clickable when any of those fields has content.
    var formatInfo = function (d) {
      return "<p>" + d[2] + "</p> <p>" + d[3] + "</p> <p>" + d[4] + "</p>";
    };
    var showInfo = function (td) {
      var data = table.row($(td).closest("tr")).data();
      if (!data || !hasProlificInfoDetail(null, data)) return;
      toggleChildRow(table, td, formatInfo(data));
    };

    table.on("click", "td.information-control1", function () {
      showInfo(this);
    });
    table.on("click", "td.information-control2", function () {
      showInfo(this);
    });
    table.on("click", "td.information-control3", function () {
      showInfo(this);
    });

    $('div.has-feedback input[type="search"]').attr("placeholder", "");

    $("#search").keyup(function () {
      table.search($(this).val()).draw();
    });
  };
})(window, window.jQuery);
