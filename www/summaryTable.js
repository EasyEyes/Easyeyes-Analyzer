// Summary-table DataTables helpers. Wired from R via DT callback body:
//   if (window.initSummaryTableCallbacks) {
//     window.initSummaryTableCallbacks(table);
//   }
// (DT wraps that body in function(table) { ... }.)

(function (window, $) {
  "use strict";

  window.errorExplanations = window.errorExplanations || {};

  function setErrorExplanations(map) {
    window.errorExplanations = map && typeof map === "object" ? map : {};
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("setErrorExplanations", setErrorExplanations);
  } else {
    document.addEventListener("DOMContentLoaded", function () {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler("setErrorExplanations", setErrorExplanations);
      }
    });
    $(document).on("shiny:connected", function () {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler("setErrorExplanations", setErrorExplanations);
      }
    });
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
          "</div>"
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

  // Longest-name-first scan: find every sheet `name` that appears in the
  // error cell (comma-separated, <br>-separated, or embedded). Longer names
  // win over shorter prefixes (e.g. rc:camera… before rc:).
  function findAllErrorMatches(rawHtml) {
    var map = window.errorExplanations || {};
    var plain = String(rawHtml || "")
      .replace(/<br\s*\/?>/gi, "\n")
      .replace(/<[^>]+>/g, " ");
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
      // Allow match after whitespace, comma, semicolon, or newline.
      return /[\s,;]/.test(plain.charAt(idx - 1));
    }

    for (var i = 0; i < names.length; i++) {
      var name = names[i];
      if (!name) continue;
      var from = 0;
      while (from < plain.length) {
        var idx = plain.indexOf(name, from);
        if (idx < 0) break;
        var end = idx + name.length;
        if (!isTokenStart(idx) || overlaps(idx, end)) {
          from = idx + 1;
          continue;
        }
        matched.push({ name: name, explanation: map[name], start: idx, end: end });
        used.push([idx, end]);
        from = end;
      }
    }

    matched.sort(function (a, b) {
      return a.start - b.start;
    });
    return matched;
  }

  // Show an explanation for every matched name in the cell; if none match,
  // fall back to the original error text.
  function formatErrorPopupContent(rawHtml) {
    if (rawHtml == null || rawHtml === "" || rawHtml === "null") {
      return "<em>No details</em>";
    }

    var matches = findAllErrorMatches(rawHtml);
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
          "</p>"
      );
    }
    return out.join("<hr style=\"margin:8px 0;border:0;border-top:1px solid #ddd;\">");
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
    table.column(18).nodes().to$().css({ cursor: "pointer" });
    table.on("click", "td.errorC-control", function () {
      var data = table.row($(this).closest("tr")).data();
      if (!data) return;
      showSummaryDetailModal("Error", formatErrorPopupContent(data[18]));
    });

    // warning column: expand under row
    table.column(19).nodes().to$().css({ cursor: "pointer" });
    table.on("click", "td.warnC-control", function () {
      var data = table.row($(this).closest("tr")).data();
      toggleChildRow(table, this, "<p>" + data[19] + "</p>");
    });

    // computer51Deg
    table.column(34).nodes().to$().css({ cursor: "pointer" });
    table.on("click", "td.computer51Deg", function () {
      var data = table.row($(this).closest("tr")).data();
      toggleChildRow(table, this, "<p>" + data[34] + "</p>");
    });

    // loudspeakerSurvey
    table.column(35).nodes().to$().css({ cursor: "pointer" });
    table.on("click", "td.loudspeakerSurvey", function () {
      var data = table.row($(this).closest("tr")).data();
      toggleChildRow(table, this, "<p>" + data[35] + "</p>");
    });

    // microphoneSurvey
    table.column(36).nodes().to$().css({ cursor: "pointer" });
    table.on("click", "td.microphoneSurvey", function () {
      var data = table.row($(this).closest("tr")).data();
      toggleChildRow(table, this, "<p>" + data[36] + "</p>");
    });

    // comment
    table.column(40).nodes().to$().css({ cursor: "pointer" });
    table.on("click", "td.comment", function () {
      var data = table.row($(this).closest("tr")).data();
      toggleChildRow(table, this, "<p>" + data[40] + "</p>");
    });

    // prolific / info columns
    table.column(2).nodes().to$().css({ cursor: "pointer" });
    table.column(3).nodes().to$().css({ cursor: "pointer" });
    table.column(4).nodes().to$().css({ cursor: "pointer" });

    var formatInfo = function (d) {
      return "<p>" + d[2] + "</p> <p>" + d[3] + "</p> <p>" + d[4] + "</p>";
    };

    table.on("click", "td.information-control1", function () {
      toggleChildRow(table, this, formatInfo(table.row($(this).closest("tr")).data()));
    });
    table.on("click", "td.information-control2", function () {
      toggleChildRow(table, this, formatInfo(table.row($(this).closest("tr")).data()));
    });
    table.on("click", "td.information-control3", function () {
      toggleChildRow(table, this, formatInfo(table.row($(this).closest("tr")).data()));
    });

    $('div.has-feedback input[type="search"]').attr("placeholder", "");

    $("#search").keyup(function () {
      table.search($(this).val()).draw();
    });
  };
})(window, window.jQuery);
