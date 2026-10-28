// Run: node --test tests/summaryTableWarnings.test.js
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const root = path.join(__dirname, "..");
const read = (rel) => fs.readFileSync(path.join(root, rel), "utf8");

function loadSummaryTable(jQuery) {
  const window = { jQuery };
  const document = { readyState: "loading", addEventListener() {} };
  window.document = document;
  vm.runInContext(
    read("www/summaryTable.js"),
    vm.createContext({ window, document, jQuery }),
  );
  return window;
}

function warningClickHarness(raw, headers = {}) {
  const handlers = {};
  const drawHandlers = {};
  const modal = { title: "", body: "", shown: false };
  const child = { html: "", shown: false };
  const cells = Array.from({ length: 41 }, (_, column) => ({
    __column: column,
    classes: new Set(),
  }));
  let modalExists = false;
  const rowData = [];
  rowData[18] = raw;
  rowData[19] = raw;

  function $(arg) {
    if (arg && arg.__column !== undefined) {
      return {
        closest: () => ({}),
        toggleClass: (name, state) => {
          if (state) arg.classes.add(name);
          else arg.classes.delete(name);
        },
      };
    }
    if (arg && arg.__header) return { text: () => arg.text };
    if (arg === "body") return { append: () => { modalExists = true; } };
    if (arg === "#summaryDetailModal") {
      return {
        length: modalExists ? 1 : 0,
        modal: () => { modal.shown = true; },
      };
    }
    if (arg === "#summaryDetailModalTitle") {
      return { text: (value) => { modal.title = value; } };
    }
    if (arg === "#summaryDetailModalBody") {
      return { html: (value) => { modal.body = value; } };
    }
    return {
      on: () => {},
      attr: () => {},
      keyup: () => {},
      closest: () => ({}),
    };
  }
  const childApi = (html) => ({
    show: () => { child.html = html; child.shown = true; },
    hide: () => { child.shown = false; },
  });
  childApi.isShown = () => child.shown;
  const table = {
    column: (index) => ({
      nodes: () => ({
        to$: () => ({
          css: () => {},
          each: (fn) => { fn.call(cells[index], 0, cells[index]); },
        }),
      }),
    }),
    columns: () => ({
      every: (fn) => {
        for (const [index, text] of Object.entries(headers)) {
          fn.call({
            index: () => Number(index),
            header: () => ({ __header: true, text }),
          });
        }
      },
    }),
    on: (event, selector, fn) => {
      if (typeof selector === "function") {
        (drawHandlers[event] ||= []).push(selector);
      } else handlers[selector] = fn;
    },
    row: () => ({ data: () => rowData, child: childApi }),
    cell: (td) => ({ index: () => ({ column: td.__column }) }),
  };
  const window = loadSummaryTable($);
  window.initSummaryTableCallbacks(table);
  return { handlers, drawHandlers, modal, child, cells, rowData, window };
}

test("warning cells are underlined only when a Sheet explanation matches", () => {
  const window = loadSummaryTable();
  window.warningExplanations = {
    "tabHiddenAt:": "The participant switched away from the study tab.",
    "tabVisibleAfterMs:": "The participant returned after the stated absence.",
  };
  const explained = window.formatWarningCellDisplay("tabHiddenAt:titlePage");
  assert.match(explained, /warning-has-explanation/);
  assert.match(explained, /tabHiddenAt:titlePage/);
  assert.match(
    window.formatWarningCellDisplay(
      "tabHiddenAt:titlePage<br>tabVisibleAfterMs:17000 at:titlePage",
    ),
    /warning-has-explanation/,
  );
  assert.doesNotMatch(
    window.formatWarningCellDisplay("an undocumented warning"),
    /warning-has-explanation/,
  );
});

test("explained warning clicks show raw details and Sheet explanations", () => {
  const h = warningClickHarness("tabHiddenAt:titlePage");
  h.window.warningExplanations = {
    "tabHiddenAt:": "The participant switched away from the study tab.",
  };
  h.handlers["td.warnC-control"].call({});
  assert.equal(h.modal.title, "Warning");
  assert.equal(h.modal.shown, true);
  assert.match(h.modal.body, /tabHiddenAt:titlePage/);
  assert.match(h.modal.body, /switched away from the study tab/);
  assert.equal(h.child.shown, false);
});

test("undocumented warning clicks open a raw-details modal", () => {
  const h = warningClickHarness("an undocumented warning");
  h.handlers["td.warnC-control"].call({});
  assert.equal(h.modal.title, "Warning");
  assert.equal(h.modal.shown, true);
  assert.equal(h.modal.body, "<p>an undocumented warning</p>");
  assert.equal(h.child.shown, false);
});

test("empty warning clicks do nothing", () => {
  const h = warningClickHarness("");
  h.handlers["td.warnC-control"].call({});
  assert.equal(h.modal.shown, false);
  assert.equal(h.child.shown, false);
});

test("recalibration explanation does not claim unrelated Block warnings", () => {
  const firstCsvCell = (line) => {
    if (!line.startsWith('"')) return line.split(",")[0];
    let value = "";
    for (let i = 1; i < line.length; i++) {
      if (line[i] !== '"') value += line[i];
      else if (line[i + 1] === '"') { value += '"'; i++; }
      else break;
    }
    return value;
  };
  const names = read("data/warning_explanations.csv")
    .split(/\r?\n/)
    .slice(1)
    .filter(Boolean)
    .map(firstCsvCell);
  const window = loadSummaryTable();
  window.warningExplanations = Object.fromEntries(
    names.map((name) => [name, "explanation"]),
  );
  assert.doesNotMatch(
    window.formatWarningCellDisplay("Block 2 was skipped for an unrelated reason"),
    /warning-has-explanation/,
  );
  assert.match(
    window.formatWarningCellDisplay(
      "Block 2 restarted after mid-experiment recalibration: earlier rows used the old distance",
    ),
    /warning-has-explanation/,
  );
});

test("Error popup formatting remains unchanged when raw inclusion is optional", () => {
  const h = warningClickHarness("fullscreenExit");
  h.window.errorExplanations = { fullscreenExit: "Participant quit." };
  h.handlers["td.errorC-control"].call({});
  assert.equal(h.modal.title, "Error");
  assert.equal(
    h.modal.body,
    "<p><strong>fullscreenExit</strong><br>Participant quit.</p>",
  );
});

test("Warning popup delegates raw-plus-explanation formatting in one scan", () => {
  const src = read("www/summaryTable.js");
  const match = src.match(
    /function formatWarningPopupContent\(rawHtml\) \{([\s\S]*?)\n  \}/,
  );
  assert.ok(match);
  assert.doesNotMatch(match[1], /findAllNameMatches/);
  assert.match(match[1], /includeRaw:\s*true/);
});

test("Shiny can refresh the Warning explanation map", () => {
  const src = read("www/summaryTable.js");
  assert.match(src, /Shiny\.addCustomMessageHandler\("setWarningExplanations"/);
});

test("R, DataTable, and CSS warning-explanation paths are wired", () => {
  assert.match(read("server.R"), /setWarningExplanations/);
  assert.match(read("ui.R"), /warning_explanations_json\(\)/);
  assert.match(read("R/report/summary_table.R"), /formatWarningCellDisplay/);
  assert.match(read("www/ui.css"), /warning-has-explanation/);
  assert.match(read("R/report/warning_explanations.R"), /load_warning_explanations/);
});

test("detail cursor uses one shared class, never unconditional inline pointers", () => {
  const css = read("www/ui.css");
  assert.match(css, /\.summary-clickable[\s\S]*?cursor:\s*pointer;/);
  const js = read("www/summaryTable.js");
  assert.doesNotMatch(js, /\.css\(\{ cursor: "pointer" \}\)/);
});

test("Warning cursor tracks actual detail across redraws, including whitespace", () => {
  const h = warningClickHarness("tabHiddenAt:titlePage");
  assert.equal(h.cells[19].classes.has("summary-clickable"), true);
  h.rowData[19] = "   ";
  for (const fn of h.drawHandlers["draw.dt"] || []) fn();
  assert.equal(h.cells[19].classes.has("summary-clickable"), false);
  h.handlers["td.warnC-control"].call(h.cells[19]);
  assert.equal(h.modal.shown, false);
});

for (const [index, selector, name] of [
  [34, "td.computer51Deg", "computer51Deg"],
  [35, "td.loudspeakerSurvey", "loudspeakerSurvey"],
  [36, "td.microphoneSurvey", "microphoneSurvey"],
  [40, "td.comment", "comment"],
]) {
  test(`${name} uses the shared detail guard for cursor and expansion`, () => {
    const h = warningClickHarness("");
    h.rowData[index] = "details";
    for (const fn of h.drawHandlers["draw.dt"] || []) fn();
    assert.equal(h.cells[index].classes.has("summary-clickable"), true);
    h.handlers[selector].call(h.cells[index]);
    assert.equal(h.child.shown, true);
    h.child.shown = false;
    h.child.html = "";
    h.rowData[index] = "  ";
    for (const fn of h.drawHandlers["draw.dt"] || []) fn();
    assert.equal(h.cells[index].classes.has("summary-clickable"), false);
    h.handlers[selector].call(h.cells[index]);
    assert.equal(h.child.shown, false);
  });
}

test("unmetNeeds and longTaskSec use content-aware cursor and click guards", () => {
  const h = warningClickHarness("", { 17: "unmetNeeds", 20: "longTaskSec" });
  h.rowData[17] = "_needMemoryGB";
  h.rowData[20] = "1.2, 3.4";
  for (const fn of h.drawHandlers["draw.dt"] || []) fn();
  assert.equal(h.cells[17].classes.has("summary-clickable"), true);
  assert.equal(h.cells[20].classes.has("summary-clickable"), true);
  h.handlers["tbody td"].call(h.cells[17]);
  assert.equal(h.modal.title, "unmetNeeds");
  h.modal.shown = false;
  h.rowData[17] = "  ";
  for (const fn of h.drawHandlers["draw.dt"] || []) fn();
  assert.equal(h.cells[17].classes.has("summary-clickable"), false);
  h.handlers["tbody td"].call(h.cells[17]);
  assert.equal(h.modal.shown, false);
  h.handlers["tbody td"].call(h.cells[20]);
  assert.equal(h.modal.title, "longTaskSec");
});

test("info columns point when any included field has detail, never when all are empty", () => {
  const h = warningClickHarness("");
  h.rowData[3] = "Prolific ID";
  for (const fn of h.drawHandlers["draw.dt"] || []) fn();
  for (const index of [2, 3, 4])
    assert.equal(h.cells[index].classes.has("summary-clickable"), true);
  for (const index of [2, 3, 4]) h.rowData[index] = " ";
  for (const fn of h.drawHandlers["draw.dt"] || []) fn();
  for (const index of [2, 3, 4])
    assert.equal(h.cells[index].classes.has("summary-clickable"), false);
  h.handlers["td.information-control1"].call(h.cells[2]);
  assert.equal(h.child.shown, false);
});

test("compatibilityNotMet remains nonclickable in the Error column", () => {
  const h = warningClickHarness("compatibilityNotMet");
  for (const fn of h.drawHandlers["draw.dt"] || []) fn();
  assert.equal(h.cells[18].classes.has("summary-clickable"), false);
  h.handlers["td.errorC-control"].call(h.cells[18]);
  assert.equal(h.modal.shown, false);
});

test("checked-in Warning explanations bootstrap the app before Sheet creation", () => {
  const csv = read("data/warning_explanations.csv");
  assert.match(csv, /^name,explanation\r?\n/);
  assert.match(csv, /tabHiddenAt:/);
  assert.match(csv, /quitPsychoJS re-entry after termination ignored;/);
});
