#!/usr/bin/env node

import { readFileSync } from "node:fs";

const files = ["www/app.js", "www/pincards.js"];
const pattern = /Shiny\.addCustomMessageHandler\(\s*["']([^"']+)["']\s*,\s*function\s*\(([^)]*)\)/g;
const handlers = [];
const invalid = [];
for (const file of files) {
  const source = readFileSync(file, "utf8");
  for (const match of source.matchAll(pattern)) {
    const parameters = match[2].split(",").map((value) => value.trim()).filter(Boolean);
    handlers.push(match[1]);
    if (parameters.length !== 1) invalid.push(`${file}: ${match[1]} (${parameters.length} parameters)`);
  }
}
if (!handlers.length) throw new Error("no Shiny custom message handlers found");
if (new Set(handlers).size !== handlers.length) {
  throw new Error(`duplicate Shiny custom message handler: ${handlers.join(", ")}`);
}
if (invalid.length) {
  throw new Error(`every Shiny custom message handler must accept one payload argument:\n${invalid.join("\n")}`);
}

const ui = readFileSync("ui.R", "utf8");
const app = readFileSync("www/app.js", "utf8");
const server = readFileSync("server.R", "utf8");
const mapPicker = readFileSync("R/map_picker.R", "utf8");
const styles = readFileSync("www/styles.css", "utf8");
const vegStyles = readFileSync("www/veg.css", "utf8");
const requireText = (source, needle, message) => {
  if (!source.includes(needle)) throw new Error(message);
};
for (const forbidden of [
  /fonts\.googleapis\.com/i, /fonts\.gstatic\.com/i, /cdnjs\.cloudflare\.com/i,
  /unpkg\.com/i, /cdn\.jsdelivr\.net/i,
]) {
  if (forbidden.test(ui)) throw new Error(`ui.R contains a cold-start/browser CDN dependency: ${forbidden}`);
}
for (const local of [
  "vendor/sweetalert2/sweetalert2.min.css", "vendor/sweetalert2/sweetalert2.all.min.js",
  "vendor/html-to-image/html-to-image.js", "vendor/driver/driver.css",
  "vendor/driver/driver.js.iife.js",
]) {
  if (!ui.includes(local)) throw new Error(`ui.R does not load vendored dependency ${local}`);
}
if (!/id\s*=\s*["']loadOverlay["']/.test(ui) ||
    !/role\s*=\s*["']dialog["']/.test(ui) ||
    !/aria-modal/.test(ui) || !/aria-live/.test(ui)) {
  throw new Error("loading overlay must expose dialog, modal, and live-region semantics");
}
if (/setTimeout\([^)]*smtLoadDone/s.test(app)) {
  throw new Error("loading overlay must not auto-dismiss before the server reports completion");
}
for (const token of [
  "smtSetLoadBoundary(true)", "smtSetLoadBoundary(false)",
  "if (smtInertState.length) return",
  "if (!wasOpen && !smtInertState.length) return",
  'e.key !== "Tab"', "smtCanReceiveFocus(smtLastFocus)",
  "node !== document.body", "function smtDismissModalForLoad()",
]) {
  requireText(app, token, `loading modal focus boundary is missing ${token}`);
}
if (!/querySelectorAll\("\.app-skip-link, \.top-bar, #appMain, \.modal, \.modal-backdrop"\)/.test(app) ||
    !/setAttribute\("inert",\s*""\)/.test(app)) {
  throw new Error("loading dialog must make every background application region inert");
}
if (!/animate:\s*!reduceMotion/.test(app) ||
    !/prefers-reduced-motion:\s*reduce/.test(app) ||
    !/@media\s*\(prefers-reduced-motion:\s*reduce\)[\s\S]*?\.driver-fade \.driver-overlay,[\s\S]*?animation:\s*none\s*!important/.test(styles)) {
  throw new Error("guided-tour motion must be disabled when reduced motion is requested");
}
if (/lpa-trust|real tagged plants|public measurements/i.test(ui)) {
  throw new Error("in-app Living Poster must not carry an above-fold metric/trust strip");
}
if (!/class\s*=\s*["']lpa-skip["'][^\n]*Pick a place/.test(ui)) {
  throw new Error("in-app Living Poster must retain its single Pick a place CTA");
}
for (const asset of [
  "assets/vegetation-living-poster-840.webp",
  "assets/vegetation-living-poster.webp",
  "assets/vegetation-living-poster.png",
]) {
  if (!ui.includes(asset)) throw new Error(`in-app Living Poster is missing responsive art asset ${asset}`);
}

// Evidence and non-scientist pathways are release contracts, not optional copy.
for (const field of [
  "n_measurement_only_contexts",
  "n_measurement_records_without_opportunity_source",
  "measurement_records_all_growth_forms",
  "absence_inferred = FALSE",
]) {
  requireText(server, field, `site-wide source-gap evidence is missing ${field}`);
}
requireText(server, 'output$sourceGapCsv', "source-gap notice lacks its exact CSV evidence path");
requireText(server, 'output$plotSummaryCsv', "active-channel plot summary lacks a standalone CSV evidence path");
requireText(ui, 'downloadButton("plotSummaryCsv"', "standalone plot-summary CSV is missing from the Place tools");
requireText(server, "plots_export(rv$snap, rv$plots, SP(), rv$meta)", "plot export frame must use the active physical channel");
if ((server.match(/active_plots_export\(\)/g) || []).length !== 2) {
  throw new Error("standalone and ZIP plot CSVs must consume the same active_plots_export frame");
}
requireText(server, "every recorded plant form", "source-gap notice must state its all-growth-form scope");
requireText(server, "never read as zero or plant absence", "source-gap notice must reject zero/absence inference");
if (/sidebar picker/i.test(server)) throw new Error("visible plant-picker guidance still refers to the removed sidebar");

for (const className of ["help-steps", "help-step-number", "help-methods"]) {
  requireText(server, className, `plain-language Help path is missing ${className}`);
}
requireText(server, "basal_unaligned", "multi-stem basal career evidence guard is missing");
requireText(server, "Measured · stems not aligned for change", "multi-stem evidence label is not plain-language or internally consistent");
requireText(server, "measurement_unaligned", "changed measurement points must guard the career trajectory and evidence state");
requireText(server, "Measured · measurement point changed", "measurement-point withholding needs a plain-language evidence label");
if (!/baBar_click\s*<-\s*reactive\s*\(\{[\s\S]*?req\(rv\$site,\s*rv\$snap,\s*rv\$plots,\s*rv\$spec\)[\s\S]*?event_data\("plotly_click",\s*source\s*=\s*"baBar",\s*priority\s*=\s*"event"\)/.test(server)) {
  throw new Error("baBar click handling must wait for a loaded site before registering its Plotly source");
}
if (/observeEvent\(event_data\("plotly_click",\s*source\s*=\s*"baBar"\)/.test(server)) {
  throw new Error("baBar must not request a hidden Plotly source during the landing-page flush");
}

// 320px layout and export-source card must shrink without changing its 340px cap.
if (!/\.trade-card\s*\{[^}]*width:\s*min\(340px,\s*100%\)[^}]*max-width:\s*340px/s.test(styles)) {
  throw new Error("Plant Career card must shrink to its container while preserving the 340px export cap");
}
if (!/\.thresh-slider\s*\{[^}]*min-width:\s*0[^}]*max-width:\s*100%/s.test(styles)) {
  throw new Error("threshold controls must be allowed to shrink at 320px");
}
if (!/\[data-bs-theme="dark"\]\s+\.evidence-chip\.comparable/.test(styles) ||
    !/\[data-bs-theme="dark"\]\s+\.evidence-chip\.held/.test(styles)) {
  throw new Error("dark mode needs explicit readable evidence-chip colors");
}
if (!/\[data-bs-theme="dark"\]\s+\.help-step-number\s*\{[^}]*color:\s*#0e1d15/s.test(vegStyles) ||
    !/\[data-bs-theme="dark"\]\s+\.smt-snap-btn/.test(styles) ||
    !/\[data-bs-theme="dark"\]\s+\.driver-popover\.driverjs-theme\s+button/.test(styles)) {
  throw new Error("dark-mode canopy controls must use readable dark ink");
}
for (const selector of [".home-btn", ".popover .popover-header", ".modal-header"]) {
  const escaped = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  if (!new RegExp(`\\[data-bs-theme="dark"\\][\\s\\S]{0,900}${escaped}[\\s\\S]{0,240}color:\\s*#0e1d15`).test(styles)) {
    throw new Error(`${selector} must use dark ink on the bright dark-mode canopy surface`);
  }
}
if (!/\[data-bs-theme="dark"\]\s+\.dataTables_wrapper\s+\.dataTables_filter\s+input\s*\{[^}]*background:\s*#0d2016/s.test(styles) ||
    !/\[data-bs-theme="dark"\]\s+table\.dataTable\s+tbody\s+tr:hover[^\{]*\{[^}]*background:\s*#16412a/s.test(styles)) {
  throw new Error("dark-mode DataTables controls and row states need dark readable surfaces");
}
if (!/--pine:\s*#256f41/.test(styles) || !/--green:\s*#256f41/.test(styles)) {
  throw new Error("normal green text must use the AA-safe canopy token");
}
for (const selector of [".tc-save-btn", ".smt-pin .smt-open", ".smt-pin-key-btn", ".smt-snap-btn"]) {
  const escaped = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  if (!new RegExp(`${escaped}[^}]*min-height:\\s*44px`, "s").test(styles)) {
    throw new Error(`${selector} must expose a 44px touch target`);
  }
}
const pinCards = readFileSync("www/pincards.js", "utf8");
for (const token of [
  "window.smtPinViewed", 'item.name === "★ viewing"',
  "pin.tabIndex = 0", 'document.createElement("button")', 'grip.type = "button"',
  'grip.setAttribute("role", "slider")', 'grip.setAttribute("aria-valuenow"',
  '"Pinned plant card for " + plantIdentity', "pin.__returnFocus", "pin.focus()",
  'grip.addEventListener("keydown"', 'pin.setAttribute("role", "group")',
  'pin.addEventListener("keydown"', "applyPinScale(pin",
]) {
  requireText(pinCards, token, `keyboard pin creation/move/resize contract is missing ${token}`);
}
requireText(ui, 'onclick = "smtPinViewed(\'labScatter\')"',
  "Size Lab needs a keyboard-operable Pin viewed plant control");
requireText(ui, 'selectizeInput("labTreeSel"', "Size Lab needs its own keyboard plant selector");
requireText(server, "size_lab_rows(one, rv$spec)", "Size Lab selector must use the chart's exact eligible plants");
requireText(server, "size_lab_rows(one, sp)", "Size Lab chart must share the selector eligibility helper");
requireText(server, 'observeEvent(input$labTreeSel', "Size Lab plant selection must not navigate away");
requireText(server, "smtDismissModalForLoad();smtLoadStart", "modal-footer loading must deactivate its focus trap first");
if (!/\.smt-pin-picker\s*\{[^}]*min-width:\s*0[^}]*max-width:\s*420px/s.test(styles) ||
    !/@media\s*\(max-width:\s*400px\)[\s\S]*?\.smt-pin-picker\s*\{[^}]*width:\s*100%/s.test(styles)) {
  throw new Error("Size Lab keyboard picker must shrink and stack at compact widths");
}
if (!/\.dropdown-menu\s+\.dropdown-item\s*\{[^}]*min-height:\s*44px/s.test(vegStyles) ||
    !/\.pg-actions\s+a\s*\{[^}]*min-height:\s*44px/s.test(vegStyles)) {
  throw new Error("gateway and overflow navigation targets must be at least 44px tall");
}
for (const contract of [
  [/\.channel-picker\s+\.radio-inline\s*\{[^}]*min-height:\s*44px/s, "measurement-channel choices"],
  [/\.leader-cats[\s\S]*min-height:\s*44px/, "Champion choices"],
  [/\.search-mode\s+\.radio-inline[^\{]*\{[^}]*min-height:\s*44px/s, "search-mode choices"],
  [/\.picker-mode\s+label\.radio-inline[^\{]*\{[^}]*min-height:\s*44px/s, "site/species choices"],
]) {
  if (!contract[0].test(`${styles}\n${vegStyles}`)) throw new Error(`${contract[1]} must expose 44px touch targets`);
}
if (/\.leader-cats\s+input\[type=radio\]\s*\{[^}]*display:\s*none/s.test(styles) ||
    !/\.leader-cats[\s\S]*focus-visible/.test(`${styles}\n${vegStyles}`)) {
  throw new Error("Champion radios must remain keyboard-focusable with a visible focus state");
}
for (const token of ["cloneNode(true)", "smt-export-card", "fixedWidth: 340"]) {
  requireText(pinCards, token,
    `Tree Card export lacks stable 340px source contract: ${token}`);
}
for (const token of ["function clampPin", "boxRect.width - pinRect.width - 4", "fitScale", "clampPin(pin, pin.offsetLeft, pin.offsetTop)"]) {
  requireText(readFileSync("www/pincards.js", "utf8"), token,
    `pinned cards must remain fully reachable after drag, resize, and viewport changes: ${token}`);
}

requireText(mapPicker, "hit_radius <- pmax(22, visible_radius)", "map markers need an invisible 44px-minimum hit target");
requireText(mapPicker, "pathOptions(interactive = FALSE)", "visible map rings must not steal the enlarged hit target");
for (const token of ["markerClusterOptions", "maxClusterRadius = 44", "spiderfyOnMaxZoom = TRUE", "STEI/TREE"]) {
  requireText(mapPicker, token, `dense national map targets lack disambiguation contract: ${token}`);
}
requireText(mapPicker, "addControl", "map measurement channels need a custom visible key");
requireText(mapPicker, "veg-channel-legend-ring", "map key must render ring patterns, not solid color swatches");
for (const label of ["Tree DBH · solid ring", "Shrub & sapling basal · dashed ring", "Held / unknown · dotted ring"]) {
  requireText(server, label, `map key is missing ${label}`);
}
requireText(vegStyles, ".veg-channel-legend", "map key needs light/dark readable styling");

console.log(`Browser contracts OK: ${handlers.length} one-payload handlers, local dependencies, accessible loading state, responsive controls, evidence paths, and map targets.`);
