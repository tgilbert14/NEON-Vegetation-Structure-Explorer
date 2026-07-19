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

console.log(`Browser contracts OK: ${handlers.length} one-payload handlers, local dependencies, and accessible loading state.`);
