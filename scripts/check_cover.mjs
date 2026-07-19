#!/usr/bin/env node

import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const html = readFileSync(resolve(root, "docs/index.html"), "utf8");
let failed = false;
const fail = (message) => { failed = true; console.error(`FAIL: ${message}`); };
const count = (pattern) => (html.match(pattern) || []).length;
const requireText = (pattern, message) => { if (!pattern.test(html)) fail(message); };

function pngDimensions(buffer) {
  const signature = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]);
  if (!buffer.subarray(0, 8).equals(signature) || buffer.length < 24) {
    throw new Error("not a valid PNG header");
  }
  return [buffer.readUInt32BE(16), buffer.readUInt32BE(20)];
}

if (count(/<h1\b/gi) !== 1) fail("cover must contain exactly one h1");
if (count(/<main\b/gi) !== 1) fail("cover must contain exactly one main landmark");
requireText(/<html\s+lang=["']en["']/i, "document language must be English");
requireText(/<a\b[^>]*class=["'][^"']*skip[^"']*["'][^>]*href=["']#main["']/i,
  "cover needs a skip link to #main");
requireText(/<nav\b[^>]+aria-label=["'][^"']+["']/i,
  "cover navigation needs an accessible label");
requireText(/<link\s+rel=["']canonical["']\s+href=["']https:\/\/tgilbert14\.github\.io\/NEON-Vegetation-Structure-Explorer\/["']\s*\/?>/i,
  "canonical Pages URL is missing or incorrect");
requireText(/property=["']og:image:width["']\s+content=["']1200["']/i,
  "Open Graph width must be 1200");
requireText(/property=["']og:image:height["']\s+content=["']630["']/i,
  "Open Graph height must be 630");
requireText(/property=["']og:image:alt["']\s+content=["'][^"']+["']/i,
  "Open Graph image needs alternative text");
requireText(/name=["']twitter:image:alt["']\s+content=["'][^"']+["']/i,
  "Twitter image needs alternative text");
requireText(/Tagged\.[\s\S]{0,120}Measured\.[\s\S]{0,120}Still changing\./i,
  "Living Poster hook is missing");
requireText(/Follow real trees and shrubs through years of change\./i,
  "Living Poster promise is missing");
requireText(/Editorial illustration[^<]*(not|isn.t) a field photograph/i,
  "generated art must be disclosed as editorial, not documentary");
requireText(/42 places/i, "cover must state its 42-place scope");
requireText(/DP1\.10098\.001/i, "cover must identify the source data product");
requireText(/Driver Cascade/i, "cover must identify the suite ambassador");
requireText(/stand numbers describe the sampled plots/i,
  "compact honesty note must identify sampled-plot support");
requireText(/not every tree across an entire landscape/i,
  "compact honesty note must reject wall-to-wall census interpretation");

const driverUrl = "https://tgilbert14.github.io/NEON-Driver-Cascade/";
if (!html.includes(driverUrl)) fail("cover must hand the full suite to Driver Cascade");

const companionUrls = [
  "NEON-Small-Mammal-Tracker-App",
  "NEON-Plant-Phenology-Explorer", "NEON-Plant-Diversity",
  "NEON-Breeding-Birds", "NEON-Ground-Beetle-Tracker", "NEON-Mosquito-Pulse",
  "NEON-My-Little-Inverts", "NEON-WaterChemistry-Analyte-Viewer-App",
];
for (const slug of companionUrls) {
  if (html.includes(`https://tgilbert14.github.io/${slug}/`)) {
    fail(`companion cover should point to Driver, not reproduce its suite directory: ${slug}`);
  }
}
for (const stalePattern of [/question-grid/i, /measure-card/i, /suite-rail/i]) {
  if (stalePattern.test(html)) fail(`stale long-form cover block remains: ${stalePattern}`);
}

for (const forbidden of [
  /fonts\.googleapis\.com/i, /fonts\.gstatic\.com/i, /cdnjs\.cloudflare\.com/i,
  /unpkg\.com/i, /jsdelivr\.net/i, /fetch\s*\(/i, /mode\s*:\s*["']no-cors["']/i,
  /(?:href|src)=["']http:\/\//i,
]) {
  if (forbidden.test(html)) fail(`forbidden cover runtime pattern: ${forbidden}`);
}

for (const match of html.matchAll(/<(?:img|source)\b[^>]+(?:src|srcset)=["']([^"']+)["'][^>]*>/gi)) {
  const relative = match[1].split(/\s+/)[0];
  if (/^(?:https?:|data:|\/\/)/i.test(relative)) continue;
  const path = resolve(root, "docs", relative.split(/[?#]/)[0]);
  if (!existsSync(path)) fail(`referenced cover image is missing: ${relative}`);
  if (/^<img\b/i.test(match[0]) && !/\balt=["'][^"']*["']/i.test(match[0])) {
    fail(`cover image lacks alt text: ${relative}`);
  }
}

try {
  const social = readFileSync(resolve(root, "docs/og-image.png"));
  const [width, height] = pngDimensions(social);
  if (width !== 1200 || height !== 630) fail(`og-image.png is ${width}x${height}, expected 1200x630`);
  if (social.length < 20_000 || new Set(social).size < 64) fail("og-image.png appears blank or placeholder-sized");
} catch (error) {
  fail(`docs/og-image.png: ${error.message}`);
}
if (!existsSync(resolve(root, "docs/social-card.html"))) {
  fail("the separately composed social-card source is missing");
}

for (const match of html.matchAll(/<a\b[^>]*target=["']_blank["'][^>]*>/gi)) {
  if (!/rel=["'][^"']*noopener[^"']*["']/i.test(match[0])) {
    fail("target=_blank link is missing rel=noopener");
  }
}

if (failed) process.exit(1);
console.log("Cover OK: concise Living Poster, Driver handoff, scope/honesty, local art, and 1200x630 social card passed.");
