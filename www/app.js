/* =========================================================================
   app.js — NEON Vegetation Structure Explorer
   count-up stat counters, celebratory confetti, loading overlay, the on-demand
   guided tour, and the Shiny custom-message handlers. (Forest theme.)
   ========================================================================= */

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
  // A freshly-rendered hero counter means a site just finished loading — the
  // most reliable signal to dismiss the loading overlay (no reliance on a
  // custom Shiny message, which doesn't always register in time).
  if (typeof smtLoadDone === "function") smtLoadDone();
  const target = parseFloat(el.getAttribute("data-target")) || 0;
  const suffix = el.dataset.suffix || "";
  const isFloat = !Number.isInteger(target);
  const fmt = (v) => (isFloat ? v.toFixed(1) : Math.round(v).toLocaleString()) + suffix;
  if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    el.textContent = fmt(target); return;
  }
  const dur = 900;
  const start = performance.now();
  function tick(now) {
    const t = Math.min(1, (now - start) / dur);
    const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
    el.textContent = fmt(target * eased);
    if (t < 1) requestAnimationFrame(tick);
    else el.textContent = fmt(target);
  }
  requestAnimationFrame(tick);
}

function runCounters() {
  document.querySelectorAll(".count-up").forEach(animateCount);
}
// rAF-coalesce so a burst of body mutations (pin drags, plotly relayouts, DT
// redraws) triggers at most one full-document scan per frame.
var countPending = false;
function runCountersSoon() {
  if (countPending) return;
  countPending = true;
  requestAnimationFrame(function () { countPending = false; runCounters(); });
}

const heroObserver = new MutationObserver(runCountersSoon);
document.addEventListener("DOMContentLoaded", function () {
  heroObserver.observe(document.body, { childList: true, subtree: true });
  runCounters();
});

// ---- confetti on standout trees (biggest / tallest / record grower) ------
function forestConfetti(big) {
  if (typeof confetti !== "function") return;
  // DDL desert-night palette: teal, coral, gold, bright-teal, sky.
  const colors = ["#2dd4bf", "#fb8a7e", "#ffd24a", "#5eead4", "#43b8e8"];
  const burst = (opts) => confetti(Object.assign({ colors, disableForReducedMotion: true }, opts));
  burst({ particleCount: big ? 140 : 70, spread: big ? 100 : 70, origin: { y: 0.3 }, startVelocity: 42 });
  if (big) {
    setTimeout(() => burst({ particleCount: 80, angle: 60, spread: 70, origin: { x: 0 } }), 180);
    setTimeout(() => burst({ particleCount: 80, angle: 120, spread: 70, origin: { x: 1 } }), 320);
  }
}

// ---- loading overlay (opaque, indeterminate) -----------------------------
// A site load is one synchronous blocking call (decompress + clean + build the
// stand/size profiles) whose duration we can't know, so we show an INDETERMINATE
// animated bar (no fake %) on an OPAQUE backdrop, raised client-side on the click.
var smtSafetyTimer = null;
function smtLoadStart(label) {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  var siteText = label || "";
  if (!siteText) {
    var sel = document.getElementById("site");
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  }
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  ov.style.display = "flex";
  if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }  // tactile "got it"
  clearTimeout(smtSafetyTimer);
  smtSafetyTimer = setTimeout(function () {  // safety net so it can never stick
    var note = document.querySelector(".load-note");
    if (note) note.textContent = "Still working — building the stand-structure and size profiles. You can close this and try again.";
    setTimeout(smtLoadDone, 4000);
  }, 25000);
}
function smtLoadDone() {
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  if (ov) ov.style.display = "none";
}

// ---- guided tour (driver.js) — ON DEMAND only (no auto-fire) --------------
function vegTour() {
  if (!window.driver || !window.driver.js) {
    if (typeof Swal !== "undefined") Swal.fire({ toast: true, position: "top-end",
      icon: "info", title: "Tour unavailable offline", showConfirmButton: false, timer: 2200 });
    return;
  }
  var D = window.driver.js.driver;
  var steps = [
    { element: ".site-cards", popover: { title: "Pick a forest", side: "bottom",
        description: "Tap a <b>site card</b> to load it, or open the Harvard Forest demo below — it loads instantly." } },
    { element: "#demoBtn2", popover: { title: "In a hurry?", side: "top",
        description: "Jump straight into the <b>Harvard Forest</b> demo — a New England mixed hardwood–conifer stand." } },
    { element: ".home-nav", popover: { title: "Five ways in", side: "bottom",
        description: "From the Overview, jump to <b>Stand Structure</b>, <b>Growth &amp; Mortality</b>, the <b>Forest Size Lab</b>, a single <b>Tree Career</b>, or the <b>Map</b>." } },
    { element: ".home-btn-star", popover: { title: "The Forest Size Lab", side: "top",
        description: "Every tree is a dot in <b>diameter × height</b> space. <b>Tap a dot</b> to pin its card, drag &amp; resize, then download the chart with the cards on it." } }
  ].filter(function (s) { return document.querySelector(s.element); });
  if (!steps.length) return;
  var d = D({ showProgress: true, allowClose: true, steps: steps, popoverClass: "driverjs-theme",
    nextBtnText: "Next", prevBtnText: "Back", doneBtnText: "Got it" });
  d.drive();
}

// ---- dismiss any open info popover (click-outside + Esc) -----------------
function smtClosePopovers() {
  document.querySelectorAll(".popover").forEach(function (pop) {
    var trig = pop.id ? document.querySelector('[aria-describedby="' + pop.id + '"]') : null;
    if (trig && window.bootstrap && bootstrap.Popover) {
      var inst = bootstrap.Popover.getInstance(trig);
      if (inst) { inst.hide(); return; }
    }
    pop.remove();
  });
}
document.addEventListener("click", function (e) {
  if (e.target.closest(".popover") || e.target.closest(".info-dot") ||
      e.target.closest("bslib-popover")) return;
  if (document.querySelector(".popover")) smtClosePopovers();
});
document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") smtClosePopovers();
});

// ---- Shiny custom message handlers ---------------------------------------
document.addEventListener("DOMContentLoaded", function () {
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("countUp", function () { setTimeout(runCounters, 60); });
    Shiny.addCustomMessageHandler("confetti", function (msg) { forestConfetti(msg && msg.big); });
    Shiny.addCustomMessageHandler("loadDone", function () { smtLoadDone(); });
    Shiny.addCustomMessageHandler("smtLoadStart", function (msg) { smtLoadStart(msg && msg.label); });
    Shiny.addCustomMessageHandler("startTour", function () { setTimeout(vegTour, 150); });
    // remember the current site so exported PNG filenames are self-describing
    Shiny.addCustomMessageHandler("siteCtx", function (msg) { window.__vegSite = msg && msg.site; });
  }
});

// Re-fit any Leaflet map the moment its tab becomes visible (hidden-init blank fix).
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} }, 60);
});
