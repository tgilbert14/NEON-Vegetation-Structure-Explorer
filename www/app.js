/* =========================================================================
   app.js — NEON Vegetation Structure Explorer
   count-up stat counters, accessible loading state, the on-demand guided tour,
   and the Shiny custom-message handlers.
   ========================================================================= */

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
  // A freshly-rendered hero counter means a site just finished loading — the
  // most reliable signal to dismiss the loading overlay (no reliance on a
  // custom Shiny message, which doesn't always register in time).
  if (typeof smtLoadDone === "function") smtLoadDone();
  const rawTarget = el.getAttribute("data-target") || "—";
  const target = parseFloat(rawTarget);
  if (!Number.isFinite(target)) {
    el.textContent = rawTarget;
    return;
  }
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

// ---- loading overlay (opaque, indeterminate) -----------------------------
// A site load is one synchronous blocking call (decompress + clean + build the
// stand/size profiles) whose duration we can't know, so we show an INDETERMINATE
// animated bar (no fake %) on an OPAQUE backdrop, raised client-side on the click.
var smtSafetyTimer = null;
var smtLastFocus = null;
var smtInertState = [];

function smtSetLoadBoundary(active) {
  var nodes = document.querySelectorAll(".app-skip-link, .top-bar, #appMain, .modal, .modal-backdrop");
  if (active) {
    // A load can announce itself from both the initiating control and Shiny.
    // Preserve the original boundary snapshot so a duplicate start cannot make
    // the app stay inert when the single matching completion arrives.
    if (smtInertState.length) return;
    smtInertState = [];
    nodes.forEach(function (node) {
      smtInertState.push({ node: node, hadInert: node.hasAttribute("inert") });
      node.setAttribute("inert", "");
    });
    return;
  }
  smtInertState.forEach(function (state) {
    if (state.node && !state.hadInert) state.node.removeAttribute("inert");
  });
  smtInertState = [];
}

// A Bootstrap/Shiny modal lives outside #appMain. Deactivate its focus trap
// synchronously before a modal-footer action opens the loading dialog; Shiny's
// removeModal() then performs the authoritative server-side cleanup.
function smtDismissModalForLoad() {
  var modal = document.querySelector(".modal.show");
  if (!modal) return;
  try {
    if (window.bootstrap && window.bootstrap.Modal) {
      var instance = window.bootstrap.Modal.getInstance(modal) ||
        window.bootstrap.Modal.getOrCreateInstance(modal);
      if (instance) instance.hide();
    } else if (window.jQuery) {
      window.jQuery(modal).modal("hide");
    }
  } catch (e) {}
  if (modal.contains(document.activeElement)) document.activeElement.blur();
  modal.classList.remove("show");
  modal.style.display = "none";
  modal.setAttribute("aria-hidden", "true");
  document.querySelectorAll(".modal-backdrop").forEach(function (node) { node.remove(); });
  document.body.classList.remove("modal-open");
}

function smtCanReceiveFocus(node) {
  return !!(node && node !== document.body && node !== document.documentElement &&
    node.isConnected && !node.closest("[inert]") &&
    node.getClientRects().length && !node.disabled);
}

function smtLoadStart(label) {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  if (ov.getAttribute("aria-hidden") !== "false") smtLastFocus = document.activeElement;
  var siteText = label || "";
  if (!siteText) {
    var sel = document.getElementById("site");
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  }
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  var note = document.querySelector(".load-note");
  if (note) note.textContent = "Opening the measurements and building this place's view.";
  ov.style.display = "flex";
  ov.setAttribute("aria-hidden", "false");
  ov.setAttribute("aria-busy", "true");
  smtSetLoadBoundary(true);
  try { ov.focus({ preventScroll: true }); } catch (e) { ov.focus(); }
  if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }  // tactile "got it"
  clearTimeout(smtSafetyTimer);
  smtSafetyTimer = setTimeout(function () {
    var slowNote = document.querySelector(".load-note");
    if (slowNote) slowNote.textContent = "Still working—this place has more measurements to open.";
  }, 25000);
}
function smtLoadDone() {
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  var wasOpen = !!(ov && ov.getAttribute("aria-hidden") === "false");
  // Both Shiny and each freshly rendered counter can report completion. Only
  // the first signal owns focus restoration; later signals must be no-ops.
  if (!wasOpen && !smtInertState.length) return;
  if (ov) {
    ov.style.display = "none";
    ov.setAttribute("aria-hidden", "true");
    ov.setAttribute("aria-busy", "false");
  }
  smtSetLoadBoundary(false);
  var activeTab = document.querySelector("#mainTabsWrap .nav-link.active");
  var target = smtCanReceiveFocus(smtLastFocus) ? smtLastFocus : activeTab;
  if (!smtCanReceiveFocus(target)) target = document.getElementById("appMain");
  if (smtCanReceiveFocus(target) && typeof target.focus === "function") {
    try { target.focus({ preventScroll: true }); } catch (e) { target.focus(); }
  }
  smtLastFocus = null;
}

// The overlay has no interactive choices: while its aria-modal dialog is open,
// Tab remains on the progress announcement and background regions are inert.
document.addEventListener("keydown", function (e) {
  var ov = document.getElementById("loadOverlay");
  if (!ov || ov.getAttribute("aria-hidden") !== "false" || e.key !== "Tab") return;
  e.preventDefault();
  ov.focus();
}, true);
document.addEventListener("focusin", function (e) {
  var ov = document.getElementById("loadOverlay");
  if (!ov || ov.getAttribute("aria-hidden") !== "false" || ov.contains(e.target)) return;
  ov.focus();
}, true);

// ---- guided tour (driver.js) — ON DEMAND only (no auto-fire) --------------
function vegTour() {
  if (!window.driver || !window.driver.js) {
    if (typeof Swal !== "undefined") Swal.fire({ toast: true, position: "top-end",
      icon: "info", title: "Tour unavailable offline", showConfirmButton: false, timer: 2200 });
    return;
  }
  var D = window.driver.js.driver;
  var steps = [
    { element: ".living-poster-app", popover: { title: "A living record", side: "bottom",
        description: "This app follows <b>tagged woody plants</b>—trees and shrubs that NEON crews measure again over time." } },
    { element: ".map-picker-wrap", popover: { title: "Pick a place", side: "top",
        description: "Tap any dot for a short place card, then choose <b>Open this place</b>." } },
    { element: ".select-panel-compact", popover: { title: "Or type a name", side: "top",
        description: "Search by site code, place, or state. There is only one choice to make." } },
    { element: "#searchNetworkBtn", popover: { title: "No place required", side: "left",
        description: "Search species and stand sizes across all 42 bundled places before opening one." } },
    { element: ".home-nav", popover: { title: "Three questions", side: "bottom",
        description: "After a place opens, explore what stands there, what changed, or a single tagged plant." } }
  ].filter(function (s) {
    var el = document.querySelector(s.element);
    return el && el.getClientRects().length > 0;
  });
  if (!steps.length) return;
  var reduceMotion = !!(window.matchMedia &&
    window.matchMedia("(prefers-reduced-motion: reduce)").matches);
  var d = D({ showProgress: true, allowClose: true, animate: !reduceMotion,
    smoothScroll: false, steps: steps, popoverClass: "driverjs-theme",
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

// ---- keyboard a11y: open role="button" triggers (info-dot popovers) on -----
// Enter/Space. The info-dot triggers are <span tabindex="0" role="button">, so
// they're focusable but a plain <span> doesn't fire a click on key press; this
// forwards Enter/Space to a click so keyboard users can open every popover.
document.addEventListener("keydown", function (e) {
  if (e.key !== "Enter" && e.key !== " ") return;
  var el = e.target.closest && e.target.closest('[role="button"]');
  if (el) { e.preventDefault(); el.click(); }
});

// ---- Shiny custom message handlers ---------------------------------------
document.addEventListener("DOMContentLoaded", function () {
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("countUp", function (_msg) { setTimeout(runCounters, 60); });
    Shiny.addCustomMessageHandler("loadDone", function (_msg) { smtLoadDone(); });
    Shiny.addCustomMessageHandler("smtLoadStart", function (msg) { smtLoadStart(msg && msg.label); });
    Shiny.addCustomMessageHandler("startTour", function (_msg) { setTimeout(vegTour, 150); });
    // remember the current site so exported PNG filenames are self-describing
    Shiny.addCustomMessageHandler("siteCtx", function (msg) { window.__vegSite = msg && msg.site; });
    // A Leaflet map that initialised inside a hidden container (the picker map
    // re-shown after "change site") can paint blank/half-width until it
    // recomputes its size. Dispatching 'resize' makes every Leaflet map
    // invalidateSize. Fire across several frames so the page_fillable layout
    // (and the relocated select-panel) settles its width before Leaflet measures,
    // or the map captures a half-width and paints narrow.
    Shiny.addCustomMessageHandler("kickMaps", function (_msg) {
      var kick = function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} };
      requestAnimationFrame(kick);
      [80, 250, 500, 900].forEach(function (t) { setTimeout(kick, t); });
    });
  }
});

// Re-fit any Leaflet map the moment its tab becomes visible (hidden-init blank fix).
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} }, 60);
});
