/* =========================================================================
   pincards.js — tap-to-pin cards + export-with-pins for the Size Lab scatter.

   Ported from the Big 12 Girth Index pin-card system (ggiraph) onto PLOTLY:
   the same draggable/resizable navy card + gold leader line + html-to-image
   "download with the pins baked in" — but the click is detected with plotly's
   native `plotly_click` event and the card HTML rides in each point's
   `customdata`, so there's no second rendering stack (the app stays plotly).

   The pins, the leader-line SVG layer, and the gold anchor dots all live INSIDE
   the chart's `.smt-pinnable` box (not the plot div), so they travel with the
   box and — crucially — get captured when html-to-image snapshots the box.

   Anchors are stored as DATA coordinates (not frozen pixels), so they reposition
   correctly on resize / fullscreen / relayout. The plotly_click handler is
   RE-attached on every (re)render: a Shiny+plotly re-render runs purge+newPlot
   on the SAME div, which silently wipes gd.on() listeners — so a one-time bind
   (the old __smtPinBound guard) left the chart dead after the first re-render.
   ========================================================================= */
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg";

  function boxOf(node) { return node ? node.closest(".smt-pinnable") : null; }
  var LEADER = "#ffd24a";   // gold leader line + anchor dot (Canopy & Bark theme)
  var ANCHOR_STROKE = "#0a140e";
  function isDark() {
    return document.documentElement.getAttribute("data-bs-theme") === "dark" ||
           document.body.getAttribute("data-bs-theme") === "dark";
  }
  function bgColor() { return isDark() ? "#182e20" : "#ffffff"; }

  /* map a data point (dx, dy) to box-relative pixels via plotly's live axes, so
     a pin's leader line follows the dot through any resize/relayout. */
  /* l2p wants a NUMBER; on a date axis plotly_click hands x back as a string
     ("2018-06-15"), so coerce against the axis type before projecting — otherwise
     the trajectory chart's pins fall back to box-centre with no leader anchor. */
  function l2pVal(ax, v) {
    if (ax && ax.type === "date" && typeof v === "string") { var t = Date.parse(v); if (!isNaN(t)) v = t; }
    return ax.l2p(v);
  }
  function anchorPx(gd, box, dx, dy) {
    var fl = gd && gd._fullLayout;
    if (!fl || !fl.xaxis || !fl.yaxis || typeof fl.xaxis.l2p !== "function") return null;
    var gdR = gd.getBoundingClientRect(), boxR = box.getBoundingClientRect();
    return {
      ax: l2pVal(fl.xaxis, dx) + fl.xaxis._offset + (gdR.left - boxR.left),
      ay: l2pVal(fl.yaxis, dy) + fl.yaxis._offset + (gdR.top - boxR.top)
    };
  }

  function linesLayer(box) {
    var s = box.querySelector(":scope > svg.smt-pin-lines");
    if (!s) {
      s = document.createElementNS(NS, "svg");
      s.setAttribute("class", "smt-pin-lines");
      box.appendChild(s);
    }
    return s;
  }
  /* x2/y2 chase the card centre (honours its scale() transform via getBoundingClientRect) */
  function updateLine(pin) {
    if (!pin.__line) return;
    var r = pin.getBoundingClientRect(), b = pin.__box.getBoundingClientRect();
    pin.__line.setAttribute("x2", r.left - b.left + r.width / 2);
    pin.__line.setAttribute("y2", r.top - b.top + r.height / 2);
  }

  /* Keep the whole rendered card—not merely a 40px grab strip—inside its
     chart. Because cards resize with transform: scale(), use the rendered
     rectangle for the clamp and CSS offsets for the requested position. */
  function clampPin(pin, left, top) {
    var boxRect = pin.__box.getBoundingClientRect();
    var pinRect = pin.getBoundingClientRect();
    var maxLeft = Math.max(4, boxRect.width - pinRect.width - 4);
    var maxTop = Math.max(4, boxRect.height - pinRect.height - 4);
    pin.style.left = Math.max(4, Math.min(left, maxLeft)) + "px";
    pin.style.top = Math.max(4, Math.min(top, maxTop)) + "px";
  }

  function applyPinScale(pin, requested) {
    var current = pin.__scale || 1;
    var rect = pin.getBoundingClientRect();
    var baseW = rect.width / current;
    var baseH = rect.height / current;
    var boxRect = pin.__box.getBoundingClientRect();
    var fitScale = Math.max(0.5, Math.min(
      2.4,
      (boxRect.width - 8) / baseW,
      (boxRect.height - 8) / baseH
    ));
    var scale = Math.min(fitScale, Math.max(0.5, requested));
    pin.__scale = scale;
    pin.style.transform = scale === 1 ? "" : "scale(" + scale + ")";
    var grip = pin.querySelector(".smt-pin-resize");
    if (grip) {
      grip.setAttribute("aria-valuemax", String(Math.round(fitScale * 100)));
      grip.setAttribute("aria-valuenow", String(Math.round(scale * 100)));
      grip.setAttribute("aria-valuetext", Math.round(scale * 100) + " percent");
    }
    clampPin(pin, pin.offsetLeft, pin.offsetTop);
    updateLine(pin);
  }

  /* clear pins. With a boxId, scope to that one chart's box (so a multi-chart page
     can clear one without wiping the others); with no arg, clear every box. */
  window.smtClearPins = function (boxId) {
    var root = boxId ? document.getElementById(boxId) : document;
    if (!root) return;
    root.querySelectorAll(".smt-pin").forEach(function (p) { p.remove(); });
    root.querySelectorAll("svg.smt-pin-lines").forEach(function (s) { s.innerHTML = ""; });
  };

  /* re-anchor every pin's leader line + dot from its stored DATA coords */
  function repositionAll(gd, box) {
    box.querySelectorAll(".smt-pin").forEach(function (pin) {
      if (pin.__dx == null || !pin.__line) return;
      var a = anchorPx(gd, box, pin.__dx, pin.__dy);
      if (!a) return;
      pin.__line.setAttribute("x1", a.ax); pin.__line.setAttribute("y1", a.ay);
      if (pin.__dot) { pin.__dot.setAttribute("cx", a.ax); pin.__dot.setAttribute("cy", a.ay); }
      clampPin(pin, pin.offsetLeft, pin.offsetTop);
      updateLine(pin);
    });
  }

  function makePin(gd, box, html, dx, dy, key) {
    var bR = box.getBoundingClientRect();
    var a = anchorPx(gd, box, dx, dy) || { ax: bR.width / 2, ay: bR.height / 2 };
    var pin = document.createElement("div");
    pin.className = "smt-pin";
    pin.tabIndex = 0;
    pin.setAttribute("role", "group");
    pin.__box = box; pin.__key = key; pin.__dx = dx; pin.__dy = dy;
    pin.__returnFocus = document.activeElement;
    pin.innerHTML = "<button class='smt-pin-close' title='Close' aria-label='Close pinned card'>&times;</button>" + html;
    var plantId = pin.querySelector("b");
    var plantSpecies = pin.querySelector("em");
    var plantIdentity = [plantId && plantId.textContent, plantSpecies && plantSpecies.textContent]
      .filter(Boolean).join(", ") || String(key || "selected plant");
    pin.setAttribute("aria-label", "Pinned plant card for " + plantIdentity + ". Arrow keys move the card.");
    pin.querySelectorAll("em.smt-pin-hint").forEach(function (em) {
      var br = em.previousElementSibling;
      if (br && br.tagName === "BR") br.remove();
      em.remove();
    });
    var grip = document.createElement("button");
    grip.type = "button";
    grip.className = "smt-pin-resize";
    grip.title = "Drag or use arrow keys to resize · double-tap to reset";
    grip.setAttribute("role", "slider");
    grip.setAttribute("aria-label", "Resize pinned card");
    grip.setAttribute("aria-orientation", "horizontal");
    grip.setAttribute("aria-valuemin", "50");
    grip.setAttribute("aria-valuemax", "240");
    grip.setAttribute("aria-valuenow", "100");
    grip.setAttribute("aria-valuetext", "100 percent");
    pin.appendChild(grip);

    /* position the card near the dot, clamped on BOTH axes so the close button
       and resize grip always stay inside the box (and inside an exported PNG) */
    pin.style.left = Math.max(4, Math.min(a.ax + 24, bR.width - 250)) + "px";
    pin.style.top = Math.max(4, Math.min(a.ay + 14, bR.height - 150)) + "px";
    box.appendChild(pin);
    clampPin(pin, pin.offsetLeft, pin.offsetTop);

    var layer = linesLayer(box);
    var ln = document.createElementNS(NS, "line");
    ln.setAttribute("x1", a.ax); ln.setAttribute("y1", a.ay);
    ln.setAttribute("stroke", LEADER); ln.setAttribute("stroke-width", "2.5");
    ln.setAttribute("stroke-linecap", "round");
    layer.appendChild(ln);
    var dot = document.createElementNS(NS, "circle");
    dot.setAttribute("cx", a.ax); dot.setAttribute("cy", a.ay); dot.setAttribute("r", "4.5");
    dot.setAttribute("fill", LEADER); dot.setAttribute("stroke", ANCHOR_STROKE);
    dot.setAttribute("stroke-width", "1.5");
    layer.appendChild(dot);
    pin.__line = ln; pin.__dot = dot;
    updateLine(pin);

    pin.querySelector(".smt-pin-close").addEventListener("click", function () {
      var returnFocus = pin.__returnFocus;
      ln.remove(); dot.remove(); pin.remove();
      if (!returnFocus || returnFocus === document.body || !returnFocus.isConnected) {
        returnFocus = document.getElementById("pinViewedBtn");
      }
      if (returnFocus && returnFocus.isConnected && typeof returnFocus.focus === "function") {
        try { returnFocus.focus({ preventScroll: true }); } catch (e) { returnFocus.focus(); }
      }
    });

    pin.addEventListener("keydown", function (ev) {
      if (ev.target !== pin || !/^Arrow(Left|Right|Up|Down)$/.test(ev.key)) return;
      ev.preventDefault();
      var step = ev.shiftKey ? 20 : 8;
      var left = pin.offsetLeft;
      var top = pin.offsetTop;
      if (ev.key === "ArrowLeft") left -= step;
      if (ev.key === "ArrowRight") left += step;
      if (ev.key === "ArrowUp") top -= step;
      if (ev.key === "ArrowDown") top += step;
      clampPin(pin, left, top);
      updateLine(pin);
    });

    /* drag-to-move (clamped so a fat-thumb drag can't fling the card off-box).
       A 4px move threshold (S8): a near-miss TAP — e.g. a few px off the QC chip —
       never engages the drag or preventDefaults, so the intended click still fires.
       move/up/cancel bound on WINDOW + capture released (S7): a pointerup off the
       card (scroll-steal, edge swipe, pointercancel) can never leave it stuck. */
    pin.addEventListener("pointerdown", function (ev) {
      if (ev.target.closest("a, .smt-pin-close, .smt-open, .smt-pin-resize")) return;
      var sx = ev.clientX - pin.offsetLeft, sy = ev.clientY - pin.offsetTop;
      var startX = ev.clientX, startY = ev.clientY, dragging = false;
      function mv(em) {
        if (!dragging) {
          if (Math.abs(em.clientX - startX) < 4 && Math.abs(em.clientY - startY) < 4) return;
          dragging = true;
          try { pin.setPointerCapture(ev.pointerId); } catch (e) {}
        }
        em.preventDefault();
        clampPin(pin, em.clientX - sx, em.clientY - sy);
        updateLine(pin);
      }
      function up() {
        window.removeEventListener("pointermove", mv);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", up);
        try { pin.releasePointerCapture(ev.pointerId); } catch (e) {}
      }
      window.addEventListener("pointermove", mv);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", up);
    });

    /* grip-to-resize via scale() (clientX throughout, matching drag, so a
       scrolling mobile viewport can't make the scale jump) */
    grip.addEventListener("pointerdown", function (ev) {
      ev.preventDefault(); ev.stopPropagation();
      try { grip.setPointerCapture(ev.pointerId); } catch (e) {}
      var startX = ev.clientX, startScale = pin.__scale || 1;
      var startRect = pin.getBoundingClientRect();
      var baseW = startRect.width / startScale;
      function mv(em) {
        applyPinScale(pin, startScale + (em.clientX - startX) / baseW);
      }
      function up() {
        window.removeEventListener("pointermove", mv);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", up);
        try { grip.releasePointerCapture(ev.pointerId); } catch (e) {}
      }
      window.addEventListener("pointermove", mv);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", up);
    });
    grip.addEventListener("keydown", function (ev) {
      var direction = 0;
      if (ev.key === "ArrowRight" || ev.key === "ArrowUp" || ev.key === "+" || ev.key === "=") direction = 1;
      if (ev.key === "ArrowLeft" || ev.key === "ArrowDown" || ev.key === "-") direction = -1;
      if (ev.key === "Home") {
        ev.preventDefault();
        applyPinScale(pin, 1);
        return;
      }
      if (!direction) return;
      ev.preventDefault();
      applyPinScale(pin, (pin.__scale || 1) + direction * (ev.shiftKey ? 0.25 : 0.1));
    });
    grip.addEventListener("dblclick", function () {
      applyPinScale(pin, 1);
    });
    return pin;
  }

  function handleClick(gd, box, d) {
    if (!d || !d.points || !d.points.length) return;
    var pt = d.points[0];
    var html = pt.customdata;
    if (!html) return;                 // only points carrying a card (skip the fit line)
    var key = (html.match(/data-tag='([^']+)'/) || [])[1] || html.slice(0, 48);
    var dup = Array.prototype.find.call(box.querySelectorAll(".smt-pin"),
      function (p) { return p.__key === key; });
    if (dup) { dup.classList.remove("smt-pulse"); void dup.offsetWidth; dup.classList.add("smt-pulse"); return dup; }
    /* anchor from the DATA point (not the finger): exact, and touch-safe — on
       touch devices plotly's synthesized event has no usable clientX/Y */
    return makePin(gd, box, html, pt.x, pt.y, key);
  }

  // Keyboard route for the gold “viewing” point in Size Lab. The local plant
  // picker is the accessible selector; this button creates the same card as a
  // pointer click and moves focus to it for keyboard repositioning/resizing.
  window.smtPinViewed = function (plotId) {
    var gd = document.getElementById(plotId);
    var box = boxOf(gd);
    var trace = gd && gd.data && gd.data.find(function (item) {
      return item && item.name === "★ viewing" && item.customdata && item.customdata.length;
    });
    if (!gd || !box || !trace) {
      toastDone("Choose a plant first, then pin the viewed plant", true);
      return;
    }
    var first = function (value) { return Array.isArray(value) ? value[0] : value; };
    var pin = handleClick(gd, box, { points: [{
      customdata: first(trace.customdata), x: first(trace.x), y: first(trace.y)
    }] });
    if (pin) pin.focus();
  };

  /* identity-based signature: the sorted set of pinnable point tags. Stable when
     the chart is merely recoloured or a point is selected (same plot set), so
     pinned cards survive those re-renders; changes only when the plotted
     entities actually change (e.g. a new site) -> then pins clear. NOT trace
     count / x-ordering, which recolour and select would falsely trip. */
  function dataSig(gd) {
    if (!gd.data || !gd.data.length) return "";
    var tags = [];
    gd.data.forEach(function (t) {
      if (t.customdata) t.customdata.forEach(function (h) {
        var m = String(h).match(/data-tag='([^']+)'/); if (m) tags.push(m[1]);
      });
    });
    return tags.sort().join(",");
  }

  /* (re)bind a plotly graph div. Safe to call repeatedly — it removes any prior
     handler first. Clears pins when the underlying DATA changed (filter / select). */
  function bindPlot(gd) {
    if (!gd || typeof gd.on !== "function") return;
    var box = boxOf(gd); if (!box) return;

    var sig = dataSig(gd);
    if (gd.__smtSig !== undefined && gd.__smtSig !== sig) {
      box.querySelectorAll(".smt-pin").forEach(function (p) { p.remove(); });
      var ll = box.querySelector("svg.smt-pin-lines"); if (ll) ll.innerHTML = "";
    }
    gd.__smtSig = sig;

    if (gd.__smtClick && gd.removeListener) { try { gd.removeListener("plotly_click", gd.__smtClick); } catch (e) {} }
    gd.__smtClick = function (d) { handleClick(gd, box, d); };
    gd.on("plotly_click", gd.__smtClick);

    if (gd.__smtRelayout && gd.removeListener) { try { gd.removeListener("plotly_relayout", gd.__smtRelayout); } catch (e) {} }
    gd.__smtRelayout = function () { repositionAll(gd, box); };
    gd.on("plotly_relayout", gd.__smtRelayout);

    if (!box.__smtRO && window.ResizeObserver) {
      box.__smtRO = new ResizeObserver(function () {
        var g = box.querySelector(".js-plotly-plot"); if (g) repositionAll(g, box);
      });
      box.__smtRO.observe(box);
    }
  }

  function scan() {
    document.querySelectorAll(".smt-pinnable .js-plotly-plot").forEach(bindPlot);
  }
  /* rAF-coalesced scan so the document-wide observer doesn't run scan() on every
     count-up / DT / plotly mutation (it fires in bursts during animations) */
  var scanPending = false;
  function scanSoon() {
    if (scanPending) return;
    scanPending = true;
    requestAnimationFrame(function () { scanPending = false; scan(); });
  }

  /* ---- exports (with a toast + double-fire guard) ------------------------- */
  function toastStart(msg) {
    if (typeof Swal === "undefined") return;
    Swal.fire({ toast: true, position: "top-end", title: msg, showConfirmButton: false,
      allowOutsideClick: false, didOpen: function () { if (Swal.showLoading) Swal.showLoading(); } });
  }
  function toastDone(msg, isErr) {
    if (typeof Swal === "undefined") return;
    Swal.fire({ toast: true, position: "top-end", icon: isErr ? "error" : "success",
      title: msg, showConfirmButton: false, timer: 2200 });
  }
  function downloadUrl(url, name) {
    var a = document.createElement("a"); a.download = name; a.href = url; a.click();
  }
  var saving = false;
  function snap(node, name, exportOptions) {
    if (!node || saving) return;
    if (typeof htmlToImage === "undefined") {
      toastDone("Image export is unavailable in this build", true);
      return;
    }
    saving = true;
    var captureNode = node;
    var cleanup = function () {};
    var fixedWidth = exportOptions && exportOptions.fixedWidth;
    if (fixedWidth) {
      captureNode = node.cloneNode(true);
      captureNode.removeAttribute("id");
      captureNode.setAttribute("aria-hidden", "true");
      captureNode.classList.add("smt-export-card");
      captureNode.style.position = "fixed";
      captureNode.style.left = "-10000px";
      captureNode.style.top = "0";
      captureNode.style.width = fixedWidth + "px";
      captureNode.style.maxWidth = fixedWidth + "px";
      captureNode.style.minWidth = fixedWidth + "px";
      document.body.appendChild(captureNode);
      cleanup = function () {
        if (captureNode.parentNode) captureNode.parentNode.removeChild(captureNode);
      };
    }
    // force the plotly chart to its current size first (a tab that rendered while
    // hidden, or a just-toggled fullscreen, can leave a 0-sized / stale SVG)
    var gd = captureNode.querySelector ? captureNode.querySelector(".js-plotly-plot") : null;
    if (gd && window.Plotly && Plotly.Plots) { try { Plotly.Plots.resize(gd); } catch (e) {} }
    captureNode.querySelectorAll(".smt-pin.smt-pulse").forEach(function (p) { p.classList.remove("smt-pulse"); });
    toastStart("Rendering image…");
    setTimeout(function () {
      htmlToImage.toPng(captureNode, { pixelRatio: 2, width: fixedWidth || undefined,
        backgroundColor: bgColor(), cacheBust: true, skipFonts: true,
        filter: function (n) { return !(n.classList && (n.classList.contains("smt-pin-close") ||
          n.classList.contains("smt-pin-resize") || n.classList.contains("smt-snap-btn") ||
          n.classList.contains("smt-clear-btn"))); } })   // keep cards + leader lines; drop chrome buttons
        .then(function (url) { downloadUrl(url, name); toastDone("Saved ✓"); })
        .catch(function () { toastDone("Render failed — try again", true); })
        .then(function () { cleanup(); saving = false; });
    }, 90);
  }
  function stamp() {
    var d = new Date();
    return d.getFullYear() + ("0" + (d.getMonth() + 1)).slice(-2) + ("0" + d.getDate()).slice(-2);
  }
  function siteTag() { return (window.__vegSite || "site").replace(/[^A-Za-z0-9]+/g, ""); }

  /* per-box export: snapshot ONE chart's .smt-pinnable box (pins + leader lines
     baked in) by its id. filename may be a literal or contain "<site>"/"<date>"
     placeholders the toolbar buttons don't need to compute in R. */
  window.smtSave = function (boxId, filename) {
    var node = boxId ? document.getElementById(boxId) : document.querySelector(".smt-pinnable");
    if (!node) return;
    var name = String(filename || ("neon-veg-" + (boxId || "chart") + "-" + siteTag() + "_" + stamp()))
      .replace(/<site>/g, siteTag()).replace(/<date>/g, stamp());
    if (!/\.png$/i.test(name)) name += ".png";
    snap(node, name);
  };

  window.smtSaveScatter = function () {
    window.smtSave("labScatterBox", "NEON-VegStructure_" + siteTag() + "_size-lab_" + stamp() + ".png");
  };
  window.smtSaveQcCard = function () {
    var node = document.getElementById("qcCardNode");
    if (!node) return;
    var short = node.getAttribute("data-short") || "tree";
    snap(node, "NEON-VegStructure_" + siteTag() + "_tree-" + short.replace(/[^A-Za-z0-9]+/g, "") + "_" + stamp() + ".png");
  };
  /* the shareable Tree Card (holographic) on the Tree Career tab */
  window.smtSaveTreeCard = function () {
    var node = document.getElementById("treeCardNode");
    if (!node) return;
    var short = node.getAttribute("data-short") || "tree";
    snap(node, "NEON-VegStructure_" + siteTag() + "_treecard-" + short.replace(/[^A-Za-z0-9]+/g, "") + "_" + stamp() + ".png",
      { fixedWidth: 340 });
  };

  /* Scroll the rendered QC/tree card into view once it materialises. Hard-won:
     • the card re-renders async (uiOutput) after the server selects the plant, so
       poll for the actual rendered node (#qcCardNode), ~2.5s.
     • behavior:"auto" (instant), NOT "smooth": the scroll happens inside a nested
       bslib fill container where smooth scrollIntoView silently no-ops — instant
       is reliable and respects reduced-motion by definition.
     Driven client-side from the chip click (below) so it can't depend on a server
     round-trip message to fire. */
  function revealQcCard() {
    var tries = 0;
    (function go() {
      var n = document.getElementById("qcCardNode");
      if (n && n.getBoundingClientRect().height > 1) {
        n.scrollIntoView({ behavior: "auto", block: "start" });
        return;
      }
      if (++tries < 50) setTimeout(go, 50);
    })();
  }

  /* tap (or keyboard-activate) a highlighted "Open career" chip inside a pinned
     card -> select that plant server-side + scroll its QC/tree card into view */
  function openChip(el) {
    if (!el || !window.Shiny) return;
    var tag = el.getAttribute("data-tag");
    if (!tag) return;
    Shiny.setInputValue("qcCardRequest", tag, { priority: "event" });
    revealQcCard();
  }
  document.addEventListener("click", function (e) {
    var el = e.target.closest(".smt-open");
    if (el) openChip(el);
  });
  document.addEventListener("keydown", function (e) {
    if (e.key !== "Enter" && e.key !== " ") return;
    var el = e.target.closest && e.target.closest(".smt-open");
    if (el) { e.preventDefault(); openChip(el); }
  });

  document.addEventListener("DOMContentLoaded", function () {
    var mo = new MutationObserver(scanSoon);
    mo.observe(document.body, { childList: true, subtree: true });
    scan();
  });
  /* re-bind/re-fit when the Size Lab tab becomes visible (plotly in a hidden tab
     can render late, and a tab-show dispatches a resize that relayouts the plot) */
  document.addEventListener("shown.bs.tab", function () { setTimeout(scan, 80); });

  /* the server (qcCardRequest observer) may also fire "smtRevealQc" after selecting
     the plant; bring its card into view (it re-renders async via uiOutput, so a
     short settle). The chip click (openChip) already scrolls client-side; this is a
     backup. Registered LAST and fully guarded so a duplicate/late registration can
     never kill the pin-binding listeners above; self-polls because this IIFE runs at
     <head> before Shiny exists. */
  (function registerReveal() {
    try {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler("smtRevealQc", function (_msg) { revealQcCard(); });
        return;
      }
    } catch (e) { return; }
    setTimeout(registerReveal, 60);
  })();
})();
