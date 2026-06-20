# ==========================================================================
# map_picker.R — the reusable national site-PICKER map for the NEON suite.
# The feature that defines the flagship: a US map of every bundled site, tap a
# dot to load it. Marker size = a headline count (log1p-scaled), colour = a
# headline metric. CRITICAL: the leafletOutput is STATIC in ui.R (placed via
# mapPickerUI) — NEVER inside a renderUI — because a leaflet htmlwidget delivered
# through renderUI loses the dependency-deliver → re-bind race on Posit Connect
# Cloud and the spinner hangs forever. mapPickerServer keeps it alive while the
# splash is hidden (suspendWhenHidden=FALSE) so the proxy stays valid.
# Source in global.R; put mapPickerUI("picker") in the splash; call
# mapPickerServer("picker", ...) once. See docs/neonize-playbook.md.
# ==========================================================================

# log1p-scaled dot radius 6..24 px (flagship convention)
picker_radius <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x[!is.finite(x)] <- 0
  l <- log1p(pmax(0, x)); rng <- range(l, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) <= 0) return(rep(11, length(x)))
  6 + 18 * (l - rng[1]) / diff(rng)
}

mapPickerUI <- function(id, height = "560px", spinner = "#2f7fb5") {
  ns <- shiny::NS(id)
  # Plain full-width block: a shinycssloaders wrapper here collapses to 0 WIDTH
  # (the leaflet's width:100% then resolves to nothing -> blank map, no markers).
  # leafletOutput renders fast; validate() covers the no-data case.
  htmltools::div(class = "map-picker-wrap", style = "width:100%;",
    leaflet::leafletOutput(ns("map"), width = "100%", height = height))
}

# site_table : data.frame with columns site, lat, lng (+ the metric columns used below)
# radius_metric : column name driving dot size
# color_fn  : function(site_table) -> vector of fill colours (length = nrow)
# label_fn  : function(one-row df) -> HTML string for the hover label
# RETURNS a reactiveVal of the tapped site code. The CALLER observes it and loads
# the site IN THE MAIN SERVER — do NOT load inside the module: shinyjs::hide("splash")
# called from a module session namespaces the id ("picker-splash") and silently no-ops.
mapPickerServer <- function(id, site_table, radius_metric, color_fn, label_fn) {
  shiny::moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      st <- site_table[is.finite(site_table$lat) & is.finite(site_table$lng), , drop = FALSE]
      shiny::validate(shiny::need(nrow(st) > 0,
        "The national site map couldn't load its data. Use the site list below, or the demo."))
      labs <- lapply(seq_len(nrow(st)), function(i) htmltools::HTML(label_fn(st[i, , drop = FALSE])))
      leaflet::leaflet(st, options = leaflet::leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
        leaflet::addProviderTiles("CartoDB.Positron", options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
        leaflet::setView(lng = -96, lat = 41, zoom = 4) %>%
        leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, layerId = ~site,
          radius = picker_radius(st[[radius_metric]]), stroke = TRUE, color = "#ffffff",
          weight = 1.4, opacity = 1, fillColor = color_fn(st), fillOpacity = 0.85,
          label = labs, labelOptions = leaflet::labelOptions(direction = "auto", textsize = "13px",
            style = list("font-family" = "Rubik, sans-serif", "box-shadow" = "0 3px 12px rgba(0,0,0,.18)")),
          options = leaflet::markerOptions(riseOnHover = TRUE)) %>%
        # Self-fix sizing: the splash leaflet binds before its container has a width
        # (the map has no intrinsic width, so the container can resolve to 0 until a
        # reflow happens) -> blank tiles/markers. invalidateSize alone can't help a
        # 0-width container, so we FORCE a reflow (dispatch resize) on a few timers,
        # invalidateSize each time, and watch for the container finally getting width
        # via ResizeObserver. Belt-and-suspenders, but reliably fills the map.
        htmlwidgets::onRender("function(el, x) { var m = this;
          function kick(){ try { window.dispatchEvent(new Event('resize')); m.invalidateSize(); } catch(e){} }
          [120, 400, 900, 1800, 3000].forEach(function(t){ setTimeout(kick, t); });
          if (window.ResizeObserver) { var ro = new ResizeObserver(function(){ if (el.clientWidth > 0) m.invalidateSize(); }); ro.observe(el); }
          window.addEventListener('resize', function(){ m.invalidateSize(); }); }")
    })
    # keep the map bound after the splash hides, so leafletProxy stays valid
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    # expose the tapped site; the main server observes this and loads it
    picked <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$map_marker_click, {
      s <- input$map_marker_click$id
      if (!is.null(s) && nzchar(s)) picked(s)
    }, ignoreInit = TRUE)
    picked
  })
}
