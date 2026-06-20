# ===========================================================================
# NEON Vegetation Structure Explorer — server.R
# ===========================================================================
server <- function(input, output, session) {

  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark()
    ink  <- if (dark) "#e8ece6" else "#1d2a24"; grid <- if (dark) "rgba(220,232,228,0.10)" else "rgba(32,40,36,0.08)"
    zero <- if (dark) "rgba(220,232,228,0.22)" else "rgba(32,40,36,0.15)"; lin <- if (dark) "#2c352f" else "#d9d5c8"
    legc <- if (dark) "#c2cfc8" else "#33403a"
    hov  <- if (dark) list(bg = "rgba(14,24,22,0.97)", bd = "#e8b54a", fg = "#f1f6f3")
            else        list(bg = "rgba(20,42,39,0.96)", bd = "#E0A500", fg = "#ffffff")
    p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Rubik"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 55, r = 30, t = 48, b = 44),
      hoverlabel = list(bgcolor = hov$bg, bordercolor = hov$bd,
        font = list(color = hov$fg, family = "Rubik", size = 13))) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  note_plot <- function(msg, icon = "\U0001F332") {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = paste0(icon, "<br>", msg), showarrow = FALSE,
          font = list(color = if (is_dark()) "#9fb0c4" else "#6b7a85", size = 15), align = "center"))) %>%
      plotly::config(displayModeBar = FALSE)
  }

  rv <- reactiveValues(trees = NULL, snap = NULL, one = NULL, plots = NULL, lb = NULL,
                       pal = NULL, label = NULL, site = NULL, tree = NULL, ctx = NULL, is_demo = FALSE,
                       stype = "forest", spec = SIZE_FOREST)
  SP <- function() rv$spec %||% SIZE_FOREST     # the active site's size paradigm

  observe({ ch <- veg_state_choices(); updateSelectInput(session, "stateSel", choices = ch,
            selected = if ("MA" %in% ch) "MA" else NULL) })
  observeEvent(input$stateSel, updateSelectInput(session, "site", choices = veg_sites_in_state(input$stateSel)), ignoreInit = FALSE)
  output$siteBio <- renderUI({ req(input$site); b <- site_bio(input$site); if (is.null(b)) return(NULL)
    div(class = "site-bio", bs_icon("info-circle-fill"), span(b)) })

  output$siteCards <- renderUI({
    if (is.null(SITE_INDEX) || !nrow(site_table)) return(NULL)
    div(class = "site-cards", lapply(seq_len(nrow(site_table)), function(i) {
      r <- site_table[i, ]
      shrub <- identical(r$structure_type, "shrubland")
      emoji <- if (shrub) "\U0001F33F" else "\U0001F333"
      noun  <- if (shrub) "shrubs" else "trees"
      tags$a(class = paste0("site-card", if (shrub) " site-card-shrub" else ""), href = "#",
        onclick = sprintf("smtLoadStart('%s — loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});return false;",
                          gsub("'", "", r$name), r$site),
        div(class = "sc-emoji", emoji),
        div(class = "sc-body",
          div(class = "sc-name", tags$b(r$site), sprintf(" · %s", r$name)),
          div(class = "sc-meta", sprintf("%s · %s %s · %s species · tallest %sm",
            r$state, format(r$n_trees, big.mark = ","), noun, r$n_species, r$tallest_m)))) }))
  })

  shinyjs::hide("mainTabsWrap")

  ingest <- function(b, label, is_demo = FALSE) {
    if (is.null(b) || is.null(b$trees) || !nrow(b$trees)) {
      session$sendCustomMessage("loadDone", list()); showNotification("No vegetation data for that site.", type = "warning"); return(invisible()) }
    rv$trees <- b$trees
    rv$snap  <- tree_snapshot(b$trees)             # latest bout per plant
    rv$stype <- b$meta$structure_type %||% classify_structure(rv$snap)
    rv$spec  <- size_spec(rv$stype)                # forest (DBH) vs shrubland (basal ø)
    rv$one   <- one_per_tree(live_only(rv$snap), rv$spec)   # one row per LIVE plant (largest stem)
    rv$plots <- b$plots
    rv$lb    <- plot_summary_veg(rv$snap, b$plots, rv$spec)
    rv$pal   <- make_species_pal(species_level_only(rv$snap))
    rv$label <- label; rv$site <- b$meta$site; rv$is_demo <- is_demo; rv$tree <- NULL
    yrs <- range(b$trees$year, na.rm = TRUE)
    rv$ctx <- paste0(b$meta$site, " · ", if (yrs[1] == yrs[2]) yrs[1] else paste0(yrs[1], "–", yrs[2]))
    shinyjs::show("mainTabsWrap"); shinyjs::show("treePickerWrap"); shinyjs::hide("splash")
    one <- rv$one; sz <- one[[rv$spec$col]]
    lab_meas <- ifelse(is.finite(sz), paste0(round(sz), "cm"),
                       ifelse(is.finite(one$height), paste0(round(one$height), "m tall"), "—"))
    ch <- setNames(one$individualID, sprintf("%s · %s · %s",
            short_tree(one$individualID), ifelse(is.na(one$scientificName), "—", one$scientificName), lab_meas))
    updateSelectizeInput(session, "treeSel", choices = c("Pick a tree…" = "", ch), selected = "", server = TRUE)
    session$sendCustomMessage("siteCtx", list(site = rv$site %||% "site"))
    nav_select("tabs", "overview"); session$sendCustomMessage("countUp", list()); session$sendCustomMessage("loadDone", list())
    invisible(TRUE)
  }
  load_site <- function(site) {
    if (is.null(site) || site == "") { session$sendCustomMessage("loadDone", list()); return() }
    b <- load_site_bundle(site)
    if (is.null(b)) { session$sendCustomMessage("loadDone", list()); showNotification("That site isn't bundled in this demo.", type = "error"); return() }
    row <- site_table[site_table$site == site, ]
    ingest(b, sprintf("%s · %s", site, if (nrow(row)) row$name else site))
  }
  observeEvent(input$loadBtn, load_site(input$site))
  observeEvent(input$pickSite, load_site(input$pickSite))
  observeEvent(input$demoBtn,  ingest(load_demo(), DEMO_META$label, is_demo = TRUE))
  observeEvent(input$demoBtn2, ingest(load_demo(), DEMO_META$label, is_demo = TRUE))

  # national site-picker map on the splash: dot size = stems measured, colour =
  # forest (teal) vs shrubland (ochre). Tap a dot to load — the flagship front door.
  local({
    picked_site <- mapPickerServer("picker", site_table = site_table, radius_metric = "n_trees",
      color_fn = function(st) ifelse(st$structure_type %in% "shrubland", DDL$bark, DDL$navy),
      label_fn = function(r) sprintf(
        "<b>%s</b> · %s, %s<br><b>%s</b> %s · <b>%s</b> species<br>tallest %sm · widest %scm",
        r$site, r$name %||% r$site, r$state %||% "",
        format(r$n_trees %||% 0, big.mark = ","),
        if (identical(r$structure_type, "shrubland")) "shrubs" else "trees",
        r$n_species %||% "?", r$tallest_m %||% "?", r$biggest_dbh_cm %||% "?"))
    # load in the MAIN server context so ingest()'s shinyjs::hide("splash") isn't namespaced
    observeEvent(picked_site(), { s <- picked_site(); if (!is.null(s) && nzchar(s)) load_site(s) }, ignoreInit = TRUE)
  })

  pick_tree <- function(id, navigate = FALSE) {
    if (is.null(id) || is.na(id) || id == "") return()
    if (is.null(rv$snap) || !(id %in% rv$snap$individualID)) return()
    rv$tree <- id
    if (!identical(input$treeSel, id)) updateSelectizeInput(session, "treeSel", selected = id)
    if (navigate) nav_select("tabs", "tree")
    # celebrate a genuine standout: the site's biggest or tallest live plant
    sp <- SP(); td <- woody_only(live_only(rv$snap), sp); lh <- live_only(rv$snap)
    big_id  <- if (!is.null(td) && nrow(td)) td$individualID[which.max(td[[sp$col]])] else NA_character_
    tall_id <- if (!is.null(lh) && nrow(lh) && any(is.finite(lh$height))) lh$individualID[which.max(lh$height)] else NA_character_
    if (id %in% stats::na.omit(c(big_id, tall_id))) session$sendCustomMessage("confetti", list(big = TRUE))
  }
  observeEvent(input$treeSel, if (nzchar(input$treeSel %||% "")) pick_tree(input$treeSel, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_tree(input$qcCardRequest, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$surpriseBtn, { one <- rv$one; req(one); pick_tree(sample(one$individualID, 1), navigate = TRUE) })

  observeEvent(input$goStand,  nav_select("tabs", "stand"))
  observeEvent(input$goGrowth, nav_select("tabs", "growth"))
  observeEvent(input$goLab,    nav_select("tabs", "lab"))
  observeEvent(input$goTree,   { if (is.null(rv$tree) && !is.null(rv$one) && nrow(rv$one)) { i <- which.max(rv$one[[SP()$col]]); if (length(i)) rv$tree <- rv$one$individualID[i] }; nav_select("tabs", "tree") })
  observeEvent(input$goMap,    nav_select("tabs", "map"))

  # ---- hero ---------------------------------------------------------------
  output$heroStats <- renderUI({
    one <- rv$one; snap <- rv$snap; if (is.null(one)) return(NULL)
    sp <- SP(); shrub <- identical(sp$type, "shrubland")
    live_snap <- live_only(snap)
    woody_sp <- species_level_only(woody_only(one, sp))            # stand species
    tallest <- smax(live_snap$height)
    biggest <- smax(woody_only(live_snap, sp)[[sp$col]])
    thresh  <- if (shrub) "(any basal stem diameter)" else "≥ 10 cm DBH (the protocol tree threshold)"
    hero <- function(v, l, suf = "", icon, tone, ttl = NULL, click = NULL) {
      attrs <- list(class = paste0("hero-stat hero-", tone, if (!is.null(click)) " hero-click" else ""), title = ttl)
      if (!is.null(click)) {
        attrs$onclick <- sprintf("Shiny.setInputValue('heroClick','%s',{priority:'event'})", click)
        attrs$style <- "cursor:pointer"
      }
      do.call(div, c(attrs, list(
        div(class = "hs-icon", bs_icon(icon)),
        div(div(class = "hs-v count-up", `data-target` = v, `data-suffix` = suf, "0"),
            div(class = "hs-l", l, if (!is.null(click)) tags$span(class = "stat-q", bs_icon("chevron-right")))))))
    }
    div(class = "hero-band",
      div(class = "hero-title", bs_icon(if (shrub) "flower2" else "tree-fill"), tags$b(rv$label)),
      div(class = "hero-grid",
        hero(nrow(woody_only(one, sp)), paste0("live ", sp$nouns), icon = if (shrub) "flower2" else "tree", tone = "pine",
             ttl = sprintf("Live tagged %s %s — one count per individual.", sp$nouns, thresh)),
        hero(dplyr::n_distinct(woody_sp$scientificName), paste0(sp$noun, " species"), icon = "diagram-3", tone = "sky",
             ttl = sprintf("Species among live %s %s — the stand. Tap to rank species by basal area.", sp$nouns, thresh), click = "species"),
        hero(ifelse(is.finite(tallest), round(tallest, 1), 0), "m tallest", icon = "arrows-vertical", tone = "gold",
             ttl = sprintf("Tallest live %s at the site.", sp$noun)),
        hero(ifelse(is.finite(biggest), round(biggest, 1), 0), paste0("cm biggest ", sp$size_lab), icon = "circle", tone = "bark",
             ttl = sprintf("Largest live %s by %s. Tap to see the biggest.", sp$noun, sp$size_full), click = "biggest")))
  })

  # ---- OVERVIEW -----------------------------------------------------------
  output$baBar <- renderPlotly({
    ssall <- species_structure(rv$snap, rv$plots, SP()); if (is.null(ssall) || !nrow(ssall)) return(note_plot("No basal-area data"))
    tot <- sum(ssall$ba_m2, na.rm = TRUE)
    ss <- head(ssall, 18); ss$share <- if (is.finite(tot) && tot > 0) round(100 * ss$ba_m2 / tot) else 0
    ss$scientificName <- factor(ss$scientificName, levels = rev(ss$scientificName))
    pal <- rv$pal %||% make_species_pal(rv$snap)
    plot_ly(ss, x = ~ba_m2, y = ~scientificName, type = "bar", orientation = "h",
      marker = list(color = unname(pal[as.character(ss$scientificName)] %||% DDL$green)),
      text = ~paste0(share, "% of stand · ", stems, " stems"),
      hovertemplate = "%{y}<br>%{x:.1f} m² basal area · %{text}<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Total basal area (m²)"), yaxis = list(title = ""), margin = list(l = 200))
  })
  output$overviewInsight <- renderUI({
    sp <- SP(); ss <- species_structure(rv$snap, rv$plots, sp); req(!is.null(ss), nrow(ss) > 0)
    st <- stand_site(rv$snap, rv$plots, sp)
    stand_word <- if (identical(sp$type, "shrubland")) "shrubland" else "stand"
    insight_banner("stars", tone = "pine",
      HTML(sprintf("<b><i>%s</i></b> dominates the %s by basal area (%d stems). It holds <span class='ci-hero'>%d</span> %s species at about <b>%s</b> m²/ha basal area.",
        ss$scientificName[1], stand_word, ss$stems[1], dplyr::n_distinct(species_level_only(woody_only(rv$one, sp))$scientificName),
        sp$noun, if (is.null(st)) "—" else st$ba_ha)))
  })
  output$siteInsights <- renderUI({
    snap <- rv$snap; one <- rv$one; req(snap, one); sp <- SP()
    st <- stand_site(snap, rv$plots, sp); g <- tree_growth(rv$trees, sp); ss <- species_structure(snap, rv$plots, sp)
    one_d <- one[is.finite(one[[sp$col]]), ]; one_h <- one[is.finite(one$height), ]
    big  <- if (nrow(one_d)) one_d[which.max(one_d[[sp$col]]), ] else one[0, ]
    tall <- if (nrow(one_h)) one_h[which.max(one_h$height), ]      else one[0, ]
    pts <- c()
    if (!is.null(st)) pts <- c(pts, sprintf("%s density is about <b>%s stems/ha</b> at <b>%s m²/ha</b> basal area%s (quadratic mean %s %s cm).", if (identical(sp$type,"shrubland")) "Shrubland" else "Stand", format(st$density_ha, big.mark=","), st$ba_ha, if (!is.null(st$ba_se) && is.finite(st$ba_se)) sprintf(" (±%s SE, n=%d plots)", st$ba_se, st$n_plots) else "", sp$size_lab, st$qmd))
    if (nrow(big) && nrow(tall)) pts <- c(pts, sprintf("The biggest %s is a <b><i>%s</i></b> at <b>%s cm</b> %s; the tallest reaches <b>%s m</b> (<i>%s</i>).", sp$noun, big$scientificName, round(big[[sp$col]],1), sp$size_lab, round(tall$height,1), tall$scientificName))
    else if (nrow(big)) pts <- c(pts, sprintf("The biggest %s is a <b><i>%s</i></b> at <b>%s cm</b> %s.", sp$noun, big$scientificName, round(big[[sp$col]],1), sp$size_lab))
    if (!is.null(g) && nrow(g)) { gg <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5, ]; if (nrow(gg)) pts <- c(pts, sprintf("Across <b>%s</b> remeasured %s, %s grows a median of <b>%.2f cm/yr</b>.", format(nrow(gg), big.mark=","), sp$nouns, sp$size_lab, stats::median(gg$growth_cm_yr, na.rm=TRUE))) }
    pts <- c(pts, "Basal area and density are stand indices from the sampled plots — not a wall-to-wall inventory; biomass isn't estimated (it needs an allometric model NEON doesn't publish here).")
    div(class = "insight-list", lapply(pts, function(t) div(class = "il-item", bs_icon("dot"), HTML(t))))
  })

  # ---- STAND STRUCTURE ----------------------------------------------------
  output$sizePlot <- renderPlotly({
    sp <- SP(); sc <- size_class(rv$snap, rv$plots, sp); if (is.null(sc)) return(note_plot("No diameter data"))
    has_ha <- "stems_ha" %in% names(sc) && any(is.finite(sc$stems_ha))
    sc$yval <- if (has_ha) sc$stems_ha else sc$stems
    ylab <- if (has_ha) "Live stems / ha" else "Live stems (sampled)"
    forest <- !identical(sp$type, "shrubland")
    p <- plot_ly(sc, x = ~cls, y = ~yval, type = "bar", name = "observed", marker = list(color = DDL$green),
      hovertemplate = paste0("%{x} cm ", sp$size_lab, "<br>%{y} ", if (has_ha) "stems/ha" else "stems", "<extra></extra>"))
    # FOREST only: overlay the expected reverse-J (de Liocourt negative-exponential)
    # decline, fit log-linearly to the populated classes and drawn across ALL of
    # them — so a recruitment gap shows as bars sitting BELOW the smooth reference.
    # Shrublands use basal-diameter classes that don't follow de Liocourt, so it's
    # omitted there (the insight text already characterises their shape).
    if (forest) {
      sc$idx <- seq_len(nrow(sc))
      d <- sc[is.finite(sc$yval) & sc$yval > 0, , drop = FALSE]
      if (nrow(d) >= 3) {
        fit <- try(stats::lm(log(yval) ~ idx, data = d), silent = TRUE)
        if (!inherits(fit, "try-error")) {
          sc$exp <- exp(as.numeric(stats::predict(fit, sc)))
          p <- p %>% add_trace(data = sc, x = ~cls, y = ~exp, type = "scatter", mode = "lines",
            name = "expected reverse-J", inherit = FALSE,
            line = list(color = DDL$bark, width = 2.5, dash = "dash"),
            hovertemplate = paste0("expected reverse-J<br>~%{y:.0f} ", if (has_ha) "stems/ha" else "stems", "<extra></extra>"))
        }
      }
    }
    p %>% plotly_theme(legend = forest) %>%
      plotly::layout(showlegend = forest,
        legend = list(orientation = "h", y = 1.08, x = 0),
        xaxis = list(title = paste0(if (forest) "Diameter" else "Basal diameter", " class (cm ", sp$size_lab, ")")),
        yaxis = list(title = ylab))
  })
  output$sizeInsight <- renderUI({
    sp <- SP(); sc <- size_class(rv$snap, NULL, sp); req(!is.null(sc))
    bk <- size_breaks(sp)
    small <- sum(sc$stems[sc$cls %in% bk$small]); big <- sum(sc$stems[sc$cls %in% bk$big])
    ratio <- if (big > 0) round(small / big, 1) else NA
    if (identical(sp$type, "shrubland")) {
      shape <- if (is.na(ratio)) "concentrated in the smallest stems" else
               if (ratio >= 3) "strongly bottom-heavy (many small stems, few large) — an actively recruiting shrubland" else
               if (ratio >= 1.2) "moderately bottom-heavy" else
               "relatively even across basal sizes, with a notable share of large established shrubs"
      insight_banner("bar-chart-fill", tone = "pine",
        HTML(sprintf("By <b>basal stem diameter</b>, the size distribution is <b>%s</b>%s. Desert shrubs are too short for a breast-height diameter, so basal diameter is the honest size measure here.",
          shape, if (!is.na(ratio)) sprintf(" (a rough %.1f small per large stem)", ratio) else "")))
    } else {
      shape <- if (is.na(ratio)) "concentrated in the smaller classes" else
               if (ratio >= 3) "a clear descending, reverse-J-like shape — typical of a regenerating, uneven-aged stand" else
               if (ratio >= 1.2) "a moderate descending shape" else
               "top-heavy (relatively few small trees) — which can indicate an aging or even-aged stand; verify against site history"
      insight_banner("bar-chart-fill", tone = "pine",
        HTML(sprintf("Among trees ≥10 cm DBH, the size distribution is <b>%s</b>%s — the reverse-J / de Liocourt pattern. Smaller saplings are sampled in separate nested subplots and aren't shown here.",
          shape, if (!is.na(ratio)) sprintf(" (a rough %.1f small per large stem)", ratio) else "")))
    }
  })
  # "this site vs the network" — all bundled sites by woody richness, current gold
  output$networkStrip <- renderPlotly({
    si <- site_table; if (is.null(si) || !nrow(si) || !"n_species" %in% names(si)) return(note_plot("No network index"))
    d <- si[is.finite(si$n_species), c("site", "name", "n_species"), drop = FALSE]
    if (!nrow(d)) return(note_plot("No richness data"))
    d <- d[order(d$n_species), ]; d$site <- factor(d$site, levels = d$site)
    cur <- rv$site %||% ""
    d$col <- ifelse(as.character(d$site) == cur, "#E0A500", DDL$navy)
    plot_ly(d, y = ~site, x = ~n_species, type = "bar", orientation = "h",
      marker = list(color = ~col),
      hovertemplate = ~paste0("<b>", site, "</b> · ", name, "<br>", n_species, " woody species",
        ifelse(as.character(site) == cur, "  ◀ this site", ""), "<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, bargap = 0.22,
        xaxis = list(title = "Woody species (richness)"),
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 9)))
  })
  output$htPlot <- renderPlotly({
    s <- live_only(rv$snap); h <- s$height[is.finite(s$height) & s$height > 0]; if (!length(h)) return(note_plot("No height data"))
    plot_ly(x = h, type = "histogram", nbinsx = 24, marker = list(color = DDL$bark),
      hovertemplate = "%{x} m<br>%{y} stems<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Height (m)"), yaxis = list(title = "Live stems"))
  })
  output$densityBanner <- renderUI({
    sp <- SP(); st <- stand_site(rv$snap, rv$plots, sp); req(!is.null(st))
    pre  <- if (st$n_plots < 3) "Preliminary (few plots): " else ""
    se_ba <- if (is.finite(st$ba_se)) sprintf(" ±%s SE", st$ba_se) else ""
    se_d  <- if (is.finite(st$density_se)) sprintf(" ±%s", format(st$density_se, big.mark = ",")) else ""
    scope <- if (identical(sp$type, "shrubland")) "shrubs (basal diameter)" else "trees ≥10 cm DBH"
    # 4th FIA number: the dominant species' share of total basal area (who owns the stand)
    ss <- species_structure(rv$snap, rv$plots, sp)
    dom_txt <- if (!is.null(ss) && nrow(ss) && is.finite(ss$ba_m2[1]) && sum(ss$ba_m2, na.rm = TRUE) > 0)
      sprintf(" Dominated by <b><i>%s</i></b> (<b>%.0f%%</b> of basal area).",
              ss$scientificName[1], 100 * ss$ba_m2[1] / sum(ss$ba_m2, na.rm = TRUE)) else ""
    insight_banner("calculator", tone = "gold",
      HTML(sprintf("%sAcross <b>%d</b> sampled plots (%s): <span class='ci-hero'>%s m²/ha</span>%s basal area, <b>%s stems/ha</b>%s, quadratic mean %s <b>%s cm</b>.%s <span class='dim'>Mean ± SE across plots (the sampling unit) — a stand fingerprint, not a wall-to-wall inventory.</span>",
        pre, st$n_plots, scope, st$ba_ha, se_ba, format(st$density_ha, big.mark = ","), se_d, sp$size_lab, st$qmd, dom_txt)))
  })

  # ---- GROWTH & MORTALITY -------------------------------------------------
  output$growthPlot <- renderPlotly({
    sp <- SP(); g <- tree_growth(rv$trees, sp); if (is.null(g) || !nrow(g)) return(note_plot(sprintf("No remeasured %s yet for a growth estimate", sp$nouns)))
    gg <- g$growth_cm_yr[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2 & !g$mh_change]
    plot_ly(x = gg, type = "histogram", nbinsx = 30, marker = list(color = DDL$green),
      hovertemplate = paste0("%{x} cm/yr<br>%{y} ", sp$nouns, "<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = paste0(if (identical(sp$type,"shrubland")) "Basal-diameter" else "Diameter", " growth (cm/yr)")), yaxis = list(title = sp$Nouns),
        shapes = list(list(type="line", x0=0, x1=0, yref="paper", y0=0, y1=1, line=list(color="rgba(122,82,48,0.7)", dash="dot", width=1))))
  })
  output$growthInsight <- renderUI({
    sp <- SP(); g <- tree_growth(rv$trees, sp); req(!is.null(g), nrow(g) > 0)
    nmh <- sum(g$mh_change, na.rm = TRUE)
    gv <- g[!g$mh_change & is.finite(g$growth_cm_yr), ]
    clean <- gv[gv$growth_cm_yr <= 5 & gv$growth_cm_yr >= -2, ]; req(nrow(clean) > 0)
    trunc_n <- nrow(gv) - nrow(clean)
    med <- stats::median(clean$growth_cm_yr, na.rm = TRUE)
    q <- stats::quantile(clean$growth_cm_yr, c(.25, .75), na.rm = TRUE, names = FALSE)
    neg <- round(100 * mean(clean$growth_cm_yr < -0.1, na.rm = TRUE))
    insight_banner("graph-up", tone = "pine",
      HTML(sprintf("Across <b>%s</b> remeasured %s, %s grows a median of <span class='ci-hero'>%.2f cm/yr</span> (IQR %.2f–%.2f). About <b>%d%%</b> show a decrease between visits — usually real (bark, drought), kept and flagged.%s%s",
        format(nrow(clean), big.mark = ","), sp$nouns, sp$size_lab, med, q[1], q[2], neg,
        if (nmh > 0) sprintf(" %s %s with a changed measurement height are excluded.", format(nmh, big.mark = ","), sp$nouns) else "",
        if (trunc_n > 0) sprintf(" %s with >5 or <−2 cm/yr (likely measurement issues) are off-chart but kept in the data.", format(trunc_n, big.mark = ",")) else "")))
  })
  output$statusPlot <- renderPlotly({
    sp <- SP(); ss <- status_summary(rv$snap, sp); if (is.null(ss) || !nrow(ss)) return(note_plot("No status data"))
    cols <- c("Live" = DDL$live, "Dead / standing dead" = DDL$dead,
              "Lost track / removed" = "#c2b280", "Other / unknown" = DDL$muted)
    ss$lab <- as.character(ss$cls)
    plot_ly(ss, labels = ~lab, values = ~n, type = "pie", hole = 0.55, sort = FALSE,
      marker = list(colors = unname(cols[ss$lab])), textinfo = "label+percent",
      hovertemplate = paste0("%{label}<br>%{value} ", sp$nouns, "<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>% plotly::layout(showlegend = FALSE)
  })
  output$fastTable <- DT::renderDT({
    sp <- SP(); g <- tree_growth(rv$trees, sp)
    if (is.null(g) || !nrow(g)) return(DT::datatable(data.frame(Message = sprintf("No remeasured %s yet.", sp$nouns)), rownames = FALSE, options = list(dom = "t")))
    g <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5 & !g$mh_change, ]
    if (!nrow(g)) return(DT::datatable(data.frame(Message = "No clean remeasurement growth yet."), rownames = FALSE, options = list(dom = "t")))
    g <- g[order(-g$growth_cm_yr), ][seq_len(min(20, nrow(g))), ]
    lab0 <- sprintf("%s start (cm)", sp$size_lab); lab1 <- sprintf("%s now (cm)", sp$size_lab)
    df <- data.frame(Plant = short_tree(g$individualID), Species = g$scientificName,
                     v0 = round(g$d0,1), v1 = round(g$d1,1),
                     `Growth (cm/yr)` = g$growth_cm_yr, check.names = FALSE)
    names(df)[3:4] <- c(lab0, lab1)
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tp", order = list(list(4, "desc"))))
  })

  # growth-allometry: does annual diameter increment slow as plants get bigger?
  # (the structural twin of the mammal Size Lab — current size vs growth rate.)
  output$growthSize <- renderPlotly({
    sp <- SP(); g <- tree_growth(rv$trees, sp)
    if (is.null(g) || !nrow(g)) return(note_plot(sprintf("No remeasured %s yet", sp$nouns)))
    g <- g[is.finite(g$d1) & g$d1 > 0 & is.finite(g$growth_cm_yr) &
           g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2 & !g$mh_change, , drop = FALSE]
    if (nrow(g) < 5) return(note_plot(sprintf("Not enough remeasured %s for a size–growth view", sp$nouns)))
    topsp <- names(sort(table(g$scientificName[!is.na(g$scientificName)]), decreasing = TRUE))
    topsp <- topsp[seq_len(min(6, length(topsp)))]
    g$grp <- ifelse(g$scientificName %in% topsp, g$scientificName, "other species")
    pal <- rv$pal; p <- plot_ly()
    for (s in unique(g$grp)) {
      gs <- g[g$grp == s, ]; col <- if (!is.null(pal) && s %in% names(pal)) pal[[s]] else DDL$muted
      p <- p %>% add_trace(data = gs, x = ~d1, y = ~growth_cm_yr, type = "scatter", mode = "markers",
        name = s, marker = list(size = 7, color = col, opacity = 0.7, line = list(color = "#fff", width = 0.5)),
        hovertemplate = paste0("<b>", s, "</b><br>%{x:.0f} cm now<br>%{y:.2f} cm/yr<extra></extra>"))
    }
    # gated trend line: drawn ONLY when n & |Spearman r| & p clear the bar (honest —
    # no fabricated line where the relationship isn't there)
    ct <- suppressWarnings(stats::cor.test(g$d1, g$growth_cm_yr, method = "spearman"))
    sub <- if (nrow(g) >= 12 && is.finite(ct$estimate) && abs(ct$estimate) >= 0.15 &&
               is.finite(ct$p.value) && ct$p.value < 0.05) {
      fit <- stats::lm(growth_cm_yr ~ d1, data = g)
      xs <- seq(min(g$d1), max(g$d1), length.out = 50)
      yh <- as.numeric(stats::predict(fit, data.frame(d1 = xs)))
      p <- p %>% add_trace(x = xs, y = yh, type = "scatter", mode = "lines", inherit = FALSE,
        name = "trend", line = list(color = DDL$ink, width = 2.5, dash = "dash"),
        hovertemplate = "trend<extra></extra>")
      sprintf("Trend: %s (Spearman r = %+.2f, p = %.3f, n = %d)",
        if (ct$estimate < 0) "growth slows as plants get bigger" else "growth rises with size",
        ct$estimate, ct$p.value, nrow(g))
    } else sprintf("No clear size–growth trend at this site — shown as scatter only (n = %d)", nrow(g))
    p %>% plotly_theme(legend = TRUE) %>%
      plotly::layout(legend = list(orientation = "h", y = -0.25),
        title = list(text = sub, x = 0, xref = "paper", font = list(size = 12, color = DDL$muted)),
        margin = list(t = 46),
        xaxis = list(title = paste0(if (identical(sp$type, "shrubland")) "Basal diameter" else "DBH", " now (cm)")),
        yaxis = list(title = "Growth (cm/yr)", zeroline = TRUE))
  })

  # compound ANNUAL mortality rate (distinct from the snapshot pie)
  output$mortalityBanner <- renderUI({
    sp <- SP(); mr <- stand_mortality(rv$trees, sp)
    if (is.null(mr)) return(insight_banner("info-circle", tone = "navy",
      HTML("<b>Annual mortality:</b> needs ≥2 censuses of a big-enough cohort — not estimable here yet, so only the live/standing-dead snapshot below is shown.")))
    ci <- if (is.finite(mr$lo) && is.finite(mr$hi)) sprintf(" (95%% CI %.2f–%.2f)", mr$lo, mr$hi) else ""
    insight_banner("heart-pulse", tone = "pine",
      HTML(sprintf("Compound <b>annual mortality ≈ <span class='ci-hero'>%.2f%%/yr</span></b>%s — <b>%s</b> of <b>%s</b> tracked %s died over a median ~%.1f-yr interval. The forestry-standard rate; the breakdown below is a point-in-time snapshot, not a rate.",
        mr$rate_pct, ci, format(mr$deaths, big.mark = ","), format(mr$n0, big.mark = ","), sp$nouns, mr$t_yrs)))
  })

  # ---- SITE DATA-QUALITY scan (clickable inspector + downloadable report) -
  veg_qc <- reactive({ req(rv$trees); tree_qc_site(rv$trees, SP()) })
  qc_modal_rows <- reactiveVal(NULL)
  output$vegQcFlags <- renderUI({
    q <- veg_qc()
    if (is.null(q) || !q$n_flag) return(div(class = "qc-flag qc-flag-clean", bs_icon("check2-circle"),
      HTML(" <b>No data-quality flags.</b> No impossible status flips, implausible diameter jumps, or unexplained shrinks among the remeasured plants — clean.")))
    ic <- c(high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill")
    div(class = "qc-flag-list", lapply(q$flags, function(f)
      tags$button(class = paste0("qc-flag qc-flag-", f$level), type = "button",
        onclick = sprintf("Shiny.setInputValue('vegQcFlagClick', '%s', {priority:'event'})", f$key),
        bs_icon(ic[[f$level]]),
        tags$span(class = "qc-flag-txt", HTML(sprintf(" <b>%d</b> · %s", f$n, f$label))),
        tags$span(class = "qc-flag-go", bs_icon("chevron-right")))))
  })
  observeEvent(input$vegQcFlagClick, {
    q <- veg_qc(); req(q); f <- Filter(function(x) identical(x$key, input$vegQcFlagClick), q$flags)
    if (!length(f)) return(); f <- f[[1]]; rows <- f$rows; rows$flag <- NULL
    qc_modal_rows(list(key = f$key, rows = rows))
    shown <- utils::head(rows, 60)
    showModal(modalDialog(easyClose = TRUE, size = "l",
      title = tagList(bs_icon("clipboard-check"), sprintf(" %s — %d plants", f$label, f$n)),
      p(class = "qc-why", f$why),
      tags$div(class = "qc-modal-tbl",
        tags$table(class = "inspect-tbl",
          tags$thead(tags$tr(lapply(names(shown), function(nm) tags$th(nm)))),
          tags$tbody(lapply(seq_len(nrow(shown)), function(i)
            tags$tr(lapply(shown[i, ], function(v) tags$td(as.character(v)))))))),
      if (nrow(rows) > 60) p(class = "dim", sprintf("Showing 60 of %d — download for all.", nrow(rows))),
      footer = tagList(downloadButton("vegQcFlagCsv", "Download these (CSV)", class = "btn-outline-dark btn-sm"), modalButton("Close"))))
  })
  output$vegQcFlagCsv <- downloadHandler(
    filename = function() sprintf("NEON-veg-%s-QC-%s-%s.csv", rv$site %||% "site",
      (qc_modal_rows() %||% list(key = "flag"))$key, format(Sys.Date(), "%Y%m%d")),
    content = function(file) utils::write.csv((qc_modal_rows() %||% list(rows = data.frame()))$rows %||% data.frame(), file, row.names = FALSE, na = ""))
  output$vegQcReport <- downloadHandler(
    filename = function() sprintf("NEON-veg-%s-QC-report-%s.csv", rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    content = function(file) { q <- veg_qc(); utils::write.csv(if (is.null(q)) data.frame() else q$report, file, row.names = FALSE, na = "") })

  # ---- SIZE LAB (flagship) -----------------------------------------------
  output$labScatter <- renderPlotly({
    one <- rv$one; req(one); sp <- SP()
    one$size <- one[[sp$col]]
    pts <- one[is.finite(one$size) & one$size > 0 & is.finite(one$height) & one$height > 0 &
                 !is.na(one$scientificName), , drop = FALSE]
    if (!nrow(pts)) return(note_plot(sprintf("No %s with both a %s and a height to map", sp$nouns, sp$size_lab)))
    pts$short <- short_tree(pts$individualID)
    if (nrow(pts) > 1800) { set.seed(7); pts <- pts[sort(sample.int(nrow(pts), 1800)), ] }
    keycol <- input$labColor %||% "species"
    pts$key <- if (keycol == "species") as.character(pts$scientificName)
               else if (keycol == "canopyPosition") as.character(pts$canopyPosition)
               else ifelse(grepl("^Live", pts$plantStatus), "Live", "Dead/other")
    pts$key[is.na(pts$key) | pts$key == ""] <- "—"
    keys <- sort(unique(pts$key))
    kpal <- if (keycol == "species") (rv$pal %||% make_species_pal(pts))
            else stats::setNames(forest_ramp(length(keys)), keys)
    muted_col <- if (is_dark()) "#9fb0c4" else "#6b7a85"; qcol <- if (is_dark()) "#7e8da0" else "#9aa6b2"
    pts$tip <- paste0(
      "<span class='smt-pin-emoji'>", sp$emoji, "</span> <b>", pts$short, "</b><br/>",
      "<em>", ifelse(is.na(pts$scientificName), "—", pts$scientificName), "</em><br/>",
      "<span class='smt-pin-stats'>", round(pts$size,1), " cm ", sp$size_lab, " · ", round(pts$height,1), " m tall",
        ifelse(is.na(pts$canopyPosition), "", paste0("<br/>", pts$canopyPosition)), "</span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", pts$individualID,
        "'>", sp$emoji, " Open ", sp$noun, " career &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    p <- plot_ly()
    for (k in keys) { sub <- pts[pts$key == k, ]
      p <- p %>% add_trace(data = sub, x = ~size, y = ~height, type = "scatter", mode = "markers",
        name = k, customdata = ~tip, showlegend = length(keys) <= 12,
        marker = list(color = unname(kpal[k] %||% DDL$green), size = 9, opacity = 0.78, line = list(color = "#fff", width = 0.5)),
        text = ~paste0(sp$noun, " ", short, " · ", round(size,1), " cm"),
        hovertemplate = "%{text}<br>%{y:.1f} m tall<extra></extra>") }
    mx <- stats::median(pts$size); my <- stats::median(pts$height)
    xr <- range(pts$size); yr <- range(pts$height); px <- diff(xr)*0.02; py <- diff(yr)*0.02
    qlab <- function(x,y,t,xa,ya) list(text=t, x=x, y=y, xref="x", yref="y", showarrow=FALSE, xanchor=xa, yanchor=ya, font=list(color=qcol, size=10.5))
    ann <- list(
      list(text = sprintf("each dot is a %s · %s × height, by species", sp$noun, sp$size_lab), x=0, y=1.07, xref="paper", yref="paper",
           showarrow=FALSE, xanchor="left", font=list(color=muted_col, size=11)),
      qlab(xr[2]-px, yr[2]-py, sp$quad[["bigtall"]], "right", "top"),
      qlab(xr[1]+px, yr[2]-py, sp$quad[["smalltall"]], "left", "top"),
      qlab(xr[2]-px, yr[1]+py, sp$quad[["bigshort"]], "right", "bottom"),
      qlab(xr[1]+px, yr[1]+py, sp$quad[["smallshort"]], "left", "bottom"))
    tag <- rv$tree
    if (!is.null(tag)) { ir <- pts[pts$individualID == tag, ]
      if (nrow(ir) == 1) p <- p %>% add_trace(x = ir$size, y = ir$height, type="scatter", mode="markers",
        name = "★ viewing", customdata = ir$tip, showlegend = TRUE,
        marker = list(symbol="diamond", size=18, color="#E0A500", line=list(color="#fff", width=1.6)),
        hovertemplate = paste0("viewing ", ir$short, "<extra></extra>")) }
    p %>% plotly_theme() %>% plotly::layout(
      xaxis = list(title = paste0(if (identical(sp$type,"shrubland")) "Basal stem diameter" else "Diameter at breast height", " (cm)")), yaxis = list(title = "Height (m)"),
      shapes = list(list(type="line", xref="x", yref="paper", x0=mx, x1=mx, y0=0, y1=1, line=list(color=qcol, dash="dot", width=1)),
                    list(type="line", xref="paper", yref="y", x0=0, x1=1, y0=my, y1=my, line=list(color=qcol, dash="dot", width=1))),
      annotations = ann, hovermode = "closest")
  })
  output$labNote <- renderUI({
    one <- rv$one; req(one); sp <- SP(); sz <- one[[sp$col]]
    donly <- sum(is.finite(sz) & sz > 0 & !(is.finite(one$height) & one$height > 0))
    if (donly == 0) return(NULL)
    div(class = "qc-cap-note", style = "margin-top:6px", bs_icon("info-circle"),
      sprintf(" %s live stems were measured for %s but not height, so they can't be placed in this 2-D space — they're not shown here.", format(donly, big.mark = ","), sp$size_full))
  })
  output$treeCardSlot <- renderUI({
    sp <- SP()
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", sp$emoji), h4(sprintf("Tap a %s to see its card", sp$noun)),
      p(sprintf("Tap a dot above and choose “Open %s career”, or pick one in the sidebar.", sp$noun))))
    snap <- rv$snap; row <- one_per_tree(snap[snap$individualID == rv$tree, ], sp); if (!nrow(row)) return(NULL)
    div(class = "lab-sel", span(class = "ls-emoji", sp$emoji),
      div(class = "ls-body",
        div(class = "ls-id", tags$b(short_tree(rv$tree)), sprintf(" — %s · %s cm %s · %s m",
          ifelse(is.na(row$scientificName),"—",row$scientificName), round(row[[sp$col]],1), sp$size_lab, ifelse(is.na(row$height),"—",round(row$height,1)))),
        div(class = "ls-dom", ifelse(is.na(row$plantStatus),"",row$plantStatus))),
      actionButton("goTreeFromCard", tagList(bs_icon("arrows-fullscreen"), " Open full career"), class = "btn-outline-dark btn-sm"))
  })
  observeEvent(input$goTreeFromCard, nav_select("tabs", "tree"))

  # ---- TREE CAREER (profile, downloadable) -------------------------------
  tree_card_ui <- function(id) {
    SZ <- SP()
    snap <- rv$snap; row <- one_per_tree(snap[snap$individualID == id, ], SZ); if (!nrow(row)) return(NULL)
    hist <- tree_history(rv$trees, id); flags <- tree_qc_flags(hist, SZ)
    sp <- row$scientificName; dcol <- SZ$col
    # how big for its species (size percentile within species, this site)
    cohort <- rv$one[[dcol]][rv$one$scientificName %in% sp & is.finite(rv$one[[dcol]])]
    ncoh <- length(cohort)
    d_now <- row[[dcol]]
    pct <- if (ncoh >= 5 && is.finite(d_now)) round(100 * mean(cohort <= d_now)) else NA
    tile <- function(v, l) div(class = "qc-tile", div(class = "qc-tile-v", v), div(class = "qc-tile-l", l))
    growth <- { g <- tree_growth(rv$trees[rv$trees$individualID == id, ], SZ); if (!is.null(g) && nrow(g)) g$growth_cm_yr[1] else NA }
    # honest size tier: by within-species percentile if the cohort is big enough,
    # else by absolute size (thresholds differ by paradigm).
    tier_hi <- if (identical(SZ$type, "shrubland")) 10 else 60
    tier_mid <- if (identical(SZ$type, "shrubland")) 3 else 25
    tier_names <- if (identical(SZ$type, "shrubland")) c("Giant","Mature","Seedling") else c("Giant","Canopy","Sapling")
    tier <- if (!is.na(pct)) { if (pct >= 90) tier_names[1] else if (pct >= 50) tier_names[2] else tier_names[3] }
            else if (is.finite(d_now) && d_now >= tier_hi) tier_names[1]
            else if (is.finite(d_now) && d_now >= tier_mid) tier_names[2] else tier_names[3]
    tier_col <- stats::setNames(c("#1c4d2c", "#2f7d46", "#5aa46a"), tier_names)[[tier]]
    sparkid <- paste0("spark_", gsub("[^A-Za-z0-9]", "", id))
    flag_ic <- c(high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill")
    flags_ui <- if (length(flags) == 0)
      div(class = "qc-flag clean", span(class = "qc-flag-ic", bs_icon("check-circle-fill")),
          span(HTML(sprintf("<b>No QC flags.</b> This %s's remeasurements are internally consistent.", SZ$noun))))
    else tagList(lapply(flags, function(f) div(class = paste("qc-flag", f$level),
      span(class = "qc-flag-ic", bs_icon(flag_ic[[f$level]] %||% "info-circle-fill")), span(HTML(f$text)))))
    cap_tbl <- if (is.null(hist) || !nrow(hist)) NULL else {
      fnum <- function(x) ifelse(is.na(x) | !is.finite(x), "—", formatC(round(x,1), format="f", digits=1))
      dvals <- if (dcol %in% names(hist)) hist[[dcol]] else hist$stemDiameter
      tagList(div(class = "qc-section-h", bs_icon("clock-history"), " Every measurement"),
        div(class = "qc-cap-scroll", tags$table(class = "inspect-tbl",
          tags$thead(tags$tr(lapply(c("Date", sprintf("%s (cm)", SZ$size_lab), "Height (m)", "Status"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(hist)), function(i) tags$tr(
            tags$td(format(hist$date[i], "%Y-%m-%d")), tags$td(fnum(dvals[i])),
            tags$td(fnum(hist$height[i])), tags$td(ifelse(is.na(hist$plantStatus[i]),"—",hist$plantStatus[i]))))))))
    }
    body <- div(id = "qcCardNode", class = "qc-card", `data-short` = short_tree(id),
      div(class = "qc-head", span(class = "qc-emoji", SZ$emoji),
        div(div(class = "qc-id", short_tree(id)),
            div(class = "qc-sci", em(ifelse(is.na(sp),"unidentified",sp)),
                sprintf(" · %s · %s", row$growthForm %||% "", row$plotID))),
        div(class = "qc-head-badges", glow_badge(ifelse(is.na(row$plantStatus),"—",row$plantStatus),
            if (grepl("^Live", row$plantStatus %||% "")) DDL$green else DDL$dead))),
      div(class = "qc-tiles",
        tile(ifelse(is.finite(d_now), round(d_now,1), "—"), paste0("cm ", SZ$size_lab)),
        tile(ifelse(is.finite(row$height), round(row$height,1), "—"), "m tall"),
        tile(ifelse(is.finite(growth), sprintf("%+.2f", growth), "—"), "cm/yr"),
        tile(ifelse(is.na(pct), "—", paste0(pct, "%")), if (!is.na(pct)) sprintf("%%ile of %d live", ncoh) else "size %ile"),
        tile(if (is.null(hist)) "—" else nrow(hist), "visits"),
        tile(ifelse(is.na(row$canopyPosition), "—", gsub(" .*","",row$canopyPosition)), "canopy")),
      div(class = "qc-section-h", bs_icon("graph-up"), sprintf(" Growth trajectory (%s over time)", SZ$size_full)),
      if (!is.null(hist) && sum(is.finite(if (dcol %in% names(hist)) hist[[dcol]] else hist$stemDiameter)) >= 2) plotlyOutput("treeSpark", height = "170px") else p(class = "qc-cap-note", "Single visit — no trajectory yet."),
      div(class = "qc-section-h", bs_icon("clipboard-check"), " Data-quality check"), flags_ui,
      cap_tbl,
      p(class = "qc-cap-note", style = "margin-top:8px", bs_icon("info-circle"),
        sprintf(" A flag means “verify against the field record”, not “wrong”. %s are remeasured every few years, so gaps are normal.", SZ$Nouns)))
    tcstat <- function(v, l) div(class = "tc-stat", div(class = "tc-stat-v", v), div(class = "tc-stat-l", l))
    tcard <- div(class = "tradingcard-wrap",
      div(id = "treeCardNode", class = "trade-card", `data-short` = short_tree(id),
          style = sprintf("--rc:%s", tier_col),
        div(class = "tc-holo"),
        div(class = "tc-top", span(class = "tc-tier", toupper(tier)), span(class = "tc-brand", "NEON · VST")),
        div(class = "tc-emoji-wrap", span(class = "tc-emoji", SZ$emoji)),
        div(class = "tc-id", short_tree(id)),
        div(class = "tc-sci", em(ifelse(is.na(sp), "unidentified", sp))),
        div(class = "tc-nick", row$plotID),
        div(class = "tc-stats",
          tcstat(ifelse(is.finite(d_now), round(d_now, 1), "—"), paste0("cm ", SZ$size_lab)),
          tcstat(ifelse(is.finite(row$height), round(row$height, 1), "—"), "m tall"),
          tcstat(ifelse(is.finite(growth), sprintf("%+.2f", growth), "—"), "cm/yr"),
          tcstat(if (is.null(hist)) "—" else nrow(hist), "visits")),
        div(class = "tc-foot", span(class = "tc-foot-app", "Vegetation Structure"),
            span(if (is.na(pct)) "" else paste0(pct, "%ile for species")))),
      div(class = "tc-toolbar",
        tags$button(class = "tc-save-btn", type = "button", onclick = "smtSaveTreeCard()",
                    bsicons::bs_icon("download"), " Save card (PNG)"),
        tags$span(class = "tc-hint", "A shareable tree card — downloads as a PNG")))
    div(tcard, body, div(class = "qc-toolbar",
      tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveQcCard()", bsicons::bs_icon("download"), " Save QC record (PNG)"),
      downloadButton("treeCsv", "Download tree data (CSV)", class = "smt-clear-btn")))
  }
  # ONE fixed output (not a per-tree id) — avoids accumulating a new binding for
  # every tree the user opens; recomputed on rv$tree change.
  output$treeSpark <- renderPlotly({
    id <- rv$tree; req(id); sp <- SP(); dcol <- if (sp$col %in% names(rv$trees)) sp$col else "stemDiameter"
    h <- tree_history(rv$trees, id); h$.d <- h[[dcol]]; h <- h[is.finite(h$.d), ]; if (is.null(h) || nrow(h) < 2) return(note_plot("—"))
    plot_ly(h, x = ~date, y = ~.d, type = "scatter", mode = "lines+markers",
      line = list(color = DDL$green, width = 2.5), marker = list(color = DDL$green2, size = 7),
      hovertemplate = "%{x|%Y}<br>%{y:.1f} cm<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = ""), yaxis = list(title = paste0(sp$size_lab, " (cm)")), margin = list(l = 45, r = 10, t = 10, b = 30))
  })
  output$treeProfile <- renderUI({
    sp <- SP()
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", sp$emoji), h4(sprintf("Pick a %s to open its career", sp$noun)),
      p("Use the Size Lab (tap a dot → “Open career”) or the sidebar picker.")))
    div(class = "plot-profile-wrap", tree_card_ui(rv$tree))
  })
  output$treeCsv <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_tree-%s_%s.csv", rv$site %||% "site", short_tree(rv$tree %||% "tree"), format(Sys.Date(), "%Y%m%d")),
    content = function(file) { id <- rv$tree; req(id)
      d <- tidy_trees_export(rv$trees[rv$trees$individualID == id, , drop = FALSE]); req(!is.null(d))
      utils::write.csv(d, file, row.names = FALSE, na = "") }, contentType = "text/csv")

  # ---- FULL-SITE DATA EXPORT (tidy CSVs + codebook, zipped) ---------------
  output$allDataZip <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_data_%s.zip", rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    contentType = "application/zip",
    content = function(file) {
      req(rv$trees)
      tmp <- tempfile("vstexport"); dir.create(tmp)
      site <- rv$site %||% "site"; sp <- SP()
      tl <- tidy_trees_export(rv$trees)
      pl <- plots_export(rv$snap, rv$plots, sp)
      cb <- veg_codebook()
      st <- stand_site(rv$snap, rv$plots, sp)
      scope <- if (identical(sp$type, "shrubland")) "live shrubs by basal stem diameter over totalSampledAreaShrubSapling" else "live trees >=10 cm DBH over totalSampledAreaTrees"
      readme <- c(
        sprintf("NEON Vegetation Structure Explorer — data export for site %s (%s)", site, sp$type),
        sprintf("Generated %s by an unofficial Desert Data Labs explorer.", format(Sys.Date(), "%Y-%m-%d")),
        "Source: NEON Vegetation structure DP1.10098.001 (vst_mappingandtagging x vst_apparentindividual; vst_perplotperyear).",
        "",
        "FILES",
        " trees_long.csv  — one row per individual x measurement bout (the raw growth career; aggregate it yourself).",
        " plots.csv       — one row per plot: sampled area + per-hectare stand summary.",
        " data_dictionary.csv — column definitions, types, units.",
        "",
        "NOTES",
        sprintf(" * This is a %s site: plants are sized by %s (dbh_cm is ~empty for short desert shrubs; use basal_stem_diam_cm).", sp$type, sp$size_full),
        " * 'snapshot' analyses elsewhere in the app use each plant's LATEST bout; here you get every bout.",
        sprintf(" * Stand metrics scope to %s (an index, not a wall-to-wall inventory).", scope),
        " * Tower vs distributed plots differ in selection probability — split on plots.csv$plotType before pooling.",
        if (!is.null(st)) sprintf(" * Pooled stand: %s m2/ha (+/-%s SE) basal area, %s stems/ha, QMD %s cm, n=%d plots.",
                                  st$ba_ha, st$ba_se, format(st$density_ha, big.mark=","), st$qmd, st$n_plots) else "")
      if (!is.null(tl)) utils::write.csv(tl, file.path(tmp, "trees_long.csv"), row.names = FALSE, na = "")
      if (!is.null(pl)) utils::write.csv(pl, file.path(tmp, "plots.csv"), row.names = FALSE, na = "")
      utils::write.csv(cb, file.path(tmp, "data_dictionary.csv"), row.names = FALSE, na = "")
      writeLines(readme, file.path(tmp, "README.txt"))
      fs <- list.files(tmp, full.names = TRUE)
      old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = basename(fs), flags = "-q")
    })

  # ---- STAND REPORT PDF ---------------------------------------------------
  output$reportPdf <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_stand-report_%s.pdf", rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    contentType = "application/pdf",
    content = function(file) {
      req(rv$snap)
      build_stand_report(file, snap = rv$snap, trees = rv$trees, plots = rv$plots,
                         one = rv$one, label = rv$label %||% rv$site %||% "site", spec = SP())
    })

  # ---- MAP ----------------------------------------------------------------
  output$map <- leaflet::renderLeaflet({
    lb <- rv$lb
    if (is.null(lb) || !nrow(lb)) {
      ctr <- if (!is.null(rv$plots) && nrow(rv$plots))
        c(stats::median(rv$plots$lng, na.rm = TRUE), stats::median(rv$plots$lat, na.rm = TRUE)) else c(-98, 39)
      return(leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(ctr[1], ctr[2], zoom = if (all(is.finite(ctr))) 9 else 4) %>%
        leaflet::addControl("No plot-level stand data to map for this site — try another site.", position = "topright"))
    }
    metric <- input$mapMetric %||% "ba_ha"
    val <- lb[[metric]]; val[is.na(val)] <- 0
    dom <- if (diff(range(val, na.rm=TRUE)) > 0) range(val, na.rm=TRUE) else c(val[1]-1, val[1]+1)
    pal <- leaflet::colorNumeric("viridis", domain = dom)
    rr <- range(lb$ba_ha, na.rm = TRUE); lb$radius <- if (diff(rr) > 0) 6 + 14*(lb$ba_ha - rr[1])/diff(rr) else 11
    leaflet::leaflet(lb) %>% leaflet::addProviderTiles(input$view %||% "CartoDB.Positron") %>%
      leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~radius, fillColor = pal(val),
        color = "#fff", weight = 1, fillOpacity = 0.85, layerId = ~plotID,
        label = ~lapply(sprintf("<b>%s</b><br>%s m²/ha · %s stems/ha · %s species", short_plot(plotID), ba_ha, format(density_ha, big.mark=","), n_species), htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = val,
        title = switch(metric, ba_ha = "m²/ha", density_ha = "stems/ha", "species"))
  })

  # ---- ABOUT --------------------------------------------------------------
  output$aboutPanel <- renderUI({
    div(class = "about-wrap",
      div(class = "about-card", h4("\U0001F333 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Vegetation structure"), " product (", tags$code("DP1.10098.001"),
          ") across ", tags$b("42 sites and every biome"), " — temperate and boreal forests, desert and sage shrublands, grasslands, alpine tundra, and tropical. NEON tags individual woody plants, maps them, and remeasures their ", tags$b("diameter, height, and status"), " over the years — so each plant has a growth career.")),
      div(class = "about-card", h4(bs_icon("rulers"), " Two ways woody plants are measured"),
        p("The app adapts to each site. In ", tags$b("forests"), ", trees are sized by ", tags$b("DBH"), " (diameter at breast height, ~130 cm) and tallied over the sampled tree area (≥10 cm). In ", tags$b("desert & shrubland"), " sites, plants are too short for a breast-height diameter, so they're sized by ", tags$b("basal stem diameter"), " (near the ground) over the shrub-sapling sampled area — the honest size measure NEON records for ~96–99% of desert stems."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Stand metrics are scaled by each plot's sampled area to per-hectare values (mean ± SE across plots), but they're indices from the sampled plots — not a wall-to-wall inventory. Tower and distributed plots are pooled — split them on ", tags$code("plotType"), " in the data export for a design-based estimate. QMD is stem-weighted; basal area and density are averaged across plots (equal plot weight).")),
      div(class = "about-card", h4(bs_icon("graph-up"), " Growth & status"),
        p("Diameter increments come from remeasured plants (one rate each). Decreases are common and usually real (bark sloughing, drought, a changed measurement height) — kept and flagged, not deleted. Live/dead is a snapshot ratio, not an annual mortality rate."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Above-ground biomass is deliberately ", tags$b("not"), " estimated — it requires an allometric model whose error compounds; basal area (directly measured) is the honest stand measure shown here.")),
      div(class = "about-card", h4(bs_icon("diagram-3"), " A NEONize sibling"),
        p("Built to the Desert Data Labs NEON quality bar — the same flow, bundling, and pin-card interaction as its siblings — with a ", tags$b("cross-biome"), " identity and woody-structure-native analyses that adapt from old-growth conifers to desert shrubs. See the NEONize playbook."),
        p(bs_icon("envelope"), " ", tags$a(href = "mailto:desertdatalabs@gmail.com", "desertdatalabs@gmail.com"),
          " · ", tags$a(href = "https://data.neonscience.org/data-products/DP1.10098.001", target = "_blank", "NEON data product"))))
  })

  # ---- clickable hero stats -> ranked-breakdown modals -------------------
  observeEvent(input$heroClick, {
    sp <- SP()
    if (identical(input$heroClick, "species")) {
      ss <- species_structure(rv$snap, rv$plots, sp); req(!is.null(ss), nrow(ss) > 0)
      tot <- sum(ss$ba_m2, na.rm = TRUE)
      items <- lapply(seq_len(min(20, nrow(ss))), function(i) tags$li(class = "rank-row",
        span(class = paste("rank-num", if (i <= 3) "top"), i),
        span(class = "rank-name", em(ifelse(is.na(ss$scientificName[i]), "—", ss$scientificName[i]))),
        span(class = "rank-metric", sprintf("%.1f m²", ss$ba_m2[i])),
        span(class = "rank-sub", sprintf("%s%% · %s stems", round(100 * ss$ba_m2[i] / tot), ss$stems[i]))))
      showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("bar-chart-steps"), " Species by basal area"),
        div(class = "rank-modal-sub", sprintf("Live %s, ranked by total basal area (relative dominance).", sp$nouns)),
        tags$ul(class = "rank-list", items), footer = modalButton("Close")))
    } else if (identical(input$heroClick, "biggest")) {
      one <- rv$one; req(one); d <- woody_only(one[is.finite(one[[sp$col]]), ], sp); req(nrow(d) > 0)
      d <- d[order(-d[[sp$col]]), ][seq_len(min(20, nrow(d))), ]
      items <- lapply(seq_len(nrow(d)), function(i) tags$li(class = "rank-row rank-click",
        onclick = sprintf("Shiny.setInputValue('rankPick','%s',{priority:'event'})", d$individualID[i]),
        span(class = paste("rank-num", if (i <= 3) "top"), i),
        span(class = "rank-name", tags$b(short_tree(d$individualID[i])), " ",
             em(ifelse(is.na(d$scientificName[i]), "—", d$scientificName[i]))),
        span(class = "rank-metric", sprintf("%.1f cm", d[[sp$col]][i])),
        span(class = "rank-go", bs_icon("arrow-right-circle"))))
      showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("trophy"), sprintf(" Biggest %s by %s", sp$nouns, sp$size_lab)),
        div(class = "rank-modal-sub", sprintf("Tap a %s to open its career.", sp$noun)),
        tags$ul(class = "rank-list", items), footer = modalButton("Close")))
    }
  })
  observeEvent(input$rankPick, { removeModal(); pick_tree(input$rankPick, navigate = TRUE) })

  # ---- CHAMPION TREES (leaderboard) --------------------------------------
  champion_df <- function(metric) {
    if (is.null(rv$one) || !nrow(rv$one)) return(NULL)
    one <- rv$one; sp <- SP()
    if (identical(metric, "fastest")) {
      g <- tree_growth(rv$trees, sp); if (is.null(g) || !nrow(g)) return(NULL)
      g <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5 & !g$mh_change, , drop = FALSE]
      if (!nrow(g)) return(NULL); g <- g[order(-g$growth_cm_yr), ]
      data.frame(id = g$individualID, tree = short_tree(g$individualID), species = g$scientificName,
                 value = g$growth_cm_yr, unit = "cm/yr", stringsAsFactors = FALSE)
    } else if (identical(metric, "career")) {
      b <- rv$trees %>% dplyr::group_by(.data$individualID) %>%
        dplyr::summarise(visits = dplyr::n_distinct(.data$date),
                         yrs = round(as.numeric(max(.data$date) - min(.data$date)) / 365.25, 1),
                         species = dplyr::first(.data$scientificName), .groups = "drop")
      b <- b[order(-b$visits, -b$yrs), ]
      data.frame(id = b$individualID, tree = short_tree(b$individualID), species = b$species,
                 value = b$visits, unit = "visits", stringsAsFactors = FALSE)
    } else {
      col <- if (identical(metric, "tallest")) "height" else sp$col
      d <- one[is.finite(one[[col]]), ]; if (identical(metric, "biggest")) d <- woody_only(d, sp)
      if (!nrow(d)) return(NULL); d <- d[order(-d[[col]]), ]
      data.frame(id = d$individualID, tree = short_tree(d$individualID), species = d$scientificName,
                 value = round(d[[col]], 1), unit = if (identical(metric, "tallest")) "m" else paste0("cm ", sp$size_lab),
                 stringsAsFactors = FALSE)
    }
  }
  output$famePodium <- renderUI({
    df <- champion_df(input$fameMetric %||% "biggest"); if (is.null(df) || !nrow(df)) return(NULL)
    top <- utils::head(df, 3); medals <- c("\U0001F947", "\U0001F948", "\U0001F949")
    cls <- c("podium-1", "podium-2", "podium-3"); cols <- c(DDL$gold, DDL$muted, DDL$bark)
    cards <- lapply(c(2, 1, 3), function(k) { if (k > nrow(top)) return(NULL); r <- top[k, ]
      div(class = paste("podium-card", cls[k]), style = sprintf("--rc:%s", cols[k]),
        onclick = sprintf("Shiny.setInputValue('famePick','%s',{priority:'event'})", r$id),
        div(class = "podium-medal", medals[k]), div(class = "podium-emoji", SP()$emoji),
        div(class = "podium-id", r$tree),
        div(class = "podium-stat", sprintf("%s %s", r$value, r$unit)),
        div(class = "podium-sp", em(ifelse(is.na(r$species), "—", r$species)))) })
    div(class = "podium", cards)
  })
  output$fameTable <- DT::renderDT({
    sp <- SP(); df <- champion_df(input$fameMetric %||% "biggest")
    if (is.null(df) || !nrow(df)) return(DT::datatable(data.frame(Message = sprintf("No %s for this ranking yet.", sp$nouns)),
      rownames = FALSE, options = list(dom = "t")))
    df2 <- utils::head(df, 25)
    show <- data.frame(Rank = seq_len(nrow(df2)), Plant = df2$tree, Species = df2$species,
                       Value = df2$value, Unit = df2$unit, check.names = FALSE)
    names(show)[2] <- sp$Noun
    DT::datatable(show, rownames = FALSE, selection = "single", class = "leader-dt",
      options = list(pageLength = 10, dom = "tp", columnDefs = list(list(className = "dt-right", targets = 3))))
  })
  observeEvent(input$fameTable_rows_selected, {
    df <- champion_df(input$fameMetric %||% "biggest"); req(!is.null(df))
    i <- input$fameTable_rows_selected; if (length(i) && i <= nrow(df)) pick_tree(df$id[i], navigate = TRUE)
  })
  observeEvent(input$famePick, pick_tree(input$famePick, navigate = TRUE))
  observeEvent(input$goFame, nav_select("tabs", "fame"))

  # ---- quick-pick chips (Biggest / Tallest / Fastest) --------------------
  observeEvent(input$pickBiggest, { df <- champion_df("biggest"); if (!is.null(df) && nrow(df)) pick_tree(df$id[1], navigate = TRUE) })
  observeEvent(input$pickTallest, { df <- champion_df("tallest"); if (!is.null(df) && nrow(df)) pick_tree(df$id[1], navigate = TRUE) })
  observeEvent(input$pickFastest, { df <- champion_df("fastest"); if (!is.null(df) && nrow(df)) pick_tree(df$id[1], navigate = TRUE) })

  # ---- COMPARE TWO STANDS ------------------------------------------------
  compare_stats <- function(site) {
    b <- load_site_bundle(site); if (is.null(b)) return(NULL)
    snap <- tree_snapshot(b$trees)
    spc <- size_spec(b$meta$structure_type %||% classify_structure(snap))
    st <- stand_site(snap, b$plots, spc)
    one <- one_per_tree(live_only(snap), spc); woody_sp <- species_level_only(woody_only(one, spc))
    list(site = site, st = st, type = spc$type, size_lab = spc$size_lab,
         n_species = dplyr::n_distinct(woody_sp$scientificName),
         tallest = round(smax(live_only(snap)$height), 1),
         biggest = round(smax(woody_only(live_only(snap), spc)[[spc$col]]), 1))
  }
  observeEvent(input$compareBtn, {
    sites <- stats::setNames(site_table$site, sprintf("%s — %s", site_table$site, site_table$name))
    if (!length(sites)) return(showNotification("No other sites bundled to compare.", type = "warning"))
    showModal(modalDialog(size = "l", easyClose = TRUE, title = tagList(bs_icon("layout-split"), " Compare two stands"),
      div(class = "compare-pickers",
        selectInput("cmpA", "Stand A", choices = as.list(sites), selected = rv$site %||% unname(sites)[1]),
        selectInput("cmpB", "Stand B", choices = as.list(sites), selected = if (length(sites) > 1) unname(sites)[2] else unname(sites)[1])),
      div(class = "cmp-run", actionButton("runCompare", tagList(bs_icon("play-fill"), " Compare"), class = "btn-primary btn-sm")),
      uiOutput("compareOut"),
      footer = modalButton("Close")))
  })
  output$compareOut <- renderUI({
    req(input$runCompare)
    a <- isolate(compare_stats(input$cmpA)); b <- isolate(compare_stats(input$cmpB))
    if (is.null(a) || is.null(b) || is.null(a$st) || is.null(b$st))
      return(div(class = "compare-hint", bs_icon("exclamation-triangle"), " One of those sites isn't bundled with usable plot data."))
    rowf <- function(lab, va, vb, fmt = "%s") {
      na <- suppressWarnings(as.numeric(va)); nb <- suppressWarnings(as.numeric(vb))
      wa <- is.finite(na) && is.finite(nb) && na > nb; wb <- is.finite(na) && is.finite(nb) && nb > na
      tags$tr(tags$td(class = "cmp-lab", lab),
        tags$td(class = paste("cmp-val", if (wa) "cmp-win"), sprintf(fmt, va)),
        tags$td(class = paste("cmp-val", if (wb) "cmp-win"), sprintf(fmt, vb)))
    }
    mixed <- !identical(a$type, b$type)
    sizelab <- if (mixed) "stem ø" else a$size_lab
    tbl <- tags$table(class = "compare-table",
      tags$thead(tags$tr(tags$th(""),
        tags$th(div(class = "cmp-head", a$site, tags$small(sprintf(" · %s", a$type)))),
        tags$th(div(class = "cmp-head", b$site, tags$small(sprintf(" · %s", b$type)))))),
      tags$tbody(
        rowf("Basal area (m²/ha)", a$st$ba_ha, b$st$ba_ha),
        rowf("Stem density (/ha)", a$st$density_ha, b$st$density_ha),
        rowf(sprintf("Quadratic mean %s (cm)", sizelab), a$st$qmd, b$st$qmd),
        rowf("Species", a$n_species, b$n_species),
        rowf("Tallest (m)", a$tallest, b$tallest),
        rowf(sprintf("Biggest %s (cm)", sizelab), a$biggest, b$biggest),
        rowf("Plots sampled", a$st$n_plots, b$st$n_plots)))
    div(tbl, div(class = "compare-foot", bs_icon("info-circle"),
      if (mixed) " Note: one site is a forest (sized by DBH) and one a shrubland (sized by basal diameter) — size rows aren't directly comparable. " else " ",
      "Stand indices from the sampled plots (mean across plots), not a wall-to-wall inventory; tower and distributed plots are pooled."))
  })

  # ---- guided tour (on demand) -------------------------------------------
  observeEvent(input$tourBtn, session$sendCustomMessage("startTour", list()))

  observeEvent(input$help, {
    showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("question-circle"), " How it works"),
      tags$ul(
        tags$li(HTML("Pick a <b>site</b> (or open the Harvard Forest demo). Numbers describe each plant's <b>most recent measurement</b>. Forest sites are sized by tree <b>DBH</b>; desert/shrubland sites by <b>basal stem diameter</b> — the app adapts.")),
        tags$li(HTML("<b>Stand Structure</b> — the size-class curve, height profile, and per-hectare basal area & density.")),
        tags$li(HTML("<b>Growth & Mortality</b> — how fast stems grow between visits, the fastest growers, and the live/dead split.")),
        tags$li(HTML("<b>Size Lab</b> — every plant as a dot (size × height); <b>tap one</b> to pin its card, then “Open career” for its full growth history.")),
        tags$li(HTML("<b>Champion plants</b> — the biggest, tallest, fastest-growing, and longest-tracked; tap one to open it.")),
        tags$li(HTML("<b>Compare</b> two stands head-to-head, and download the <b>full data</b> (CSV + codebook) or a <b>stand report PDF</b>.")),
        tags$li(HTML("Most plots are remeasured every ~5 years, so growth is per-year between visits."))),
      footer = tagList(tags$button(type = "button", class = "btn btn-outline-dark btn-sm",
        onclick = "(function(){var m=document.querySelector('.modal.show button[data-bs-dismiss=modal],.modal.show .btn-close');if(m)m.click();setTimeout(vegTour,250);})()",
        bsicons::bs_icon("signpost-2"), " Take the tour"), modalButton("Got it"))))
  })
}
