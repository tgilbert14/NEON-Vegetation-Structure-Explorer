# ===========================================================================
# NEON Vegetation Structure Explorer — server.R
# ===========================================================================
server <- function(input, output, session) {

  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark()
    ink  <- if (dark) "#e8eef2" else "#1f2a30"; grid <- if (dark) "rgba(220,230,240,0.10)" else "rgba(31,42,48,0.08)"
    zero <- if (dark) "rgba(220,230,240,0.22)" else "rgba(31,42,48,0.15)"; lin <- if (dark) "#3a4759" else "#d6ddd4"
    legc <- if (dark) "#c3cedd" else "#344049"
    p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Rubik"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 55, r = 30, t = 48, b = 44),
      hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#FFD200",
        font = list(color = "#ffffff", family = "Rubik", size = 13))) %>%
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
                       pal = NULL, label = NULL, site = NULL, tree = NULL, ctx = NULL, is_demo = FALSE)

  observe({ ch <- veg_state_choices(); updateSelectInput(session, "stateSel", choices = ch,
            selected = if ("MA" %in% ch) "MA" else NULL) })
  observeEvent(input$stateSel, updateSelectInput(session, "site", choices = veg_sites_in_state(input$stateSel)), ignoreInit = FALSE)
  output$siteBio <- renderUI({ req(input$site); b <- site_bio(input$site); if (is.null(b)) return(NULL)
    div(class = "site-bio", bs_icon("info-circle-fill"), span(b)) })

  output$siteCards <- renderUI({
    if (is.null(SITE_INDEX) || !nrow(site_table)) return(NULL)
    div(class = "site-cards", lapply(seq_len(nrow(site_table)), function(i) {
      r <- site_table[i, ]
      tags$a(class = "site-card", href = "#",
        onclick = sprintf("smtLoadStart('%s — loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});return false;",
                          gsub("'", "", r$name), r$site),
        div(class = "sc-emoji", "\U0001F333"),
        div(class = "sc-body",
          div(class = "sc-name", tags$b(r$site), sprintf(" · %s", r$name)),
          div(class = "sc-meta", sprintf("%s · %s trees · %s species · tallest %sm",
            r$state, format(r$n_trees, big.mark = ","), r$n_species, r$tallest_m)))) }))
  })

  shinyjs::hide("mainTabsWrap")

  ingest <- function(b, label, is_demo = FALSE) {
    if (is.null(b) || is.null(b$trees) || !nrow(b$trees)) {
      session$sendCustomMessage("loadDone", list()); showNotification("No vegetation data for that site.", type = "warning"); return(invisible()) }
    rv$trees <- b$trees
    rv$snap  <- tree_snapshot(b$trees)             # latest bout per tree
    rv$one   <- one_per_tree(live_only(rv$snap))   # one row per LIVE tree (largest stem)
    rv$plots <- b$plots
    rv$lb    <- plot_summary_veg(rv$snap, b$plots)
    rv$pal   <- make_species_pal(species_level_only(rv$snap))
    rv$label <- label; rv$site <- b$meta$site; rv$is_demo <- is_demo; rv$tree <- NULL
    yrs <- range(b$trees$year, na.rm = TRUE)
    rv$ctx <- paste0(b$meta$site, " · ", if (yrs[1] == yrs[2]) yrs[1] else paste0(yrs[1], "–", yrs[2]))
    shinyjs::show("mainTabsWrap"); shinyjs::show("treePickerWrap"); shinyjs::hide("splash")
    one <- rv$one[is.finite(rv$one$stemDiameter), ]
    ch <- setNames(one$individualID, sprintf("%s · %s · %scm",
            short_tree(one$individualID), ifelse(is.na(one$scientificName), "—", one$scientificName), round(one$stemDiameter)))
    updateSelectizeInput(session, "treeSel", choices = c("Pick a tree…" = "", ch), selected = "", server = TRUE)
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

  pick_tree <- function(id, navigate = FALSE) {
    if (is.null(id) || is.na(id) || id == "") return()
    if (is.null(rv$snap) || !(id %in% rv$snap$individualID)) return()
    rv$tree <- id
    if (!identical(input$treeSel, id)) updateSelectizeInput(session, "treeSel", selected = id)
    if (navigate) nav_select("tabs", "tree")
  }
  observeEvent(input$treeSel, if (nzchar(input$treeSel %||% "")) pick_tree(input$treeSel, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_tree(input$qcCardRequest, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$surpriseBtn, { one <- rv$one; req(one); pick_tree(sample(one$individualID, 1), navigate = TRUE) })

  observeEvent(input$goStand,  nav_select("tabs", "stand"))
  observeEvent(input$goGrowth, nav_select("tabs", "growth"))
  observeEvent(input$goLab,    nav_select("tabs", "lab"))
  observeEvent(input$goTree,   { if (is.null(rv$tree) && !is.null(rv$one)) rv$tree <- rv$one$individualID[which.max(rv$one$stemDiameter)]; nav_select("tabs", "tree") })
  observeEvent(input$goMap,    nav_select("tabs", "map"))

  # ---- hero ---------------------------------------------------------------
  output$heroStats <- renderUI({
    one <- rv$one; snap <- rv$snap; if (is.null(one)) return(NULL)
    sp <- species_level_only(one)
    hero <- function(v, l, suf = "", icon, tone, ttl = NULL) div(class = paste0("hero-stat hero-", tone), title = ttl,
      div(class = "hs-icon", bs_icon(icon)),
      div(div(class = "hs-v count-up", `data-target` = v, `data-suffix` = suf, "0"), div(class = "hs-l", l)))
    div(class = "hero-band",
      div(class = "hero-title", bs_icon("broadcast"), tags$b(rv$label)),
      div(class = "hero-grid",
        hero(nrow(one[one$growthForm %in% TREE_FORMS, ]), "live trees", icon = "tree", tone = "pine"),
        hero(dplyr::n_distinct(sp$scientificName), "species", icon = "diagram-3", tone = "navy"),
        hero(round(max(snap$height, na.rm = TRUE), 1), "m tallest", icon = "arrows-vertical", tone = "gold"),
        hero(round(max(snap$stemDiameter, na.rm = TRUE), 1), "cm biggest DBH", icon = "circle", tone = "terra")))
  })

  # ---- OVERVIEW -----------------------------------------------------------
  output$baBar <- renderPlotly({
    ss <- species_structure(rv$snap, rv$plots); if (is.null(ss) || !nrow(ss)) return(note_plot("No basal-area data"))
    ss <- head(ss, 18); ss$scientificName <- factor(ss$scientificName, levels = rev(ss$scientificName))
    pal <- rv$pal %||% make_species_pal(rv$snap)
    plot_ly(ss, x = ~ba_m2, y = ~scientificName, type = "bar", orientation = "h",
      marker = list(color = unname(pal[as.character(ss$scientificName)] %||% DDL$green)),
      text = ~paste0(stems, " stems"), hovertemplate = "%{y}<br>%{x:.1f} m² basal area · %{text}<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Total basal area (m²)"), yaxis = list(title = ""), margin = list(l = 200))
  })
  output$overviewInsight <- renderUI({
    ss <- species_structure(rv$snap, rv$plots); req(!is.null(ss), nrow(ss) > 0)
    st <- stand_site(rv$snap, rv$plots)
    insight_banner("stars", tone = "pine",
      HTML(sprintf("<b><i>%s</i></b> dominates the stand by basal area (%d stems). The forest holds <span class='ci-hero'>%d</span> tree species at about <b>%s</b> m²/ha basal area.",
        ss$scientificName[1], ss$stems[1], dplyr::n_distinct(species_level_only(rv$one)$scientificName),
        if (is.null(st)) "—" else st$ba_ha)))
  })
  output$siteInsights <- renderUI({
    snap <- rv$snap; one <- rv$one; req(snap, one)
    st <- stand_site(snap, rv$plots); g <- tree_growth(rv$trees); ss <- species_structure(snap, rv$plots)
    big <- one[which.max(one$stemDiameter), ]; tall <- one[which.max(one$height), ]
    pts <- c()
    if (!is.null(st)) pts <- c(pts, sprintf("Stand density is about <b>%s stems/ha</b> at <b>%s m²/ha</b> basal area (quadratic mean diameter %s cm).", format(st$density_ha, big.mark=","), st$ba_ha, st$qmd))
    if (nrow(big)) pts <- c(pts, sprintf("The biggest tree is a <b><i>%s</i></b> at <b>%s cm</b> diameter; the tallest reaches <b>%s m</b> (<i>%s</i>).", big$scientificName, round(big$stemDiameter,1), round(tall$height,1), tall$scientificName))
    if (!is.null(g) && nrow(g)) { gg <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5, ]; if (nrow(gg)) pts <- c(pts, sprintf("Across <b>%s</b> remeasured trees, diameter grows a median of <b>%.2f cm/yr</b>.", format(nrow(gg), big.mark=","), stats::median(gg$growth_cm_yr, na.rm=TRUE))) }
    pts <- c(pts, "Basal area and density are stand indices from the sampled plots — not a wall-to-wall inventory; biomass isn't estimated (it needs an allometric model NEON doesn't publish here).")
    div(class = "insight-list", lapply(pts, function(t) div(class = "il-item", bs_icon("dot"), HTML(t))))
  })

  # ---- STAND STRUCTURE ----------------------------------------------------
  output$sizePlot <- renderPlotly({
    sc <- size_class(rv$snap); if (is.null(sc)) return(note_plot("No diameter data"))
    plot_ly(sc, x = ~cls, y = ~stems, type = "bar", marker = list(color = DDL$green),
      hovertemplate = "%{x} cm DBH<br>%{y} stems<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Diameter class (cm DBH)"), yaxis = list(title = "Live stems"))
  })
  output$sizeInsight <- renderUI({
    sc <- size_class(rv$snap); req(!is.null(sc))
    small <- sum(sc$stems[sc$cls %in% c("0–10","10–20")]); big <- sum(sc$stems[sc$cls %in% c("50–70","70+")])
    shape <- if (small > 3 * big) "a descending reverse-J — a regenerating, uneven-aged stand with plenty of young stems" else "flatter than a classic reverse-J — fewer small stems than an actively regenerating stand"
    insight_banner("bar-chart-fill", tone = "pine", HTML(sprintf("The size distribution is <b>%s</b>.", shape)))
  })
  output$htPlot <- renderPlotly({
    s <- live_only(rv$snap); h <- s$height[is.finite(s$height) & s$height > 0]; if (!length(h)) return(note_plot("No height data"))
    plot_ly(x = h, type = "histogram", nbinsx = 24, marker = list(color = DDL$navy2),
      hovertemplate = "%{x} m<br>%{y} stems<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Height (m)"), yaxis = list(title = "Live stems"))
  })
  output$densityBanner <- renderUI({
    st <- stand_site(rv$snap, rv$plots); req(!is.null(st))
    insight_banner("calculator", tone = "gold",
      HTML(sprintf("Across <b>%d</b> sampled plots: <span class='ci-hero'>%s m²/ha</span> basal area, <b>%s stems/ha</b>, quadratic mean diameter <b>%s cm</b>.",
        st$n_plots, st$ba_ha, format(st$density_ha, big.mark = ","), st$qmd)))
  })

  # ---- GROWTH & MORTALITY -------------------------------------------------
  output$growthPlot <- renderPlotly({
    g <- tree_growth(rv$trees); if (is.null(g) || !nrow(g)) return(note_plot("No remeasured trees yet for a growth estimate"))
    gg <- g$growth_cm_yr[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2]
    plot_ly(x = gg, type = "histogram", nbinsx = 30, marker = list(color = DDL$green),
      hovertemplate = "%{x} cm/yr<br>%{y} trees<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Diameter growth (cm/yr)"), yaxis = list(title = "Trees"),
        shapes = list(list(type="line", x0=0, x1=0, yref="paper", y0=0, y1=1, line=list(color="rgba(150,80,60,0.6)", dash="dot", width=1))))
  })
  output$growthInsight <- renderUI({
    g <- tree_growth(rv$trees); req(!is.null(g), nrow(g) > 0)
    gg <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5, ]
    neg <- round(100 * mean(g$growth_cm_yr < -0.1, na.rm = TRUE))
    insight_banner("graph-up", tone = "pine",
      HTML(sprintf("Of <b>%s</b> remeasured trees, diameter grows a median of <span class='ci-hero'>%.2f cm/yr</span>. About <b>%d%%</b> show a decrease between visits — usually real (bark, drought, a changed measurement height), kept and flagged, not deleted.",
        format(nrow(gg), big.mark=","), stats::median(gg$growth_cm_yr, na.rm=TRUE), neg)))
  })
  output$statusPlot <- renderPlotly({
    ss <- status_summary(rv$snap); if (is.null(ss)) return(note_plot("No status data"))
    cols <- c("Live" = DDL$live, "Dead / standing dead" = DDL$dead, "Other / unknown" = DDL$muted)
    plot_ly(ss, labels = ~cls, values = ~n, type = "pie", hole = 0.55, sort = FALSE,
      marker = list(colors = unname(cols[ss$cls])), textinfo = "label+percent",
      hovertemplate = "%{label}<br>%{value} stems<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>% plotly::layout(showlegend = FALSE)
  })
  output$fastTable <- DT::renderDT({
    g <- tree_growth(rv$trees)
    if (is.null(g) || !nrow(g)) return(DT::datatable(data.frame(Message = "No remeasured trees yet."), rownames = FALSE, options = list(dom = "t")))
    g <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5, ]
    g <- g[order(-g$growth_cm_yr), ][seq_len(min(20, nrow(g))), ]
    df <- data.frame(Tree = short_tree(g$individualID), Species = g$scientificName,
                     `DBH start (cm)` = round(g$d0,1), `DBH now (cm)` = round(g$d1,1),
                     `Growth (cm/yr)` = g$growth_cm_yr, check.names = FALSE)
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tp", order = list(list(4, "desc"))))
  })

  # ---- FOREST SIZE LAB (flagship) ----------------------------------------
  output$labScatter <- renderPlotly({
    one <- rv$one; req(one)
    pts <- one[is.finite(one$stemDiameter) & one$stemDiameter > 0 & is.finite(one$height) & one$height > 0 &
                 !is.na(one$scientificName), , drop = FALSE]
    if (!nrow(pts)) return(note_plot("No trees with both a diameter and a height to map"))
    pts$short <- short_tree(pts$individualID)
    if (nrow(pts) > 1800) { set.seed(7); pts <- pts[sort(sample.int(nrow(pts), 1800)), ] }
    keycol <- input$labColor %||% "species"
    pts$key <- if (keycol == "species") as.character(pts$scientificName)
               else if (keycol == "canopyPosition") as.character(pts$canopyPosition)
               else ifelse(grepl("^Live", pts$plantStatus), "Live", "Dead/other")
    pts$key[is.na(pts$key) | pts$key == ""] <- "—"
    keys <- sort(unique(pts$key))
    kpal <- if (keycol == "species") (rv$pal %||% make_species_pal(pts))
            else setNames(grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(keys)), keys)
    muted_col <- if (is_dark()) "#9fb0c4" else "#6b7a85"; qcol <- if (is_dark()) "#7e8da0" else "#9aa6b2"
    pts$tip <- paste0(
      "<span class='smt-pin-emoji'>\U0001F333</span> <b>", pts$short, "</b><br/>",
      "<em>", ifelse(is.na(pts$scientificName), "—", pts$scientificName), "</em><br/>",
      "<span class='smt-pin-stats'>", round(pts$stemDiameter,1), " cm DBH · ", round(pts$height,1), " m tall",
        ifelse(is.na(pts$canopyPosition), "", paste0("<br/>", pts$canopyPosition)), "</span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", pts$individualID,
        "'>\U0001F332 Open tree career &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    p <- plot_ly()
    for (k in keys) { sub <- pts[pts$key == k, ]
      p <- p %>% add_trace(data = sub, x = ~stemDiameter, y = ~height, type = "scatter", mode = "markers",
        name = k, customdata = ~tip, showlegend = length(keys) <= 12,
        marker = list(color = unname(kpal[k] %||% DDL$green), size = 9, opacity = 0.78, line = list(color = "#fff", width = 0.5)),
        text = ~paste0("tree ", short, " · ", round(stemDiameter,1), " cm"),
        hovertemplate = "%{text}<br>%{y:.1f} m tall<extra></extra>") }
    mx <- stats::median(pts$stemDiameter); my <- stats::median(pts$height)
    xr <- range(pts$stemDiameter); yr <- range(pts$height); px <- diff(xr)*0.02; py <- diff(yr)*0.02
    qlab <- function(x,y,t,xa,ya) list(text=t, x=x, y=y, xref="x", yref="y", showarrow=FALSE, xanchor=xa, yanchor=ya, font=list(color=qcol, size=10.5))
    ann <- list(
      list(text = "each dot is a tree · diameter × height, by species", x=0, y=1.07, xref="paper", yref="paper",
           showarrow=FALSE, xanchor="left", font=list(color=muted_col, size=11)),
      qlab(xr[2]-px, yr[2]-py, "GIANTS \U0001F3C6", "right", "top"),
      qlab(xr[1]+px, yr[2]-py, "SPIRES", "left", "top"),
      qlab(xr[2]-px, yr[1]+py, "STOUT", "right", "bottom"),
      qlab(xr[1]+px, yr[1]+py, "SAPLINGS", "left", "bottom"))
    tag <- rv$tree
    if (!is.null(tag)) { ir <- pts[pts$individualID == tag, ]
      if (nrow(ir) == 1) p <- p %>% add_trace(x = ir$stemDiameter, y = ir$height, type="scatter", mode="markers",
        name = "★ viewing", customdata = ir$tip, showlegend = TRUE,
        marker = list(symbol="diamond", size=18, color="#c9a300", line=list(color="#fff", width=1.6)),
        hovertemplate = paste0("viewing ", ir$short, "<extra></extra>")) }
    p %>% plotly_theme() %>% plotly::layout(
      xaxis = list(title = "Diameter at breast height (cm)"), yaxis = list(title = "Height (m)"),
      shapes = list(list(type="line", xref="x", yref="paper", x0=mx, x1=mx, y0=0, y1=1, line=list(color=qcol, dash="dot", width=1)),
                    list(type="line", xref="paper", yref="y", x0=0, x1=1, y0=my, y1=my, line=list(color=qcol, dash="dot", width=1))),
      annotations = ann, hovermode = "closest")
  })
  output$labNote <- renderUI({
    one <- rv$one; req(one)
    donly <- sum(is.finite(one$stemDiameter) & one$stemDiameter > 0 & !(is.finite(one$height) & one$height > 0))
    if (donly == 0) return(NULL)
    div(class = "qc-cap-note", style = "margin-top:6px", bs_icon("info-circle"),
      sprintf(" %s live stems were measured for diameter but not height, so they can't be placed in this 2-D space — they're not shown here.", format(donly, big.mark = ",")))
  })
  output$treeCardSlot <- renderUI({
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", "\U0001F332"), h4("Tap a tree to see its card"),
      p("Tap a dot above and choose “Open tree career”, or pick a tree in the sidebar.")))
    snap <- rv$snap; row <- one_per_tree(snap[snap$individualID == rv$tree, ]); if (!nrow(row)) return(NULL)
    div(class = "lab-sel", span(class = "ls-emoji", "\U0001F333"),
      div(class = "ls-body",
        div(class = "ls-id", tags$b(short_tree(rv$tree)), sprintf(" — %s · %s cm DBH · %s m",
          ifelse(is.na(row$scientificName),"—",row$scientificName), round(row$stemDiameter,1), ifelse(is.na(row$height),"—",round(row$height,1)))),
        div(class = "ls-dom", ifelse(is.na(row$plantStatus),"",row$plantStatus))),
      actionButton("goTreeFromCard", tagList(bs_icon("arrows-fullscreen"), " Open full career"), class = "btn-outline-dark btn-sm"))
  })
  observeEvent(input$goTreeFromCard, nav_select("tabs", "tree"))

  # ---- TREE CAREER (profile, downloadable) -------------------------------
  tree_card_ui <- function(id) {
    snap <- rv$snap; row <- one_per_tree(snap[snap$individualID == id, ]); if (!nrow(row)) return(NULL)
    hist <- tree_history(rv$trees, id); flags <- tree_qc_flags(hist)
    sp <- row$scientificName
    # how big for its species (DBH percentile within species, this site)
    cohort <- rv$one$stemDiameter[rv$one$scientificName %in% sp & is.finite(rv$one$stemDiameter)]
    pct <- if (length(cohort) >= 5 && is.finite(row$stemDiameter)) round(100 * mean(cohort <= row$stemDiameter)) else NA
    tile <- function(v, l) div(class = "qc-tile", div(class = "qc-tile-v", v), div(class = "qc-tile-l", l))
    growth <- { g <- tree_growth(rv$trees[rv$trees$individualID == id, ]); if (!is.null(g) && nrow(g)) g$growth_cm_yr[1] else NA }
    sparkid <- paste0("spark_", gsub("[^A-Za-z0-9]", "", id))
    flag_ic <- c(high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill")
    flags_ui <- if (length(flags) == 0)
      div(class = "qc-flag clean", span(class = "qc-flag-ic", bs_icon("check-circle-fill")),
          span(HTML("<b>No QC flags.</b> This tree's remeasurements are internally consistent.")))
    else tagList(lapply(flags, function(f) div(class = paste("qc-flag", f$level),
      span(class = "qc-flag-ic", bs_icon(flag_ic[[f$level]] %||% "info-circle-fill")), span(HTML(f$text)))))
    cap_tbl <- if (is.null(hist) || !nrow(hist)) NULL else {
      fnum <- function(x) ifelse(is.na(x) | !is.finite(x), "—", formatC(round(x,1), format="f", digits=1))
      tagList(div(class = "qc-section-h", bs_icon("clock-history"), " Every measurement"),
        div(class = "qc-cap-scroll", tags$table(class = "inspect-tbl",
          tags$thead(tags$tr(lapply(c("Date","DBH (cm)","Height (m)","Status"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(hist)), function(i) tags$tr(
            tags$td(format(hist$date[i], "%Y-%m-%d")), tags$td(fnum(hist$stemDiameter[i])),
            tags$td(fnum(hist$height[i])), tags$td(ifelse(is.na(hist$plantStatus[i]),"—",hist$plantStatus[i]))))))))
    }
    body <- div(id = "qcCardNode", class = "qc-card", `data-short` = short_tree(id),
      div(class = "qc-head", span(class = "qc-emoji", "\U0001F333"),
        div(div(class = "qc-id", short_tree(id)),
            div(class = "qc-sci", em(ifelse(is.na(sp),"unidentified",sp)),
                sprintf(" · %s · %s", row$growthForm %||% "", row$plotID))),
        div(class = "qc-head-badges", glow_badge(ifelse(is.na(row$plantStatus),"—",row$plantStatus),
            if (grepl("^Live", row$plantStatus %||% "")) DDL$green else DDL$dead))),
      div(class = "qc-tiles",
        tile(ifelse(is.finite(row$stemDiameter), round(row$stemDiameter,1), "—"), "cm DBH"),
        tile(ifelse(is.finite(row$height), round(row$height,1), "—"), "m tall"),
        tile(ifelse(is.finite(growth), sprintf("%+.2f", growth), "—"), "cm/yr"),
        tile(ifelse(is.na(pct), "—", paste0(pct, "%")), "size %ile"),
        tile(if (is.null(hist)) "—" else nrow(hist), "visits"),
        tile(ifelse(is.na(row$canopyPosition), "—", gsub(" .*","",row$canopyPosition)), "canopy")),
      div(class = "qc-section-h", bs_icon("graph-up"), " Growth trajectory (diameter over time)"),
      if (!is.null(hist) && sum(is.finite(hist$stemDiameter)) >= 2) plotlyOutput(sparkid, height = "170px") else p(class = "qc-cap-note", "Single visit — no trajectory yet."),
      div(class = "qc-section-h", bs_icon("clipboard-check"), " Data-quality check"), flags_ui,
      cap_tbl,
      p(class = "qc-cap-note", style = "margin-top:8px", bs_icon("info-circle"),
        " A flag means “verify against the field record”, not “wrong”. Trees are remeasured every few years, so gaps are normal."))
    div(body, div(class = "qc-toolbar",
      tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveQcCard()", bsicons::bs_icon("download"), " Save tree card (PNG)"),
      downloadButton("treeCsv", "Download tree data (CSV)", class = "smt-clear-btn")))
  }
  observe({
    id <- rv$tree; req(id)
    sparkid <- paste0("spark_", gsub("[^A-Za-z0-9]", "", id))
    output[[sparkid]] <- renderPlotly({
      h <- tree_history(rv$trees, id); h <- h[is.finite(h$stemDiameter), ]; if (is.null(h) || nrow(h) < 2) return(note_plot("—"))
      plot_ly(h, x = ~date, y = ~stemDiameter, type = "scatter", mode = "lines+markers",
        line = list(color = DDL$green, width = 2.5), marker = list(color = DDL$green2, size = 7),
        hovertemplate = "%{x|%Y}<br>%{y:.1f} cm<extra></extra>") %>%
        plotly_theme(legend = FALSE) %>%
        plotly::layout(xaxis = list(title = ""), yaxis = list(title = "DBH (cm)"), margin = list(l = 45, r = 10, t = 10, b = 30))
    })
  })
  output$treeProfile <- renderUI({
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", "\U0001F332"), h4("Pick a tree to open its career"),
      p("Use the Forest Size Lab (tap a dot → “Open tree career”) or the sidebar tree picker.")))
    div(class = "plot-profile-wrap", tree_card_ui(rv$tree))
  })
  output$treeCsv <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_%s.csv", short_tree(rv$tree %||% "tree"), format(Sys.Date(), "%Y%m%d")),
    content = function(file) { id <- rv$tree; req(id); h <- tree_history(rv$trees, id); req(!is.null(h))
      utils::write.csv(data.frame(individualID = id, h), file, row.names = FALSE, na = "") }, contentType = "text/csv")

  # ---- MAP ----------------------------------------------------------------
  output$map <- leaflet::renderLeaflet({
    lb <- rv$lb; req(lb); metric <- input$mapMetric %||% "ba_ha"
    val <- lb[[metric]]; val[is.na(val)] <- 0
    dom <- if (diff(range(val, na.rm=TRUE)) > 0) range(val, na.rm=TRUE) else c(val[1]-1, val[1]+1)
    pal <- leaflet::colorNumeric("viridis", domain = dom)
    rr <- range(lb$ba_ha, na.rm = TRUE); lb$radius <- if (diff(rr) > 0) 6 + 14*(lb$ba_ha - rr[1])/diff(rr) else 11
    leaflet::leaflet(lb) %>% leaflet::addProviderTiles(input$view %||% "Esri.WorldImagery") %>%
      leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~radius, fillColor = pal(val),
        color = "#fff", weight = 1, fillOpacity = 0.85, layerId = ~plotID,
        label = ~lapply(sprintf("<b>%s</b><br>%s m²/ha · %s stems/ha · %s species", short_plot(plotID), ba_ha, format(density_ha, big.mark=","), n_species), htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = val,
        title = switch(metric, ba_ha = "m²/ha", density_ha = "stems/ha", "species"))
  })

  # ---- ABOUT --------------------------------------------------------------
  output$aboutPanel <- renderUI({
    div(class = "about-wrap",
      div(class = "about-card", h4("\U0001F332 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Vegetation structure"), " product (", tags$code("DP1.10098.001"),
          "). NEON tags individual woody stems, maps them, and remeasures their ", tags$b("diameter, height, and status"), " over the years — so each tree has a growth career.")),
      div(class = "about-card", h4(bs_icon("rulers"), " How it's measured"),
        p("Diameter is ", tags$b("DBH"), " (at 130 cm) for trees; height in metres. Most plots are remeasured every ~5 years, so growth is computed per-year ", tags$b("between visits"), ", not annually."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Stand metrics are scaled by each plot's sampled tree area to per-hectare values, but they're indices from the sampled plots — not a wall-to-wall inventory.")),
      div(class = "about-card", h4(bs_icon("graph-up"), " Growth & status"),
        p("Diameter increments come from remeasured trees (one rate each). Decreases are common and usually real (bark sloughing, drought, a changed measurement height) — kept and flagged, not deleted. Live/dead is a snapshot ratio, not an annual mortality rate."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Above-ground biomass is deliberately ", tags$b("not"), " estimated — it requires an allometric model whose error compounds; basal area (directly measured) is the honest stand measure shown here.")),
      div(class = "about-card", h4(bs_icon("diagram-3"), " A NEONize sibling"),
        p("Built to the NEON Small Mammal Tracker quality bar — same Desert Data Labs design system, bundling, and pin-card interaction — with woody-structure-native analyses. See the NEONize playbook."),
        p(bs_icon("envelope"), " ", tags$a(href = "mailto:desertdatalabs@gmail.com", "desertdatalabs@gmail.com"),
          " · ", tags$a(href = "https://data.neonscience.org/data-products/DP1.10098.001", target = "_blank", "NEON data product"))))
  })

  observeEvent(input$help, {
    showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("question-circle"), " How it works"),
      tags$ul(
        tags$li(HTML("Pick a <b>site</b> (or open the Harvard Forest demo). Numbers describe each tree's <b>most recent measurement</b>.")),
        tags$li(HTML("<b>Stand Structure</b> — the diameter size-class curve, height profile, and per-hectare basal area & density.")),
        tags$li(HTML("<b>Growth & Mortality</b> — how fast diameters grow between visits, the fastest growers, and the live/dead split.")),
        tags$li(HTML("<b>Forest Size Lab</b> — every tree as a dot (diameter × height); <b>tap one</b> to pin its card, then “Open tree career” for its full growth history.")),
        tags$li(HTML("Most plots are remeasured every ~5 years, so growth is per-year between visits."))),
      footer = modalButton("Got it")))
  })
}
