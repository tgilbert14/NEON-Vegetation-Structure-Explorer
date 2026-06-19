# ===========================================================================
# NEON Vegetation Structure Explorer — server.R
# ===========================================================================
server <- function(input, output, session) {

  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark()
    ink  <- if (dark) "#e8ece3" else "#20281f"; grid <- if (dark) "rgba(220,235,222,0.10)" else "rgba(36,40,32,0.08)"
    zero <- if (dark) "rgba(220,235,222,0.22)" else "rgba(36,40,32,0.15)"; lin <- if (dark) "#33402f" else "#d8d4c6"
    legc <- if (dark) "#c3d0c0" else "#344039"
    hov  <- if (dark) list(bg = "rgba(12,24,16,0.97)", bd = "#e8c552", fg = "#f1f6ee")
            else        list(bg = "rgba(22,42,28,0.96)", bd = "#E6A700", fg = "#ffffff")
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
    one <- rv$one
    lab_meas <- ifelse(is.finite(one$stemDiameter), paste0(round(one$stemDiameter), "cm"),
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

  pick_tree <- function(id, navigate = FALSE) {
    if (is.null(id) || is.na(id) || id == "") return()
    if (is.null(rv$snap) || !(id %in% rv$snap$individualID)) return()
    rv$tree <- id
    if (!identical(input$treeSel, id)) updateSelectizeInput(session, "treeSel", selected = id)
    if (navigate) nav_select("tabs", "tree")
    # celebrate a genuine standout: the site's biggest or tallest live tree
    td <- trees_only(live_only(rv$snap)); lh <- live_only(rv$snap)
    big_id  <- if (!is.null(td) && nrow(td)) td$individualID[which.max(td$stemDiameter)] else NA_character_
    tall_id <- if (!is.null(lh) && nrow(lh) && any(is.finite(lh$height))) lh$individualID[which.max(lh$height)] else NA_character_
    if (id %in% stats::na.omit(c(big_id, tall_id))) session$sendCustomMessage("confetti", list(big = TRUE))
  }
  observeEvent(input$treeSel, if (nzchar(input$treeSel %||% "")) pick_tree(input$treeSel, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_tree(input$qcCardRequest, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$surpriseBtn, { one <- rv$one; req(one); pick_tree(sample(one$individualID, 1), navigate = TRUE) })

  observeEvent(input$goStand,  nav_select("tabs", "stand"))
  observeEvent(input$goGrowth, nav_select("tabs", "growth"))
  observeEvent(input$goLab,    nav_select("tabs", "lab"))
  observeEvent(input$goTree,   { if (is.null(rv$tree) && !is.null(rv$one) && nrow(rv$one)) { i <- which.max(rv$one$stemDiameter); if (length(i)) rv$tree <- rv$one$individualID[i] }; nav_select("tabs", "tree") })
  observeEvent(input$goMap,    nav_select("tabs", "map"))

  # ---- hero ---------------------------------------------------------------
  output$heroStats <- renderUI({
    one <- rv$one; snap <- rv$snap; if (is.null(one)) return(NULL)
    live_snap <- live_only(snap)
    tree_sp <- species_level_only(trees_only(one))                 # tree species (>=10 cm DBH)
    tallest <- smax(live_snap$height)
    biggest <- smax(trees_only(live_snap)$stemDiameter)
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
      div(class = "hero-title", bs_icon("tree-fill"), tags$b(rv$label)),
      div(class = "hero-grid",
        hero(nrow(trees_only(one)), "live trees", icon = "tree", tone = "pine",
             ttl = "Live tagged trees ≥ 10 cm DBH (the protocol tree threshold) — one count per individual."),
        hero(dplyr::n_distinct(tree_sp$scientificName), "tree species", icon = "diagram-3", tone = "sky",
             ttl = "Species among live trees ≥ 10 cm DBH — the stand. Tap to rank species by basal area.", click = "species"),
        hero(ifelse(is.finite(tallest), round(tallest, 1), 0), "m tallest", icon = "arrows-vertical", tone = "gold",
             ttl = "Tallest live tree at the site."),
        hero(ifelse(is.finite(biggest), round(biggest, 1), 0), "cm biggest DBH", icon = "circle", tone = "bark",
             ttl = "Largest live tree by diameter at breast height. Tap to see the biggest trees.", click = "biggest")))
  })

  # ---- OVERVIEW -----------------------------------------------------------
  output$baBar <- renderPlotly({
    ssall <- species_structure(rv$snap, rv$plots); if (is.null(ssall) || !nrow(ssall)) return(note_plot("No basal-area data"))
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
    one_d <- one[is.finite(one$stemDiameter), ]; one_h <- one[is.finite(one$height), ]
    big  <- if (nrow(one_d)) one_d[which.max(one_d$stemDiameter), ] else one[0, ]
    tall <- if (nrow(one_h)) one_h[which.max(one_h$height), ]      else one[0, ]
    pts <- c()
    if (!is.null(st)) pts <- c(pts, sprintf("Stand density is about <b>%s stems/ha</b> at <b>%s m²/ha</b> basal area%s (quadratic mean diameter %s cm).", format(st$density_ha, big.mark=","), st$ba_ha, if (!is.null(st$ba_se) && is.finite(st$ba_se)) sprintf(" (±%s SE, n=%d plots)", st$ba_se, st$n_plots) else "", st$qmd))
    if (nrow(big) && nrow(tall)) pts <- c(pts, sprintf("The biggest tree is a <b><i>%s</i></b> at <b>%s cm</b> diameter; the tallest reaches <b>%s m</b> (<i>%s</i>).", big$scientificName, round(big$stemDiameter,1), round(tall$height,1), tall$scientificName))
    else if (nrow(big)) pts <- c(pts, sprintf("The biggest tree is a <b><i>%s</i></b> at <b>%s cm</b> diameter.", big$scientificName, round(big$stemDiameter,1)))
    if (!is.null(g) && nrow(g)) { gg <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5, ]; if (nrow(gg)) pts <- c(pts, sprintf("Across <b>%s</b> remeasured trees, diameter grows a median of <b>%.2f cm/yr</b>.", format(nrow(gg), big.mark=","), stats::median(gg$growth_cm_yr, na.rm=TRUE))) }
    pts <- c(pts, "Basal area and density are stand indices from the sampled plots — not a wall-to-wall inventory; biomass isn't estimated (it needs an allometric model NEON doesn't publish here).")
    div(class = "insight-list", lapply(pts, function(t) div(class = "il-item", bs_icon("dot"), HTML(t))))
  })

  # ---- STAND STRUCTURE ----------------------------------------------------
  output$sizePlot <- renderPlotly({
    sc <- size_class(rv$snap, rv$plots); if (is.null(sc)) return(note_plot("No diameter data"))
    has_ha <- "stems_ha" %in% names(sc) && any(is.finite(sc$stems_ha))
    sc$yval <- if (has_ha) sc$stems_ha else sc$stems
    ylab <- if (has_ha) "Live stems / ha" else "Live stems (sampled)"
    plot_ly(sc, x = ~cls, y = ~yval, type = "bar", marker = list(color = DDL$green),
      hovertemplate = paste0("%{x} cm DBH<br>%{y} ", if (has_ha) "stems/ha" else "stems", "<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Diameter class (cm DBH)"), yaxis = list(title = ylab))
  })
  output$sizeInsight <- renderUI({
    sc <- size_class(rv$snap); req(!is.null(sc))
    small <- sum(sc$stems[sc$cls %in% c("10–20","20–30")]); big <- sum(sc$stems[sc$cls %in% c("50–70","70+")])
    ratio <- if (big > 0) round(small / big, 1) else NA
    shape <- if (is.na(ratio)) "concentrated in the smaller classes" else
             if (ratio >= 3) "a clear descending, reverse-J-like shape — typical of a regenerating, uneven-aged stand" else
             if (ratio >= 1.2) "a moderate descending shape" else
             "top-heavy (relatively few small trees) — which can indicate an aging or even-aged stand; verify against site history"
    insight_banner("bar-chart-fill", tone = "pine",
      HTML(sprintf("Among trees ≥10 cm DBH, the size distribution is <b>%s</b>%s — the reverse-J / de Liocourt pattern. Smaller saplings are sampled in separate nested subplots and aren't shown here.",
        shape, if (!is.na(ratio)) sprintf(" (a rough %.1f small per large stem)", ratio) else "")))
  })
  output$htPlot <- renderPlotly({
    s <- live_only(rv$snap); h <- s$height[is.finite(s$height) & s$height > 0]; if (!length(h)) return(note_plot("No height data"))
    plot_ly(x = h, type = "histogram", nbinsx = 24, marker = list(color = DDL$bark),
      hovertemplate = "%{x} m<br>%{y} stems<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Height (m)"), yaxis = list(title = "Live stems"))
  })
  output$densityBanner <- renderUI({
    st <- stand_site(rv$snap, rv$plots); req(!is.null(st))
    pre  <- if (st$n_plots < 3) "Preliminary (few plots): " else ""
    se_ba <- if (is.finite(st$ba_se)) sprintf(" ±%s SE", st$ba_se) else ""
    se_d  <- if (is.finite(st$density_se)) sprintf(" ±%s", format(st$density_se, big.mark = ",")) else ""
    insight_banner("calculator", tone = "gold",
      HTML(sprintf("%sAcross <b>%d</b> sampled plots (trees ≥10 cm DBH): <span class='ci-hero'>%s m²/ha</span>%s basal area, <b>%s stems/ha</b>%s, quadratic mean diameter <b>%s cm</b>. <span class='dim'>Mean ± SE across plots (the sampling unit).</span>",
        pre, st$n_plots, st$ba_ha, se_ba, format(st$density_ha, big.mark = ","), se_d, st$qmd)))
  })

  # ---- GROWTH & MORTALITY -------------------------------------------------
  output$growthPlot <- renderPlotly({
    g <- tree_growth(rv$trees); if (is.null(g) || !nrow(g)) return(note_plot("No remeasured trees yet for a growth estimate"))
    gg <- g$growth_cm_yr[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2 & !g$mh_change]
    plot_ly(x = gg, type = "histogram", nbinsx = 30, marker = list(color = DDL$green),
      hovertemplate = "%{x} cm/yr<br>%{y} trees<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Diameter growth (cm/yr)"), yaxis = list(title = "Trees"),
        shapes = list(list(type="line", x0=0, x1=0, yref="paper", y0=0, y1=1, line=list(color="rgba(122,82,48,0.7)", dash="dot", width=1))))
  })
  output$growthInsight <- renderUI({
    g <- tree_growth(rv$trees); req(!is.null(g), nrow(g) > 0)
    nmh <- sum(g$mh_change, na.rm = TRUE)
    gv <- g[!g$mh_change & is.finite(g$growth_cm_yr), ]
    clean <- gv[gv$growth_cm_yr <= 5 & gv$growth_cm_yr >= -2, ]; req(nrow(clean) > 0)
    trunc_n <- nrow(gv) - nrow(clean)
    med <- stats::median(clean$growth_cm_yr, na.rm = TRUE)
    q <- stats::quantile(clean$growth_cm_yr, c(.25, .75), na.rm = TRUE, names = FALSE)
    neg <- round(100 * mean(clean$growth_cm_yr < -0.1, na.rm = TRUE))
    insight_banner("graph-up", tone = "pine",
      HTML(sprintf("Across <b>%s</b> remeasured trees, diameter grows a median of <span class='ci-hero'>%.2f cm/yr</span> (IQR %.2f–%.2f). About <b>%d%%</b> show a decrease between visits — usually real (bark, drought), kept and flagged.%s%s",
        format(nrow(clean), big.mark = ","), med, q[1], q[2], neg,
        if (nmh > 0) sprintf(" %s trees with a changed measurement height are excluded.", format(nmh, big.mark = ",")) else "",
        if (trunc_n > 0) sprintf(" %s with >5 or <−2 cm/yr (likely measurement issues) are off-chart but kept in the data.", format(trunc_n, big.mark = ",")) else "")))
  })
  output$statusPlot <- renderPlotly({
    ss <- status_summary(rv$snap); if (is.null(ss) || !nrow(ss)) return(note_plot("No status data"))
    cols <- c("Live" = DDL$live, "Dead / standing dead" = DDL$dead,
              "Lost track / removed" = "#c2b280", "Other / unknown" = DDL$muted)
    ss$lab <- as.character(ss$cls)
    plot_ly(ss, labels = ~lab, values = ~n, type = "pie", hole = 0.55, sort = FALSE,
      marker = list(colors = unname(cols[ss$lab])), textinfo = "label+percent",
      hovertemplate = "%{label}<br>%{value} trees<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>% plotly::layout(showlegend = FALSE)
  })
  output$fastTable <- DT::renderDT({
    g <- tree_growth(rv$trees)
    if (is.null(g) || !nrow(g)) return(DT::datatable(data.frame(Message = "No remeasured trees yet."), rownames = FALSE, options = list(dom = "t")))
    g <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5 & !g$mh_change, ]
    if (!nrow(g)) return(DT::datatable(data.frame(Message = "No clean remeasurement growth yet."), rownames = FALSE, options = list(dom = "t")))
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
            else stats::setNames(forest_ramp(length(keys)), keys)
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
        marker = list(symbol="diamond", size=18, color="#E6A700", line=list(color="#fff", width=1.6)),
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
    ncoh <- length(cohort)
    pct <- if (ncoh >= 5 && is.finite(row$stemDiameter)) round(100 * mean(cohort <= row$stemDiameter)) else NA
    tile <- function(v, l) div(class = "qc-tile", div(class = "qc-tile-v", v), div(class = "qc-tile-l", l))
    growth <- { g <- tree_growth(rv$trees[rv$trees$individualID == id, ]); if (!is.null(g) && nrow(g)) g$growth_cm_yr[1] else NA }
    # honest size tier (not the mammal "rarity"): by within-species percentile if
    # the cohort is big enough, else by absolute DBH.
    d_now <- row$stemDiameter
    tier <- if (!is.na(pct)) { if (pct >= 90) "Giant" else if (pct >= 50) "Canopy" else "Sapling" }
            else if (is.finite(d_now) && d_now >= 60) "Giant"
            else if (is.finite(d_now) && d_now >= 25) "Canopy" else "Sapling"
    tier_col <- c(Giant = "#14532a", Canopy = "#1f6b3a", Sapling = "#3f9f5a")[[tier]]
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
        tile(ifelse(is.na(pct), "—", paste0(pct, "%")), if (!is.na(pct)) sprintf("%%ile of %d live", ncoh) else "size %ile"),
        tile(if (is.null(hist)) "—" else nrow(hist), "visits"),
        tile(ifelse(is.na(row$canopyPosition), "—", gsub(" .*","",row$canopyPosition)), "canopy")),
      div(class = "qc-section-h", bs_icon("graph-up"), " Growth trajectory (diameter over time)"),
      if (!is.null(hist) && sum(is.finite(hist$stemDiameter)) >= 2) plotlyOutput("treeSpark", height = "170px") else p(class = "qc-cap-note", "Single visit — no trajectory yet."),
      div(class = "qc-section-h", bs_icon("clipboard-check"), " Data-quality check"), flags_ui,
      cap_tbl,
      p(class = "qc-cap-note", style = "margin-top:8px", bs_icon("info-circle"),
        " A flag means “verify against the field record”, not “wrong”. Trees are remeasured every few years, so gaps are normal."))
    tcstat <- function(v, l) div(class = "tc-stat", div(class = "tc-stat-v", v), div(class = "tc-stat-l", l))
    tcard <- div(class = "tradingcard-wrap",
      div(id = "treeCardNode", class = "trade-card", `data-short` = short_tree(id),
          style = sprintf("--rc:%s", tier_col),
        div(class = "tc-holo"),
        div(class = "tc-top", span(class = "tc-tier", toupper(tier)), span(class = "tc-brand", "NEON · VST")),
        div(class = "tc-emoji-wrap", span(class = "tc-emoji", "\U0001F333")),
        div(class = "tc-id", short_tree(id)),
        div(class = "tc-sci", em(ifelse(is.na(sp), "unidentified", sp))),
        div(class = "tc-nick", row$plotID),
        div(class = "tc-stats",
          tcstat(ifelse(is.finite(row$stemDiameter), round(row$stemDiameter, 1), "—"), "cm DBH"),
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
    id <- rv$tree; req(id)
    h <- tree_history(rv$trees, id); h <- h[is.finite(h$stemDiameter), ]; if (is.null(h) || nrow(h) < 2) return(note_plot("—"))
    plot_ly(h, x = ~date, y = ~stemDiameter, type = "scatter", mode = "lines+markers",
      line = list(color = DDL$green, width = 2.5), marker = list(color = DDL$green2, size = 7),
      hovertemplate = "%{x|%Y}<br>%{y:.1f} cm<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = ""), yaxis = list(title = "DBH (cm)"), margin = list(l = 45, r = 10, t = 10, b = 30))
  })
  output$treeProfile <- renderUI({
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", "\U0001F332"), h4("Pick a tree to open its career"),
      p("Use the Forest Size Lab (tap a dot → “Open tree career”) or the sidebar tree picker.")))
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
      site <- rv$site %||% "site"
      tl <- tidy_trees_export(rv$trees)
      pl <- plots_export(rv$snap, rv$plots)
      cb <- veg_codebook()
      st <- stand_site(rv$snap, rv$plots)
      readme <- c(
        sprintf("NEON Vegetation Structure Explorer — data export for site %s", site),
        sprintf("Generated %s by an unofficial Desert Data Labs explorer.", format(Sys.Date(), "%Y-%m-%d")),
        "Source: NEON Vegetation structure DP1.10098.001 (vst_mappingandtagging x vst_apparentindividual; vst_perplotperyear).",
        "",
        "FILES",
        " trees_long.csv  — one row per individual x measurement bout (the raw growth career; aggregate it yourself).",
        " plots.csv       — one row per plot: sampled tree area + per-hectare stand summary (trees >=10 cm DBH).",
        " data_dictionary.csv — column definitions, types, units.",
        "",
        "NOTES",
        " * 'snapshot' analyses elsewhere in the app use each tree's LATEST bout; here you get every bout.",
        " * Stand metrics scope to live trees >=10 cm DBH over totalSampledAreaTrees (an index, not a wall-to-wall inventory).",
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
                         one = rv$one, label = rv$label %||% rv$site %||% "site")
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
      div(class = "about-card", h4("\U0001F332 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Vegetation structure"), " product (", tags$code("DP1.10098.001"),
          "). NEON tags individual woody stems, maps them, and remeasures their ", tags$b("diameter, height, and status"), " over the years — so each tree has a growth career.")),
      div(class = "about-card", h4(bs_icon("rulers"), " How it's measured"),
        p("Diameter is ", tags$b("DBH"), " (at 130 cm) for trees; height in metres. Most plots are remeasured every ~5 years, so growth is computed per-year ", tags$b("between visits"), ", not annually."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Stand metrics are scaled by each plot's sampled tree area to per-hectare values (reported as mean ± SE across plots), but they're indices from the sampled plots — not a wall-to-wall inventory. The ", tags$b("≥10 cm DBH"), " cut is a proxy for the tree growth-forms NEON tallies over that area, and tower and distributed plots are pooled — split them on ", tags$code("plotType"), " in the data export for a design-based estimate. QMD is stem-weighted; basal area and density are averaged across plots (equal plot weight).")),
      div(class = "about-card", h4(bs_icon("graph-up"), " Growth & status"),
        p("Diameter increments come from remeasured trees (one rate each). Decreases are common and usually real (bark sloughing, drought, a changed measurement height) — kept and flagged, not deleted. Live/dead is a snapshot ratio, not an annual mortality rate."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Above-ground biomass is deliberately ", tags$b("not"), " estimated — it requires an allometric model whose error compounds; basal area (directly measured) is the honest stand measure shown here.")),
      div(class = "about-card", h4(bs_icon("diagram-3"), " A NEONize sibling"),
        p("Built to the Desert Data Labs NEON quality bar — the same flow, bundling, and pin-card interaction as its siblings — but with its own ", tags$b("Old-Growth Canopy"), " forest identity and woody-structure-native analyses. See the NEONize playbook."),
        p(bs_icon("envelope"), " ", tags$a(href = "mailto:desertdatalabs@gmail.com", "desertdatalabs@gmail.com"),
          " · ", tags$a(href = "https://data.neonscience.org/data-products/DP1.10098.001", target = "_blank", "NEON data product"))))
  })

  # ---- clickable hero stats -> ranked-breakdown modals -------------------
  observeEvent(input$heroClick, {
    if (identical(input$heroClick, "species")) {
      ss <- species_structure(rv$snap, rv$plots); req(!is.null(ss), nrow(ss) > 0)
      tot <- sum(ss$ba_m2, na.rm = TRUE)
      items <- lapply(seq_len(min(20, nrow(ss))), function(i) tags$li(class = "rank-row",
        span(class = paste("rank-num", if (i <= 3) "top"), i),
        span(class = "rank-name", em(ifelse(is.na(ss$scientificName[i]), "—", ss$scientificName[i]))),
        span(class = "rank-metric", sprintf("%.1f m²", ss$ba_m2[i])),
        span(class = "rank-sub", sprintf("%s%% · %s stems", round(100 * ss$ba_m2[i] / tot), ss$stems[i]))))
      showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("bar-chart-steps"), " Species by basal area"),
        div(class = "rank-modal-sub", "Live trees ≥ 10 cm DBH, ranked by total basal area (relative dominance)."),
        tags$ul(class = "rank-list", items), footer = modalButton("Close")))
    } else if (identical(input$heroClick, "biggest")) {
      one <- rv$one; req(one); d <- trees_only(one[is.finite(one$stemDiameter), ]); req(nrow(d) > 0)
      d <- d[order(-d$stemDiameter), ][seq_len(min(20, nrow(d))), ]
      items <- lapply(seq_len(nrow(d)), function(i) tags$li(class = "rank-row rank-click",
        onclick = sprintf("Shiny.setInputValue('rankPick','%s',{priority:'event'})", d$individualID[i]),
        span(class = paste("rank-num", if (i <= 3) "top"), i),
        span(class = "rank-name", tags$b(short_tree(d$individualID[i])), " ",
             em(ifelse(is.na(d$scientificName[i]), "—", d$scientificName[i]))),
        span(class = "rank-metric", sprintf("%.1f cm", d$stemDiameter[i])),
        span(class = "rank-go", bs_icon("arrow-right-circle"))))
      showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("trophy"), " Biggest trees by DBH"),
        div(class = "rank-modal-sub", "Tap a tree to open its career."),
        tags$ul(class = "rank-list", items), footer = modalButton("Close")))
    }
  })
  observeEvent(input$rankPick, { removeModal(); pick_tree(input$rankPick, navigate = TRUE) })

  # ---- CHAMPION TREES (leaderboard) --------------------------------------
  champion_df <- function(metric) {
    if (is.null(rv$one) || !nrow(rv$one)) return(NULL)
    one <- rv$one
    if (identical(metric, "fastest")) {
      g <- tree_growth(rv$trees); if (is.null(g) || !nrow(g)) return(NULL)
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
      col <- if (identical(metric, "tallest")) "height" else "stemDiameter"
      d <- one[is.finite(one[[col]]), ]; if (identical(metric, "biggest")) d <- trees_only(d)
      if (!nrow(d)) return(NULL); d <- d[order(-d[[col]]), ]
      data.frame(id = d$individualID, tree = short_tree(d$individualID), species = d$scientificName,
                 value = round(d[[col]], 1), unit = if (identical(metric, "tallest")) "m" else "cm DBH",
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
        div(class = "podium-medal", medals[k]), div(class = "podium-emoji", "\U0001F333"),
        div(class = "podium-id", r$tree),
        div(class = "podium-stat", sprintf("%s %s", r$value, r$unit)),
        div(class = "podium-sp", em(ifelse(is.na(r$species), "—", r$species)))) })
    div(class = "podium", cards)
  })
  output$fameTable <- DT::renderDT({
    df <- champion_df(input$fameMetric %||% "biggest")
    if (is.null(df) || !nrow(df)) return(DT::datatable(data.frame(Message = "No trees for this ranking yet."),
      rownames = FALSE, options = list(dom = "t")))
    df2 <- utils::head(df, 25)
    show <- data.frame(Rank = seq_len(nrow(df2)), Tree = df2$tree, Species = df2$species,
                       Value = df2$value, Unit = df2$unit, check.names = FALSE)
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
    snap <- tree_snapshot(b$trees); st <- stand_site(snap, b$plots)
    one <- one_per_tree(live_only(snap)); tree_sp <- species_level_only(trees_only(one))
    list(site = site, st = st,
         n_species = dplyr::n_distinct(tree_sp$scientificName),
         tallest = round(smax(live_only(snap)$height), 1),
         biggest = round(smax(trees_only(live_only(snap))$stemDiameter), 1))
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
    tbl <- tags$table(class = "compare-table",
      tags$thead(tags$tr(tags$th(""),
        tags$th(div(class = "cmp-head", a$site)), tags$th(div(class = "cmp-head", b$site)))),
      tags$tbody(
        rowf("Basal area (m²/ha)", a$st$ba_ha, b$st$ba_ha),
        rowf("Stem density (/ha)", a$st$density_ha, b$st$density_ha),
        rowf("Quadratic mean diameter (cm)", a$st$qmd, b$st$qmd),
        rowf("Tree species (≥10 cm)", a$n_species, b$n_species),
        rowf("Tallest (m)", a$tallest, b$tallest),
        rowf("Biggest DBH (cm)", a$biggest, b$biggest),
        rowf("Plots sampled", a$st$n_plots, b$st$n_plots)))
    div(tbl, div(class = "compare-foot", bs_icon("info-circle"),
      " Stand indices from the sampled plots (mean across plots), not a wall-to-wall inventory; tower and distributed plots are pooled."))
  })

  # ---- guided tour (on demand) -------------------------------------------
  observeEvent(input$tourBtn, session$sendCustomMessage("startTour", list()))

  observeEvent(input$help, {
    showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("question-circle"), " How it works"),
      tags$ul(
        tags$li(HTML("Pick a <b>site</b> (or open the Harvard Forest demo). Numbers describe each tree's <b>most recent measurement</b>.")),
        tags$li(HTML("<b>Stand Structure</b> — the diameter size-class curve, height profile, and per-hectare basal area & density.")),
        tags$li(HTML("<b>Growth & Mortality</b> — how fast diameters grow between visits, the fastest growers, and the live/dead split.")),
        tags$li(HTML("<b>Forest Size Lab</b> — every tree as a dot (diameter × height); <b>tap one</b> to pin its card, then “Open tree career” for its full growth history.")),
        tags$li(HTML("<b>Champion Trees</b> — the biggest, tallest, fastest-growing, and longest-tracked trees; tap one to open it.")),
        tags$li(HTML("<b>Compare</b> two stands head-to-head, and download the <b>full data</b> (CSV + codebook) or a <b>stand report PDF</b>.")),
        tags$li(HTML("Most plots are remeasured every ~5 years, so growth is per-year between visits."))),
      footer = tagList(tags$button(type = "button", class = "btn btn-outline-dark btn-sm",
        onclick = "(function(){var m=document.querySelector('.modal.show button[data-bs-dismiss=modal],.modal.show .btn-close');if(m)m.click();setTimeout(vegTour,250);})()",
        bsicons::bs_icon("signpost-2"), " Take the tour"), modalButton("Got it"))))
  })
}
