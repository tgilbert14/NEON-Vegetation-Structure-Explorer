# ===========================================================================
# NEON Vegetation Structure Explorer — server.R
# ===========================================================================
server <- function(input, output, session) {

  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark()
    ink  <- if (dark) "#eaf4ec" else "#16261c"; grid <- if (dark) "rgba(224,236,228,0.10)" else "rgba(30,70,42,0.08)"
    zero <- if (dark) "rgba(224,236,228,0.22)" else "rgba(30,70,42,0.15)"; lin <- if (dark) "rgba(255,255,255,0.12)" else "#d6e2d8"
    legc <- if (dark) "#bcd3c2" else "#33503e"
    hov  <- if (dark) list(bg = "rgba(16,32,24,0.96)", bd = "#4eb86a", fg = "#eaf4ec")
            else        list(bg = "rgba(22,50,34,0.96)", bd = "#2f8a52", fg = "#ffffff")
    p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Aptos, Segoe UI, system-ui, sans-serif"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 55, r = 30, t = 48, b = 44),
      hoverlabel = list(bgcolor = hov$bg, bordercolor = hov$bd,
        font = list(color = hov$fg, family = "Aptos, Segoe UI, system-ui, sans-serif", size = 13))) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  note_plot <- function(msg, icon = "\U0001F332") {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = paste0(icon, "<br>", msg), showarrow = FALSE,
          font = list(color = if (is_dark()) "#a4c0aa" else "#5a6a82", size = 15), align = "center"))) %>%
      plotly::config(displayModeBar = FALSE)
  }
  fmt_num <- function(x, digits = 1, big = FALSE) {
    x <- suppressWarnings(as.numeric(x))
    if (!length(x) || !is.finite(x[[1]])) return("—")
    formatC(x[[1]], format = "f", digits = digits,
            big.mark = if (big) "," else "", drop0trailing = FALSE)
  }
  fmt_count <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!length(x) || !is.finite(x[[1]])) return("—")
    format(round(x[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  rv <- reactiveValues(trees = NULL, snap = NULL, one = NULL, plots = NULL, meta = NULL, lb = NULL,
                       pal = NULL, label = NULL, site = NULL, tree = NULL, ctx = NULL, is_demo = FALSE,
                       stype = NULL, spec = NULL, bundle = NULL,
                       available_channels = character(), lab_keys = character())
  SP <- function() { req(rv$spec); rv$spec }     # exact v2 active physical channel

  # Site-wide source-gap evidence is intentionally separate from the active
  # physical channel. RELEASE-2026 includes measurement rows for some plot
  # visits that have no matching published plot-opportunity row. They remain in
  # the preserved download, but are ineligible for every zero, denominator, and
  # derived summary. Keep that coverage fact visible beside the summaries rather
  # than making a user discover it one plant record at a time.
  site_source_gap <- reactive({
    req(rv$meta)
    one_nonnegative_int <- function(x) {
      value <- suppressWarnings(as.integer(x))
      if (length(value) != 1L || is.na(value) || value < 0L) 0L else value
    }
    contexts <- one_nonnegative_int(rv$meta$n_measurement_only_contexts)
    records <- one_nonnegative_int(rv$meta$n_measurement_records_without_opportunity_source)
    if (contexts == 0L && records == 0L) return(NULL)
    list(contexts = contexts, records = records)
  })

  source_gap_notice <- function() {
    gap <- site_source_gap()
    if (is.null(gap)) return(NULL)
    channel <- SP()$channel_label
    div(id = "siteSourceGap", class = "source-gap-note", role = "note",
      div(class = "source-gap-icon", bs_icon("exclamation-diamond-fill")),
      div(class = "source-gap-copy",
        div(class = "source-gap-title", "Coverage note · all plant forms at this place"),
        p(HTML(sprintf(
          "NEON has <b>%s preserved measurement records</b> from <b>%s plot visit%s</b> without the matching plot sampling record. They are not used in the active <b>%s</b> summary and are never read as zero or plant absence.",
          fmt_count(gap$records), fmt_count(gap$contexts),
          if (gap$contexts == 1L) "" else "s", channel))),
        p(class = "source-gap-scope",
          "These site-wide counts cover every recorded plant form, including forms outside this measurement view. The ledger gives the exact plot and visit keys; the full data download contains every matching measurement row.")),
      downloadButton("sourceGapCsv", tagList(bs_icon("download"), " Download gap ledger (CSV)"),
                     class = "btn-outline-dark btn-sm source-gap-download"))
  }

  source_gap_inline <- function() {
    gap <- site_source_gap()
    if (is.null(gap)) return(NULL)
    div(class = "source-gap-inline", bs_icon("exclamation-diamond-fill"),
      HTML(sprintf(
        " Separately, the all-plant-form coverage note holds <b>%s records from %s plot visit%s</b> that lack a matching sampling record; they are not zero or absence. ",
        fmt_count(gap$records), fmt_count(gap$contexts), if (gap$contexts == 1L) "" else "s")),
      tags$a(href = "#siteSourceGap", "Review the exact evidence path"))
  }

  supported_channels <- function(bundle) {
    ids <- c("tree_dbh", "shrub_sapling_basal")
    summaries <- bundle$contract$channel_summary %||% list()
    ids[vapply(ids, function(id) {
      summary <- summaries[[id]] %||% list()
      n <- suppressWarnings(as.integer(summary$n_supported_plots %||% NA_integer_))
      length(n) == 1L && is.finite(n) && n > 0L
    }, logical(1))]
  }

  # individualID is not globally unique inside a site. The same identifier can
  # occur in different plots, so every interactive selection uses the composite
  # plotID + individualID key. Keep it local to the app rather than exposing an
  # invented identifier as a NEON field in downloads.
  plant_key_vec <- function(d) {
    if (is.null(d) || !nrow(d)) return(character())
    paste(as.character(d$plotID), as.character(d$individualID), sep = "::")
  }
  plant_rows <- function(d, key = rv$tree) {
    if (is.null(d) || !nrow(d) || is.null(key) || !nzchar(key)) return(d[0, , drop = FALSE])
    d[plant_key_vec(d) %in% key, , drop = FALSE]
  }
  size_lab_rows <- function(one, sp) {
    if (is.null(one)) return(data.frame())
    if (!nrow(one)) return(one)
    size <- one[[sp$col]]
    one[is.finite(size) & size > 0 & is.finite(one$height) & one$height > 0 &
          !is.na(one$scientificName), , drop = FALSE]
  }
  growth_key_vec <- function(d) {
    if (is.null(d) || !nrow(d)) return(character())
    if ("plotID" %in% names(d)) paste(as.character(d$plotID), as.character(d$individualID), sep = "::")
    else as.character(d$individualID)
  }

  # One searchable place picker replaces the former state -> site cascade.
  # Server-side Selectize keeps its searchable choices in a session data object,
  # so every picker update must re-register the same validated site family. In
  # particular, returning from an explored site must not leave only the cleared
  # placeholder in the browser with an empty remote search source.
  site_picker_choices <- function() {
    rows <- site_table[order(site_table$name, site_table$site), , drop = FALSE]
    choices <- stats::setNames(rows$site,
      sprintf("%s · %s, %s", rows$site, rows$name, rows$state))
    c("Choose a place…" = "", choices)
  }
  refresh_site_picker <- function(selected = "") {
    updateSelectizeInput(session, "site", choices = site_picker_choices(),
                         selected = selected, server = TRUE)
  }
  observeEvent(TRUE, {
    refresh_site_picker()
    if (!isTRUE(VEG_FAMILY_READY)) {
      shinyjs::disable("loadBtn")
      shinyjs::disable("searchNetworkBtn")
    }
  }, once = TRUE)
  output$siteBio <- renderUI({
    if (!isTRUE(VEG_FAMILY_READY)) {
      return(div(class = "site-bio search-empty", role = "status", `aria-live` = "polite",
        bs_icon("exclamation-triangle-fill"),
        div(tags$b("Candidate data family on hold. "),
          "The exact 42-site v2 contract, official source receipt, and search index do not yet agree. Derived counts, site views, and network search are unavailable—this is not evidence of zero vegetation.")))
    }
    req(input$site)
    b <- site_bio(input$site)
    if (is.null(b)) return(NULL)
    div(class = "site-bio", bs_icon("info-circle-fill"), span(b))
  })

  shinyjs::hide("mainTabsWrap")

  ingest <- function(b, label, is_demo = FALSE, expected_site = NULL,
                     requested_channel = NULL) {
    contract_check <- bundle_contract_check(b, expected_site = expected_site)
    available_channels <- if (isTRUE(contract_check$ok)) supported_channels(b) else character()
    supported_site <- isTRUE(contract_check$ok) &&
      identical(.veg_scalar_chr(b$contract$index$site$support_status), "supported_sampled_context") &&
      length(available_channels) > 0L
    if (!isTRUE(supported_site)) {
      session$sendCustomMessage("loadDone", list())
      if (!isTRUE(contract_check$ok)) {
        warning(sprintf("refused Vegetation bundle: %s", paste(contract_check$reason, collapse = "; ")))
        showNotification("This site bundle is outside the exact validated v2 data family. Derived views remain on hold.", type = "warning", duration = 8)
      } else {
        showNotification("No supported census is available for this site; that is held—not zero.", type = "warning", duration = 8)
      }
      return(invisible(FALSE))
    }
    b$trees$.plant_key <- plant_key_vec(b$trees)
    selected_channel <- .veg_scalar_chr(requested_channel)
    if (is.na(selected_channel) || !selected_channel %in% available_channels) {
      selected_channel <- .veg_scalar_chr(b$meta$primary_channel)
    }
    if (is.na(selected_channel) || !selected_channel %in% available_channels) {
      selected_channel <- available_channels[[1L]]
    }
    rv$bundle <- b
    rv$available_channels <- available_channels
    rv$trees <- b$trees
    # The physical channels are selected from the bundled protocol metadata.
    # Never decide by comparing raw DBH cross-section with basal-cover totals;
    # those are different measurements with different sampled areas.
    rv$stype <- selected_channel
    rv$spec  <- size_spec(rv$stype)
    rv$snap  <- tree_snapshot(rv$trees, b$plots, rv$spec) # latest supported event per plot + plant
    rv$one   <- one_per_tree(woody_only(live_only(rv$snap), rv$spec), rv$spec)
    rv$plots <- b$plots
    rv$meta  <- b$meta
    rv$lb    <- plot_summary_veg(rv$snap, b$plots, rv$spec)
    rv$pal   <- make_species_pal(species_level_only(woody_only(rv$snap, rv$spec)))
    rv$label <- label; rv$site <- b$meta$site; rv$is_demo <- is_demo; rv$tree <- NULL
    # A held stand result is an explicit support state, never a treeless claim.
    rv$no_stand <- is.null(stand_site(rv$snap, rv$plots, rv$spec))
    years <- sort(unique(suppressWarnings(as.integer(b$trees$year))))
    years <- years[is.finite(years)]
    year_text <- if (!length(years)) "no measurement records" else if (length(years) == 1) years else paste0(min(years), "–", max(years))
    rv$ctx <- paste0(b$meta$site, " · ", rv$spec$channel_label, " · ", year_text,
                     if (isTRUE(rv$no_stand)) " · plot estimate held" else "")
    shinyjs::show("mainTabsWrap"); shinyjs::show("treePickerWrap"); shinyjs::hide("splash")
    one <- rv$one; sz <- one[[rv$spec$col]]
    lab_meas <- ifelse(is.finite(sz), paste0(round(sz), "cm"),
                       ifelse(is.finite(one$height), paste0(round(one$height), "m tall"), "—"))
    ch <- setNames(plant_key_vec(one), sprintf("%s · %s · %s · %s",
            short_tree(one$individualID), one$plotID,
            ifelse(is.na(one$scientificName), "—", one$scientificName), lab_meas))
    lab_one <- size_lab_rows(one, rv$spec)
    rv$lab_keys <- plant_key_vec(lab_one)
    lab_ch <- ch[plant_key_vec(one) %in% rv$lab_keys]
    updateSelectizeInput(session, "treeSel", choices = c("Pick a plant…" = "", ch), selected = "", server = TRUE)
    updateSelectizeInput(session, "labTreeSel", choices = c("Choose a plant..." = "", lab_ch), selected = "", server = TRUE)
    plant_controls <- c("treeSel", "pickBiggest", "pickTallest", "pickFastest", "surpriseBtn")
    if (nrow(one) > 0L) {
      lapply(plant_controls, shinyjs::enable)
    } else {
      lapply(plant_controls, shinyjs::disable)
    }
    lab_controls <- c("labTreeSel", "pinViewedBtn")
    if (length(rv$lab_keys)) lapply(lab_controls, shinyjs::enable) else lapply(lab_controls, shinyjs::disable)
    session$sendCustomMessage("siteCtx", list(site = rv$site %||% "site"))
    nav_select("tabs", "overview"); session$sendCustomMessage("countUp", list()); session$sendCustomMessage("loadDone", list())
    invisible(TRUE)
  }
  load_site <- function(site, requested_channel = NULL) {
    if (is.null(site) || site == "") { session$sendCustomMessage("loadDone", list()); return() }
    if (!isTRUE(VEG_FAMILY_READY)) {
      session$sendCustomMessage("loadDone", list())
      showNotification("The candidate data family is on hold. Site views and derived counts are unavailable.", type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    if (!site %in% BUNDLED) {
      session$sendCustomMessage("loadDone", list())
      showNotification("That site is not part of the exact validated 42-site family.", type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    b <- load_site_bundle(site)
    if (is.null(b)) { session$sendCustomMessage("loadDone", list()); showNotification("That validated site bundle could not be read.", type = "error"); return() }
    row <- site_table[site_table$site == site, ]
    refresh_site_picker(selected = site)
    ingest(b, sprintf("%s · %s", site, if (nrow(row)) row$name else site),
           expected_site = site, requested_channel = requested_channel)
  }
  observeEvent(input$loadBtn, load_site(input$site))

  output$channelPicker <- renderUI({
    req(rv$spec, rv$bundle)
    labels <- c(
      tree_dbh = "Tree DBH",
      shrub_sapling_basal = "Shrub & sapling basal"
    )
    available <- rv$available_channels
    counts <- vapply(available, function(id) {
      summary <- rv$bundle$contract$channel_summary[[id]] %||% list()
      as.integer(summary$n_supported_plots %||% 0L)
    }, integer(1))
    choices <- stats::setNames(
      available,
      sprintf("%s · %d plot%s", labels[available], counts,
              ifelse(counts == 1L, "", "s"))
    )
    if (length(choices) <= 1L) {
      return(div(class = "channel-picker channel-picker-single",
        span(class = "channel-picker-label", "Measurement channel"),
        span(class = "channel-picker-value", labels[[rv$stype]])))
    }
    div(class = "channel-picker",
      radioButtons("activeChannel", "View measurement channel", choices = choices,
                   selected = rv$stype, inline = TRUE))
  })

  observeEvent(input$activeChannel, {
    requested <- input$activeChannel %||% ""
    if (!nzchar(requested) || identical(requested, rv$stype) ||
        is.null(rv$bundle) || !requested %in% rv$available_channels) return()
    ingest(rv$bundle, rv$label, rv$is_demo, expected_site = rv$site,
           requested_channel = requested)
  }, ignoreInit = TRUE)

  reset_to_places <- function() {
    rv$trees <- NULL; rv$snap <- NULL; rv$one <- NULL; rv$plots <- NULL; rv$meta <- NULL; rv$lb <- NULL
    rv$site <- NULL; rv$label <- NULL; rv$tree <- NULL; rv$bundle <- NULL
    rv$available_channels <- character(); rv$lab_keys <- character()
    refresh_site_picker()
    shinyjs::hide("mainTabsWrap"); shinyjs::hide("treePickerWrap"); shinyjs::show("splash")
    session$sendCustomMessage("kickMaps", list())
  }
  observeEvent(input$changeSite, reset_to_places())
  observeEvent(input$browsePlaces, reset_to_places())
  observeEvent(input$searchNetworkBtn, {
    if (!isTRUE(VEG_FAMILY_READY)) {
      showNotification("Network search is held until the exact v2 site and search indexes agree.", type = "warning", duration = 8)
      return()
    }
    shinyjs::hide("splash"); shinyjs::hide("treePickerWrap"); shinyjs::show("mainTabsWrap")
    nav_select("tabs", "search")
  })

  # ---- the site-choice popup + "About this site" card --------------------
  # Tapping a dot no longer auto-loads. It opens a small popup anchored on the
  # dot offering a CLEAR choice: "Explore this site" (loads the record) or
  # "About this site" (an instant info card) — mirroring the flagship Small
  # Mammal Tracker. Both built from the clicked site code.
  site_popup_html <- function(row) {
    code  <- row$site[1]
    shrub <- identical(row$primary_channel[1], "shrub_sapling_basal")
    supported <- identical(row$support_status[1], "supported_sampled_context")
    emoji <- if (!supported) "\u26A0\uFE0F" else if (shrub) "\U0001F33F" else "\U0001F333"
    noun  <- if (!supported) "live plants" else if (shrub) "shrubs & saplings" else "trees"
    where <- paste(stats::na.omit(c(as.character(row$name[1]), as.character(row$state[1]))),
                   collapse = ", ")
    size_line <- if (supported && (!is.na(row$tallest_m[1]) || !is.na(row$biggest_diam_cm[1])))
      sprintf("<div class='sp-years'>tallest %sm &middot; widest %scm</div>",
              fmt_num(row$tallest_m[1]), fmt_num(row$biggest_diam_cm[1])) else ""
    support_line <- if (supported) "" else
      "<div class='sp-years'><b>No supported census; not zero.</b> Derived values are unavailable.</div>"
    actions <- if (supported) sprintf(
      "<button type='button' class='sp-btn sp-go' onclick=\"smtLoadStart('%s \\u00b7 loading\\u2026');Shiny.setInputValue('siteExplore','%s',{priority:'event'});\">Explore this site &rarr;</button>",
      gsub("'", "", row$name[1] %||% code), code
    ) else ""
    actions <- paste0(actions, sprintf(
      "<button type='button' class='sp-btn sp-info' onclick=\"Shiny.setInputValue('siteInfo','%s',{priority:'event'});\">About this site</button>",
      code
    ))
    htmltools::HTML(sprintf(
      "<div class='pm-pop site-pop'>
         <div class='pm-pop-t'>%s %s <span class='sp-code'>(%s)</span></div>
         <div class='pm-pop-s'>%s</div>
         <div class='pm-pop-n'><b>%s</b> %s &middot; <b>%s</b> species</div>
         %s
         %s
         <div class='sp-actions'>%s</div>
       </div>",
      emoji, row$name[1] %||% code, code, where,
      if (supported) fmt_count(row$n_trees[1]) else "—", noun,
      if (supported) fmt_count(row$n_species[1]) else "—",
      size_line, support_line, actions))
  }

  site_info_modal <- function(code) {
    row <- site_table[site_table$site == code, , drop = FALSE]
    if (is.null(row) || !nrow(row))
      return(modalDialog(title = "Site info", easyClose = TRUE, footer = modalButton("Close"),
                         p("No details are available for this site.")))
    dash   <- function(x) if (length(x) == 0 || is.na(x) || !nzchar(as.character(x))) "—" else as.character(x)
    supported <- identical(row$support_status[1], "supported_sampled_context")
    shrub  <- identical(row$primary_channel[1], "shrub_sapling_basal")
    emoji  <- if (!supported) "\u26A0\uFE0F" else if (shrub) "\U0001F33F" else "\U0001F333"
    noun   <- if (!supported) "live plants" else if (shrub) "shrubs & saplings" else "trees"
    coords <- if (!is.na(row$lat[1]) && !is.na(row$lng[1]))
      sprintf("%.3f, %.3f", row$lat[1], row$lng[1]) else "—"
    bio    <- site_bio(code)
    stat <- function(v, lab) div(class = "si-stat",
      div(class = "si-stat-n", if (is.null(v) || is.na(v)) "—" else format(v, big.mark = ",")),
      div(class = "si-stat-l", lab))
    modalDialog(
      title = HTML(sprintf("%s %s <span class='si-code'>(%s)</span>", emoji, dash(row$name[1]), code)),
      easyClose = TRUE, size = "m",
      footer = tagList(
        modalButton("Close"),
        if (supported) tags$button(type = "button", class = "btn btn-primary",
            onclick = sprintf("smtDismissModalForLoad();smtLoadStart('%s \\u00b7 loading\\u2026');Shiny.setInputValue('siteExplore','%s',{priority:'event'});",
                              gsub("'", "\\\\'", dash(row$name[1])), code),
            HTML("Explore this site&rsquo;s data &rarr;"))),
      div(class = "site-info",
        div(class = "si-sec",
          div(class = "si-h", "Where"),
          div(class = "si-row", dash(row$state[1]), " · NEON ", dash(row$domain[1])),
          if (!is.null(bio)) div(class = "si-row si-bio", bio),
          div(class = "si-coords", bs_icon("geo-alt"), " ", coords)),
        div(class = "si-sec",
          div(class = "si-h", "Supported sampled records"),
          if (supported) tagList(
            div(class = "si-stats",
              stat(row$n_trees[1], noun),
              stat(row$n_species[1], "species")),
            div(class = "si-row",
              "Tallest ", fmt_num(row$tallest_m[1]), "m · widest ", fmt_num(row$biggest_diam_cm[1]), "cm")
          ) else tagList(
            div(class = "si-stats", stat(NA, noun), stat(NA, "species")),
            div(class = "si-row", tags$b("No supported census; not zero."),
                " Derived record, size, and richness values are unavailable."))),
        div(class = "si-sec",
          div(class = "si-h", "Default measurement view"),
          div(class = "si-row si-fam",
            if (!supported) "Held / unavailable" else if (shrub) "Shrub & sapling basal diameter" else "Tree DBH (trees ≥10 cm)"))))
  }

  # national site-picker map on the splash: dot size = sampled live plants, colour =
  # active physical channel. Tap a dot to OPEN the Explore | About
  # popup — the flagship front door. The load now comes from the popup's Explore
  # button (input$siteExplore), so the module no longer drives picked().
  local({
    mapPickerServer("picker", site_table = site_table, radius_metric = "n_trees",
      color_fn = function(st) ifelse(st$support_status != "supported_sampled_context", "#7b827d",
        ifelse(st$primary_channel %in% "shrub_sapling_basal", DDL$bark, DDL$navy)),
      dash_fn = function(st) ifelse(st$support_status != "supported_sampled_context", "2 4",
        ifelse(st$primary_channel %in% "shrub_sapling_basal", "7 4", "")),
      label_fn = function(r) {
        supported <- identical(r$support_status, "supported_sampled_context")
        if (!supported) return(sprintf(
          "<b>%s</b> · %s, %s<br><b>—</b> live plants · <b>—</b> species<br><b>No supported census; not zero.</b><br><span style='color:#2f8fc4;font-weight:700'>Tap for site details</span>",
          r$site, r$name %||% r$site, r$state %||% ""))
        sprintf(
          "<b>%s</b> · %s, %s<br><b>%s</b> %s · <b>%s</b> species<br>tallest %sm · widest %scm<br><span style='color:#2f8fc4;font-weight:700'>Tap for site options</span>",
          r$site, r$name %||% r$site, r$state %||% "",
          fmt_count(r$n_trees),
          if (identical(r$primary_channel, "shrub_sapling_basal")) "shrubs & saplings" else "trees",
          fmt_count(r$n_species), fmt_num(r$tallest_m), fmt_num(r$biggest_diam_cm))
      },
      popup_fn = site_popup_html,
      legend_colors = c(DDL$navy, DDL$bark, "#7b827d"),
      legend_labels = c("Tree DBH · solid ring", "Shrub & sapling basal · dashed ring",
                        "Held / unknown · dotted ring"),
      legend_dashes = c("", "7 4", "2 4"),
      legend_title = "Measurement view")
  })

  # "Explore this site" (popup button OR About-modal footer button) -> load it.
  # Runs in the MAIN server context so ingest()'s shinyjs::hide("splash") isn't namespaced.
  observeEvent(input$siteExplore, { removeModal(); load_site(input$siteExplore) })
  # "About this site" -> instant info card (no bundle load)
  observeEvent(input$siteInfo, showModal(site_info_modal(input$siteInfo)))

  pick_tree <- function(key, navigate = FALSE) {
    if (is.null(key) || is.na(key) || key == "" || is.null(rv$snap)) return()
    # Backward-compatible bridge for old click payloads: accept a raw id only
    # when it resolves unambiguously, then immediately promote it to the key.
    if (!grepl("::", key, fixed = TRUE)) {
      hits <- unique(plant_key_vec(rv$snap[rv$snap$individualID %in% key, , drop = FALSE]))
      if (length(hits) != 1) return()
      key <- hits[[1]]
    }
    if (!nrow(plant_rows(rv$snap, key))) return()
    rv$tree <- key
    if (!identical(input$treeSel, key)) {
      shiny::freezeReactiveValue(input, "treeSel")
      updateSelectizeInput(session, "treeSel", selected = key)
    }
    lab_selected <- if (key %in% rv$lab_keys) key else ""
    if (!identical(input$labTreeSel, lab_selected)) {
      shiny::freezeReactiveValue(input, "labTreeSel")
      updateSelectizeInput(session, "labTreeSel", selected = lab_selected)
    }
    if (navigate) nav_select("tabs", "tree")
  }
  observeEvent(input$treeSel, {
    key <- input$treeSel %||% ""
    if (nzchar(key) && !identical(key, rv$tree)) pick_tree(key, navigate = TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$labTreeSel, {
    key <- input$labTreeSel %||% ""
    if (nzchar(key) && !identical(key, rv$tree)) pick_tree(key)
  }, ignoreInit = TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_tree(input$qcCardRequest, navigate = TRUE), ignoreInit = TRUE)
  observeEvent(input$surpriseBtn, {
    one <- rv$one
    if (is.null(one) || !nrow(one)) return()
    pick_tree(sample(plant_key_vec(one), 1), navigate = TRUE)
  })

  observeEvent(input$goStand,  nav_select("tabs", "stand"))
  observeEvent(input$goGrowth, nav_select("tabs", "growth"))
  observeEvent(input$goLab,    nav_select("tabs", "lab"))
  observeEvent(input$goSearch, nav_select("tabs", "search"))

  # ---- hero ---------------------------------------------------------------
  output$heroStats <- renderUI({
    one <- rv$one; snap <- rv$snap; if (is.null(one)) return(NULL)
    sp <- SP(); shrub <- identical(sp$type, "shrubland")
    live_snap <- woody_only(live_only(snap), sp)
    woody_sp <- species_level_only(woody_only(one, sp))            # stand species
    tallest <- smax(live_snap$height)
    biggest <- smax(woody_only(live_snap, sp)[[sp$col]])
    thresh  <- if (shrub) "(any basal stem diameter)" else "≥ 10 cm DBH (the protocol tree threshold)"
    hero <- function(v, l, suf = "", icon, tone, ttl = NULL, click = NULL) {
      attrs <- list(class = paste0("hero-stat hero-", tone, if (!is.null(click)) " hero-click" else ""), title = ttl)
      if (!is.null(click)) {
        attrs$type <- "button"
        attrs$onclick <- sprintf("Shiny.setInputValue('heroClick','%s',{priority:'event'})", click)
      }
      tag_fn <- if (is.null(click)) div else tags$button
      do.call(tag_fn, c(attrs, list(
        div(class = "hs-icon", bs_icon(icon)),
        div(div(class = "hs-v count-up", `data-target` = v, `data-suffix` = suf, "0"),
            div(class = "hs-l", l, if (!is.null(click)) tags$span(class = "stat-q", bs_icon("chevron-right")))))))
    }
    st <- stand_site(snap, rv$plots, sp)
    supported <- !is.null(st)
    div(class = "hero-band",
      div(class = "hero-title",
        bs_icon(if (shrub) "flower2" else "tree-fill"), tags$b(rv$label),
        if (isTRUE(rv$is_demo)) span(class = "demo-pill", bs_icon("stars"), " DEMO"),
        actionLink("changeSite", tagList(bs_icon("arrow-left-circle"), " change site"),
                   class = "hero-change"),
        downloadLink("reportPdf", tagList(bs_icon("file-earmark-arrow-down"), " report (PDF)"),
                     class = "hero-report")),
      source_gap_notice(),
      div(class = "hero-grid",
        hero(if (supported) nrow(one) else "—", paste0("live ", sp$nouns), icon = if (shrub) "flower2" else "tree", tone = "pine",
             ttl = sprintf("Live tagged %s %s, one count per individual.", sp$nouns, thresh)),
        hero(if (supported) dplyr::n_distinct(woody_sp$scientificName) else "—", "identified species", icon = "diagram-3", tone = "sky",
             ttl = sprintf("Identified species among live %s %s. Tap to inspect measured taxon contribution within this channel.", sp$nouns, thresh), click = if (nrow(woody_sp)) "species" else NULL),
        hero(if (is.finite(tallest)) round(tallest, 1) else "—", "m tallest", icon = "arrows-vertical", tone = "gold",
             ttl = sprintf("Tallest live %s at the site.", sp$noun)),
        hero(if (is.finite(biggest)) round(biggest, 1) else "—", paste0("cm biggest ", sp$size_lab), icon = "circle", tone = "bark",
             ttl = sprintf("Largest live %s by %s. Tap to see the biggest.", sp$noun, sp$size_full), click = if (is.finite(biggest)) "biggest" else NULL)))
  })

  # ---- OVERVIEW -----------------------------------------------------------
  output$baBar <- renderPlotly({
    ssall <- species_structure(rv$snap, rv$plots, SP()); if (is.null(ssall) || !nrow(ssall)) return(note_plot("No supported measured contribution"))
    tot <- sum(ssall$ba_m2_ha, na.rm = TRUE)
    ss <- head(ssall, 18); ss$share <- if (is.finite(tot) && tot > 0) round(100 * ss$ba_m2_ha / tot) else 0
    ss$sciKey <- as.character(ss$scientificName)
    ss$scientificName <- factor(ss$scientificName, levels = rev(ss$scientificName))
    pal <- rv$pal %||% make_species_pal(rv$snap)
    # source + per-bar customdata(species) make the bars CLICKABLE: the
    # plotly_click observer below filters this site's live plants to the clicked
    # species and reveals them in a table + a "Download these (CSV)" button.
    plot_ly(ss, x = ~ba_m2_ha, y = ~scientificName, type = "bar", orientation = "h",
      source = "baBar", customdata = ~sciKey,
      marker = list(color = unname(pal[as.character(ss$scientificName)] %||% DDL$green)),
      text = ~paste0(share, "% of measured total · ", stems, " stem records · click to list plants"),
      hovertemplate = "%{y}<br>%{x:.1f} m²/ha mean plot contribution · %{text}<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE,
        xaxis = list(title = "Mean sampled-plot contribution (m²/ha)"),
        yaxis = list(title = ""), margin = list(l = 200)) %>%
      plotly::event_register("plotly_click")
  })

  # ---- baBar member-reveal: click a species bar -> list its plants + CSV -----
  # Reuses the QC inspector modal pattern. The clicked species' customdata is the
  # member key; we filter this site's live plants (one row per plant) for the
  # on-screen table and the full per-measurement careers for the CSV.
  baBar_members <- reactiveVal(NULL)   # list(sci=, rows=on-screen df, full=career df)
  # Do not ask plotly for this hidden, site-scoped event during the landing-page
  # flush. The chart cannot register its source until a place has loaded; eager
  # event_data() calls therefore emit a misleading runtime warning on every
  # home-page connection. Once the site context exists, the rendered chart and
  # this reactive register in the same flush and repeated clicks remain events.
  baBar_click <- reactive({
    req(rv$site, rv$snap, rv$plots, rv$spec)
    plotly::event_data("plotly_click", source = "baBar", priority = "event")
  })
  observeEvent(baBar_click(), {
    ev <- baBar_click(); req(ev)
    sci <- ev$customdata; req(!is.null(sci), nzchar(sci))
    sp <- SP(); one <- rv$one; req(one)
    one$.taxon_label <- .taxon_name(one)
    m <- one[one$.taxon_label == sci, , drop = FALSE]
    if (!nrow(m)) return()
    m <- m[order(-m[[sp$col]]), , drop = FALSE]
    shown <- data.frame(
      plant = short_tree(m$individualID),
      plot = m$plotID,
      size_cm = round(m[[sp$col]], 1),
      height_m = ifelse(is.finite(m$height), round(m$height, 1), NA),
      status = m$plantStatus,
      stringsAsFactors = FALSE)
    names(shown)[3] <- paste0(sp$size_lab, "_cm")
    all_labels <- .taxon_name(rv$trees)
    full <- tidy_trees_export(rv$trees[all_labels == sci, , drop = FALSE], rv$meta)
    baBar_members(list(sci = sci, full = full %||% data.frame()))
    head_rows <- utils::head(shown, 80)
    showModal(modalDialog(easyClose = TRUE, size = "l",
      title = tagList(bs_icon("tree-fill"), tags$em(sci), sprintf(" · %d live %s", nrow(m), sp$nouns)),
      p(class = "qc-why", sprintf("Live %s with this identification at this site (one row per plant, biggest stem), ranked by %s. Download for the full preserved records.", sp$nouns, sp$size_full)),
      tags$div(class = "qc-modal-tbl",
        tags$table(class = "inspect-tbl",
          tags$thead(tags$tr(lapply(names(head_rows), function(nm) tags$th(nm)))),
          tags$tbody(lapply(seq_len(nrow(head_rows)), function(i)
            tags$tr(lapply(head_rows[i, ], function(v) tags$td(as.character(v)))))))),
      if (nrow(shown) > 80) p(class = "dim", sprintf("Showing 80 of %d. Download for all.", nrow(shown))),
      footer = tagList(downloadButton("baBarMembersCsv", "Download these (CSV)", class = "btn-outline-dark btn-sm"), modalButton("Close"))))
  }, ignoreInit = TRUE)
  output$baBarMembersCsv <- downloadHandler(
    filename = function() sprintf("NEON-veg-%s-species-%s-%s.csv", rv$site %||% "site",
      gsub("[^A-Za-z0-9]+", "", (baBar_members() %||% list(sci = "species"))$sci), format(Sys.Date(), "%Y%m%d")),
    content = function(file) utils::write.csv((baBar_members() %||% list(full = data.frame()))$full %||% data.frame(), file, row.names = FALSE, na = ""))

  output$overviewInsight <- renderUI({
    sp <- SP(); ss <- species_structure(rv$snap, rv$plots, sp); req(!is.null(ss), nrow(ss) > 0)
    st <- stand_site(rv$snap, rv$plots, sp)
    insight_banner("stars", tone = "pine",
      HTML(sprintf("Within the supported <b>%s</b>, <b><i>%s</i></b> has the largest equal-plot measured contribution (%d stem records). <span class='ci-hero'>%d</span> identified species are represented; the channel mean is <b>%s</b> m²/ha. This is sampled measurement contribution—not ecological dominance.",
        sp$channel_label, ss$scientificName[1], ss$stems[1],
        dplyr::n_distinct(species_level_only(woody_only(rv$one, sp))$scientificName),
        if (is.null(st)) "—" else fmt_num(st$ba_ha))))
  })
  output$siteInsights <- renderUI({
    snap <- rv$snap; one <- rv$one; req(snap, one); sp <- SP()
    st <- stand_site(snap, rv$plots, sp); g <- tree_growth(rv$trees, sp, rv$plots); ss <- species_structure(snap, rv$plots, sp)
    one_d <- one[is.finite(one[[sp$col]]), ]; one_h <- one[is.finite(one$height), ]
    big  <- if (nrow(one_d)) one_d[which.max(one_d[[sp$col]]), ] else one[0, ]
    tall <- if (nrow(one_h)) one_h[which.max(one_h$height), ]      else one[0, ]
    pts <- c()
    if (!is.null(st)) pts <- c(pts, sprintf("Across <b>%d supported sampled plots</b>, the mean is <b>%s stems/ha</b> and <b>%s m²/ha</b>%s (stem-weighted quadratic mean %s: %s cm). This describes the sampled plot channel, not the whole site.", st$n_plots, fmt_count(st$density_ha), fmt_num(st$ba_ha), if (!is.null(st$ba_se) && is.finite(st$ba_se)) sprintf(" (plot SE %s m²/ha)", fmt_num(st$ba_se)) else "", sp$size_lab, fmt_num(st$qmd)))
    if (nrow(big) && nrow(tall)) pts <- c(pts, sprintf("The biggest %s is a <b><i>%s</i></b> at <b>%s cm</b> %s; the tallest reaches <b>%s m</b> (<i>%s</i>).", sp$noun, big$scientificName, round(big[[sp$col]],1), sp$size_lab, round(tall$height,1), tall$scientificName))
    else if (nrow(big)) pts <- c(pts, sprintf("The biggest %s is a <b><i>%s</i></b> at <b>%s cm</b> %s.", sp$noun, big$scientificName, round(big[[sp$col]],1), sp$size_lab))
    if (!is.null(g) && nrow(g)) {
      gg <- g[!g$mh_change & is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2, ]
      if (nrow(gg)) pts <- c(pts, sprintf("Across <b>%s</b> comparable remeasured %s, median annualized %s change is <b>%.2f cm/yr</b>.", format(nrow(gg), big.mark=","), sp$nouns, sp$size_lab, stats::median(gg$growth_cm_yr, na.rm=TRUE)))
    }
    pts <- c(pts, "Sampled absence is retained as zero; impractical, dendrometer-only, or otherwise unsupported plot-events are held out rather than turned into zeros. Biomass is not estimated.")
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
    # A one-time size distribution is descriptive. Do not overlay an "expected"
    # curve or infer recruitment, regeneration, or age from its shape alone.
    p %>% plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE,
        xaxis = list(title = paste0(if (forest) "Diameter" else "Basal diameter", " class (cm ", sp$size_lab, ")")),
        yaxis = list(title = ylab))
  })
  output$sizeInsight <- renderUI({
    sp <- SP(); sc <- size_class(rv$snap, rv$plots, sp); req(!is.null(sc))
    bk <- size_breaks(sp)
    small <- sum(sc$stems[sc$cls %in% bk$small]); big <- sum(sc$stems[sc$cls %in% bk$big])
    ratio <- if (big > 0) round(small / big, 1) else NA
    if (identical(sp$type, "shrubland")) {
      shape <- if (is.na(ratio)) "concentrated in the smaller measured stems" else
               if (ratio >= 3) "strongly weighted toward smaller measured stems" else
               if (ratio >= 1.2) "moderately weighted toward smaller measured stems" else
               "fairly even across the displayed basal-size classes"
      insight_banner("bar-chart-fill", tone = "pine",
        HTML(sprintf("By <b>basal stem diameter</b>, this sampled snapshot is <b>%s</b>%s. That is a descriptive size pattern—not evidence of recruitment, age, or regeneration.",
          shape, if (!is.na(ratio)) sprintf(" (about %.1f small per large measured stem)", ratio) else "")))
    } else {
      shape <- if (is.na(ratio)) "concentrated in the smaller classes" else
               if (ratio >= 3) "strongly weighted toward the smaller displayed classes" else
               if (ratio >= 1.2) "a moderate descending shape" else
               "weighted toward the larger displayed classes"
      insight_banner("bar-chart-fill", tone = "pine",
        HTML(sprintf("Among trees ≥10 cm DBH, this sampled snapshot is <b>%s</b>%s. Smaller saplings use a different nested sampling channel and are not mixed into this chart; shape alone does not establish recruitment or stand age.",
          shape, if (!is.na(ratio)) sprintf(" (about %.1f small per large measured stem)", ratio) else "")))
    }
  })
  # "this site vs the network" — all bundled sites by woody richness, current gold
  output$networkStrip <- renderPlotly({
    si <- site_table; if (is.null(si) || !nrow(si) || !"n_species" %in% names(si)) return(note_plot("No network index"))
    cols <- intersect(c("site", "name", "n_species", "n_trees", "n_plots"), names(si))
    d <- si[is.finite(si$n_species), cols, drop = FALSE]
    if (!nrow(d)) return(note_plot("No richness data"))
    d <- d[order(d$n_species), ]; d$site <- factor(d$site, levels = d$site)
    cur <- rv$site %||% ""
    d$col <- ifelse(as.character(d$site) == cur, "#ffd24a", DDL$sky)
    # Raw observed-species count is sampling-effort-confounded (more plots / more
    # stems find more species). Label it honestly and surface effort (n_trees,
    # n_plots) in every bar's tooltip so the reader can see what drove the count.
    eff <- if (all(c("n_trees","n_plots") %in% names(d)))
      paste0("<br>", format(d$n_trees, big.mark=","), " live plants over ", d$n_plots, " plots (effort varies)")
      else if ("n_trees" %in% names(d)) paste0("<br>", format(d$n_trees, big.mark=","), " live plants (effort varies)")
      else ""
    d$tip <- paste0("<b>", d$site, "</b> · ", d$name, "<br>", d$n_species, " observed woody species", eff,
        ifelse(as.character(d$site) == cur, "<br>◀ this site", ""))
    plot_ly(d, y = ~site, x = ~n_species, type = "bar", orientation = "h",
      marker = list(color = ~col), customdata = ~tip,
      hovertemplate = "%{customdata}<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, bargap = 0.22,
        xaxis = list(title = "Observed woody species (sampling effort varies)"),
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 9)))
  })
  output$htPlot <- renderPlotly({
    s <- woody_only(live_only(rv$snap), SP()); h <- s$height[is.finite(s$height) & s$height > 0]; if (!length(h)) return(note_plot("No height data"))
    plot_ly(x = h, type = "histogram", nbinsx = 24, marker = list(color = DDL$bark),
      hovertemplate = "%{x} m<br>%{y} stems<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = "Height (m)"), yaxis = list(title = "Live stems"))
  })
  output$densityBanner <- renderUI({
    sp <- SP(); st <- stand_site(rv$snap, rv$plots, sp)
    if (is.null(st)) {
      return(div(class = "chart-insight ci-bark stand-empty", bs_icon("info-circle"),
        div(class = "ci-text", HTML(paste0(
          "<b>Plot estimate held.</b> The bundled record does not contain a matched, supported ",
          if (identical(sp$type, "shrubland")) "shrub/sapling" else "tree",
          " sampling opportunity with a valid area denominator. That is an unknown/unsupported state—not evidence of zero woody vegetation."))),
        source_gap_inline()))
    }
    pre  <- if (st$n_plots < 3) "Preliminary (few plots): " else ""
    se_ba <- if (is.finite(st$ba_se)) sprintf(" ±%s SE", fmt_num(st$ba_se)) else ""
    se_d  <- if (is.finite(st$density_se)) sprintf(" ±%s", fmt_count(st$density_se)) else ""
    scope <- if (identical(sp$type, "shrubland")) "shrubs (basal diameter)" else "trees ≥10 cm DBH"
    # Largest measured contributor within this channel; no ecological dominance claim.
    ss <- species_structure(rv$snap, rv$plots, sp)
    dom_txt <- if (!is.null(ss) && nrow(ss) && is.finite(ss$ba_m2_ha[1]) && sum(ss$ba_m2_ha, na.rm = TRUE) > 0)
      sprintf(" Largest measured contributor: <b><i>%s</i></b> (<b>%.0f%%</b> of this channel's area).",
              ss$scientificName[1], 100 * ss$ba_m2_ha[1] / sum(ss$ba_m2_ha, na.rm = TRUE)) else ""
    # Keep plot designs visible as context, without promoting either subset to a
    # certified site-wide estimator.
    design_pop <- NULL
    types <- if ("plotType" %in% names(rv$plots)) unique(rv$plots$plotType[rv$plots$plotID %in% rv$snap$plotID]) else character()
    if (sum(c("distributed", "tower") %in% types) == 2) {
      dst <- stand_site(rv$snap, rv$plots, sp, plot_types = "distributed")
      if (!is.null(dst)) design_pop <- bslib::popover(
        tags$span(class = "info-dot", tabindex = "0", role = "button",
                  `aria-label` = "More info: sampling-design context",
                  bs_icon("info-circle", `aria-hidden` = "true")),
        title = "Sampling-design context",
        p("The headline pools ", tags$b("tower"), " (clustered at the flux tower) and ", tags$b("distributed"),
          " plots. Shown separately for transparency, the ", tags$b("distributed-only"), " sampled subset is:"),
        p(HTML(sprintf("<b>%s m²/ha</b>%s measured cross-sectional area, <b>%s stems/ha</b> across <b>%d</b> distributed plots.",
          fmt_num(dst$ba_ha), if (is.finite(dst$ba_se)) sprintf(" ±%s SE", fmt_num(dst$ba_se)) else "",
          fmt_count(dst$density_ha), dst$n_plots))),
        p(class = "dim", "This remains a summary of supported sampled plots, not a wall-to-wall site estimate."))
    }
    support_txt <- stand_support_message(st, rv$plots, sp)
    insight_banner("calculator", tone = "gold",
      HTML(sprintf("%sAcross <b>%d supported sampled plots</b> (%s): <span class='ci-hero'>%s m²/ha</span>%s measured cross-sectional area, <b>%s stems/ha</b>%s, stem-weighted quadratic mean %s <b>%s cm</b>.%s <span class='dim'>Mean ± plot SE. %s This is not a wall-to-wall inventory.</span>",
        pre, st$n_plots, scope, fmt_num(st$ba_ha), se_ba, fmt_count(st$density_ha), se_d, sp$size_lab, fmt_num(st$qmd), dom_txt, support_txt)),
      design_pop, source_gap_inline())
  })

  # ---- GROWTH & MORTALITY -------------------------------------------------
  output$growthPlot <- renderPlotly({
    sp <- SP(); g <- tree_growth(rv$trees, sp, rv$plots); if (is.null(g) || !nrow(g)) return(note_plot(sprintf("No supported remeasured %s yet for a growth estimate", sp$nouns)))
    gg <- g$growth_cm_yr[is.finite(g$growth_cm_yr) & g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2 & !g$mh_change]
    plot_ly(x = gg, type = "histogram", nbinsx = 30, marker = list(color = DDL$green),
      hovertemplate = paste0("%{x} cm/yr<br>%{y} ", sp$nouns, "<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(showlegend = FALSE, xaxis = list(title = paste0(if (identical(sp$type,"shrubland")) "Basal-diameter" else "Diameter", " change (cm/yr)")), yaxis = list(title = sp$Nouns),
        shapes = list(list(type="line", x0=0, x1=0, yref="paper", y0=0, y1=1, line=list(color="rgba(224,180,58,0.8)", dash="dot", width=1))))
  })
  output$growthInsight <- renderUI({
    sp <- SP(); g <- tree_growth(rv$trees, sp, rv$plots); req(!is.null(g), nrow(g) > 0)
    nmh <- sum(g$mh_change, na.rm = TRUE)
    gv <- g[!g$mh_change & is.finite(g$growth_cm_yr), ]
    clean <- gv[gv$growth_cm_yr <= 5 & gv$growth_cm_yr >= -2, ]; req(nrow(clean) > 0)
    trunc_n <- nrow(gv) - nrow(clean)
    med <- stats::median(clean$growth_cm_yr, na.rm = TRUE)
    q <- stats::quantile(clean$growth_cm_yr, c(.25, .75), na.rm = TRUE, names = FALSE)
    neg <- round(100 * mean(clean$growth_cm_yr < -0.1, na.rm = TRUE))
    insight_banner("graph-up", tone = "pine",
      HTML(sprintf("Across <b>%s</b> comparable remeasurements, median annualized %s change is <span class='ci-hero'>%.2f cm/yr</span> (IQR %.2f–%.2f). About <b>%d%%</b> decrease between events; decreases can reflect biology, damage, or measurement differences, so they are retained and flagged.%s%s",
        format(nrow(clean), big.mark = ","), sp$size_lab, med, q[1], q[2], neg,
        if (nmh > 0) sprintf(" %s %s with a changed measurement height are excluded.", format(nmh, big.mark = ","), sp$nouns) else "",
        if (trunc_n > 0) sprintf(" %s outside the −2 to +5 cm/yr review screen are off-chart but kept in the data.", format(trunc_n, big.mark = ",")) else "")))
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
  # shared so a clicked row maps to the exact plant (DT row index = data index)
  fast_growers <- reactive({
    sp <- SP(); g <- tree_growth(rv$trees, sp, rv$plots); if (is.null(g) || !nrow(g)) return(NULL)
    g <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5 & !g$mh_change, , drop = FALSE]
    if (!nrow(g)) return(NULL)
    g[order(-g$growth_cm_yr), , drop = FALSE][seq_len(min(20, nrow(g))), , drop = FALSE]
  })
  output$fastTable <- DT::renderDT({
    sp <- SP(); g <- fast_growers()
    if (is.null(g)) return(DT::datatable(data.frame(Message = sprintf("No comparable positive diameter-change records for %s yet.", sp$nouns)), rownames = FALSE, options = list(dom = "t")))
    lab0 <- sprintf("First %s (cm)", sp$size_lab); lab1 <- sprintf("Last %s (cm)", sp$size_lab)
    df <- data.frame(Plant = short_tree(g$individualID), Plot = g$plotID %||% "—", Species = g$scientificName,
                     v0 = round(g$d0,1), v1 = round(g$d1,1),
                     `Annualized change (cm/yr)` = g$growth_cm_yr, check.names = FALSE)
    names(df)[4:5] <- c(lab0, lab1)
    DT::datatable(df, rownames = FALSE, selection = "single", class = "leader-dt",
      options = list(pageLength = 8, dom = "tp", order = list(list(5, "desc"))))
  })
  # click a fast-grower row -> open that plant's career (matches the Champions table)
  observeEvent(input$fastTable_rows_selected, {
    g <- fast_growers(); i <- input$fastTable_rows_selected
    if (!is.null(g) && length(i) && i <= nrow(g)) pick_tree(growth_key_vec(g)[i], navigate = TRUE)
  })

  # Descriptive last-recorded size versus annualized diameter change.
  output$growthSize <- renderPlotly({
    sp <- SP(); g <- tree_growth(rv$trees, sp, rv$plots)
    if (is.null(g) || !nrow(g)) return(note_plot(sprintf("No remeasured %s yet", sp$nouns)))
    g <- g[is.finite(g$d1) & g$d1 > 0 & is.finite(g$growth_cm_yr) &
           g$growth_cm_yr <= 5 & g$growth_cm_yr >= -2 & !g$mh_change, , drop = FALSE]
    if (nrow(g) < 5) return(note_plot(sprintf("Not enough remeasured %s for a size–change view", sp$nouns)))
    topsp <- names(sort(table(g$scientificName[!is.na(g$scientificName)]), decreasing = TRUE))
    topsp <- topsp[seq_len(min(6, length(topsp)))]
    g$grp <- ifelse(g$scientificName %in% topsp, g$scientificName, "other species")
    g$short <- short_tree(g$individualID)
    # pin-card HTML per dot: name + the size/growth stats + an "Open career" chip
    # (the delegated .smt-open listener in pincards.js selects the plant); never an
    # inline onclick (CSP / dsvg-curl rule). Carried in customdata for plotly_click.
    g$tip <- paste0(
      "<span class='smt-pin-emoji'>", sp$emoji, "</span> <b>", g$short, "</b><br/>",
      "<em>", ifelse(is.na(g$scientificName), "—", g$scientificName), "</em><br/>",
      "<span class='smt-pin-stats'>", round(g$d1, 1), " cm last recorded ", sp$size_lab, " · ",
        sprintf("%+.2f", g$growth_cm_yr), " cm/yr</span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", growth_key_vec(g),
        "'>", sp$emoji, " Open ", sp$noun, " career &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    pal <- rv$pal; p <- plot_ly()
    for (s in unique(g$grp)) {
      gs <- g[g$grp == s, ]; col <- if (!is.null(pal) && s %in% names(pal)) pal[[s]] else DDL$muted
      p <- p %>% add_trace(data = gs, x = ~d1, y = ~growth_cm_yr, type = "scatter", mode = "markers",
        name = s, customdata = ~tip, marker = list(size = 7, color = col, opacity = 0.7, line = list(color = "#fff", width = 0.5)),
        text = ~paste0(sp$noun, " ", short),
        hovertemplate = paste0("<b>", s, "</b><br>%{x:.0f} cm last recorded<br>%{y:.2f} cm/yr change<extra></extra>"))
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
        name = "trend", line = list(color = if (is_dark()) "#eaf4ec" else "#16261c", width = 2.5, dash = "dash"),
        hovertemplate = "trend<extra></extra>")
      sprintf("Observed association: %s (Spearman r = %+.2f, p = %.3f, n = %d)",
        if (ct$estimate < 0) "larger last-recorded plants had lower changes" else "larger last-recorded plants had higher changes",
        ct$estimate, ct$p.value, nrow(g))
    } else sprintf("No clear size–change association at this site, shown as scatter only (n = %d)", nrow(g))
    p %>% plotly_theme(legend = TRUE) %>%
      plotly::layout(legend = list(orientation = "h", y = -0.25),
        title = list(text = sub, x = 0, xref = "paper", font = list(size = 12, color = DDL$muted)),
        margin = list(t = 46),
        xaxis = list(title = paste0(if (identical(sp$type, "shrubland")) "Basal diameter" else "DBH", " last supported record (cm)")),
        yaxis = list(title = "Annualized diameter change (cm/yr)", zeroline = TRUE))
  })

  # compound ANNUAL mortality rate (distinct from the snapshot pie)
  output$mortalityBanner <- renderUI({
    sp <- SP(); mr <- stand_mortality(rv$trees, sp, rv$plots)
    if (is.null(mr)) return(insight_banner("info-circle", tone = "navy",
      HTML("<b>Annual mortality:</b> needs ≥2 censuses of a big-enough cohort, not estimable here yet, so only the live/standing-dead snapshot below is shown.")))
    ci <- if (is.finite(mr$lo) && is.finite(mr$hi))
      sprintf(" (95%% plot-cluster jackknife interval %s–%s)", fmt_num(mr$lo, 2), fmt_num(mr$hi, 2)) else ""
    insight_banner("heart-pulse", tone = "pine",
      HTML(sprintf("Descriptive compound <b>annual cohort mortality ≈ <span class='ci-hero'>%s%%/yr</span></b>%s. <b>%s</b> of <b>%s</b> trackable plants transitioned from any-live to all-dead over a mean ~%s-year interval; lost/unknown fates are censored. The breakdown below is a separate point-in-time snapshot.",
        fmt_num(mr$rate_pct, 2), ci, fmt_count(mr$deaths), fmt_count(mr$n0), fmt_num(mr$t_yrs, 1))))
  })

  # ---- SITE DATA-QUALITY scan (clickable inspector + downloadable report) -
  veg_qc <- reactive({ req(rv$trees); tree_qc_site(rv$trees, SP(), rv$plots) })
  qc_modal_rows <- reactiveVal(NULL)
  output$vegQcFlags <- renderUI({
    q <- veg_qc()
    if (is.null(q) || !q$n_flag) return(div(class = "qc-flag qc-flag-clean", bs_icon("check2-circle"),
      HTML(" <b>No listed checks triggered.</b> This is not proof that every record is error-free; it means the app's explicit status, measurement-point, and increment rules found no flags.")))
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
    qc_modal_rows(list(key = f$key, rows = rows))   # rows keep FULL individualID for the CSV (joins to trees_long)
    shown <- utils::head(rows, 60); shown$individualID <- NULL   # on-screen shows the short 'plant' id only
    showModal(modalDialog(easyClose = TRUE, size = "l",
      title = tagList(bs_icon("clipboard-check"), sprintf(" %s · %d plants", f$label, f$n)),
      p(class = "qc-why", f$why),
      tags$div(class = "qc-modal-tbl",
        tags$table(class = "inspect-tbl",
          tags$thead(tags$tr(lapply(names(shown), function(nm) tags$th(nm)))),
          tags$tbody(lapply(seq_len(nrow(shown)), function(i)
            tags$tr(lapply(shown[i, ], function(v) tags$td(as.character(v)))))))),
      if (nrow(rows) > 60) p(class = "dim", sprintf("Showing 60 of %d. Download for all.", nrow(rows))),
      footer = tagList(downloadButton("vegQcFlagCsv", "Download these (CSV)", class = "btn-outline-dark btn-sm"), modalButton("Close"))))
  })
  output$vegQcFlagCsv <- downloadHandler(
    filename = function() sprintf("NEON-veg-%s-QC-%s-%s.csv", rv$site %||% "site",
      (qc_modal_rows() %||% list(key = "flag"))$key, format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      payload <- qc_modal_rows() %||% list(key = "flag", rows = data.frame())
      rows <- payload$rows %||% data.frame()
      rows <- cbind(contract_id = VEG_CONTRACT_ID, flag = payload$key, rows, stringsAsFactors = FALSE)
      utils::write.csv(with_export_receipt(rows, rv$meta), file, row.names = FALSE, na = "")
    })
  output$vegQcReport <- downloadHandler(
    filename = function() sprintf("NEON-veg-%s-QC-report-%s.csv", rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      q <- veg_qc()
      rows <- with_export_receipt(if (is.null(q)) data.frame() else q$report, rv$meta)
      utils::write.csv(rows, file, row.names = FALSE, na = "")
    })

  # ---- SIZE LAB (flagship) -----------------------------------------------
  output$labScatter <- renderPlotly({
    one <- rv$one; req(one); sp <- SP()
    pts <- size_lab_rows(one, sp)
    pts$size <- pts[[sp$col]]
    if (!nrow(pts)) return(note_plot(sprintf("No %s with both a %s and a height to map", sp$nouns, sp$size_lab)))
    pts$short <- short_tree(pts$individualID)
    if (nrow(pts) > 1800) {
      # Force-keep the currently viewed tree so its gold ★ diamond never gets
      # sampled out (it was vanishing on ~10/42 sites incl the HARV demo). Pull
      # the viewing row aside, downsample the rest, then re-bind and de-dup.
      keep_row <- if (!is.null(rv$tree)) plant_rows(pts, rv$tree) else pts[0, ]
      set.seed(7); samp <- pts[sort(sample.int(nrow(pts), 1800)), , drop = FALSE]
      pts <- if (nrow(keep_row)) {
        rb <- rbind(keep_row, samp)
        rb[!duplicated(plant_key_vec(rb)), , drop = FALSE]
      } else samp
    }
    keycol <- input$labColor %||% "species"
    pts$key <- if (keycol == "species") as.character(pts$scientificName)
               else if (keycol == "canopyPosition") as.character(pts$canopyPosition)
               else ifelse(grepl("^Live", pts$plantStatus), "Live", "Dead/other")
    pts$key[is.na(pts$key) | pts$key == ""] <- "—"
    keys <- sort(unique(pts$key))
    kpal <- if (keycol == "species") (rv$pal %||% make_species_pal(pts))
            else stats::setNames(forest_ramp(length(keys)), keys)
    muted_col <- if (is_dark()) "#a4c0aa" else "#5a6a82"; qcol <- if (is_dark()) "#7e8da0" else "#9aa6b2"
    pts$tip <- paste0(
      "<span class='smt-pin-emoji'>", sp$emoji, "</span> <b>", pts$short, "</b><br/>",
      "<em>", ifelse(is.na(pts$scientificName), "—", pts$scientificName), "</em><br/>",
      "<span class='smt-pin-stats'>", round(pts$size,1), " cm ", sp$size_lab, " · ", round(pts$height,1), " m tall",
        ifelse(is.na(pts$canopyPosition), "", paste0("<br/>", pts$canopyPosition)), "</span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", plant_key_vec(pts),
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
    if (!is.null(tag)) { ir <- plant_rows(pts, tag)
      if (nrow(ir) == 1) p <- p %>% add_trace(x = ir$size, y = ir$height, type="scatter", mode="markers",
        name = "★ viewing", customdata = ir$tip, showlegend = TRUE,
        marker = list(symbol="diamond", size=18, color="#ffd24a", line=list(color="#fff", width=1.6)),
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
      sprintf(" %s live plants were measured for %s but not height, so they can't be placed in this 2-D space, and they're not shown here.", format(donly, big.mark = ","), sp$size_full))
  })
  output$treeCardSlot <- renderUI({
    sp <- SP()
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", sp$emoji), h4(sprintf("Tap a %s to see its card", sp$noun)),
      p(sprintf("Tap a dot above and choose “Open %s career”, or use the plant picker above.", sp$noun))))
    snap <- rv$snap; row <- one_per_tree(plant_rows(snap, rv$tree), sp); if (!nrow(row)) return(NULL)
    div(class = "lab-sel", span(class = "ls-emoji", sp$emoji),
      div(class = "ls-body",
        div(class = "ls-id", tags$b(short_tree(row$individualID)), sprintf(" · %s · %s · %s cm %s · %s m",
          row$plotID,
          ifelse(is.na(row$scientificName),"—",row$scientificName), round(row[[sp$col]],1), sp$size_lab, ifelse(is.na(row$height),"—",round(row$height,1)))),
        div(class = "ls-dom", ifelse(is.na(row$plantStatus),"",row$plantStatus))),
      actionButton("goTreeFromCard", tagList(bs_icon("arrows-fullscreen"), " Open full career"), class = "btn-outline-dark btn-sm"))
  })
  observeEvent(input$goTreeFromCard, nav_select("tabs", "tree"))

  # ---- TREE CAREER (profile, downloadable) -------------------------------
  tree_card_ui <- function(key) {
    SZ <- SP()
    snap <- rv$snap; selected <- plant_rows(snap, key)
    row <- one_per_tree(selected, SZ); if (!nrow(row)) return(NULL)
    id <- as.character(row$individualID[[1]])
    career <- plant_rows(rv$trees, key)
    hist <- tree_history(career, id); flags <- tree_qc_flags(hist, SZ, rv$plots)
    sp <- row$scientificName; dcol <- SZ$col
    # how big for its species (size percentile within species, this site)
    cohort <- rv$one[[dcol]][rv$one$scientificName %in% sp & is.finite(rv$one[[dcol]])]
    ncoh <- length(cohort)
    d_now <- row[[dcol]]
    pct <- if (ncoh >= 5 && is.finite(d_now)) round(100 * mean(cohort <= d_now)) else NA
    tile <- function(v, l) div(class = "qc-tile", div(class = "qc-tile-v", v), div(class = "qc-tile-l", l))
    growth_all <- tree_growth(career, SZ, rv$plots)
    measurement_unaligned <- !is.null(growth_all) && nrow(growth_all) &&
      any(growth_all$mh_change %in% TRUE)
    growth <- {
      g <- if (!is.null(growth_all) && nrow(growth_all))
        growth_all[!growth_all$mh_change & is.finite(growth_all$growth_cm_yr), , drop = FALSE] else NULL
      if (!is.null(g) && nrow(g)) g$growth_cm_yr[1] else NA_real_
    }
    n_visits <- if (is.null(hist) || !nrow(hist)) 0L else
      if ("eventID" %in% names(hist)) dplyr::n_distinct(hist$eventID) else dplyr::n_distinct(hist$date)
    trajectory <- tree_trajectory(career, id, dcol, rv$plots)
    supported_career <- .supported_history(career, rv$plots, SZ)
    basal_unaligned <- FALSE
    if (identical(SZ$channel, "shrub_sapling_basal") &&
        !is.null(supported_career) && nrow(supported_career)) {
      supported_career <- .ensure_event_columns(supported_career)
      basal_d <- suppressWarnings(as.numeric(supported_career[[dcol]]))
      basal_live <- if ("live" %in% names(supported_career)) {
        supported_career$live %in% TRUE
      } else {
        rep(TRUE, nrow(supported_career))
      }
      basal_rows <- supported_career[
        supported_career$growthForm %in% SZ$forms & basal_live &
          is.finite(basal_d) & basal_d > 0 & basal_d >= SZ$min,
        , drop = FALSE]
      basal_unaligned <- nrow(basal_rows) > 0L &&
        any(table(as.character(basal_rows$eventID)) > 1L)
    }
    n_comparable_events <- if (is.null(trajectory) || !nrow(trajectory$per_date)) {
      0L
    } else {
      nrow(trajectory$per_date)
    }
    hist_evidence <- if (is.null(hist) || !nrow(hist)) character(0) else {
      plot_key <- paste(rv$plots$plotID, rv$plots$eventID, sep = "\r")
      history_key <- paste(hist$plotID, hist$eventID, sep = "\r")
      support <- as.character(rv$plots[[SZ$support]][match(history_key, plot_key)])
      source_gap <- if ("opportunity_source_missing" %in% names(hist)) {
        hist$opportunity_source_missing %in% TRUE
      } else {
        rep(FALSE, nrow(hist))
      }
      comparable <- !is.na(support) & support == "sampled_with_records"
      held_label <- function(value) {
        labels <- c(
          held_sampling_impractical = "Not used · sampling could not be completed",
          held_dendrometer_only = "Not used · different field method",
          held_missing_area = "Not used · sampled area missing",
          held_opportunity_unknown = "Not used · sampling context unclear",
          held_presence_record_conflict = "Not used · presence records conflict",
          held_metric_invalid = "Not used · required measurement unavailable",
          held_identity_conflict = "Not used · record identity conflict",
          held_opportunity_source_missing = "Not used · missing sampling record",
          held_snapshot_event_mismatch = "Not used · visit does not match snapshot"
        )
        out <- unname(labels[value])
        out[is.na(out)] <- "Not used · comparison unavailable"
        out
      }
      ifelse(
        source_gap, "Not used · missing sampling record",
        ifelse(
          comparable & basal_unaligned, "Measured · stems not aligned for change",
          ifelse(
            comparable & measurement_unaligned, "Measured · measurement point changed",
          ifelse(comparable, "Comparable for change",
          ifelse(
            is.na(support) | !nzchar(support), "Not used · sampling context unclear",
            held_label(support)
          )
          )
          )
        )
      )
    }
    # honest size tier: by within-species percentile if the cohort is big enough,
    # else by absolute size (thresholds differ by paradigm).
    tier_hi <- if (identical(SZ$type, "shrubland")) 10 else 60
    tier_mid <- if (identical(SZ$type, "shrubland")) 3 else 25
    tier_names <- c("Upper size tier", "Middle size tier", "Lower size tier")
    tier <- if (!is.na(pct)) { if (pct >= 90) tier_names[1] else if (pct >= 50) tier_names[2] else tier_names[3] }
            else if (is.finite(d_now) && d_now >= tier_hi) tier_names[1]
            else if (is.finite(d_now) && d_now >= tier_mid) tier_names[2] else tier_names[3]
    tier_col <- stats::setNames(c("#1c4d2c", "#2f7d46", "#5aa46a"), tier_names)[[tier]]
    sparkid <- paste0("spark_", gsub("[^A-Za-z0-9]", "", key))
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
          tags$thead(tags$tr(lapply(c("Date", sprintf("%s (cm)", SZ$size_lab), "Height (m)", "Status", "Evidence"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(hist)), function(i) tags$tr(
            tags$td(ifelse(is.na(hist$date[i]), "—", format(hist$date[i], "%Y-%m-%d"))), tags$td(fnum(dvals[i])),
            tags$td(fnum(hist$height[i])), tags$td(ifelse(is.na(hist$plantStatus[i]),"—",hist$plantStatus[i])),
            tags$td(span(
              class = if (identical(hist_evidence[[i]], "Comparable for change"))
                "evidence-chip comparable" else "evidence-chip held",
              hist_evidence[[i]]
            ))))))))
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
        tile(if (!n_visits) "—" else sprintf("%d/%d", n_comparable_events, n_visits),
             "comparable / recorded"),
        tile(ifelse(is.na(row$canopyPosition), "—", gsub(" .*","",row$canopyPosition)), "canopy")),
      div(class = "qc-section-h", bs_icon("graph-up"), sprintf(" Recorded diameter by event (%s)", SZ$size_full)),
      if (!is.null(trajectory) && nrow(trajectory$per_date) >= 2)
        tagList(
          div(class = "sizelab-toolbar", style = "margin-bottom:4px",
            tags$button(class = "smt-snap-btn", type = "button", onclick = sprintf("smtSave('treeSparkBox','NEON-VegStructure_<site>_trajectory-%s_<date>.png')", short_tree(id)), bsicons::bs_icon("camera-fill"), " Download (with pins)"),
            tags$button(class = "smt-clear-btn", type = "button", onclick = "smtClearPins('treeSparkBox')", bsicons::bs_icon("eraser-fill"), " Clear pins"),
            tags$span(class = "sizelab-hint", bs_icon("hand-index-thumb"), " tap a point to pin it")),
          div(class = "smt-pinnable", id = "treeSparkBox", plotlyOutput("treeSpark", height = "170px")))
      else p(class = "qc-cap-note",
        if (isTRUE(basal_unaligned))
          "This plant has multiple basal stems in at least one visit. Those measurements still describe current structure, but the stem labels cannot be matched safely through time, so no change line or rate is shown."
        else if (isTRUE(measurement_unaligned))
          "The field measurement point changed between visits. Every measurement remains visible below, but they are not treated as like-for-like, so no change line or rate is shown."
        else
          "No like-for-like multi-visit change line is available. The preserved measurements and their evidence state remain listed below."),
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
          tcstat(if (!n_visits) "—" else n_visits, "events")),
        div(class = "tc-foot", span(class = "tc-foot-app", "Vegetation Structure"),
            span(if (is.na(pct)) "" else paste0(pct, "%ile for species")))),
      div(class = "tc-toolbar",
        tags$button(class = "tc-save-btn", type = "button", onclick = "smtSaveTreeCard()",
                    bsicons::bs_icon("download"), " Save card (PNG)"),
        tags$span(class = "tc-hint", "A shareable tree card, downloads as a PNG")))
    div(tcard, body, div(class = "qc-toolbar",
      tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveQcCard()", bsicons::bs_icon("download"), " Save QC record (PNG)"),
      downloadButton("treeCsv", "Download tree data (CSV)", class = "smt-clear-btn")))
  }
  # ONE fixed output (not a per-tree id) — avoids accumulating a new binding for
  # every tree the user opens; recomputed on rv$tree change.
  output$treeSpark <- renderPlotly({
    key <- rv$tree; req(key); sp <- SP(); dcol <- if (sp$col %in% names(rv$trees)) sp$col else "stemDiameter"
    career <- plant_rows(rv$trees, key); req(nrow(career) > 0)
    id <- as.character(career$individualID[[1]])
    # Plot the per-visit WHOLE-PLANT girth (the same D_eq the cm/yr stat uses), not
    # raw per-stem rows — otherwise a multi-stem shrub's line wanders/falls while
    # the plant is actually growing (the +cm/yr stat and the line must agree).
    tr <- tree_trajectory(career, id, dcol, rv$plots)
    if (is.null(tr) || nrow(tr$per_date) < 2) return(note_plot("—"))
    pd <- tr$per_date; multi <- any(pd$n_stems > 1)
    pd$lab <- ifelse(pd$n_stems > 1,
      sprintf("%.1f cm whole-plant (%d stems)", pd$dbh, pd$n_stems), sprintf("%.1f cm", pd$dbh))
    short <- short_tree(id)
    # per-visit pin-card HTML (unique key = plant + visit year so each measurement
    # bout pins as its own card). No "open career" chip — this IS the open plant.
    pd$tip <- paste0(
      "<span class='smt-pin-emoji'>", sp$emoji, "</span> <b>", short, "</b><br/>",
      "<span class='smt-pin-stats'>", format(pd$date, "%Y-%m-%d"), " · ", pd$lab, "</span>",
      "<span style='display:none' data-tag='", short, "_", format(pd$date, "%Y%m%d"), "'></span>")
    p <- plot_ly()
    if (multi) p <- p %>% plotly::add_markers(data = tr$raw, x = ~date, y = ~d,
      marker = list(color = DDL$green2, size = 5, opacity = 0.3),
      hovertemplate = "%{x|%Y}<br>one stem %{y:.1f} cm<extra></extra>", showlegend = FALSE)
    p <- p %>% plotly::add_trace(data = pd, x = ~date, y = ~dbh, type = "scatter", mode = "lines+markers",
      customdata = ~tip,
      line = list(color = DDL$green, width = 2.5), marker = list(color = DDL$green2, size = 7),
      text = ~lab, hovertemplate = "%{x|%Y}<br>%{text}<extra></extra>", showlegend = FALSE)
    p %>% plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = ""),
        yaxis = list(title = paste0(sp$size_lab, " (cm)", if (multi) " · whole-plant" else "")),
        margin = list(l = 45, r = 10, t = 10, b = 30))
  })
  output$treeProfile <- renderUI({
    sp <- SP()
    if (is.null(rv$tree)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", sp$emoji), h4(sprintf("Pick a %s to open its career", sp$noun)),
      p("Use the Size Lab (tap a dot → “Open career”) or the plant picker above.")))
    div(class = "plot-profile-wrap", tree_card_ui(rv$tree))
  })
  output$treeCsv <- downloadHandler(
    filename = function() {
      d <- plant_rows(rv$trees, rv$tree)
      id <- if (nrow(d)) d$individualID[[1]] else "tree"
      sprintf("NEON-VegStructure_%s_tree-%s_%s.csv", rv$site %||% "site", short_tree(id), format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      d <- tidy_trees_export(plant_rows(rv$trees, rv$tree), rv$meta); req(!is.null(d), nrow(d) > 0)
      utils::write.csv(d, file, row.names = FALSE, na = "") }, contentType = "text/csv")

  active_plots_export <- reactive({
    req(rv$snap, rv$plots, rv$meta)
    plots_export(rv$snap, rv$plots, SP(), rv$meta)
  })

  # A direct, reviewable plot-level export for the active physical channel.
  # The full ZIP carries the same frame as plot_summary_latest.csv; keeping this
  # standalone path avoids making nontechnical users unpack an archive first.
  output$plotSummaryCsv <- downloadHandler(
    filename = function() sprintf(
      "NEON-VegStructure_%s_%s_plot-summary_%s.csv",
      rv$site %||% "site", SP()$channel, format(Sys.Date(), "%Y%m%d")
    ),
    content = function(file) {
      plots <- active_plots_export()
      req(!is.null(plots), nrow(plots) > 0)
      utils::write.csv(plots, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # ---- FULL-SITE DATA EXPORT (tidy CSVs + codebook, zipped) ---------------
  output$allDataZip <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_data_%s.zip", rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    contentType = "application/zip",
    content = function(file) {
      req(rv$trees)
      tmp <- tempfile("vstexport"); dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
      site <- rv$site %||% "site"; sp <- SP()
      tl <- tidy_trees_export(rv$trees, rv$meta)
      pl <- active_plots_export()
      contexts <- with_export_receipt(as.data.frame(rv$plots, stringsAsFactors = FALSE), rv$meta)
      opportunity_source <- with_export_receipt(
        as.data.frame(rv$bundle$opportunity_source, stringsAsFactors = FALSE), rv$meta
      )
      exports <- list(trees_long = tl, plot_summary_latest = pl,
                      plot_event_contexts_all = contexts,
                      plot_opportunity_source = opportunity_source)
      cb <- complete_veg_codebook(veg_codebook(), exports)
      assert_veg_codebook(cb, exports)
      q <- tree_qc_site(rv$trees, sp, rv$plots)
      qr <- with_export_receipt(if (is.null(q)) data.frame() else q$report, rv$meta)
      qcb <- qc_dictionary(qr)
      assert_qc_dictionary(qcb, qr)
      st <- stand_site(rv$snap, rv$plots, sp)
      scope <- if (identical(sp$type, "shrubland")) "live shrubs and saplings by basal stem diameter over totalSampledAreaShrubSapling" else "live trees >=10 cm DBH over totalSampledAreaTrees"
      readme <- c(
        sprintf("NEON Vegetation Structure Explorer: data export for site %s (%s)", site, sp$channel_label),
        sprintf("Generated %s by an unofficial Desert Data Labs explorer.", format(Sys.Date(), "%Y-%m-%d")),
        sprintf("Metric contract: %s", rv$meta$contract_id %||% "unverified / legacy HOLD"),
        sprintf("Source release: %s", rv$meta$release %||% "unverified / legacy HOLD"),
        sprintf("Raw source SHA-256: %s", rv$meta$source_receipt$raw_source_digest %||% "unverified / legacy HOLD"),
        "Source: NEON Vegetation structure DP1.10098.001 (vst_mappingandtagging + vst_apparentindividual + vst_perplotperyear), DOI https://doi.org/10.48443/pypa-qf12.",
        "License: NEON DP1.10098.001, CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); aggregated and derived by this app.",
        "",
        "FILES",
        " trees_long.csv  · every preserved apparent-individual source row; source_record_key is the published uid, while protocol_stem_key exposes plot-scoped eventID + individualID + tempStemID conflicts.",
        " plot_summary_latest.csv · one canonical latest plot-event summary for the active channel, including support state and derived values.",
        " plot_event_contexts_all.csv · one deterministic row per published opportunity key plus measurement-only audit contexts; source-missing rows invent no effort, absence, date, or area and are always held.",
        " plot_opportunity_source.csv · every published vst_perplotperyear source row, including duplicate plot-event keys retained for audit.",
        " data_dictionary.csv · column definitions, types, units.",
        " qc_report.csv   · every data-quality flag; join plotID + individualID to trees_long.",
        " qc_dictionary.csv · column definitions for qc_report.csv.",
        "",
        "NOTES",
        sprintf(" * Active presentation channel: %s; plants are sized by %s.", sp$channel_label, sp$size_full),
        " * Snapshot analyses use each plot + plant's latest supported event; the long table retains every event/stem row.",
        sprintf(" * Plot metrics scope to %s and are not a wall-to-wall inventory.", scope),
        " * Sampled absence is zero. Sampling-impractical, dendrometer-only, invalid-area, invalid-required-metric, identity-conflict, and missing-opportunity-source states are held/NA with reasons.",
        " * Tower and distributed designs remain labeled; pooled summaries are descriptive, not certified site-wide estimators.",
        if (!is.null(st)) sprintf(" * Supported sampled-plot mean: %s m2/ha (+/-%s plot SE), %s stems/ha, stem-weighted QMD %s cm, n=%d plots.",
                                  fmt_num(st$ba_ha), fmt_num(st$ba_se), fmt_count(st$density_ha), fmt_num(st$qmd), st$n_plots) else "")
      if (!is.null(tl)) utils::write.csv(tl, file.path(tmp, "trees_long.csv"), row.names = FALSE, na = "")
      if (!is.null(pl)) utils::write.csv(pl, file.path(tmp, "plot_summary_latest.csv"), row.names = FALSE, na = "")
      utils::write.csv(contexts, file.path(tmp, "plot_event_contexts_all.csv"), row.names = FALSE, na = "")
      utils::write.csv(opportunity_source, file.path(tmp, "plot_opportunity_source.csv"), row.names = FALSE, na = "")
      utils::write.csv(cb, file.path(tmp, "data_dictionary.csv"), row.names = FALSE, na = "")
      # QC rows carry the full plant key and exact release receipt so they join
      # back to trees_long; the dictionary is checked against the emitted frame.
      utils::write.csv(qr, file.path(tmp, "qc_report.csv"), row.names = FALSE, na = "")
      utils::write.csv(qcb, file.path(tmp, "qc_dictionary.csv"), row.names = FALSE, na = "")
      writeLines(readme, file.path(tmp, "README.txt"))
      fs <- list.files(tmp, full.names = TRUE)
      old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = basename(fs), flags = "-q")
    })

  # A compact, direct audit path for the site-wide notice above. This ledger is
  # one row per measurement-only plot visit and counts every apparent-individual
  # row at that key, regardless of growth form. The full site ZIP remains the
  # route to the exact measurement rows themselves.
  source_gap_ledger <- reactive({
    req(rv$plots, rv$trees, rv$meta, rv$site)
    contexts <- as.data.frame(rv$plots, stringsAsFactors = FALSE)
    keep <- contexts$opportunity_source_missing %in% TRUE
    contexts <- contexts[keep, , drop = FALSE]
    if (!nrow(contexts)) return(data.frame())
    context_key <- paste(as.character(contexts$plotID), as.character(contexts$eventID), sep = "\r")
    measurement_key <- paste(as.character(rv$trees$plotID), as.character(rv$trees$eventID), sep = "\r")
    measurement_n <- as.integer(table(measurement_key)[context_key])
    measurement_n[is.na(measurement_n)] <- 0L
    field <- function(name, default = NA_character_) {
      if (name %in% names(contexts)) contexts[[name]] else rep(default, nrow(contexts))
    }
    ledger <- data.frame(
      plotID = as.character(contexts$plotID),
      eventID = as.character(contexts$eventID),
      measurement_records_all_growth_forms = measurement_n,
      opportunity_source_missing = TRUE,
      sampling_effort_known = FALSE,
      sampled_area_known = FALSE,
      absence_inferred = FALSE,
      tree_support = as.character(field("tree_support")),
      tree_support_reason = as.character(field("tree_support_reason")),
      shrub_support = as.character(field("shrub_support")),
      shrub_support_reason = as.character(field("shrub_support_reason")),
      stringsAsFactors = FALSE
    )
    ledger <- ledger[order(ledger$plotID, ledger$eventID), , drop = FALSE]
    with_export_receipt(ledger, rv$meta)
  })
  output$sourceGapCsv <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_source-gap-ledger_%s.csv",
      rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      ledger <- source_gap_ledger()
      req(nrow(ledger) > 0L)
      utils::write.csv(ledger, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv")

  # ---- STAND REPORT PDF ---------------------------------------------------
  output$reportPdf <- downloadHandler(
    filename = function() sprintf("NEON-VegStructure_%s_sampled-plot-brief_%s.pdf", rv$site %||% "site", format(Sys.Date(), "%Y%m%d")),
    contentType = "application/pdf",
    content = function(file) {
      req(rv$snap)
      build_stand_report(file, snap = rv$snap, trees = rv$trees, plots = rv$plots,
                         one = rv$one, label = rv$label %||% rv$site %||% "site", spec = SP(), meta = rv$meta)
    })

  # ---- MAP ----------------------------------------------------------------
  output$map <- leaflet::renderLeaflet({
    lb <- rv$lb
    if (is.null(lb) || !nrow(lb)) {
      ctr <- if (!is.null(rv$plots) && nrow(rv$plots))
        c(stats::median(rv$plots$lng, na.rm = TRUE), stats::median(rv$plots$lat, na.rm = TRUE)) else c(-98, 39)
      return(leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(ctr[1], ctr[2], zoom = if (all(is.finite(ctr))) 9 else 4) %>%
        leaflet::addControl("No supported, matched plot-event summary is available to map. This is held—not zero.", position = "topright"))
    }
    metric <- input$mapMetric %||% "ba_ha"
    if (!metric %in% names(lb)) metric <- "ba_ha"
    val <- lb[[metric]]                                  # KEEP NA (do not coerce to 0)
    fin <- val[is.finite(val)]
    grey_na <- if (is_dark()) "#5a6a82" else "#cfd6dd"   # NA plots render distinct grey
    # Heavy-tailed stand quantities (ba_ha, density_ha) get a quantile colour scale
    # so one big plot can't wash the rest out; bounded richness stays linear.
    if (length(unique(fin)) >= 5 && metric %in% c("ba_ha", "density_ha")) {
      pal <- leaflet::colorQuantile("viridis", domain = fin, n = 5, na.color = grey_na)
    } else {
      dom <- if (length(fin) && diff(range(fin)) > 0) range(fin) else c(0, 1)
      pal <- leaflet::colorNumeric("viridis", domain = dom, na.color = grey_na)
    }
    # Radius keyed to ba_ha on the SAME log1p channel as the picker map (so size
    # and colour move together and no single big plot dominates the radius range).
    lb$radius <- picker_radius(lb$ba_ha)
    lb$ba_label <- ifelse(is.finite(lb$ba_ha), sprintf("%.1f", lb$ba_ha), "held / unsupported")
    lb$density_label <- ifelse(is.finite(lb$density_ha), format(round(lb$density_ha), big.mark = ",", scientific = FALSE), "held")
    lb$taxa_label <- ifelse(is.finite(lb$n_taxa), format(round(lb$n_taxa), big.mark = ",", scientific = FALSE), "—")
    leaflet::leaflet(lb) %>% leaflet::addProviderTiles(input$view %||% "CartoDB.Positron") %>%
      leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~radius, fillColor = ~pal(val),
        color = "#fff", weight = 1, fillOpacity = 0.85, layerId = ~plotID,
        label = ~lapply(sprintf("<b>%s</b><br>%s m²/ha · %s stems/ha · %s recorded taxa", short_plot(plotID),
          ba_label, density_label, taxa_label), htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = fin,
        title = switch(metric, ba_ha = "measured m²/ha", density_ha = "stems/ha", "recorded taxa"), na.label = "held / unsupported")
  })

  # ---- ABOUT --------------------------------------------------------------
  output$aboutPanel <- renderUI({
    div(class = "about-wrap",
      div(class = "about-card", h4("\U0001F333 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Vegetation structure"), " product (", tags$code("DP1.10098.001"),
          ") across ", tags$b("42 bundled sites"), ". NEON tags woody plants, maps them, and revisits their ", tags$b("diameter, height, and status"), ". This app turns those field records into an explorable sampled-plot story while keeping the original keys and support states visible.")),
      div(class = "about-card", h4(bs_icon("rulers"), " Two measurement channels"),
        p("Full-plot tree records use ", tags$b("DBH"), " (diameter at breast height) with the tree sampled area. Nested shrub and sapling records use basal diameter with their separate shrub/sapling sampled area. Small-tree DBH records stay in the preserved download but are intentionally withheld from summaries until a dedicated nested-area DBH channel is registered. The app never ranks unlike physical channels as though they were one quantity."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Sampled absence is a real zero. Sampling-impractical, dendrometer-only, invalid-area, invalid-required-diameter, and unmatched records are held as unsupported—not converted to zero. Per-hectare summaries are means across supported sampled plots, not wall-to-wall site estimates. QMD is stem-weighted.")),
      div(class = "about-card", h4(bs_icon("graph-up"), " Growth & status"),
        p("Comparable diameter increments require the same plot + plant identity, stable event order, and an unchanged measurement point. Multi-stem basal records whose temporary stem labels cannot be aligned are not forced into a growth rate. Lost/unknown fates are censored from mortality."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Size distributions are descriptive snapshots; they do not establish recruitment or regeneration. Above-ground biomass is deliberately ", tags$b("not"), " estimated.")),
      div(class = "about-card", h4(bs_icon("diagram-3"), " Part of the suite"),
        p("One doorway in the unofficial NEON Explorer Suite. This app keeps tree DBH and shrub/sapling basal measurements in their own supported physical channels; ",
          tags$a(href = "https://tgilbert14.github.io/NEON-Driver-Cascade/", target = "_blank", rel = "noopener", "Driver Cascade"),
          " links the full suite."),
        p(bs_icon("envelope"), " ", tags$a(href = "mailto:desertdatalabs@gmail.com", "desertdatalabs@gmail.com"),
          " · ", tags$a(href = "https://data.neonscience.org/data-products/DP1.10098.001", target = "_blank", "NEON data product"),
          " · ", tags$a(href = "https://doi.org/10.48443/pypa-qf12", target = "_blank", "RELEASE-2026 DOI"),
          " · contract ", tags$code(rv$meta$contract_id %||% "unverified / legacy HOLD"))),
      div(class = "about-card", h4(bs_icon("award"), " Data attribution & license"),
        p(class = "caveat",
          "Built with data from the National Ecological Observatory Network (NEON), a U.S. National Science Foundation program operated by Battelle. NEON data are provided under a Creative Commons Attribution 4.0 International (CC BY 4.0) license (",
          tags$a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", "creativecommons.org/licenses/by/4.0"),
          "). This app aggregates and derives summary metrics from the raw NEON data products; the underlying measurements are unaltered. It is an independent, unofficial tool and is not endorsed by NEON, Battelle, or the NSF.")))
  })

  # ---- clickable hero stats -> ranked-breakdown modals -------------------
  observeEvent(input$heroClick, {
    sp <- SP()
    if (identical(input$heroClick, "species")) {
      ss <- species_structure(rv$snap, rv$plots, sp); req(!is.null(ss), nrow(ss) > 0)
      tot <- sum(ss$ba_m2_ha, na.rm = TRUE)
      items <- lapply(seq_len(min(20, nrow(ss))), function(i) tags$li(class = "rank-row",
        span(class = paste("rank-num", if (i <= 3) "top"), i),
        span(class = "rank-name", em(ifelse(is.na(ss$scientificName[i]), "—", ss$scientificName[i]))),
        span(class = "rank-metric", sprintf("%.1f m²/ha", ss$ba_m2_ha[i])),
        span(class = "rank-sub", sprintf("%s%% · %s stem records", round(100 * ss$ba_m2_ha[i] / tot), ss$stems[i]))))
      showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("bar-chart-steps"), " Species by measured area"),
        div(class = "rank-modal-sub", sprintf("Live %s, ranked within this supported physical channel. This is contribution to the sampled measurement, not proof of ecological dominance.", sp$nouns)),
        tags$ul(class = "rank-list", items), footer = modalButton("Close")))
    } else if (identical(input$heroClick, "biggest")) {
      one <- rv$one; req(one); d <- woody_only(one[is.finite(one[[sp$col]]), ], sp); req(nrow(d) > 0)
      d <- d[order(-d[[sp$col]]), ][seq_len(min(20, nrow(d))), ]
      items <- lapply(seq_len(nrow(d)), function(i) tags$li(class = "rank-row rank-click",
        role = "button", tabindex = "0",
        onclick = sprintf("Shiny.setInputValue('rankPick','%s',{priority:'event'})", plant_key_vec(d)[i]),
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
      g <- tree_growth(rv$trees, sp, rv$plots); if (is.null(g) || !nrow(g)) return(NULL)
      g <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5 & !g$mh_change, , drop = FALSE]
      if (!nrow(g)) return(NULL); g <- g[order(-g$growth_cm_yr), ]
      data.frame(id = growth_key_vec(g), tree = short_tree(g$individualID), species = g$scientificName,
                 value = g$growth_cm_yr, unit = "cm/yr", stringsAsFactors = FALSE)
    } else if (identical(metric, "career")) {
      history <- .supported_history(rv$trees, rv$plots, sp)
      if (is.null(history) || !nrow(history)) return(NULL)
      history <- history[plant_key_vec(history) %in% plant_key_vec(rv$one), , drop = FALSE]
      if ("permanent" %in% names(history)) history <- history[history$permanent %in% TRUE, , drop = FALSE]
      if (!nrow(history)) return(NULL)
      b <- history %>% dplyr::group_by(.data$plotID, .data$individualID) %>%
        dplyr::summarise(events = dplyr::n_distinct(.data$eventID),
                         yrs = round(as.numeric(max(.data$date) - min(.data$date)) / 365.25, 1),
                         species = dplyr::first(.data$scientificName), .groups = "drop")
      b <- b[order(-b$events, -b$yrs), ]
      data.frame(id = plant_key_vec(b), tree = short_tree(b$individualID), species = b$species,
                 value = b$events, unit = "events", stringsAsFactors = FALSE)
    } else {
      col <- if (identical(metric, "tallest")) "height" else sp$col
      d <- one[is.finite(one[[col]]), ]; if (identical(metric, "biggest")) d <- woody_only(d, sp)
      if (!nrow(d)) return(NULL); d <- d[order(-d[[col]]), ]
      data.frame(id = plant_key_vec(d), tree = short_tree(d$individualID), species = d$scientificName,
                 value = round(d[[col]], 1), unit = if (identical(metric, "tallest")) "m" else paste0("cm ", sp$size_lab),
                 stringsAsFactors = FALSE)
    }
  }
  output$famePodium <- renderUI({
    df <- champion_df(input$fameMetric %||% "biggest"); if (is.null(df) || !nrow(df)) return(NULL)
    top <- utils::head(df, 3); medals <- c("\U0001F947", "\U0001F948", "\U0001F949")
    cls <- c("podium-1", "podium-2", "podium-3"); cols <- c(DDL$gold, DDL$muted, DDL$bark)
    cards <- lapply(c(2, 1, 3), function(k) { if (k > nrow(top)) return(NULL); r <- top[k, ]
      tags$button(type = "button", class = paste("podium-card", cls[k]), style = sprintf("--rc:%s", cols[k]),
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
  # ---- quick-pick chips (Biggest / Tallest / Fastest) --------------------
  observeEvent(input$pickBiggest, { df <- champion_df("biggest"); if (!is.null(df) && nrow(df)) pick_tree(df$id[1], navigate = TRUE) })
  observeEvent(input$pickTallest, { df <- champion_df("tallest"); if (!is.null(df) && nrow(df)) pick_tree(df$id[1], navigate = TRUE) })
  observeEvent(input$pickFastest, { df <- champion_df("fastest"); if (!is.null(df) && nrow(df)) pick_tree(df$id[1], navigate = TRUE) })

  # ---- COMPARE TWO STANDS ------------------------------------------------
  compare_stats <- function(site) {
    b <- load_site_bundle(site); if (is.null(b)) return(NULL)
    contract_check <- bundle_contract_check(b, expected_site = site)
    if (!isTRUE(contract_check$ok) ||
        !identical(.veg_scalar_chr(b$contract$index$site$support_status), "supported_sampled_context") ||
        !(.veg_scalar_chr(b$meta$primary_channel) %in% c("tree_dbh", "shrub_sapling_basal"))) return(NULL)
    spc <- size_spec(b$meta$primary_channel)
    snap <- tree_snapshot(b$trees, b$plots, spc)
    st <- stand_site(snap, b$plots, spc)
    active <- woody_only(live_only(snap), spc)
    one <- one_per_tree(active, spc); woody_sp <- species_level_only(one)
    list(site = site, st = st, channel = spc$channel, channel_label = spc$channel_label, size_lab = spc$size_lab,
         n_species = dplyr::n_distinct(woody_sp$scientificName),
         tallest = round(smax(active$height), 1),
         biggest = round(smax(active[[spc$col]]), 1))
  }
  observeEvent(input$compareBtn, {
    sites <- stats::setNames(site_table$site, sprintf("%s · %s", site_table$site, site_table$name))
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
    mixed <- !identical(a$channel, b$channel)
    rowf <- function(lab, va, vb, digits = 1, comparable = TRUE) {
      na <- suppressWarnings(as.numeric(va)); nb <- suppressWarnings(as.numeric(vb))
      # Suppress the winner-highlight on diameter-based rows across the paradigm fork:
      # forest DBH basal area is bole stocking at breast height, shrubland is basal
      # cover at the base — a ~500x measurement-height artifact, not a real "more".
      # Omitting the highlight (vs adding a caveat per row) keeps the table clean.
      wa <- comparable && is.finite(na) && is.finite(nb) && na > nb
      wb <- comparable && is.finite(na) && is.finite(nb) && nb > na
      tags$tr(tags$td(class = "cmp-lab", lab),
        tags$td(class = paste("cmp-val", if (wa) "cmp-win"), fmt_num(va, digits, big = digits == 0)),
        tags$td(class = paste("cmp-val", if (wb) "cmp-win"), fmt_num(vb, digits, big = digits == 0)))
    }
    sizelab <- if (mixed) "stem ø" else a$size_lab
    cmp <- !mixed   # diameter-based rows are only comparable within one paradigm
    tbl <- tags$table(class = "compare-table",
      tags$thead(tags$tr(tags$th(""),
        tags$th(div(class = "cmp-head", a$site, tags$small(sprintf(" · %s", a$channel_label)))),
        tags$th(div(class = "cmp-head", b$site, tags$small(sprintf(" · %s", b$channel_label)))))),
      tags$tbody(
        rowf("Measured cross-sectional area (m²/ha)", a$st$ba_ha, b$st$ba_ha, comparable = cmp),
        rowf("Stem density (/ha)", a$st$density_ha, b$st$density_ha, digits = 0, comparable = cmp),
        rowf(sprintf("Quadratic mean %s (cm)", sizelab), a$st$qmd, b$st$qmd, comparable = cmp),
        rowf("Observed species", a$n_species, b$n_species, digits = 0, comparable = FALSE),
        rowf("Tallest measured (m)", a$tallest, b$tallest, comparable = FALSE),
        rowf(sprintf("Biggest %s (cm)", sizelab), a$biggest, b$biggest, comparable = cmp),
        rowf("Plots sampled", a$st$n_plots, b$st$n_plots, digits = 0)))
    div(tbl, div(class = "compare-foot", bs_icon("info-circle"),
      if (mixed) " The sites use different physical channels and sampled areas, so diameter, area, and density rows are context—not a ranking. " else " ",
      "Summaries cover supported sampled plots; observed species and record sizes also vary with sampling effort. No row is a wall-to-wall site inventory."))
  })

  # ---- guided tour (on demand) -------------------------------------------
  observeEvent(input$tourBtn, session$sendCustomMessage("startTour", list()))

  # ---- SEARCH THE NETWORK -------------------------------------------------
  # Filters the small bundled SEARCH_INDEX in memory (no fetch). Two modes:
  # (a) find a species -> supported sampled-channel records + the per-site measure; (b) a
  # size-threshold query over the reused site_index. Both jump to the picked
  # site through the shared load_site() path, landing on the Overview.
  SI_TAXA  <- if (!is.null(SEARCH_INDEX)) SEARCH_INDEX$taxa else NULL
  if (!is.null(SI_TAXA)) {
    if (!"is_species" %in% names(SI_TAXA)) SI_TAXA <- NULL
    else SI_TAXA <- SI_TAXA[SI_TAXA$is_species %in% TRUE, , drop = FALSE]
  }
  SI_SITES <- if (!is.null(SEARCH_INDEX) && !is.null(SEARCH_INDEX$sites)) SEARCH_INDEX$sites else SITE_INDEX
  SI_CHANNEL_SITES <- if (!is.null(SEARCH_INDEX) &&
                          is.data.frame(SEARCH_INDEX$channel_sites)) {
    SEARCH_INDEX$channel_sites
  } else NULL
  site_name_of <- function(s) { r <- site_table[match(s, site_table$site), ]; ifelse(is.na(r$name), s, r$name) }

  # populate the species autocomplete once the session is up (server-side for speed)
  observe({
    if (is.null(SI_TAXA) || !nrow(SI_TAXA)) return()
    sp <- sort(unique(SI_TAXA$scientificName))
    updateSelectizeInput(session, "taxonPick", choices = sp, server = TRUE,
                         selected = isolate(input$taxonPick) %||% "")
  })

  taxon_hits <- reactive({
    req(input$taxonPick, input$taxonPick != "")
    if (is.null(SI_TAXA)) return(NULL)
    h <- SI_TAXA[SI_TAXA$scientificName == input$taxonPick, , drop = FALSE]
    if (!nrow(h)) return(h)
    # Keep unlike physical channels adjacent but never rank DBH area against
    # basal-cover area. Users can inspect each row and open the source site.
    h[order(h$channel, h$site), , drop = FALSE]
  })

  output$taxonCount <- renderUI({
    if (is.null(SI_TAXA)) return(div(class = "search-empty", bs_icon("exclamation-triangle"), " The search index isn't bundled in this build."))
    if (is.null(input$taxonPick) || input$taxonPick == "")
      return(div(class = "search-empty", bs_icon("search"), " Pick a species above to see where it was recorded in a supported sampled channel."))
    h <- taxon_hits(); n <- nrow(SI_SITES %||% data.frame())
    div(class = "search-count",
      tags$b(sprintf("%d", dplyr::n_distinct(h$site))), sprintf(" of %d sites · %d channel rows", n, nrow(h)),
      tags$span(class = "sc-taxon", sprintf(" · %s", input$taxonPick)))
  })

  output$taxonHits <- DT::renderDT({
    h <- taxon_hits(); req(!is.null(h))
    if (!nrow(h)) return(DT::datatable(data.frame(Message = "Not recorded at any bundled site."),
                                       rownames = FALSE, options = list(dom = "t")))
    df <- data.frame(
      Site = sprintf("%s · %s", h$site, site_name_of(h$site)),
      Channel = ifelse(h$channel == "tree_dbh", "Tree DBH", "Shrub & sapling basal"),
      `Measured area index (m²/ha)` = round(h$ba_m2_ha, 1),
      `Live stem records` = round(h$n_stems),
      `Latest supported plot years` = ifelse(
        is.na(h$year_min), "—",
        ifelse(h$year_min == h$year_max, as.character(h$year_min), sprintf("%d–%d", h$year_min, h$year_max))
      ),
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(df, rownames = FALSE, selection = "single", class = "leader-dt",
      options = list(pageLength = 12, dom = "tp", order = list(list(1, "asc"), list(0, "asc"))))
  })
  observeEvent(input$taxonHits_rows_selected, {
    h <- taxon_hits(); i <- input$taxonHits_rows_selected
    if (!is.null(h) && length(i) && i <= nrow(h)) {
      session$sendCustomMessage("smtLoadStart", list(label = paste0(h$site[i], " · loading…")))
      load_site(h$site[i], requested_channel = h$channel[i])
    }
  })

  # ---- threshold query (one explicit row per site x physical channel) -----
  thresh_col <- function(metric) switch(metric,
    ba_ha = "ba_ha", biggest = "biggest_diam_cm", tallest = "tallest_m", "tallest_m")

  thresh_base <- reactive({
    if (is.null(SI_CHANNEL_SITES) || !nrow(SI_CHANNEL_SITES)) return(NULL)
    d <- SI_CHANNEL_SITES[
      SI_CHANNEL_SITES$support_status == "supported_sampled_context", , drop = FALSE
    ]
    # ba_ha must be emitted by the same canonical builder as the app/export.
    # Never reconstruct a divergent proxy by summing rounded taxon rows.
    if (!"ba_ha" %in% names(d)) d$ba_ha <- NA_real_
    if (!is.null(input$threshType) && input$threshType != "all")
      d <- d[d$channel %in% input$threshType, , drop = FALSE]
    d
  })

  output$threshSliderUI <- renderUI({
    d <- thresh_base(); req(!is.null(d), nrow(d) > 0)
    needs_channel <- (input$threshMetric %||% "ba_ha") %in% c("ba_ha", "biggest")
    if (needs_channel && identical(input$threshType %||% "all", "all"))
      return(div(class = "search-empty", bs_icon("arrow-left-right"),
        " Choose Tree DBH or Shrub & sapling basal before filtering a channel-specific area or diameter measure."))
    col <- thresh_col(input$threshMetric %||% "ba_ha")
    v <- suppressWarnings(as.numeric(d[[col]])); v <- v[is.finite(v)]
    if (!length(v)) return(NULL)
    lo <- floor(min(v)); hi <- ceiling(max(v))
    lab <- switch(input$threshMetric %||% "ba_ha",
      ba_ha = "Min measured area (m²/ha)", biggest = "Min biggest measured stem (cm)", tallest = "Min tallest measured plant (m)")
    sliderInput("threshMin", lab, min = lo, max = hi, value = lo, step = max(1, round((hi - lo) / 50)), width = "260px")
  })

  thresh_hits <- reactive({
    d <- thresh_base(); req(!is.null(d), nrow(d) > 0)
    needs_channel <- (input$threshMetric %||% "ba_ha") %in% c("ba_ha", "biggest")
    if (needs_channel && identical(input$threshType %||% "all", "all"))
      return(d[0, , drop = FALSE])
    col <- thresh_col(input$threshMetric %||% "ba_ha")
    d$.v <- suppressWarnings(as.numeric(d[[col]]))
    mn <- input$threshMin %||% -Inf
    d <- d[is.finite(d$.v) & d$.v >= mn, , drop = FALSE]
    d[order(-d$.v), , drop = FALSE]
  })

  output$threshCount <- renderUI({
    if (is.null(SI_CHANNEL_SITES)) return(div(class = "search-empty", bs_icon("exclamation-triangle"), " The channel search index isn't bundled in this build."))
    d <- thresh_hits(); base <- thresh_base()
    div(class = "search-count", tags$b(sprintf("%d", nrow(d))),
      sprintf(" of %d supported site-channel views", nrow(base)),
      tags$span(class = "sc-taxon",
        if (!is.null(input$threshType) && input$threshType != "all")
          sprintf(" · %s", if (input$threshType == "tree_dbh") "Tree DBH channel" else "Shrub & sapling basal channel") else ""))
  })

  output$threshHits <- DT::renderDT({
    d <- thresh_hits(); req(!is.null(d))
    if (!nrow(d)) return(DT::datatable(data.frame(Message = "No sites pass that threshold."),
                                       rownames = FALSE, options = list(dom = "t")))
    df <- data.frame(
      Site = sprintf("%s · %s", d$site, site_name_of(d$site)),
      Channel = ifelse(d$channel == "tree_dbh", "Tree DBH", "Shrub & sapling basal"),
      `Measured area index (m²/ha)` = round(d$ba_ha, 1),
      `Biggest stem (cm)` = round(d$biggest_diam_cm, 1),
      `Tallest (m)` = round(d$tallest_m, 1),
      Species = d$n_species,
      check.names = FALSE, stringsAsFactors = FALSE)
    ord <- switch(thresh_col(input$threshMetric %||% "ba_ha"),
                  ba_ha = 2, biggest_diam_cm = 3, tallest_m = 4, 4)
    DT::datatable(df, rownames = FALSE, selection = "single", class = "leader-dt",
      options = list(pageLength = 12, dom = "tp", order = list(list(ord, "desc"))))
  })
  observeEvent(input$threshHits_rows_selected, {
    d <- thresh_hits(); i <- input$threshHits_rows_selected
    if (!is.null(d) && length(i) && i <= nrow(d)) {
      session$sendCustomMessage("smtLoadStart", list(label = paste0(d$site[i], " · loading…")))
      load_site(d$site[i], requested_channel = d$channel[i])
    }
  })

  observeEvent(input$help, {
    showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("question-circle"), " How it works"),
      tags$ol(class = "help-steps",
        tags$li(div(class = "help-step-number", "1"), div(
          tags$b("Pick a place"),
          p("Tap a map dot, type a place name, or search the whole network."))),
        tags$li(div(class = "help-step-number", "2"), div(
          tags$b("Choose a story"),
          p("See what crews measured, how comparable measurements changed, or open one tagged plant."))),
        tags$li(div(class = "help-step-number", "3"), div(
          tags$b("Check the evidence"),
          p("Every summary shows its support. Grey or unavailable means “we cannot make that comparison”—never zero plants.")))),
      tags$details(class = "help-methods",
        tags$summary(bs_icon("shield-check"), " How we keep comparisons fair"),
        tags$ul(
          tags$li("Tree diameter and shrub/sapling basal diameter stay in separate measurement views."),
          tags$li("A current snapshot uses the latest supported visit for each sampled plot; the download keeps every original visit and stem key."),
          tags$li("Change uses only like-for-like remeasurements. Changed measuring points and records that cannot be aligned stay out of the rate."),
          tags$li("A confirmed sampled absence is zero. Missing effort, area, identity, or sampling records remain unavailable with a reason."),
          tags$li("Downloads include the preserved records, support ledger, codebook, QC report, and sampled-plot PDF brief."))),
      footer = tagList(tags$button(type = "button", class = "btn btn-outline-dark btn-sm",
        onclick = "(function(){var m=document.querySelector('.modal.show button[data-bs-dismiss=modal],.modal.show .btn-close');if(m)m.click();setTimeout(vegTour,250);})()",
        bsicons::bs_icon("signpost-2"), " Take the tour"), modalButton("Got it"))))
  })
}
