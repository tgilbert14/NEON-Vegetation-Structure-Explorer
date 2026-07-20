# ===========================================================================
# NEON Vegetation Structure Explorer — stand report PDF (base graphics).
# A one-page, printable stand report streamed by output$reportPdf. No LaTeX,
# no Chrome — grDevices::cairo_pdf + base plotting only. Forest-themed from the
# DDL palette. Every section is defensive (draws "—" rather than erroring), so
# it can never break the downloadHandler.
# ===========================================================================

build_stand_report <- function(file, snap, trees, plots, one, label = "site", spec = SIZE_FOREST, meta = NULL) {
  P <- list(pine = "#1f6a63", pine2 = "#164d48", bark = "#8a5a2b", gold = "#E0A500",
            goldink = "#8a6310", ink = "#1d2a24", muted = "#5c6b62", paper = "#fdfcf7",
            line = "#e1ddcf", dead = "#9a5a3a")
  ok <- function(expr) tryCatch(expr, error = function(e) NULL)
  f1 <- function(x) if (length(x) && is.finite(x[[1]])) formatC(x[[1]], format = "f", digits = 1) else "—"
  f0 <- function(x) if (length(x) && is.finite(x[[1]])) format(round(x[[1]]), big.mark = ",", scientific = FALSE) else "—"
  shrub <- identical(spec$type, "shrubland")
  SL <- spec$size_lab

  grDevices::cairo_pdf(file, width = 8.5, height = 11, bg = P$paper)
  on.exit(grDevices::dev.off(), add = TRUE)
  old <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old), add = TRUE)

  graphics::par(family = "", mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 100), ylim = c(0, 100))

  # ---- header band -------------------------------------------------------
  graphics::rect(0, 92, 100, 100, col = P$pine, border = NA)
  graphics::text(3, 96.4, "NEON Vegetation Structure: Sampled-Plot Brief", col = "#ffffff",
                 cex = 1.5, font = 2, adj = 0)
  graphics::text(3, 93.6, label, col = P$gold, cex = 1.0, font = 2, adj = 0)
  graphics::text(97, 93.6, sprintf("%s  ·  DP1.10098.001  ·  %s",
                 format(Sys.Date(), "%Y-%m-%d"), meta$release %||% "unverified / legacy HOLD"),
                 col = "#dfeee4", cex = 0.7, adj = 1)

  st <- ok(stand_site(snap, plots, spec))
  ss <- ok(species_structure(snap, plots, spec))
  sc <- ok(size_class(snap, plots, spec))
  g  <- ok(tree_growth(trees, spec, plots))

  # ---- stand summary strip ----------------------------------------------
  yb <- 84
  chip <- function(x, v, l) {
    graphics::rect(x, yb, x + 22, yb + 6.5, col = "#eef4ea", border = P$line)
    graphics::text(x + 11, yb + 4.4, v, col = P$pine, cex = 1.25, font = 2)
    graphics::text(x + 11, yb + 1.4, l, col = P$muted, cex = 0.62)
  }
  if (!is.null(st)) {
    chip(3,  f1(st$ba_ha), "MEASURED AREA m²/ha")
    chip(27, f0(st$density_ha), "STEMS / ha")
    chip(51, f1(st$qmd), "QMD (cm)")
    chip(75, paste0(st$n_plots), "SUPPORTED PLOTS")
  } else {
    graphics::text(50, yb + 3, "Plot estimate held (unsupported / unmatched is not zero).", col = P$muted, cex = 0.9)
  }

  # ---- section: measured cross-sectional contribution (top taxa) --------
  graphics::text(3, 80, "Composition: measured contribution within this channel", col = P$pine2, cex = 1.0, font = 2, adj = 0)
  if (!is.null(ss) && nrow(ss)) {
    topn <- utils::head(ss, 8); topn <- topn[nrow(topn):1, ]
    tot <- sum(ss$ba_m2_ha, na.rm = TRUE); mx <- max(topn$ba_m2_ha, na.rm = TRUE)
    x0 <- 38; x1 <- 92; ytop <- 78; ybot <- 56; n <- nrow(topn)
    yc <- seq(ybot, ytop, length.out = n); bh <- (ytop - ybot) / n * 0.62
    for (i in seq_len(n)) {
      w <- if (is.finite(mx) && mx > 0) (x1 - x0) * topn$ba_m2_ha[i] / mx else 0
      graphics::rect(x0, yc[i] - bh / 2, x0 + w, yc[i] + bh / 2, col = P$pine, border = NA)
      nm <- topn$scientificName[i]; if (is.na(nm)) nm <- "unidentified"
      graphics::text(x0 - 1.5, yc[i], nm, col = P$ink, cex = 0.6, adj = 1, font = 3)
      pc <- if (is.finite(tot) && tot > 0) round(100 * topn$ba_m2_ha[i] / tot) else 0
      graphics::text(x0 + w + 1, yc[i], sprintf("%.1f m²/ha (%d%%)", topn$ba_m2_ha[i], pc),
                     col = P$muted, cex = 0.55, adj = 0)
    }
  } else graphics::text(50, 67, "—", col = P$muted)

  # ---- section: descriptive diameter size-class distribution -------------
  graphics::text(3, 52, if (shrub) "Basal-diameter size-class distribution (live shrubs & saplings)" else "Diameter size-class distribution (live trees ≥ 10 cm DBH)", col = P$pine2, cex = 1.0, font = 2, adj = 0)
  if (!is.null(sc) && nrow(sc)) {
    x0 <- 8; x1 <- 92; ybot <- 30; ytop <- 49
    metric <- if ("stems_ha" %in% names(sc)) sc$stems_ha else sc$stems
    mx <- max(metric, na.rm = TRUE); n <- nrow(sc)
    bw <- (x1 - x0) / n
    for (i in seq_len(n)) {
      h <- if (is.finite(mx) && mx > 0) (ytop - ybot) * metric[i] / mx else 0
      graphics::rect(x0 + (i - 1) * bw + bw * 0.12, ybot, x0 + i * bw - bw * 0.12, ybot + h,
                     col = P$pine, border = NA)
      graphics::text(x0 + (i - 0.5) * bw, ybot - 1.6, as.character(sc$cls[i]), col = P$muted, cex = 0.55)
      graphics::text(x0 + (i - 0.5) * bw, ybot + h + 1.2,
                     if (is.finite(metric[i])) format(round(metric[i]), big.mark = ",", scientific = FALSE) else "—",
                     col = P$ink, cex = 0.55)
    }
    graphics::text(3, ytop + 1.5, paste0("stems/ha by cm ", SL, " class"), col = P$muted, cex = 0.55, adj = 0)
  } else graphics::text(50, 40, "—", col = P$muted)

  # ---- section: champion plants ------------------------------------------
  graphics::text(3, 26, if (shrub) "Champion shrubs" else "Champion trees", col = P$pine2, cex = 1.0, font = 2, adj = 0)
  champs <- ok({
    o <- one[is.finite(one[[spec$col]]), ]
    o <- woody_only(o, spec); o[order(-o[[spec$col]]), ][seq_len(min(5, nrow(o))), ]
  })
  yL <- 23
  if (!is.null(champs) && nrow(champs)) {
    graphics::text(3, yL, paste0("Biggest by ", SL, ":"), col = P$bark, cex = 0.7, font = 2, adj = 0)
    for (i in seq_len(nrow(champs))) {
      graphics::text(3, yL - 1.8 * i,
        sprintf("%d.  %s  ·  %s  ·  %.1f cm %s%s", i, short_tree(champs$individualID[i]),
                ifelse(is.na(champs$scientificName[i]), "—", champs$scientificName[i]),
                champs[[spec$col]][i], SL,
                if (is.finite(champs$height[i])) sprintf("  ·  %.1f m", champs$height[i]) else ""),
        col = P$ink, cex = 0.62, adj = 0)
    }
  }
  fast <- ok({
    gg <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 0 & g$growth_cm_yr <= 5 & !g$mh_change, ]
    gg[order(-gg$growth_cm_yr), ][seq_len(min(5, nrow(gg))), ]
  })
  if (!is.null(fast) && nrow(fast)) {
    graphics::text(52, yL, "Greatest positive diameter changes:", col = P$bark, cex = 0.7, font = 2, adj = 0)
    for (i in seq_len(nrow(fast))) {
      graphics::text(52, yL - 1.8 * i,
        sprintf("%d.  %s  ·  %+.2f cm/yr", i, short_tree(fast$individualID[i]), fast$growth_cm_yr[i]),
        col = P$ink, cex = 0.62, adj = 0)
    }
  }

  # ---- honesty footer ----------------------------------------------------
  graphics::abline(h = 6.5, col = P$line)
  support_line <- if (!is.null(st)) sprintf(
    "Latest supported census: %d plots; %d sampled absences; %d plots without support; %d later held attempts.",
    st$n_plots, st$n_absence %||% 0L, st$n_excluded %||% 0L, st$n_later_held %||% 0L
  ) else "No supported census is available; unavailable is not zero."
  graphics::text(3, 5, support_line,
    col = P$muted, cex = 0.55, adj = 0)
  graphics::text(3, 3.2,
    sprintf("Contract %s · channel %s · absence = 0; held = NA · size shape is descriptive; biomass is not estimated.",
            meta$contract_id %||% "unverified / legacy HOLD", spec$channel),
    col = P$muted, cex = 0.55, adj = 0)
  graphics::text(3, 1.4,
    sprintf("NEON DP1.10098.001 · source SHA-256 %s · unofficial · desertdatalabs@gmail.com",
      substr(meta$source_receipt$raw_source_digest %||% "unverified", 1, 16)),
    col = P$muted, cex = 0.55, adj = 0)
  invisible(TRUE)
}
