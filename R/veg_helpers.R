# ===========================================================================
# NEON Vegetation Structure Explorer — veg_helpers.R
# Individual-grain analyses on NEON Vegetation structure (DP1.10098.001): each
# tagged stem (individualID) is remeasured over years, so a tree has a growth
# career (the woody analog of the mammal capture career). Snapshot metrics use
# the LATEST measurement per tree (never pool bouts — a tree measured 5x would
# count 5x); the growth metric is the explicit multi-bout one. Density/basal-area
# are area-scaled (per ha). Honesty discipline ported from the small-mammal +
# plant siblings. See docs/neonize-playbook.md.
# ===========================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }
smax <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) }   # NA (not -Inf) on empty
smean <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else mean(x) }
short_tree <- function(id) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(id))   # NEON.PLA.D01.000123 -> 000123
short_plot <- function(p) sub("^[A-Z]{4}_", "", as.character(p))

TREE_FORMS <- c("single bole tree", "multi-bole tree", "small tree")  # what counts as a "tree" for density

species_level_only <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  if ("is_species" %in% names(d)) return(d[d$is_species %in% TRUE, , drop = FALSE])
  ok <- is.na(d$taxonRank) | d$taxonRank %in% c("species", "subspecies", "variety", "speciesGroup")
  d[ok, , drop = FALSE]
}
make_species_pal <- function(d) {
  sp <- sort(unique(d$scientificName[!is.na(d$scientificName)]))
  if (!length(sp)) return(character(0))
  stats::setNames(grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(sp)), sp)
}

# ---------------------------------------------------------------------------
# tree_snapshot(): the LATEST bout per individual (current state). A tree can
# have several stem rows in one bout (multi-bole) — all are kept here.
# one_per_tree(): collapse to the single largest stem per individual (the dot in
# the Size Lab + the size headline). Basal area sums ALL snapshot stems.
# ---------------------------------------------------------------------------
tree_snapshot <- function(trees) {
  if (is.null(trees) || !nrow(trees)) return(trees)
  trees %>% dplyr::group_by(.data$individualID) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>% dplyr::ungroup()
}
one_per_tree <- function(snap) {
  if (is.null(snap) || !nrow(snap)) return(snap)
  snap %>% dplyr::group_by(.data$individualID) %>%
    dplyr::slice_max(.data$stemDiameter, n = 1, with_ties = FALSE, na_rm = FALSE) %>% dplyr::ungroup()
}
live_only <- function(d) { if (is.null(d) || !nrow(d)) return(d); d[d$live %in% TRUE, , drop = FALSE] }

# ---------------------------------------------------------------------------
# Stand structure (per ha) — basal area and stem density need the plot's sampled
# tree area (plots$area_trees, m^2). Computed per plot then summarised to the site
# (mean per ha), so a big plot doesn't dominate. Live stems with a real DBH only.
# ---------------------------------------------------------------------------
stand_by_plot <- function(snap, plots) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- live_only(snap)
  s <- s[is.finite(s$stemDiameter) & s$stemDiameter > 0, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  s$ba_m2 <- pi * (s$stemDiameter / 200)^2          # cm DBH -> m^2 basal area per stem
  per <- s %>% dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(stems = dplyr::n_distinct(.data$individualID),
                     ba_m2 = sum(.data$ba_m2),
                     qmd = sqrt(mean(.data$stemDiameter^2)),
                     .groups = "drop")
  per <- dplyr::left_join(per, plots[, c("plotID", "area_trees")], by = "plotID")
  per$area_ha <- per$area_trees / 10000
  per$ba_ha <- ifelse(per$area_ha > 0, per$ba_m2 / per$area_ha, NA_real_)
  per$density_ha <- ifelse(per$area_ha > 0, per$stems / per$area_ha, NA_real_)
  per
}
stand_site <- function(snap, plots) {
  per <- stand_by_plot(snap, plots); if (is.null(per)) return(NULL)
  list(ba_ha = round(mean(per$ba_ha, na.rm = TRUE), 1),
       density_ha = round(mean(per$density_ha, na.rm = TRUE)),
       qmd = round(stats::weighted.mean(per$qmd, per$stems, na.rm = TRUE), 1),
       n_plots = nrow(per))
}

# diameter size-class distribution (live stems) — the classic reverse-J
size_class <- function(snap) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- live_only(snap); s <- s[is.finite(s$stemDiameter) & s$stemDiameter > 0, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  brks <- c(0, 10, 20, 30, 40, 50, 70, Inf)
  labs <- c("0–10", "10–20", "20–30", "30–40", "40–50", "50–70", "70+")
  s$cls <- cut(s$stemDiameter, breaks = brks, labels = labs, right = FALSE)
  s %>% dplyr::count(.data$cls, name = "stems") %>% tidyr::complete(cls = factor(labs, levels = labs), fill = list(stems = 0))
}

# ---------------------------------------------------------------------------
# Per-tree growth: diameter increment between the first and last live bouts,
# annualised. De-pseudoreplicated (one rate per tree). Negative increments
# (shrinkage = measurement error / bole damage) are flagged, not deleted.
# ---------------------------------------------------------------------------
tree_growth <- function(trees) {
  if (is.null(trees) || !nrow(trees)) return(NULL)
  g <- trees[is.finite(trees$stemDiameter) & trees$stemDiameter > 0, , drop = FALSE]
  # only PERMANENT individualIDs are the same plant year-to-year (TEMP.PLA ids are
  # re-issued, so a "growth" across them would be two different stems)
  if (!is.null(g) && "permanent" %in% names(g)) g <- g[g$permanent %in% TRUE, , drop = FALSE]
  if (is.null(g) || !nrow(g)) return(NULL)
  g %>% dplyr::group_by(.data$individualID, .data$scientificName) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::summarise(
      d0 = .data$stemDiameter[which.min(.data$date)], d1 = .data$stemDiameter[which.max(.data$date)],
      yrs = as.numeric(max(.data$date) - min(.data$date)) / 365.25,
      .groups = "drop") %>%
    dplyr::filter(.data$yrs > 0) %>%
    dplyr::mutate(growth_cm_yr = round((.data$d1 - .data$d0) / .data$yrs, 2),
                  shrank = .data$growth_cm_yr < -0.1)
}

# one tree's full measurement history (the growth trajectory on the tree card)
tree_history <- function(trees, id) {
  if (is.null(trees) || is.null(id)) return(NULL)
  h <- trees[trees$individualID == id & !is.na(trees$date), , drop = FALSE]
  if (!nrow(h)) return(NULL)
  keep <- intersect(c("date", "year", "stemDiameter", "height", "plantStatus", "growthForm",
                      "canopyPosition", "measurementHeight", "changedMeasurementLocation"), names(h))
  h[order(h$date), keep, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Per-tree QC flags from its history, ranked. Every flag is "verify", not "wrong"
# — a quarter of remeasurement intervals show diameter decreases that are real
# (bark sloughing, a changed measurement height, drought). Flag, keep, explain.
# ---------------------------------------------------------------------------
tree_qc_flags <- function(hist) {
  flags <- list(); add <- function(level, text) flags[[length(flags) + 1L]] <<- list(level = level, text = text)
  if (is.null(hist) || !nrow(hist)) return(flags)
  if (nrow(hist) < 2) { add("info", "Measured once — no remeasurement yet, so the growth trajectory and its checks don't apply."); return(flags) }
  d <- hist$stemDiameter; dt <- as.numeric(diff(hist$date)) / 365.25
  dd <- diff(d)
  # status went back to Live after dead = impossible (tag/data issue)
  st <- ifelse(grepl("^Live", hist$plantStatus), 1L, ifelse(grepl("[Dd]ead|Downed", hist$plantStatus), 0L, NA))
  sv <- st[!is.na(st)]
  if (length(sv) >= 2 && any(diff(sv) > 0)) add("high", "Status returned to Live after being recorded dead — impossible for one stem; a tagging or data-entry issue.")
  # implausible fast growth
  rate <- ifelse(dt > 0, dd / dt, NA)
  if (any(is.finite(rate) & rate > 5)) add("warn", sprintf("Diameter grew >5 cm/yr between visits (max %.1f cm/yr) — implausibly fast; check for a measurement-height change or a tag mix-up.", max(rate, na.rm = TRUE)))
  # diameter shrank
  if (any(is.finite(dd) & dd < -0.1)) {
    mh_changed <- "measurementHeight" %in% names(hist) && length(unique(stats::na.omit(hist$measurementHeight))) > 1
    add("info", paste0("Diameter decreased between visits — common and often real (bark sloughing, drought shrinkage",
      if (mh_changed) ", and here the measurement height changed between visits, which alone can explain it" else "",
      "). Verify, don't assume an error."))
  }
  flags
}

# ---------------------------------------------------------------------------
# Per-SPECIES structure (composition, leaderboard, the "champion tree").
# ---------------------------------------------------------------------------
species_structure <- function(snap, plots) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- live_only(species_level_only(snap))
  one <- one_per_tree(s)
  if (!nrow(one)) return(NULL)
  ba <- s[is.finite(s$stemDiameter) & s$stemDiameter > 0, ]
  ba$ba_m2 <- pi * (ba$stemDiameter / 200)^2
  ba_by <- ba %>% dplyr::group_by(.data$scientificName) %>% dplyr::summarise(ba_m2 = sum(.data$ba_m2), .groups = "drop")
  one %>% dplyr::group_by(.data$scientificName, .data$family) %>%
    dplyr::summarise(stems = dplyr::n_distinct(.data$individualID),
                     max_dbh = round(smax(.data$stemDiameter), 1),
                     max_ht = round(smax(.data$height), 1),
                     mean_dbh = round(smean(.data$stemDiameter), 1),
                     .groups = "drop") %>%
    dplyr::left_join(ba_by, by = "scientificName") %>%
    dplyr::arrange(dplyr::desc(.data$ba_m2))
}

# ---------------------------------------------------------------------------
# Per-PLOT (stand) summary — feeds the Map + a stand profile.
# ---------------------------------------------------------------------------
plot_summary_veg <- function(snap, plots) {
  per <- stand_by_plot(snap, plots); if (is.null(per)) return(NULL)
  s <- live_only(species_level_only(snap))
  sp <- s %>% dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(n_species = dplyr::n_distinct(.data$scientificName),
                     tallest = round(smax(.data$height), 1),
                     biggest = round(smax(.data$stemDiameter), 1),
                     dominant = .data$scientificName[which.max(.data$stemDiameter)], .groups = "drop")
  out <- per %>% dplyr::left_join(sp, by = "plotID") %>%
    dplyr::left_join(plots[, c("plotID", "plotType", "nlcdClass", "lat", "lng")], by = "plotID")
  out$ba_ha <- round(out$ba_ha, 1); out$density_ha <- round(out$density_ha)
  out %>% dplyr::arrange(dplyr::desc(.data$ba_ha))
}

# live vs dead snapshot composition (an honest crude status ratio, not a rate)
status_summary <- function(snap) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- one_per_tree(snap)
  if (!nrow(s)) return(NULL)
  s$cls <- dplyr::case_when(grepl("^Live", s$plantStatus) ~ "Live",
                            grepl("[Dd]ead|Downed", s$plantStatus) ~ "Dead / standing dead",
                            TRUE ~ "Other / unknown")
  s %>% dplyr::count(.data$cls, name = "n") %>% dplyr::arrange(dplyr::desc(.data$n))
}
