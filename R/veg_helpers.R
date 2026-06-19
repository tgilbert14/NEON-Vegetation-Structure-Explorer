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

TREE_FORMS <- c("single bole tree", "multi-bole tree", "small tree")
# The protocol tree threshold. Stems with DBH >= this are tallied over the full
# plot tree area (totalSampledAreaTrees); SMALLER stems are sampled only in a
# smaller nested subplot, so dividing them by the tree area under-counts them.
# All STAND metrics (basal area, density, size-class, QMD, composition) are
# therefore scoped to trees >= 10 cm DBH for an area-consistent, honest estimate.
TREE_DBH_MIN <- 10
trees_only <- function(d) { if (is.null(d) || !nrow(d)) return(d)
  d[is.finite(d$stemDiameter) & d$stemDiameter >= TREE_DBH_MIN, , drop = FALSE] }

species_level_only <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  if ("is_species" %in% names(d)) return(d[d$is_species %in% TRUE, , drop = FALSE])
  ok <- is.na(d$taxonRank) | d$taxonRank %in% c("species", "subspecies", "variety", "speciesGroup")
  d[ok, , drop = FALSE]
}
# CVD-safe categorical palette tuned to the "Old-Growth Canopy" chrome (Okabe-Ito
# derived + two woodland additions; distinct under deutan/protan vision, and it
# harmonizes with the canopy-green frame instead of fighting it like Dark2 did).
# The focal Size-Lab marker is amber #E6A700 — deliberately NOT in this set, so
# the "tree you're viewing" can never collide with a species hue.
FOREST_CAT <- c("#2f7d46", "#8f5524", "#3f7d8c", "#d4b73a", "#b1542a",
                "#6c4a86", "#4fa3a0", "#c98aa6", "#7a8a4f")
forest_ramp <- function(n) {
  if (n <= length(FOREST_CAT)) FOREST_CAT[seq_len(max(1, n))]
  else grDevices::colorRampPalette(FOREST_CAT)(n)
}
make_species_pal <- function(d) {
  sp <- sort(unique(d$scientificName[!is.na(d$scientificName)]))
  if (!length(sp)) return(character(0))
  stats::setNames(forest_ramp(length(sp)), sp)
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
  s <- trees_only(live_only(snap))                  # live trees >= 10 cm DBH (area-consistent)
  if (is.null(s) || !nrow(s)) return(NULL)
  s$ba_m2 <- pi * (s$stemDiameter / 200)^2          # cm DBH -> m^2 basal area per stem
  per <- s %>% dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(stems = dplyr::n_distinct(.data$individualID),
                     ba_m2 = sum(.data$ba_m2), sumD2 = sum(.data$stemDiameter^2),
                     .groups = "drop")
  per <- dplyr::left_join(per, plots[, c("plotID", "area_trees")], by = "plotID")
  per$area_ha <- per$area_trees / 10000
  per <- per[is.finite(per$area_ha) & per$area_ha > 0.005, , drop = FALSE]   # drop tiny/partial slivers (<50 m²)
  if (!nrow(per)) return(NULL)
  per$ba_ha <- per$ba_m2 / per$area_ha
  per$density_ha <- per$stems / per$area_ha
  per
}
stand_site <- function(snap, plots) {
  per <- stand_by_plot(snap, plots); if (is.null(per)) return(NULL)
  n <- nrow(per)
  se <- function(x) if (n > 1) stats::sd(x, na.rm = TRUE) / sqrt(n) else NA_real_
  list(ba_ha = round(mean(per$ba_ha, na.rm = TRUE), 1),         # plot = sampling unit (equal weight)
       ba_se = round(se(per$ba_ha), 1),                          # SE across plots (n = sampling units)
       density_ha = round(mean(per$density_ha, na.rm = TRUE)),
       density_se = round(se(per$density_ha)),
       qmd = round(sqrt(sum(per$sumD2, na.rm = TRUE) / sum(per$stems, na.rm = TRUE)), 1),  # POOLED RMS, not mean-of-QMDs
       n_plots = n)
}

# diameter size-class distribution of TREES (>= 10 cm DBH, sampled over the full
# tree area — smaller stems are nested-sampled over a smaller area, so including
# them here would under-represent the small classes as a sampling artifact).
size_class <- function(snap, plots = NULL) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- trees_only(live_only(snap))
  if (is.null(s) || !nrow(s)) return(NULL)
  brks <- c(10, 20, 30, 40, 50, 70, Inf)
  labs <- c("10–20", "20–30", "30–40", "40–50", "50–70", "70+")
  s$cls <- cut(s$stemDiameter, breaks = brks, labels = labs, right = FALSE)
  out <- s %>% dplyr::count(.data$cls, name = "stems") %>%
    tidyr::complete(cls = factor(labs, levels = labs), fill = list(stems = 0))
  # area-standardize to stems/ha when plots are supplied (raw counts are
  # sampling-effort-dependent; stems/ha is comparable across sites). Pooled over
  # the sampled tree area of the plots that contribute trees.
  if (!is.null(plots)) {
    per <- stand_by_plot(snap, plots)
    tot_ha <- if (!is.null(per)) sum(per$area_ha, na.rm = TRUE) else NA_real_
    out$stems_ha <- if (is.finite(tot_ha) && tot_ha > 0) round(out$stems / tot_ha) else NA_real_
  }
  out
}

# ---------------------------------------------------------------------------
# Per-tree growth: diameter increment between the first and last live bouts,
# annualised. De-pseudoreplicated (one rate per tree). Negative increments
# (shrinkage = measurement error / bole damage) are flagged, not deleted.
# ---------------------------------------------------------------------------
tree_growth <- function(trees) {
  if (is.null(trees) || !nrow(trees)) return(NULL)
  # LIVE bouts only (a tree dead at its last bout shouldn't contribute a growth
  # rate spanning into death), with a real DBH...
  g <- trees[is.finite(trees$stemDiameter) & trees$stemDiameter > 0 & trees$live %in% TRUE, , drop = FALSE]
  # ...and only PERMANENT individualIDs (TEMP.PLA ids are re-issued = different stems)
  if (!is.null(g) && "permanent" %in% names(g)) g <- g[g$permanent %in% TRUE, , drop = FALSE]
  if (is.null(g) || !nrow(g)) return(NULL)
  has_mh <- "measurementHeight" %in% names(g)
  # Collapse multi-bole stems to ONE equivalent whole-tree diameter per (tree, date)
  # BEFORE the first/last comparison, so the increment is like-for-like. A multi-bole
  # tree has several stem rows on the same date; comparing an arbitrary bole at each
  # end produces a meaningless increment. The equivalent DBH from summed basal area
  # (D_eq = sqrt(sum d^2)) is the standard whole-tree diameter for multi-stem trees,
  # and it reduces to d for single-bole trees. measurementHeight is averaged per date
  # so mh_change compares matched measurements. Group by individualID only (carry the
  # name) so a tree whose scientificName is revised across bouts stays one group.
  per_date <- g %>% dplyr::group_by(.data$individualID, .data$date) %>%
    dplyr::summarise(
      scientificName = dplyr::first(.data$scientificName),
      dbh = sqrt(sum(.data$stemDiameter^2, na.rm = TRUE)),
      mh  = if (has_mh) mean(.data$measurementHeight, na.rm = TRUE) else NA_real_,
      .groups = "drop")
  per_date %>% dplyr::group_by(.data$individualID) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::summarise(
      scientificName = dplyr::first(.data$scientificName),
      d0 = .data$dbh[which.min(.data$date)], d1 = .data$dbh[which.max(.data$date)],
      mh0 = .data$mh[which.min(.data$date)], mh1 = .data$mh[which.max(.data$date)],
      yrs = as.numeric(max(.data$date) - min(.data$date)) / 365.25,
      .groups = "drop") %>%
    dplyr::filter(.data$yrs > 0) %>%
    dplyr::mutate(growth_cm_yr = round((.data$d1 - .data$d0) / .data$yrs, 2),
                  shrank = .data$growth_cm_yr < -0.1,
                  # a moved measurement point makes the increment apples-to-oranges
                  mh_change = is.finite(.data$mh0) & is.finite(.data$mh1) & abs(.data$mh0 - .data$mh1) > 0.1)
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
  s <- trees_only(live_only(species_level_only(snap)))   # trees >= 10 cm DBH, for area-consistent BA
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
                     dominant = { i <- which.max(.data$stemDiameter); if (length(i)) .data$scientificName[i] else NA_character_ },
                     .groups = "drop")
  out <- per %>% dplyr::left_join(sp, by = "plotID") %>%
    dplyr::left_join(plots[, c("plotID", "plotType", "nlcdClass", "lat", "lng")], by = "plotID")
  out$ba_ha <- round(out$ba_ha, 1); out$density_ha <- round(out$density_ha)
  out %>% dplyr::arrange(dplyr::desc(.data$ba_ha))
}

# ---------------------------------------------------------------------------
# Analysis-ready exports: tidy, typed, unit-bearing column names, self-identifying
# (a downloaded record joins with no app context). One row per individual x bout.
# ---------------------------------------------------------------------------
col_or_na <- function(d, nm) if (nm %in% names(d)) d[[nm]] else NA
tidy_trees_export <- function(trees) {
  if (is.null(trees) || !nrow(trees)) return(NULL)
  d <- trees[order(trees$individualID, trees$date), , drop = FALSE]
  data.frame(
    individualID = d$individualID,
    plotID = d$plotID,
    subplotID = col_or_na(d, "subplotID"),
    scientificName = d$scientificName,
    family = col_or_na(d, "family"),
    taxonRank = col_or_na(d, "taxonRank"),
    growthForm = d$growthForm,
    date = format(d$date, "%Y-%m-%d"),
    year = d$year,
    dbh_cm = d$stemDiameter,
    basal_dbh_cm = col_or_na(d, "basalStemDiameter"),
    height_m = d$height,
    measurement_height_cm = col_or_na(d, "measurementHeight"),
    changed_measurement_location = col_or_na(d, "changedMeasurementLocation"),
    canopy_position = col_or_na(d, "canopyPosition"),
    plant_status = d$plantStatus,
    live = d$live,
    permanent = col_or_na(d, "permanent"),
    is_species = col_or_na(d, "is_species"),
    stringsAsFactors = FALSE)
}
plots_export <- function(snap, plots) {
  ps <- plot_summary_veg(snap, plots); if (is.null(ps)) return(NULL)
  data.frame(
    plotID = ps$plotID, plotType = ps$plotType, nlcdClass = ps$nlcdClass,
    lat = ps$lat, lng = ps$lng, area_trees_m2 = ps$area_trees,
    ba_m2_ha = ps$ba_ha, density_stems_ha = ps$density_ha,
    n_species_trees = ps$n_species, tallest_m = ps$tallest, biggest_dbh_cm = ps$biggest,
    dominant_species = ps$dominant, stringsAsFactors = FALSE)
}
veg_codebook <- function() {
  rows <- list(
    c("individualID","trees_long","character","","NEON stable tag for one physical stem/tree. TEMP.PLA ids are re-issued (see permanent)."),
    c("plotID","trees_long/plots","character","","NEON plot identifier."),
    c("subplotID","trees_long","character","","Nested subplot within the plot."),
    c("scientificName","trees_long","character","","Identified taxon; may be revised across bouts."),
    c("family","trees_long","character","","Taxonomic family."),
    c("taxonRank","trees_long","character","species/genus/...","Rank of the identification; is_species = TRUE only at species or finer."),
    c("growthForm","trees_long","character","single bole tree/multi-bole tree/small tree/shrub/...","NEON growth form; stand metrics scope to the tree forms."),
    c("date","trees_long","date (ISO)","YYYY-MM-DD","Measurement date of this bout."),
    c("year","trees_long","integer","","Calendar year of the bout."),
    c("dbh_cm","trees_long","numeric","cm","Diameter at breast height (~130 cm), measured at measurement_height_cm."),
    c("basal_dbh_cm","trees_long","numeric","cm","Basal stem diameter (for short/multi-bole stems lacking a DBH)."),
    c("height_m","trees_long","numeric","m","Stem height; often NA (not every stem is measured for height)."),
    c("measurement_height_cm","trees_long","numeric","cm","Height on the stem at which dbh_cm was taken; a change makes an increment apples-to-oranges."),
    c("changed_measurement_location","trees_long","character","noChange/boleChange/...","NEON reason code if the measurement point moved."),
    c("canopy_position","trees_long","character","","Crown position class."),
    c("plant_status","trees_long","character","Live*/Standing dead/Downed/Lost.../Removed/...","NEON status string."),
    c("live","trees_long","logical","TRUE/FALSE","Derived = grepl('^Live', plant_status)."),
    c("permanent","trees_long","logical","TRUE/FALSE","Derived = id starts with 'NEON'; growth metrics use permanent ids only."),
    c("is_species","trees_long","logical","TRUE/FALSE","Derived = identified to species or finer (unambiguous)."),
    c("area_trees_m2","plots","numeric","m^2","totalSampledAreaTrees — the per-hectare denominator for tree-form stems (>=10 cm DBH)."),
    c("ba_m2_ha","plots","numeric","m^2/ha","Live basal area per hectare for this plot (trees >=10 cm DBH)."),
    c("density_stems_ha","plots","numeric","stems/ha","Live stem density per hectare for this plot (trees >=10 cm DBH)."),
    c("n_species_trees","plots","integer","","Live tree species count in the plot."))
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("column", "table", "type", "allowed_values", "definition"); out
}

# live vs dead snapshot composition (an honest crude status ratio, not a rate).
# Reduced PER INDIVIDUAL across all snapshot boles: a tree is Live if ANY bole is
# live (a thick broken bole doesn't make a live tree "dead" — the old largest-bole
# rule biased the split toward dead), Dead only if a bole is dead/downed and none
# live. "Lost track / removed" (lost tag, removed, no-longer-qualifies) is a DATA
# state, split out so it isn't read as biological mortality. Scoped to tree growth
# forms so shrubs/lianas don't dilute the tree stand's status.
status_summary <- function(snap) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- snap[snap$growthForm %in% TREE_FORMS | is.na(snap$growthForm), , drop = FALSE]
  if (!nrow(s)) s <- snap
  per <- s %>% dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(
      any_live = any(grepl("^Live", .data$plantStatus)),
      any_dead = any(grepl("[Dd]ead|Downed", .data$plantStatus)),
      any_lost = any(grepl("Lost|Removed|No longer", .data$plantStatus)),
      .groups = "drop")
  per$cls <- dplyr::case_when(
    per$any_live ~ "Live",
    per$any_dead ~ "Dead / standing dead",
    per$any_lost ~ "Lost track / removed",
    TRUE ~ "Other / unknown")
  lvl <- c("Live", "Dead / standing dead", "Lost track / removed", "Other / unknown")
  out <- per %>% dplyr::count(.data$cls, name = "n")
  out$cls <- factor(out$cls, levels = lvl)
  out[order(out$cls), , drop = FALSE]
}
