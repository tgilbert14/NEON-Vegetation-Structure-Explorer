# ===========================================================================
# NEON Vegetation Structure Explorer — veg_helpers.R
# Individual-grain analyses on NEON Vegetation structure (DP1.10098.001): each
# tagged woody plant (individualID) is remeasured over years, so it has a growth
# career (the woody analog of the mammal capture career). Snapshot metrics use
# the LATEST measurement per plant (never pool bouts — a plant measured 5x would
# count 5x); the growth metric is the explicit multi-bout one. Density/basal-area
# are area-scaled (per ha). Honesty discipline ported from the siblings.
#
# TWO MEASUREMENT PARADIGMS (adaptive per site — see size_spec / classify_structure):
#   * FOREST sites — woody plants are TREES, sized by DBH (stemDiameter @ 130 cm),
#     tallied over totalSampledAreaTrees, with a 10 cm tree threshold.
#   * SHRUBLAND sites (deserts, sage, grasslands) — woody plants are SHRUBS, too
#     short for a DBH; sized by BASAL stem diameter (basalStemDiameter), tallied
#     over totalSampledAreaShrubSapling, no DBH floor. NEON reports basal diameter
#     for ~96-99% of desert stems vs ~1-11% with a DBH.
# Every size-dependent function takes a `spec` (default = forest) so the same code
# serves both; server.R passes each site's own spec.
# ===========================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }
smax <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) }   # NA (not -Inf) on empty
smean <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else mean(x) }
short_tree <- function(id) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(id))   # NEON.PLA.D01.000123 -> 000123
short_plot <- function(p) sub("^[A-Z]{4}_", "", as.character(p))

TREE_FORMS  <- c("single bole tree", "multi-bole tree", "small tree")
SHRUB_FORMS <- c("single shrub", "small shrub", "sapling", "small tree")
# The protocol tree threshold. Stems with DBH >= this are tallied over the full
# plot tree area (totalSampledAreaTrees); SMALLER stems are sampled only in a
# smaller nested subplot, so dividing them by the tree area under-counts them.
# Forest STAND metrics are therefore scoped to trees >= 10 cm DBH for an
# area-consistent estimate. Shrublands have no such floor — shrubs ARE the
# nested-sampled class, tallied over totalSampledAreaShrubSapling.
TREE_DBH_MIN <- 10

# ---------------------------------------------------------------------------
# size_spec(): everything that differs between the two measurement paradigms,
# in one place. col = the diameter measurement; area = the plot's sampled-area
# column for that class; forms = the growth forms that define the stand; quad =
# the four Size-Lab corner labels; noun/labels drive the adaptive UI copy.
# ---------------------------------------------------------------------------
SIZE_FOREST <- list(
  type = "forest", col = "stemDiameter", min = TREE_DBH_MIN, area = "area_trees",
  forms = TREE_FORMS,
  noun = "tree", nouns = "trees", Noun = "Tree", Nouns = "Trees",
  size_lab = "DBH", size_full = "diameter at breast height (DBH)", emoji = "\U0001F333",
  unit = "cm", lab_title = "Size Lab",
  quad = c(bigtall = "GIANTS \U0001F3C6", smalltall = "SPIRES", bigshort = "STOUT", smallshort = "SAPLINGS"))
SIZE_SHRUB <- list(
  type = "shrubland", col = "basalStemDiameter", min = 0, area = "area_shrub",
  forms = SHRUB_FORMS,
  noun = "shrub", nouns = "shrubs", Noun = "Shrub", Nouns = "Shrubs",
  size_lab = "basal ø", size_full = "basal stem diameter", emoji = "\U0001F33F",
  unit = "cm", lab_title = "Size Lab",
  quad = c(bigtall = "LARGEST \U0001F3C6", smalltall = "LEGGY", bigshort = "SPRAWLING", smallshort = "SMALLEST"))
size_spec <- function(type) if (identical(type, "shrubland")) SIZE_SHRUB else SIZE_FOREST

# classify a site's structure from its live snapshot by BASAL AREA — which growth
# form occupies more cross-sectional area. This beats counting individuals: a
# mature forest with a dense shrub understory (many small shrub stems) is still a
# forest because its trees dominate basal area. Trees use DBH (≥10 cm); shrubs use
# basal stem diameter. Forest if tree basal area >= shrub basal area.
classify_structure <- function(snap) {
  s <- live_only(snap); if (is.null(s) || !nrow(s)) return("forest")
  td <- suppressWarnings(as.numeric(s$stemDiameter))
  bd <- if ("basalStemDiameter" %in% names(s)) suppressWarnings(as.numeric(s$basalStemDiameter)) else rep(NA_real_, nrow(s))
  is_tree  <- s$growthForm %in% TREE_FORMS & is.finite(td) & td >= TREE_DBH_MIN
  is_shrub <- s$growthForm %in% c("single shrub", "small shrub") & is.finite(bd) & bd > 0
  tree_ba  <- sum(pi * (td[is_tree] / 200)^2, na.rm = TRUE)
  shrub_ba <- sum(pi * (bd[is_shrub] / 200)^2, na.rm = TRUE)
  if (tree_ba == 0 && shrub_ba == 0) return("forest")
  if (tree_ba >= shrub_ba) "forest" else "shrubland"
}

species_level_only <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  if ("is_species" %in% names(d)) return(d[d$is_species %in% TRUE, , drop = FALSE])
  ok <- is.na(d$taxonRank) | d$taxonRank %in% c("species", "subspecies", "variety", "speciesGroup")
  d[ok, , drop = FALSE]
}
# CVD-safe categorical palette tuned to the cross-biome chrome (Okabe-Ito spine;
# distinct under deutan/protan vision). First five are maximally separated; none
# sits near the focal amber #E0A500, so the "plant you're viewing" can never
# collide with a species hue.
FOREST_CAT <- c("#2f7d46", "#9a6a2e", "#356f80", "#7a5fa3", "#b06a8c",
                "#5a8a3a", "#3f9a96", "#c98a3a", "#8a8f9a")
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
# woody_only(): the stand's countable plants for a paradigm — finite positive
# size at/above the threshold (trees >= 10 cm DBH; shrubs any basal diameter).
# trees_only() kept as the forest alias used by older call sites.
# tree_snapshot(): the LATEST bout per individual (current state). A plant can
# have several stem rows in one bout (multi-bole) — all kept here.
# one_per_tree(): collapse to the single largest stem per individual (the dot in
# the Size Lab + the size headline). Basal area sums ALL snapshot stems.
# ---------------------------------------------------------------------------
woody_only <- function(d, spec = SIZE_FOREST) {
  if (is.null(d) || !nrow(d)) return(d)
  x <- d[[spec$col]]
  keep <- is.finite(x) & x > 0 & x >= spec$min
  d[keep, , drop = FALSE]
}
trees_only <- function(d) woody_only(d, SIZE_FOREST)

tree_snapshot <- function(trees) {
  if (is.null(trees) || !nrow(trees)) return(trees)
  trees %>% dplyr::group_by(.data$individualID) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>% dplyr::ungroup()
}
one_per_tree <- function(snap, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(snap)
  snap$.size <- snap[[spec$col]]
  out <- snap %>% dplyr::group_by(.data$individualID) %>%
    dplyr::slice_max(.data$.size, n = 1, with_ties = FALSE, na_rm = FALSE) %>% dplyr::ungroup()
  out$.size <- NULL; out
}
live_only <- function(d) { if (is.null(d) || !nrow(d)) return(d); d[d$live %in% TRUE, , drop = FALSE] }

# ---------------------------------------------------------------------------
# Stand structure (per ha) — basal area and stem density need the plot's sampled
# area for the paradigm (plots$area_trees for trees, plots$area_shrub for shrubs).
# Computed per plot then summarised to the site (mean per ha), so a big plot
# doesn't dominate. Live stems with a real size only. Basal area uses the
# paradigm's diameter — DBH for trees, basal diameter for shrubs (basal cover).
# ---------------------------------------------------------------------------
stand_by_plot <- function(snap, plots, spec = SIZE_FOREST, plot_types = NULL) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  # Optional design-based path: restrict to a plotType stratum (e.g. "distributed",
  # the spatially-balanced random design that underpins an UNBIASED site mean) so the
  # distributed-only estimate is one argument away, not a re-derivation. Default NULL
  # keeps the shipped tower+distributed pooled behaviour untouched.
  if (!is.null(plot_types) && "plotType" %in% names(plots)) {
    keep_pl <- plots$plotID[plots$plotType %in% plot_types]
    snap <- snap[snap$plotID %in% keep_pl, , drop = FALSE]
    if (!nrow(snap)) return(NULL)
  }
  s <- woody_only(live_only(snap), spec)
  if (is.null(s) || !nrow(s)) return(NULL)
  s$.d <- s[[spec$col]]
  s$ba_m2 <- pi * (s$.d / 200)^2                    # cm diameter -> m^2 basal area per stem
  per <- s %>% dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(stems = dplyr::n_distinct(.data$individualID),
                     ba_m2 = sum(.data$ba_m2), sumD2 = sum(.data$.d^2),
                     .groups = "drop")
  acol <- if (spec$area %in% names(plots)) spec$area else "area_trees"
  pa <- plots[, c("plotID", acol)]; names(pa)[2] <- "area_use"
  per <- dplyr::left_join(per, pa, by = "plotID")
  per$area_ha <- per$area_use / 10000
  per <- per[is.finite(per$area_ha) & per$area_ha > 0.005, , drop = FALSE]   # drop tiny/partial slivers (<50 m²)
  if (!nrow(per)) return(NULL)
  per$ba_ha <- per$ba_m2 / per$area_ha
  per$density_ha <- per$stems / per$area_ha
  per
}
stand_site <- function(snap, plots, spec = SIZE_FOREST, plot_types = NULL) {
  per <- stand_by_plot(snap, plots, spec, plot_types); if (is.null(per)) return(NULL)
  n <- nrow(per)
  se <- function(x) if (n > 1) stats::sd(x, na.rm = TRUE) / sqrt(n) else NA_real_
  list(ba_ha = round(mean(per$ba_ha, na.rm = TRUE), 1),         # plot = sampling unit (equal weight)
       ba_se = round(se(per$ba_ha), 1),                          # SE across plots (n = sampling units)
       density_ha = round(mean(per$density_ha, na.rm = TRUE)),
       density_se = round(se(per$density_ha)),
       qmd = round(sqrt(sum(per$sumD2, na.rm = TRUE) / sum(per$stems, na.rm = TRUE)), 1),  # POOLED RMS, not mean-of-QMDs
       n_plots = n)
}

# diameter size-class distribution of the stand's plants. Forest = DBH classes of
# trees >= 10 cm (smaller stems are nested-sampled over a smaller area, so
# including them would under-represent the small classes). Shrubland = basal
# diameter classes of all shrubs.
size_breaks <- function(spec) {
  if (identical(spec$type, "shrubland"))
    list(brks = c(0, 1, 2.5, 5, 10, 20, Inf),
         labs = c("<1", "1–2.5", "2.5–5", "5–10", "10–20", "20+"),
         small = c("<1", "1–2.5"), big = c("10–20", "20+"))
  else
    list(brks = c(10, 20, 30, 40, 50, 70, Inf),
         labs = c("10–20", "20–30", "30–40", "40–50", "50–70", "70+"),
         small = c("10–20", "20–30"), big = c("50–70", "70+"))
}
size_class <- function(snap, plots = NULL, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- woody_only(live_only(snap), spec)
  if (is.null(s) || !nrow(s)) return(NULL)
  bk <- size_breaks(spec)
  s$cls <- cut(s[[spec$col]], breaks = bk$brks, labels = bk$labs, right = FALSE)
  out <- s %>% dplyr::count(.data$cls, name = "stems") %>%
    tidyr::complete(cls = factor(bk$labs, levels = bk$labs), fill = list(stems = 0))
  # area-standardize to stems/ha when plots are supplied (raw counts are
  # sampling-effort-dependent). Pooled over the sampled area of contributing plots.
  if (!is.null(plots)) {
    per <- stand_by_plot(snap, plots, spec)
    tot_ha <- if (!is.null(per)) sum(per$area_ha, na.rm = TRUE) else NA_real_
    out$stems_ha <- if (is.finite(tot_ha) && tot_ha > 0) round(out$stems / tot_ha) else NA_real_
  }
  out
}

# ---------------------------------------------------------------------------
# Per-plant growth: diameter increment between the first and last live bouts,
# annualised. De-pseudoreplicated (one rate per plant). Negative increments
# (shrinkage = measurement error / bole damage) are flagged, not deleted. Uses
# the paradigm's diameter (DBH for trees, basal for shrubs).
# ---------------------------------------------------------------------------
tree_growth <- function(trees, spec = SIZE_FOREST) {
  if (is.null(trees) || !nrow(trees)) return(NULL)
  col <- spec$col
  trees$.d <- trees[[col]]
  # LIVE bouts only (a plant dead at its last bout shouldn't contribute a growth
  # rate spanning into death), with a real diameter...
  g <- trees[is.finite(trees$.d) & trees$.d > 0 & trees$live %in% TRUE, , drop = FALSE]
  # ...and only PERMANENT individualIDs (TEMP.PLA ids are re-issued = different stems)
  if (!is.null(g) && "permanent" %in% names(g)) g <- g[g$permanent %in% TRUE, , drop = FALSE]
  if (is.null(g) || !nrow(g)) return(NULL)
  # single-census / all-dates-missing cohorts (WOOD/DCFS/NOGP) have no first-vs-last
  # span to take — bail before the which.min/which.max date reductions so they can't
  # emit "no non-missing arguments to max; returning -Inf" warnings into the logs.
  g <- g[!is.na(g$date), , drop = FALSE]
  if (!nrow(g) || length(unique(g$date)) < 2) return(NULL)
  has_mh <- "measurementHeight" %in% names(g)
  # Collapse multi-bole stems to ONE equivalent whole-plant diameter per (plant, date)
  # BEFORE the first/last comparison, so the increment is like-for-like. D_eq =
  # sqrt(sum d^2) is the standard whole-plant diameter for multi-stem individuals,
  # and reduces to d for single-stem ones. Group by individualID only (carry the
  # name) so a plant whose scientificName is revised across bouts stays one group.
  per_date <- g %>% dplyr::group_by(.data$individualID, .data$date) %>%
    dplyr::summarise(
      scientificName = dplyr::first(.data$scientificName),
      dbh = sqrt(sum(.data$.d^2, na.rm = TRUE)),
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

# one plant's full measurement history (the growth trajectory on the card)
tree_history <- function(trees, id) {
  if (is.null(trees) || is.null(id)) return(NULL)
  h <- trees[trees$individualID == id & !is.na(trees$date), , drop = FALSE]
  if (!nrow(h)) return(NULL)
  keep <- intersect(c("date", "year", "stemDiameter", "basalStemDiameter", "height", "plantStatus", "growthForm",
                      "canopyPosition", "measurementHeight", "changedMeasurementLocation"), names(h))
  h[order(h$date), keep, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Per-plant QC flags from its history, ranked. Every flag is "verify", not "wrong"
# — a quarter of remeasurement intervals show diameter decreases that are real
# (bark sloughing, a changed measurement height, drought). Flag, keep, explain.
# ---------------------------------------------------------------------------
tree_qc_flags <- function(hist, spec = SIZE_FOREST) {
  flags <- list(); add <- function(level, text) flags[[length(flags) + 1L]] <<- list(level = level, text = text)
  if (is.null(hist) || !nrow(hist)) return(flags)
  if (nrow(hist) < 2) { add("info", "Measured once — no remeasurement yet, so the growth trajectory and its checks don't apply."); return(flags) }
  dcol <- if (spec$col %in% names(hist)) spec$col else "stemDiameter"
  d <- hist[[dcol]]; dt <- as.numeric(diff(hist$date)) / 365.25
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
# Per-SPECIES structure (composition, leaderboard, the "champion plant").
# ---------------------------------------------------------------------------
species_structure <- function(snap, plots, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  s <- woody_only(live_only(species_level_only(snap)), spec)
  one <- one_per_tree(s, spec)
  if (!nrow(one)) return(NULL)
  one$.d <- one[[spec$col]]
  ba <- s; ba$.d <- ba[[spec$col]]; ba <- ba[is.finite(ba$.d) & ba$.d > 0, ]
  ba$ba_m2 <- pi * (ba$.d / 200)^2
  ba_by <- ba %>% dplyr::group_by(.data$scientificName) %>% dplyr::summarise(ba_m2 = sum(.data$ba_m2), .groups = "drop")
  one %>% dplyr::group_by(.data$scientificName, .data$family) %>%
    dplyr::summarise(stems = dplyr::n_distinct(.data$individualID),
                     max_dbh = round(smax(.data$.d), 1),
                     max_ht = round(smax(.data$height), 1),
                     mean_dbh = round(smean(.data$.d), 1),
                     .groups = "drop") %>%
    dplyr::left_join(ba_by, by = "scientificName") %>%
    dplyr::arrange(dplyr::desc(.data$ba_m2))
}

# ---------------------------------------------------------------------------
# Per-PLOT (stand) summary — feeds the Map + a stand profile.
# ---------------------------------------------------------------------------
plot_summary_veg <- function(snap, plots, spec = SIZE_FOREST) {
  per <- stand_by_plot(snap, plots, spec); if (is.null(per)) return(NULL)
  s <- live_only(species_level_only(snap)); s$.d <- s[[spec$col]]
  sp <- s %>% dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(n_species = dplyr::n_distinct(.data$scientificName),
                     tallest = round(smax(.data$height), 1),
                     biggest = round(smax(.data$.d), 1),
                     dominant = { i <- which.max(.data$.d); if (length(i)) .data$scientificName[i] else NA_character_ },
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
    basal_stem_diam_cm = col_or_na(d, "basalStemDiameter"),
    height_m = d$height,
    max_crown_diam_m = col_or_na(d, "maxCrownDiameter"),
    measurement_height_cm = col_or_na(d, "measurementHeight"),
    changed_measurement_location = col_or_na(d, "changedMeasurementLocation"),
    canopy_position = col_or_na(d, "canopyPosition"),
    plant_status = d$plantStatus,
    live = d$live,
    permanent = col_or_na(d, "permanent"),
    is_species = col_or_na(d, "is_species"),
    stringsAsFactors = FALSE)
}
plots_export <- function(snap, plots, spec = SIZE_FOREST) {
  ps <- plot_summary_veg(snap, plots, spec); if (is.null(ps)) return(NULL)
  # Self-identify the paradigm PER ROW so a pooled file (rbind of two sites'
  # plots.csv) never silently mixes physically different measurements: forest
  # ba_m2_ha is bole cross-section at breast height (DBH), shrubland ba_m2_ha is
  # basal COVER at the stem base — a ~500x-ratio difference in kind, not degree.
  size_metric <- if (identical(spec$type, "shrubland")) "basal-diameter basal cover (stem base)" else "bole-DBH basal area (breast height)"
  data.frame(
    plotID = ps$plotID, plotType = ps$plotType, nlcdClass = ps$nlcdClass,
    structure_type = spec$type, size_metric = size_metric,
    lat = ps$lat, lng = ps$lng, sampled_area_m2 = ps$area_use,
    ba_m2_ha = ps$ba_ha, density_stems_ha = ps$density_ha,
    n_species = ps$n_species, tallest_m = ps$tallest, biggest_diam_cm = ps$biggest,
    dominant_species = ps$dominant, stringsAsFactors = FALSE)
}
veg_codebook <- function() {
  rows <- list(
    c("individualID","trees_long","character","","NEON stable tag for one physical stem/plant. TEMP.PLA ids are re-issued (see permanent)."),
    c("plotID","trees_long/plots","character","","NEON plot identifier."),
    c("subplotID","trees_long","character","","Nested subplot within the plot."),
    c("scientificName","trees_long","character","","Identified taxon; may be revised across bouts."),
    c("family","trees_long","character","","Taxonomic family."),
    c("taxonRank","trees_long","character","species/genus/...","Rank of the identification; is_species = TRUE only at species or finer."),
    c("growthForm","trees_long","character","single bole tree/multi-bole tree/small tree/single shrub/small shrub/sapling/...","NEON growth form; forest stand metrics scope to tree forms, shrubland to shrub forms."),
    c("date","trees_long","date (ISO)","YYYY-MM-DD","Measurement date of this bout."),
    c("year","trees_long","integer","","Calendar year of the bout."),
    c("dbh_cm","trees_long","numeric","cm","Diameter at breast height (~130 cm) — the FOREST-paradigm size measurement. NA = not measured under this site's paradigm (shrubland stems carry basal_stem_diam_cm instead), so it is structurally NA, not missing data — e.g. ~28% NA even at a forest site like HARV."),
    c("basal_stem_diam_cm","trees_long","numeric","cm","Basal stem diameter (near ground) — the SHRUBLAND-paradigm size measurement for shrubs / short stems. NA = not measured under this site's paradigm (forest stems carry dbh_cm instead), so it is structurally NA, not missing data — e.g. ~79% NA at a forest site like HARV."),
    c("height_m","trees_long","numeric","m","Plant height; often NA (not every stem is measured for height)."),
    c("max_crown_diam_m","trees_long","numeric","m","Maximum crown/canopy diameter (where measured)."),
    c("measurement_height_cm","trees_long","numeric","cm","Height on the stem at which dbh_cm was taken; a change makes an increment apples-to-oranges."),
    c("changed_measurement_location","trees_long","character","noChange/boleChange/...","NEON reason code if the measurement point moved."),
    c("canopy_position","trees_long","character","","Crown position class."),
    c("plant_status","trees_long","character","Live*/Standing dead/Downed/Lost.../Removed/...","NEON status string."),
    c("live","trees_long","logical","TRUE/FALSE","Derived = grepl('^Live', plant_status)."),
    c("permanent","trees_long","logical","TRUE/FALSE","Derived = id starts with 'NEON'; growth metrics use permanent ids only."),
    c("is_species","trees_long","logical","TRUE/FALSE","Derived = identified to species or finer (unambiguous)."),
    c("plotType","plots","character","distributed/tower","NEON plot design class — distributed (random placement, the basis for the unbiased site estimate) vs tower (clustered near the flux tower). Split by plotType before pooling for a design-based estimate."),
    c("structure_type","plots","character","forest/shrubland","The site's measurement paradigm — forest (woody plants sized by DBH at breast height) vs shrubland (sized by basal stem diameter at the base). Tags every row so a pooled plots.csv from multiple sites self-identifies which paradigm produced its ba_m2_ha / biggest_diam_cm — these are NOT the same measurement across the fork (see ba_m2_ha)."),
    c("size_metric","plots","character","bole-DBH basal area (breast height)/basal-diameter basal cover (stem base)","Plain-language name of the physical quantity ba_m2_ha represents for this row, set by structure_type. Forest = bole cross-section at ~130 cm; shrubland = basal cover at the stem base."),
    c("nlcdClass","plots","character","","NEON land-cover class (NLCD) at the plot."),
    c("lat","plots","numeric","decimal degrees","Plot centroid latitude (WGS84 decimal degrees)."),
    c("lng","plots","numeric","decimal degrees","Plot centroid longitude (WGS84 decimal degrees)."),
    c("sampled_area_m2","plots","numeric","m^2","Sampled area for the paradigm — totalSampledAreaTrees (forest) or totalSampledAreaShrubSapling (shrubland) — the per-hectare denominator."),
    c("ba_m2_ha","plots","numeric","m^2/ha","Live basal area per hectare for this plot. JOIN HAZARD: this is NOT one comparable measurement across sites — at a forest site it is bole cross-section at breast height (DBH), at a shrubland site it is basal COVER at the stem base, a ~500x-ratio difference in kind. Use the structure_type / size_metric column on the SAME row to know which; never pool ba_m2_ha across paradigms without splitting on structure_type."),
    c("density_stems_ha","plots","numeric","stems/ha","Live stem density per hectare for this plot."),
    c("n_species","plots","integer","","Live species count in the plot."),
    c("tallest_m","plots","numeric","m","Height of the tallest live plant in the plot."),
    c("biggest_diam_cm","plots","numeric","cm","Largest live stem diameter in the plot — DBH for forest, basal diameter for shrubland."),
    c("dominant_species","plots","character","","Live plant with the largest diameter in the plot (the plot's size-dominant taxon)."))
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("column", "table", "type", "allowed_values", "definition"); out
}

# live vs dead snapshot composition (an honest crude status ratio, not a rate).
# Reduced PER INDIVIDUAL across all snapshot stems: live if ANY stem is live, dead
# only if a stem is dead/downed and none live. "Lost track / removed" (lost tag,
# removed, no-longer-qualifies) is a DATA state, split out so it isn't read as
# biological mortality. Scoped to the paradigm's growth forms.
status_summary <- function(snap, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  forms <- spec$forms %||% TREE_FORMS                 # guard: never let %in% NULL silently drop all
  s <- snap[snap$growthForm %in% forms | is.na(snap$growthForm), , drop = FALSE]
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

# ---------------------------------------------------------------------------
# Compound ANNUAL mortality rate (Sheil & May) — the forestry standard, distinct
# from the snapshot live/dead ratio. Cohort = permanent woody individuals LIVE at
# their first census with a KNOWN fate (live or dead, not lost-track) at their
# last; m = 1 - (1 - deaths/N0)^(1/t) annualised over the MEAN per-plant exposure
# t = Σtᵢ/N₀ (each individual weighted by its own census interval — stricter than a
# single median t when cadences are mixed), with a binomial CI. NULL (→ snapshot
# only) when <2 censuses or the cohort is too thin (<10) to report honestly.
# ---------------------------------------------------------------------------
stand_mortality <- function(trees, spec = SIZE_FOREST) {
  if (is.null(trees) || !nrow(trees) || !"date" %in% names(trees)) return(NULL)
  forms <- spec$forms %||% TREE_FORMS
  d <- trees[(trees$growthForm %in% forms | is.na(trees$growthForm)) & !is.na(trees$date), , drop = FALSE]
  if ("permanent" %in% names(d)) d <- d[d$permanent %in% TRUE, , drop = FALSE]
  if (is.null(d) || !nrow(d) || !"plantStatus" %in% names(d)) return(NULL)
  # single-census sites (one date for every plant) carry no first-vs-last fate —
  # bail before the grouped which.min/which.max date reductions so an empty cohort
  # can't leak a -Inf max() warning into the logs.
  if (length(unique(d$date)) < 2) return(NULL)
  d$.live <- grepl("^Live", d$plantStatus)
  d$.dead <- grepl("[Dd]ead|Downed", d$plantStatus)
  per <- d %>% dplyr::group_by(.data$individualID) %>%
    dplyr::filter(dplyr::n_distinct(.data$date) >= 2) %>%
    dplyr::summarise(
      first_live = .data$.live[which.min(.data$date)],
      last_live  = .data$.live[which.max(.data$date)],
      last_dead  = .data$.dead[which.max(.data$date)],
      t = as.numeric(max(.data$date) - min(.data$date)) / 365.25, .groups = "drop")
  coh <- per[per$first_live %in% TRUE & per$t > 0 & (per$last_live %in% TRUE | per$last_dead %in% TRUE), , drop = FALSE]
  n0 <- nrow(coh); if (n0 < 10) return(NULL)
  deaths <- sum(coh$last_dead %in% TRUE & !(coh$last_live %in% TRUE))
  # Annualise over each plant's OWN interval and pool (the stricter Sheil & May
  # form): use mean per-plant exposure t̄ = Σtᵢ/N₀ as the compound exponent rather
  # than a single median census interval. When plots run mixed cadences (1-yr tower
  # vs 5-yr distributed) a lone median t can bias the pooled annual rate; mean
  # exposure weights every individual by the interval it was actually observed over.
  t <- mean(coh$t, na.rm = TRUE); if (!is.finite(t) || t <= 0) return(NULL)
  ann <- function(q) 100 * (1 - (1 - q)^(1 / t))
  bt <- tryCatch(stats::binom.test(deaths, n0)$conf.int, error = function(e) c(NA_real_, NA_real_))
  list(rate_pct = round(ann(deaths / n0), 2), n0 = n0, deaths = deaths, t_yrs = round(t, 1),
       lo = if (is.finite(bt[1])) round(ann(bt[1]), 2) else NA_real_,
       hi = if (is.finite(bt[2])) round(ann(bt[2]), 2) else NA_real_)
}

# ---------------------------------------------------------------------------
# SITE-LEVEL data-quality scan (the small-mammal QC signature) — ranked
# "verify, not wrong" flags across all of a site's remeasured plants, each with
# the offending individuals so the UI can show an inspector + a downloadable CSV.
# Returns list(flags = list of {level,key,label,why,n,rows}, n_flag, report).
# rows = a tidy data.frame per flag (individualID, species, the evidence columns).
# ---------------------------------------------------------------------------
tree_qc_site <- function(trees, spec = SIZE_FOREST) {
  if (is.null(trees) || !nrow(trees)) return(NULL)
  flags <- list()
  add <- function(level, key, label, why, rows) {
    if (is.null(rows) || !nrow(rows)) return(invisible())
    rows <- cbind(flag = key, rows, stringsAsFactors = FALSE)
    flags[[length(flags) + 1L]] <<- list(level = level, key = key, label = label, why = why,
      n = nrow(rows), rows = rows) }
  short <- function(x) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(x))

  # 1) Recorded Live AFTER Dead — impossible; a tag swap / data-entry issue (HIGH).
  # Collapse to ONE status per (plant, date) first — a plant is "live" that date if
  # ANY stem is live, "dead" only if a dead/downed stem AND no live stem — so a
  # multi-stem individual with a dead stem beside a live one isn't a false positive.
  d <- trees[!is.na(trees$date) & !is.na(trees$plantStatus), , drop = FALSE]
  if ("permanent" %in% names(d)) d <- d[d$permanent %in% TRUE, , drop = FALSE]
  if (nrow(d)) {
    pd <- d %>% dplyr::group_by(.data$individualID, .data$date) %>%
      dplyr::summarise(scientificName = dplyr::first(.data$scientificName),
        live = any(grepl("^Live", .data$plantStatus)),
        dead = any(grepl("[Dd]ead|Downed", .data$plantStatus)) & !any(grepl("^Live", .data$plantStatus)),
        .groups = "drop")
    pd <- pd[order(pd$individualID, pd$date), , drop = FALSE]
    res <- pd %>% dplyr::group_by(.data$individualID) %>%
      dplyr::summarise(scientificName = dplyr::first(.data$scientificName),
        resurrected = any(.data$live & cumsum(.data$dead) > 0), .groups = "drop")
    rr <- res[res$resurrected %in% TRUE, , drop = FALSE]
    if (nrow(rr)) add("high", "resurrection", "Recorded Live after Dead",
      "A plant logged dead at one visit and live at a later one — impossible biologically, so it points to a tag swap or data-entry error.",
      data.frame(plant = short(rr$individualID), species = rr$scientificName, stringsAsFactors = FALSE))
  }

  # growth-derived flags (per permanent individual, like-for-like increments)
  g <- tree_growth(trees, spec)
  if (!is.null(g) && nrow(g)) {
    jump <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 5 & !g$mh_change, , drop = FALSE]
    if (nrow(jump)) add("high", "jump", "Implausible diameter jump (>5 cm/yr)",
      "A diameter increase faster than ~5 cm/yr (with no measurement-height change) usually means a mis-measure or a tag mix-up, not real growth.",
      data.frame(plant = short(jump$individualID), species = jump$scientificName,
        start_cm = round(jump$d0, 1), now_cm = round(jump$d1, 1), cm_per_yr = jump$growth_cm_yr, stringsAsFactors = FALSE))
    shrink <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr < -2 & !g$mh_change, , drop = FALSE]
    if (nrow(shrink)) add("warn", "shrink", "Large shrink (< −2 cm/yr)",
      "Some diameter decrease is real (bark loss, drought); a drop steeper than 2 cm/yr (with no height change) is worth a second look.",
      data.frame(plant = short(shrink$individualID), species = shrink$scientificName,
        start_cm = round(shrink$d0, 1), now_cm = round(shrink$d1, 1), cm_per_yr = shrink$growth_cm_yr, stringsAsFactors = FALSE))
    mh <- g[g$mh_change %in% TRUE, , drop = FALSE]
    if (nrow(mh)) add("info", "mh", "Measurement height moved between visits",
      "The point on the stem where diameter is taken changed, so the before/after increment isn't apples-to-apples — these are kept but excluded from growth stats.",
      data.frame(plant = short(mh$individualID), species = mh$scientificName,
        start_cm = round(mh$d0, 1), now_cm = round(mh$d1, 1), stringsAsFactors = FALSE))
  }
  ord <- c(high = 1L, warn = 2L, info = 3L)
  flags <- flags[order(vapply(flags, function(f) ord[[f$level]], integer(1)))]
  # per-flag row frames have different columns (jump/shrink carry sizes, resurrection
  # doesn't) — bind_rows fills the gaps with NA so the report never errors.
  report <- if (length(flags)) dplyr::bind_rows(lapply(flags, function(f) {
    r <- f$rows; r$flag <- NULL
    cbind(level = f$level, issue = f$label, r, stringsAsFactors = FALSE) })) else data.frame()
  list(flags = flags, n_flag = length(flags), report = report)
}
