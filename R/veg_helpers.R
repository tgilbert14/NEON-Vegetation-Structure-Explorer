# ===========================================================================
# NEON Vegetation Structure Explorer — veg_helpers.R
# Science core for NEON DP1.10098.001. The contract keeps three identities apart:
#   * plant: plotID x individualID (individualID is not site-globally unique),
#   * event: plotID x eventID (date alone is not an event key), and
#   * source row: uid (the published record identity), and
#   * protocol stem locator: plotID x eventID x individualID x tempStemID.
#
# The protocol locator should be unique, but official releases can contain
# documented anomaly rows. Those source uids are preserved and the affected
# physical channel is held rather than silently deduplicated or double-counted.
#
# Forest DBH basal area and shrub/sapling basal-diameter cover are separate
# physical channels with separate sampled-area denominators. They may be shown
# beside one another, but they must never be ranked, pooled, or used to classify
# a site by comparing their raw totals. Stand estimates use the latest supported
# plot event, preserve explicit sampled absence as zero, and hold unsupported
# opportunities as NA with a reason.
# ===========================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
# Shared size-plausibility screen (also sourced by the bundle builder).
if (!exists("vst_size_screen", mode = "function")) source("R/size_plausibility.R")
# Curated NEON issue-log notes (app-side; shown beside the numbers they affect).
if (!exists("veg_issue_notes", mode = "function")) source("R/neon_issue_log.R")
mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }
smax <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) }   # NA (not -Inf) on empty
smean <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else mean(x) }
short_tree <- function(id) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(id))   # NEON.PLA.D01.000123 -> 000123
short_plot <- function(p) sub("^[A-Z]{4}_", "", as.character(p))

VEG_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v3"
VEG_CONTRACT <- list(
  id = VEG_CONTRACT_ID,
  product = "DP1.10098.001",
  release = "RELEASE-2026",
  plant_key = c("plotID", "individualID"),
  plant_id_key = c("plotID", "plant_id"),
  event_key = c("plotID", "eventID"),
  source_record_key = "source_uid",
  mapping_source_record_key = "mapping_source_uid",
  protocol_stem_locator = c("plotID", "eventID", "individualID", "tempStemID"),
  opportunity_source_record_key = "source_record_key",
  supported_status = c("sampled_with_records", "sampled_absence"),
  held_status = c("held_sampling_impractical", "held_dendrometer_only",
                  "held_missing_area", "held_opportunity_unknown",
                  "held_presence_record_conflict", "held_metric_invalid",
                  "held_identity_conflict", "held_opportunity_source_missing",
                  "held_snapshot_event_mismatch", "supported_legacy_schema"),
  zero_status = "sampled_absence"
)

# Full-plot trees and nested shrub/sapling plants are deliberately disjoint.
# In particular, `small tree` is a nested DBH class, not a basal-diameter shrub;
# silently putting it in either stand denominator would mix sampling designs.
TREE_FORMS  <- c("single bole tree", "multi-bole tree")
SHRUB_FORMS <- c("single shrub", "small shrub", "sapling")
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
  forms = TREE_FORMS, support = "tree_support", support_reason = "tree_support_reason",
  channel = "tree_dbh", metric_kind = "bole-DBH basal area, boles 10 cm and over (breast height)",
  channel_label = "Tree DBH channel", noun = "tree", nouns = "trees", Noun = "Tree", Nouns = "Trees",
  size_lab = "DBH", size_full = "diameter at breast height (DBH)", emoji = "\U0001F333",
  unit = "cm", lab_title = "Size Lab",
  quad = c(bigtall = "BIG + TALL \U0001F3C6", smalltall = "SMALL + TALL", bigshort = "BIG + SHORT", smallshort = "SMALL + SHORT"))
SIZE_SHRUB <- list(
  type = "shrubland", col = "basalStemDiameter", min = 0, area = "area_shrub",
  forms = SHRUB_FORMS, support = "shrub_support", support_reason = "shrub_support_reason",
  channel = "shrub_sapling_basal", metric_kind = "basal-diameter cover (stem base)",
  channel_label = "Shrub & sapling basal channel",
  noun = "shrub or sapling", nouns = "shrubs and saplings",
  Noun = "Shrub / sapling", Nouns = "Shrubs & saplings",
  size_lab = "basal ø", size_full = "basal stem diameter", emoji = "\U0001F33F",
  unit = "cm", lab_title = "Size Lab",
  quad = c(bigtall = "BIG + TALL \U0001F3C6", smalltall = "SMALL + TALL", bigshort = "BIG + SHORT", smallshort = "SMALL + SHORT"))
size_spec <- function(type) {
  if (identical(type, "shrubland") || identical(type, "shrub_sapling_basal")) return(SIZE_SHRUB)
  if (identical(type, "forest") || identical(type, "tree_dbh")) return(SIZE_FOREST)
  NULL
}

# Compatibility fallback only. A v3 bundle supplies its display channel in meta;
# this function no longer compares unlike DBH area and basal cover. If both
# channels occur and no metadata is available, prefer the established full-plot
# tree channel, while keeping the shrub channel available as a separate view.
classify_structure <- function(snap) {
  s <- live_only(snap); if (is.null(s) || !nrow(s) || !"growthForm" %in% names(s)) return("forest")
  has_tree <- nrow(woody_only(s, SIZE_FOREST)) > 0
  has_shrub <- nrow(woody_only(s, SIZE_SHRUB)) > 0
  if (!has_tree && has_shrub) "shrubland" else "forest"
}

.chr <- function(x) ifelse(is.na(x), NA_character_, as.character(x))
.num <- function(x) suppressWarnings(as.numeric(x))
.date_num <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  out <- suppressWarnings(as.numeric(as.Date(x)))
  raw <- suppressWarnings(as.numeric(x)); out[!is.finite(out)] <- raw[!is.finite(out)]
  out
}
.first_known <- function(x) { i <- which(!is.na(x) & nzchar(as.character(x))); if (length(i)) x[i[1]] else x[NA_integer_] }
.plant_key <- function(d) paste(.chr(d$plotID), .chr(d$individualID), sep = "\r")
# Plant-level key (contract v3): a lettered secondary bole carries its parent's
# tag in plant_id, so plotID + plant_id is one plant. .plant_key stays the BOLE
# key. Bundles without plant_id fall back to individualID.
.plant_id_vec <- function(d) {
  id <- .chr(d$individualID)
  if ("plant_id" %in% names(d)) {
    pid <- .chr(d$plant_id)
    take <- !is.na(pid) & nzchar(pid)
    id[take] <- pid[take]
  }
  id
}
.plant_id_key <- function(d) paste(.chr(d$plotID), .plant_id_vec(d), sep = "\r")
.event_key <- function(d) paste(.chr(d$plotID), .chr(d$eventID), sep = "\r")
# The legacy-schema state: a pre-2020-03-10 event whose samplingImpractical and
# dataCollected fields are blank but that passes every other check. It is not a
# supported state for structure; it is admitted to change history only.
VEG_LEGACY_SCHEMA_STATUS <- "supported_legacy_schema"
VEG_LEGACY_SCHEMA_LABEL <- "legacy schema, used for change only"
# Drop rows superseded by a later-dated row of the same individual in the same
# event, and exact duplicates of another source row. Columns are optional so
# pre-v3 frames pass through unchanged.
.current_rows <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  drop <- rep(FALSE, nrow(d))
  if ("superseded_in_event" %in% names(d)) drop <- drop | d$superseded_in_event %in% TRUE
  if ("duplicate_of_uid" %in% names(d)) drop <- drop | (!is.na(d$duplicate_of_uid) & nzchar(.chr(d$duplicate_of_uid)))
  d[!drop, , drop = FALSE]
}
# Status-only rows carry no growthForm. Fill a blank form from the same bole's
# most recent EARLIER row with a form (ordered by date, then eventID); never
# back-fill from a later row. Undated rows neither give nor receive a form.
.fill_growth_form <- function(d) {
  if (is.null(d) || !nrow(d) || !"growthForm" %in% names(d)) return(d)
  gf <- .chr(d$growthForm)
  blank <- is.na(gf) | !nzchar(trimws(gf))
  if (!any(blank)) return(d)
  ord <- .event_order(d)
  ok <- is.finite(ord$date)
  ev <- ord$id; ev[is.na(ev)] <- ""
  key <- .plant_key(d)
  # Within one bole and date, rows with a form sort before blank rows.
  o <- order(key, ord$date, ev, blank)
  key_s <- key[o]; gf_s <- gf[o]; blank_s <- blank[o]; ok_s <- ok[o]
  src <- ifelse(!blank_s & ok_s, seq_along(o), 0L)
  last <- stats::ave(src, key_s, FUN = cummax)
  fill <- blank_s & ok_s & last > 0L
  gf_s[fill] <- gf_s[last[fill]]
  out <- gf; out[o] <- gf_s
  d$growthForm <- out
  d
}
.ensure_event_columns <- function(d) {
  if (is.null(d)) return(d)
  if (!"plotID" %in% names(d)) d$plotID <- NA_character_
  if (!"eventID" %in% names(d)) {
    # Legacy bundles predate event preservation. Date is only a compatibility
    # token here; opportunity-aware stand estimates require a v3 eventID.
    tok <- if ("date" %in% names(d)) .chr(d$date) else if ("year" %in% names(d)) .chr(d$year) else rep(NA_character_, nrow(d))
    d$eventID <- paste0("LEGACY-DATE-", tok)
    attr(d, "event_key_quality") <- "legacy_date_fallback"
  }
  d
}
.event_order <- function(d) {
  dn <- if ("date" %in% names(d)) .date_num(d$date) else rep(NA_real_, nrow(d))
  if ("year" %in% names(d)) {
    yr <- .num(d$year); dn[!is.finite(dn)] <- yr[!is.finite(dn)] * 366
  }
  dn[!is.finite(dn)] <- -Inf
  list(date = dn, id = .chr(d$eventID))
}
.taxon_name <- function(d) {
  n <- nrow(d); out <- rep(NA_character_, n)
  for (nm in c("taxon_label", "scientificName", "taxonID")) {
    if (!nm %in% names(d)) next
    x <- .chr(d[[nm]]); take <- (is.na(out) | !nzchar(out)) & !is.na(x) & nzchar(trimws(x))
    out[take] <- x[take]
  }
  out[is.na(out) | !nzchar(out)] <- "Unresolved taxon"
  out
}

species_level_only <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  if ("is_species" %in% names(d)) return(d[d$is_species %in% TRUE, , drop = FALSE])
  rank <- tolower(trimws(.chr(d$taxonRank)))
  name <- if ("scientificName" %in% names(d)) .chr(d$scientificName) else rep(NA_character_, nrow(d))
  ok <- !is.na(rank) & rank %in% c("species", "subspecies", "variety", "form")
  named <- !is.na(name) & nzchar(trimws(name))
  ambiguous <- grepl("\\bsp\\.?$", ifelse(is.na(name), "", name), ignore.case = TRUE) |
    grepl("/", ifelse(is.na(name), "", name), fixed = TRUE)
  d[ok & named & !ambiguous, , drop = FALSE]
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
# tree_snapshot(): the latest supported event per plot x individual. A plant can
# have several stem rows in one bout (multi-bole) — all kept here.
# one_per_tree(): collapse to the largest stem per plot x individual (the dot in
# the Size Lab + the size headline). Basal area sums ALL snapshot stems.
# ---------------------------------------------------------------------------
woody_only <- function(d, spec = SIZE_FOREST) {
  if (is.null(d) || !nrow(d)) return(d)
  if (!(spec$col %in% names(d)) || !"growthForm" %in% names(d)) return(d[FALSE, , drop = FALSE])
  x <- .num(d[[spec$col]])
  keep <- d$growthForm %in% spec$forms & is.finite(x) & x > 0 & x >= spec$min
  d[keep, , drop = FALSE]
}
trees_only <- function(d) woody_only(d, SIZE_FOREST)

# Latest event per composite plant. When opportunity rows are supplied, their
# supported plot events define "latest"; this prevents an older live record from
# surviving an explicit later sampled absence. Without opportunities, choose the
# latest record event per plant (legacy-compatible, but not absence-aware).
tree_snapshot <- function(trees, plots = NULL, spec = NULL) {
  if (is.null(trees) || !nrow(trees)) return(trees)
  trees <- .ensure_event_columns(trees)
  trees <- .fill_growth_form(.current_rows(trees))
  if (!nrow(trees)) { attr(trees, "contract_id") <- VEG_CONTRACT_ID; return(trees) }
  trees$.plant_key <- .plant_key(trees)
  ord <- .event_order(trees); trees$.event_date <- ord$date; trees$.event_sort <- ord$id

  if (!is.null(plots) && nrow(plots) && !is.null(spec)) {
    opp <- .latest_opportunities(plots, spec)
    if (!is.null(opp) && nrow(opp)) {
      keys <- .event_key(opp[opp$supported %in% TRUE, , drop = FALSE])
      tr_keys <- .event_key(trees)
      trees <- trees[tr_keys %in% keys, , drop = FALSE]
    }
  }
  if (!nrow(trees)) {
    trees$.plant_key <- NULL; trees$.event_date <- NULL; trees$.event_sort <- NULL
    attr(trees, "contract_id") <- VEG_CONTRACT_ID
    return(trees)
  }
  # Select the latest EVENT first, then bring every source row from that event
  # back into the snapshot. Filtering source rows directly by their measurement
  # date drops legitimate stems when one multi-stem field event spans more than
  # one day. The event is the analytical unit; its rows are atomic.
  event_rows <- trees %>%
    dplyr::group_by(.data$.plant_key, .data$eventID) %>%
    dplyr::summarise(
      .event_date = max(.data$.event_date, na.rm = TRUE),
      .event_sort = .first_known(.data$.event_sort),
      .groups = "drop"
    )
  event_rows$.event_sort[is.na(event_rows$.event_sort)] <- ""
  selected <- event_rows %>%
    dplyr::group_by(.data$.plant_key) %>%
    dplyr::filter(.data$.event_date == max(.data$.event_date, na.rm = TRUE)) %>%
    dplyr::filter(.data$.event_sort == max(.data$.event_sort, na.rm = TRUE)) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  selected_key <- paste(selected$.plant_key, .chr(selected$eventID), sep = "\r")
  tree_key <- paste(trees$.plant_key, .chr(trees$eventID), sep = "\r")
  out <- trees[tree_key %in% selected_key, , drop = FALSE]
  out$.plant_key <- NULL; out$.event_date <- NULL; out$.event_sort <- NULL
  attr(out, "contract_id") <- VEG_CONTRACT_ID
  out
}
one_per_tree <- function(snap, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(snap)
  snap <- .ensure_event_columns(snap)
  snap$.size <- snap[[spec$col]]
  # One row per PLANT (plotID + plant_id): the largest bole stands for it.
  snap$.plant_key <- .plant_id_key(snap)
  out <- snap %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::slice_max(.data$.size, n = 1, with_ties = FALSE, na_rm = FALSE) %>% dplyr::ungroup()
  out$.size <- NULL; out$.plant_key <- NULL; out
}
# Size EXTREMES (tallest / biggest headlines, podium, compare) use only rows
# that pass the shared size screen for that measure: "height" for tallest,
# "diameter" for biggest, "any" for both. Every other summary keeps every row.
# Screen the full history first (with_size_screen): a frame without the
# screen columns is screened here from its own rows, which cannot see the
# plant's other events.
.size_screen_column <- function(measure) {
  switch(match.arg(measure, c("any", "height", "diameter")),
         any = "size_plausible", height = "size_height_ok", diameter = "size_diameter_ok")
}
size_ok_vec <- function(d, measure = "any") {
  if (is.null(d) || !nrow(d)) return(logical())
  column <- .size_screen_column(measure)
  if (column %in% names(d)) d[[column]] %in% TRUE else vst_size_screen(d)[[column]] %in% TRUE
}
size_screened <- function(d, measure = "any") {
  if (is.null(d) || !nrow(d)) return(d)
  d[size_ok_vec(d, measure), , drop = FALSE]
}
with_size_screen <- function(trees) {
  if (is.null(trees) || !nrow(trees)) return(trees)
  sc <- vst_size_screen(trees)
  for (column in names(sc)) trees[[column]] <- sc[[column]]
  trees
}
live_only <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  live <- if ("live" %in% names(d)) d$live %in% TRUE else if ("plantStatus" %in% names(d)) grepl("^Live", d$plantStatus) else rep(FALSE, nrow(d))
  d[live, , drop = FALSE]
}

.latest_opportunities <- function(plots, spec = SIZE_FOREST, plot_types = NULL) {
  if (is.null(plots) || !nrow(plots)) return(NULL)
  p <- .ensure_event_columns(as.data.frame(plots))
  if (!"date" %in% names(p)) p$date <- as.Date(NA)
  if (!"year" %in% names(p)) p$year <- NA_integer_
  for (nm in c("plotType", "nlcdClass", "lat", "lng")) if (!nm %in% names(p)) p[[nm]] <- NA
  if (!is.null(plot_types) && "plotType" %in% names(p)) p <- p[p$plotType %in% plot_types, , drop = FALSE]
  if (!nrow(p)) return(NULL)
  p$.support <- if (spec$support %in% names(p)) .chr(p[[spec$support]]) else "held_opportunity_unknown"
  p$.reason <- if (spec$support_reason %in% names(p)) .chr(p[[spec$support_reason]]) else "Bundle does not preserve event-level opportunity status."
  p$.area <- if (spec$area %in% names(p)) .num(p[[spec$area]]) else NA_real_
  p$.supported <- p$.support %in% VEG_CONTRACT$supported_status & is.finite(p$.area) & p$.area > 0
  p$.event_date <- .event_order(p)$date
  p$.event_sort <- .chr(p$eventID)
  missing_order <- p$.supported & !is.finite(p$.event_date)
  p$.supported[missing_order] <- FALSE
  p$.support[missing_order] <- "held_opportunity_unknown"
  p$.reason[missing_order] <- "Event date/year is missing, so the latest plot event cannot be identified."

  # Latest fully supported census is the canonical snapshot. If a plot has never
  # had one, retain its latest held opportunity so the reason remains visible.
  # Keep diagnostics from the complete opportunity history so a later held
  # attempt cannot disappear behind an older supported census.
  all_p <- p
  p <- p %>% dplyr::group_by(.data$plotID) %>%
    dplyr::arrange(.data$.event_date, .data$.event_sort, .by_group = TRUE) %>%
    dplyr::filter(if (any(.data$.supported)) .data$.supported else dplyr::row_number() == dplyr::n()) %>%
    dplyr::filter(.data$.event_date == max(.data$.event_date, na.rm = TRUE)) %>%
    dplyr::filter(.data$.event_sort == max(.data$.event_sort, na.rm = TRUE)) %>%
    dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
  p$n_held_events <- 0L
  p$n_later_held <- 0L
  p$held_reasons <- NA_character_
  p$later_held_reasons <- NA_character_
  for (i in seq_len(nrow(p))) {
    h <- all_p[all_p$plotID == p$plotID[i] & !(all_p$.supported %in% TRUE), , drop = FALSE]
    if (!nrow(h)) next
    p$n_held_events[i] <- nrow(h)
    p$held_reasons[i] <- paste(unique(stats::na.omit(h$.reason)), collapse = "; ")
    later <- h$.event_date > p$.event_date[i] |
      (h$.event_date == p$.event_date[i] & .chr(h$.event_sort) > .chr(p$.event_sort[i]))
    later[is.na(later)] <- FALSE
    p$n_later_held[i] <- sum(later)
    if (any(later))
      p$later_held_reasons[i] <- paste(unique(stats::na.omit(h$.reason[later])), collapse = "; ")
  }
  p$support_status <- p$.support
  p$support_reason <- p$.reason
  p$area_use <- p$.area
  p$supported <- p$.supported
  p$sampled_absence <- p$support_status == VEG_CONTRACT$zero_status
  p[, setdiff(names(p), c(".support", ".reason", ".area", ".supported", ".event_date", ".event_sort")), drop = FALSE]
}

# Temporal summaries may use only measurement-bearing events whose matching
# channel opportunity is supported with a positive event-specific denominator.
# Explicit absences have no plant measurement row; every held state is excluded.
# allow_legacy = TRUE also admits supported_legacy_schema events (pre-2020
# schema, NEON changelog 16306); only the change paths (growth, mortality,
# trajectory, QC) pass it. Superseded and duplicate rows are always dropped, and
# a blank growthForm is filled forward from the bole's earlier rows first.
.supported_history <- function(trees, plots, spec = SIZE_FOREST, allow_legacy = FALSE) {
  if (is.null(trees) || !nrow(trees) || is.null(plots) || !nrow(plots)) return(NULL)
  t <- .ensure_event_columns(as.data.frame(trees))
  t <- .fill_growth_form(.current_rows(t))
  if (!nrow(t)) return(NULL)
  p <- .ensure_event_columns(as.data.frame(plots))
  if (!(spec$support %in% names(p)) || !(spec$area %in% names(p))) return(NULL)
  support <- .chr(p[[spec$support]])
  area <- .num(p[[spec$area]])
  admitted <- if (isTRUE(allow_legacy)) c("sampled_with_records", VEG_LEGACY_SCHEMA_STATUS) else "sampled_with_records"
  keep <- support %in% admitted & is.finite(area) & area > 0 &
    !is.na(p$plotID) & nzchar(.chr(p$plotID)) & !is.na(p$eventID) & nzchar(.chr(p$eventID))
  keys <- unique(.event_key(p[keep, , drop = FALSE]))
  if (!length(keys)) return(NULL)
  out <- .option_a_tree_rows(t[.event_key(t) %in% keys, , drop = FALSE], spec)
  if (is.null(out) || !nrow(out)) NULL else out
}

# Contract v3, Option A: the tree channel's population is boles >= 10 cm DBH.
# A lettered secondary bole under 10 cm no longer holds its plot-event, but it
# stays out of every tree-channel estimand: basal area, density and QMD (via
# woody_only) and growth, mortality and the status snapshot (here). The rows
# remain in the bundle, the long export and the size QC.
.option_a_tree_rows <- function(d, spec = SIZE_FOREST) {
  if (is.null(d) || !nrow(d) || !identical(spec$channel, "tree_dbh") ||
      !all(c("individualID", "stemDiameter") %in% names(d))) return(d)
  dbh <- .num(d$stemDiameter)
  small_secondary <- grepl("[A-Za-z]$", .chr(d$individualID)) & is.finite(dbh) & dbh < TREE_DBH_MIN
  d[!small_secondary, , drop = FALSE]
}

.selected_stems <- function(snap, selected, spec = SIZE_FOREST, live = TRUE) {
  if (is.null(snap)) return(data.frame())
  if (!nrow(snap) || is.null(selected) || !nrow(selected)) return(snap[FALSE, , drop = FALSE])
  s <- .ensure_event_columns(snap)
  s <- s[.event_key(s) %in% .event_key(selected[selected$supported %in% TRUE & !selected$sampled_absence, , drop = FALSE]), , drop = FALSE]
  if (live) s <- live_only(s)
  woody_only(s, spec)
}

# ---------------------------------------------------------------------------
# Stand structure (per ha) — basal area and stem density need the plot's sampled
# area for the paradigm (plots$area_trees for trees, plots$area_shrub for shrubs).
# Computed per plot then summarised to the site (mean per ha), so a big plot
# doesn't dominate. Live stems with a real size only. Basal area uses the
# paradigm's diameter — DBH for trees, basal diameter for shrubs (basal cover).
# ---------------------------------------------------------------------------
stand_by_plot <- function(snap, plots, spec = SIZE_FOREST, plot_types = NULL) {
  selected <- .latest_opportunities(plots, spec, plot_types)
  if (is.null(selected) || !nrow(selected)) return(NULL)
  s <- .selected_stems(snap, selected, spec, live = TRUE)
  if (!is.null(s) && nrow(s)) {
    s$.d <- .num(s[[spec$col]])
    s$ba_m2 <- pi * (s$.d / 200)^2
    s$.plant_key <- .plant_id_key(s)
    obs <- s %>% dplyr::group_by(.data$plotID, .data$eventID) %>%
      dplyr::summarise(stems = dplyr::n(), plants = dplyr::n_distinct(.data$.plant_key),
                       ba_m2 = sum(.data$ba_m2), sumD2 = sum(.data$.d^2), .groups = "drop")
  } else {
    obs <- data.frame(plotID = character(), eventID = character(), stems = integer(), plants = integer(),
                      ba_m2 = numeric(), sumD2 = numeric(), stringsAsFactors = FALSE)
  }
  keep <- intersect(c("plotID", "eventID", "date", "year", "plotType", "nlcdClass", "lat", "lng",
                      "tree_records", "shrub_records", "tree_invalid_metric_records",
                      "shrub_invalid_metric_records", "area_use", "support_status",
                      "support_reason", "supported", "sampled_absence",
                      "n_held_events", "n_later_held", "held_reasons",
                      "later_held_reasons", "tree_channel_complete", "shrub_channel_complete",
                      "disturbance_classes", "n_status_lost_burned", "n_status_removed"), names(selected))
  per <- dplyr::left_join(selected[, keep, drop = FALSE], obs, by = c("plotID", "eventID"))
  snap_keys <- if (!is.null(snap) && nrow(snap)) unique(.event_key(.ensure_event_columns(snap))) else character()
  mismatch <- per$supported %in% TRUE & per$support_status == "sampled_with_records" & !(.event_key(per) %in% snap_keys)
  if (any(mismatch)) {
    per$supported[mismatch] <- FALSE
    per$support_status[mismatch] <- "held_snapshot_event_mismatch"
    per$support_reason[mismatch] <- "Selected event records are absent from the supplied snapshot; call tree_snapshot(trees, plots, spec)."
  }
  zero_ok <- per$supported %in% TRUE
  for (nm in c("stems", "plants", "ba_m2", "sumD2")) per[[nm]][zero_ok & is.na(per[[nm]])] <- 0
  per$area_ha <- ifelse(per$supported, per$area_use / 10000, NA_real_)
  per$ba_ha <- ifelse(per$supported, per$ba_m2 / per$area_ha, NA_real_)
  per$density_ha <- ifelse(per$supported, per$stems / per$area_ha, NA_real_)
  per$qmd <- ifelse(per$supported & per$stems > 0, sqrt(per$sumD2 / per$stems), NA_real_)
  per$contract_id <- VEG_CONTRACT_ID
  per$channel <- spec$channel
  per$metric_kind <- spec$metric_kind
  per
}
stand_site <- function(snap, plots, spec = SIZE_FOREST, plot_types = NULL) {
  per <- stand_by_plot(snap, plots, spec, plot_types); if (is.null(per)) return(NULL)
  ok <- per[per$supported %in% TRUE, , drop = FALSE]
  if (!nrow(ok)) return(NULL)
  n <- nrow(ok)
  se <- function(x) if (n > 1) stats::sd(x, na.rm = TRUE) / sqrt(n) else NA_real_
  n_stems <- sum(ok$stems, na.rm = TRUE)
  ba_mean <- mean(ok$ba_ha, na.rm = TRUE); dens_mean <- mean(ok$density_ha, na.rm = TRUE)
  list(ba_ha = ba_mean,
       ba_se = se(ok$ba_ha),
       density_ha = dens_mean,
       density_se = se(ok$density_ha),
       # Quadratic mean diameter from the same equal-plot means as basal area
       # and density (Curtis & Marshall 2000), so the three headline numbers
       # agree: QMD = sqrt(40000 * BA / (pi * density)).
       qmd = if (n_stems > 0 && is.finite(dens_mean) && dens_mean > 0) sqrt(40000 * ba_mean / (pi * dens_mean)) else NA_real_,
       n_plots = n,
       n_absence = sum(ok$sampled_absence %in% TRUE),
       n_excluded = sum(!(per$supported %in% TRUE)),
       n_later_held = sum(per$n_later_held %||% 0L, na.rm = TRUE),
       support_status = if (all(per$supported %in% TRUE) && !any((per$n_later_held %||% 0L) > 0, na.rm = TRUE)) "supported" else "partial_support",
       support_reason = {
         reasons <- c(per$support_reason[!(per$supported %in% TRUE)],
                      per$later_held_reasons[(per$n_later_held %||% 0L) > 0])
         reasons <- unique(stats::na.omit(reasons[nzchar(.chr(reasons))]))
         if (length(reasons)) paste(reasons, collapse = "; ") else "All selected plot events are supported."
       },
       contract_id = VEG_CONTRACT_ID, channel = spec$channel, metric_kind = spec$metric_kind,
       estimate_scope = "equal-plot mean across latest supported sampled plot events",
       per_plot = per)
}

stand_support_message <- function(st, plots = NULL, spec = SIZE_FOREST) {
  if (!is.null(st)) {
    if (identical(st$support_status, "supported"))
      return(sprintf("Uses the latest supported census for %d plots (%d explicit sampled absences).", st$n_plots, st$n_absence %||% 0L))
    return(sprintf("Uses the latest supported census for %d plots; %d plots have no supported census and %d later attempts are held. %s",
      st$n_plots, st$n_excluded %||% 0L, st$n_later_held %||% 0L,
      st$support_reason %||% "Reason unavailable."))
  }
  p <- .latest_opportunities(plots, spec)
  if (is.null(p) || !nrow(p)) return("Held: no event-level sampling-opportunity records are available.")
  why <- unique(stats::na.omit(p$support_reason[!(p$supported %in% TRUE)]))
  paste0("Held: ", if (length(why)) paste(why, collapse = "; ") else "no supported sampled plot event.")
}

# Held-plot disclosure (C5): how many of the site's plots feed the channel's
# stand numbers, and why the rest are held, grouped by the support state of each
# held plot's latest event. Returns "" when no plot is held.
VEG_HELD_REASON <- c(
  held_identity_conflict = "NEON published two different records for one stem",
  held_visit_conflict = "NEON published two sampling records for one visit",
  held_sampling_impractical = "NEON marked sampling impractical",
  held_dendrometer_only = "only dendrometer bands were measured",
  held_missing_area = "the sampled area is missing",
  held_presence_record_conflict = "the presence field and the records disagree",
  held_metric_invalid = "a required diameter is missing or invalid",
  held_opportunity_unknown = "the sampling opportunity is unknown",
  held_opportunity_source_missing = "NEON published no sampling record for the visit",
  held_snapshot_event_mismatch = "the visit does not match the snapshot",
  supported_legacy_schema = "only censuses on or before 2020-03-10 exist, and those feed change only")
held_plot_disclosure <- function(snap, plots, spec = SIZE_FOREST, site = NULL) {
  if (is.null(plots) || !nrow(plots) || !"plotID" %in% names(plots)) return("")
  all_plots <- unique(stats::na.omit(.chr(plots$plotID)))
  N <- length(all_plots); if (!N) return("")
  per <- stand_by_plot(snap, plots, spec)
  ok <- if (is.null(per)) character() else unique(.chr(per$plotID[per$supported %in% TRUE]))
  K <- sum(all_plots %in% ok); held <- N - K
  if (held <= 0L) return("")
  hp <- if (is.null(per)) data.frame(plotID = character(), eventID = character(), support_status = character()) else
    per[!(per$supported %in% TRUE), c("plotID", "eventID", "support_status"), drop = FALSE]
  pl <- .ensure_event_columns(as.data.frame(plots))
  at <- match(.event_key(hp), .event_key(pl))
  # Two plot-level sampling records for one visit hold it as an identity
  # conflict too; say so instead of blaming a stem.
  if ("opportunity_key_conflict" %in% names(pl)) {
    visit <- hp$support_status %in% "held_identity_conflict" & pl$opportunity_key_conflict[at] %in% TRUE
    hp$support_status[visit] <- "held_visit_conflict"
  }
  state <- .chr(hp$support_status[match(setdiff(all_plots, ok), hp$plotID)])
  state[is.na(state) | !(state %in% names(VEG_HELD_REASON))] <- "held_opportunity_unknown"
  tab <- table(state); tab <- tab[order(-as.integer(tab), names(tab))]
  conflict_col <- if (identical(spec$channel, "shrub_sapling_basal")) "shrub_identity_conflict_keys" else "tree_identity_conflict_keys"
  phrase <- function(st) {
    out <- VEG_HELD_REASON[[st]]
    if (identical(st, "held_identity_conflict") && conflict_col %in% names(pl)) {
      rows <- at[hp$support_status %in% st]
      S <- sum(.num(pl[[conflict_col]][rows]), na.rm = TRUE)
      if (S > 0) out <- sprintf("%s, at %s %s", out, .vh_count(S), .plural(S, "stem"))
    }
    out
  }
  measure <- if (identical(spec$channel, "shrub_sapling_basal")) "Basal cover" else "Basal area"
  where <- if (is.null(site) || is.na(site) || !nzchar(site)) sprintf("%d %s", N, .plural(N, "plot")) else
    sprintf("%s's %d %s", site, N, .plural(N, "plot"))
  verb <- if (held == 1L) "is" else "are"
  lead <- if (K > 0L) sprintf("%s is from %d of %s; the other %d %s held", measure, K, where, held, verb) else
    sprintf("%s is from none of %s; %s held", measure, where, if (held == 1L) "it is" else sprintf("all %d are", held))
  why <- if (length(tab) == 1L) sprintf(" because %s", phrase(names(tab)[1])) else
    paste0(": ", paste(sprintf("%d because %s", as.integer(tab), vapply(names(tab), phrase, character(1))), collapse = "; "))
  paste0(lead, why, ". Held plots are not zeros.")
}

# Cacti, yuccas and ocotillos (C6) are measured in NEON's separate non-woody
# table. One line names how many of the site's plots recorded each group at
# their latest visit with a published sampling record.
separate_table_line <- function(plots, site = NULL) {
  if (is.null(plots) || !nrow(plots) || !"plotID" %in% names(plots)) return("")
  groups <- c(cactiPresent = "cacti", yuccasPresent = "yucca", ocotillosPresent = "ocotillo")
  groups <- groups[names(groups) %in% names(plots)]
  if (!length(groups)) return("")
  present_vals <- c("Y", "Present - sampled", "Present - sampling criteria not met")
  p <- .ensure_event_columns(as.data.frame(plots))
  N <- length(unique(stats::na.omit(.chr(p$plotID))))
  published <- if ("opportunity_source_missing" %in% names(p)) !(p$opportunity_source_missing %in% TRUE) else rep(TRUE, nrow(p))
  p <- p[published & !is.na(p$plotID), , drop = FALSE]
  if (!nrow(p)) return("")
  day <- if ("date" %in% names(p)) .date_num(p$date) else rep(NA_real_, nrow(p))
  day[!is.finite(day)] <- -Inf
  ev <- if ("eventID" %in% names(p)) .chr(p$eventID) else rep("", nrow(p))
  p <- p[order(.chr(p$plotID), -day, -xtfrm(ev)), , drop = FALSE]
  latest <- p[!duplicated(.chr(p$plotID)), , drop = FALSE]
  n <- vapply(names(groups), function(cl) sum(trimws(.chr(latest[[cl]])) %in% present_vals), integer(1))
  keep <- n > 0L
  if (!any(keep)) return("")
  lab <- unname(groups[keep]); n <- unname(n[keep])
  where <- if (is.null(site) || is.na(site) || !nzchar(site)) "plots" else paste(site, "plots")
  first <- sprintf("%s%s recorded in %d of %d %s", toupper(substr(lab[1], 1, 1)), substring(lab[1], 2), n[1], N, where)
  rest <- if (length(lab) > 1L) paste0(", ", paste(sprintf("%s in %d", lab[-1], n[-1]), collapse = ", ")) else ""
  paste0(first, rest, ", measured in a NEON table this app does not load.")
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
  if (is.null(snap) || (!nrow(snap) && is.null(plots))) return(NULL)
  per <- if (!is.null(plots)) stand_by_plot(snap, plots, spec) else NULL
  s <- if (!is.null(per)) .selected_stems(snap, per, spec, live = TRUE) else woody_only(live_only(snap), spec)
  bk <- size_breaks(spec)
  labs <- factor(bk$labs, levels = bk$labs)
  if (is.null(s) || !nrow(s)) {
    if (is.null(per) || !any(per$supported %in% TRUE)) return(NULL)
    s <- s[FALSE, , drop = FALSE]
  }
  s$cls <- if (nrow(s)) cut(s[[spec$col]], breaks = bk$brks, labels = bk$labs, right = FALSE) else factor(character(), levels = bk$labs)
  out <- data.frame(cls = labs, stems = as.integer(table(factor(s$cls, levels = bk$labs))[bk$labs]))
  if (is.null(per)) return(out)
  # Equal-plot mean (hk, 2026-10-21): each supported plot's class count over
  # that plot's own area, zero-filled, averaged across supported plots, so the
  # bars sum to the headline density. The SE is plot-to-plot.
  ok <- per[per$supported %in% TRUE & is.finite(per$area_ha) & per$area_ha > 0, , drop = FALSE]
  K <- nrow(ok)
  if (!K) { out$stems_ha <- NA_real_; out$stems_ha_se <- NA_real_; out$n_plots_present <- 0L; out$n_plots <- 0L; return(out) }
  pk <- paste(ok$plotID, ok$eventID, sep = "\r")
  sk <- paste(s$plotID, s$eventID, sep = "\r")
  counts <- matrix(0, nrow = K, ncol = length(bk$labs), dimnames = list(pk, bk$labs))
  if (nrow(s)) {
    tab <- table(factor(sk, levels = pk), factor(s$cls, levels = bk$labs))
    counts[, ] <- as.numeric(tab)
  }
  dens <- counts / ok$area_ha
  out$stems_ha <- as.numeric(colMeans(dens))
  out$stems_ha_se <- if (K > 1) as.numeric(apply(dens, 2, stats::sd) / sqrt(K)) else NA_real_
  out$n_plots_present <- as.integer(colSums(counts > 0))
  out$n_plots <- K
  out
}

# ---------------------------------------------------------------------------
# Per-plant growth: diameter increment between the first and last live bouts,
# annualised. De-pseudoreplicated (one rate per plant). Negative increments
# (shrinkage = measurement error / bole damage) are flagged, not deleted. Uses
# the paradigm's diameter (DBH for trees, basal for shrubs).
# ---------------------------------------------------------------------------
# Growth gates (hk, 2026-10-21): a median needs 20 comparable remeasured plants
# in 3 plots (at n = 20 the median's distribution-free 95% interval is about
# as wide as the IQR); pairs under 0.9 years apart are excluded.
VEG_GROWTH_MIN_PLANTS <- 20L
VEG_GROWTH_MIN_PLOTS <- 3L
VEG_GROWTH_MIN_YEARS <- 0.9
VEG_GROWTH_SCREEN <- c(-2, 5)
# Measurement-point tolerance (C2): a blank changedMeasurementLocation counts as
# a moved point only when the measurement height changed by more than 5 cm.
VEG_MH_TOLERANCE_CM <- 5

tree_growth <- function(trees, spec = SIZE_FOREST, plots = NULL) {
  g <- .supported_history(trees, plots, spec, allow_legacy = TRUE)
  if (is.null(g) || !nrow(g)) return(NULL)
  if (!(spec$col %in% names(g)) || !all(c("plotID", "individualID", "growthForm") %in% names(g))) return(NULL)
  all_channel <- g[g$growthForm %in% spec$forms & is.finite(.date_num(g$date)), , drop = FALSE]
  if ("permanent" %in% names(all_channel)) all_channel <- all_channel[all_channel$permanent %in% TRUE, , drop = FALSE]
  resurrection_keys <- character()
  if (nrow(all_channel) && "plantStatus" %in% names(all_channel)) {
    all_channel$.plant_key <- .plant_key(all_channel)
    all_channel$.event_date <- .date_num(all_channel$date)
    status <- .chr(all_channel$plantStatus)
    all_channel$.live_state <- grepl("^Live", status)
    all_channel$.dead_state <- grepl("Dead|Downed", status, ignore.case = TRUE)
    sev <- all_channel %>% dplyr::group_by(.data$.plant_key, .data$eventID) %>%
      dplyr::summarise(date = as.Date(min(.data$.event_date), origin = "1970-01-01"),
                       live = any(.data$.live_state), dead = all(.data$.dead_state), .groups = "drop") %>%
      dplyr::arrange(.data$.plant_key, .data$date, .data$eventID) %>%
      dplyr::group_by(.data$.plant_key) %>%
      dplyr::summarise(resurrected = any(.data$live & cumsum(.data$dead) > 0), .groups = "drop")
    resurrection_keys <- sev$.plant_key[sev$resurrected %in% TRUE]
  }
  g$.d <- .num(g[[spec$col]])
  g <- g[g$growthForm %in% spec$forms & is.finite(g$.d) & g$.d > 0 & g$.d >= spec$min, , drop = FALSE]
  g <- live_only(g)
  if ("permanent" %in% names(g)) g <- g[g$permanent %in% TRUE, , drop = FALSE]
  if (length(resurrection_keys)) g <- g[!(.plant_key(g) %in% resurrection_keys), , drop = FALSE]
  if (!nrow(g) || !"date" %in% names(g)) return(NULL)
  g <- g[is.finite(.date_num(g$date)), , drop = FALSE]
  if (!nrow(g)) return(NULL)
  if (!"scientificName" %in% names(g)) g$scientificName <- NA_character_
  if (!"tempStemID" %in% names(g)) g$tempStemID <- NA_character_
  mh_col <- if (identical(spec$channel, "shrub_sapling_basal")) "basalMeasurementHeight" else "measurementHeight"
  if (!mh_col %in% names(g)) g[[mh_col]] <- NA_real_
  if (!"changedMeasurementLocation" %in% names(g)) g$changedMeasurementLocation <- NA_character_
  g$.mh <- .num(g[[mh_col]])
  # Measurement-point rule (C2): compare the code case-insensitively after
  # stripping non-letters; a present code other than "noChange" means moved.
  loc <- gsub("[^a-z]", "", tolower(.chr(g$changedMeasurementLocation)))
  g$.pom_present <- !is.na(loc) & nzchar(loc)
  g$.pom_changed <- g$.pom_present & loc != "nochange"
  g$.plant_key <- .plant_key(g)
  g$.event_date <- .date_num(g$date)

  ev <- g %>% dplyr::group_by(.data$plotID, .data$individualID, .data$.plant_key, .data$eventID) %>%
    dplyr::summarise(
      date = as.Date(min(.data$.event_date), origin = "1970-01-01"),
      scientificName = .first_known(.data$scientificName),
      diameter = sqrt(sum(.data$.d^2, na.rm = TRUE)),
      n_stems = dplyr::n(),
      mh = if (any(is.finite(.data$.mh))) mean(.data$.mh[is.finite(.data$.mh)]) else NA_real_,
      mh_span = if (sum(is.finite(.data$.mh)) > 1) diff(range(.data$.mh[is.finite(.data$.mh)])) else 0,
      location_present = any(.data$.pom_present),
      location_changed = any(.data$.pom_changed),
      .groups = "drop")

  # NEON tempStemID is not stable across years for multi-bole shrubs/saplings;
  # an event-level sum would manufacture a basal "trajectory" from unalignable
  # stems. Withhold those plants rather than imply stem continuity.
  if (identical(spec$channel, "shrub_sapling_basal")) {
    bad <- unique(ev$.plant_key[ev$n_stems > 1])
    ev <- ev[!(ev$.plant_key %in% bad), , drop = FALSE]
  }
  if (!nrow(ev)) return(NULL)
  ev$.event_sort <- .chr(ev$eventID)
  ev <- ev[order(ev$.plant_key, ev$date, ev$.event_sort), , drop = FALSE]
  # For each consecutive pair of a bole's events the point moved if the LATER
  # event carries a code other than noChange; with no code, if the measurement
  # height changed by more than VEG_MH_TOLERANCE_CM. The first event of a bole
  # starts no pair.
  prev_mh <- c(NA_real_, ev$mh[-nrow(ev)])
  first_of_bole <- !duplicated(ev$.plant_key)
  height_moved <- is.finite(ev$mh) & is.finite(prev_mh) & abs(ev$mh - prev_mh) > VEG_MH_TOLERANCE_CM
  ev$pair_moved <- !first_of_bole & ifelse(ev$location_present, ev$location_changed, height_moved)

  out <- ev %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::filter(dplyr::n_distinct(.data$eventID) >= 2) %>%
    dplyr::summarise(
      plotID = dplyr::first(.data$plotID), individualID = dplyr::first(.data$individualID),
      scientificName = .first_known(.data$scientificName),
      event0 = dplyr::first(.data$eventID), event1 = dplyr::last(.data$eventID),
      date0 = dplyr::first(.data$date), date1 = dplyr::last(.data$date),
      d0 = dplyr::first(.data$diameter), d1 = dplyr::last(.data$diameter),
      mh0 = dplyr::first(.data$mh), mh1 = dplyr::last(.data$mh),
      yrs = as.numeric(dplyr::last(.data$date) - dplyr::first(.data$date)) / 365.25,
      mh_change = any(.data$pair_moved %in% TRUE) || any(.data$mh_span > VEG_MH_TOLERANCE_CM),
      .groups = "drop") %>%
    dplyr::filter(.data$yrs > 0) %>%
    dplyr::mutate(growth_cm_yr = (.data$d1 - .data$d0) / .data$yrs,
                  shrank = .data$growth_cm_yr < -0.1,
                  # Remeasured less than 0.9 years apart (hk): kept for QC,
                  # left out of growth summaries.
                  short_interval = .data$yrs < VEG_GROWTH_MIN_YEARS,
                  contract_id = VEG_CONTRACT_ID)
  out$.plant_key <- NULL
  if (!nrow(out)) NULL else out
}

# one plant's full preserved measurement history (shown as records on the card)
tree_history <- function(trees, id, plotID = NULL) {
  if (is.null(trees) || is.null(id)) return(NULL)
  h <- trees[!is.na(trees$individualID) & trees$individualID == id, , drop = FALSE]
  if (!is.null(plotID)) h <- h[h$plotID == plotID, , drop = FALSE]
  if (!nrow(h)) return(NULL)
  if (length(unique(stats::na.omit(h$plotID))) > 1) return(NULL)
  h <- .ensure_event_columns(h)
  keep <- intersect(c("plotID", "eventID", "individualID", "tempStemID", "date", "year", "scientificName",
                      "stemDiameter", "basalStemDiameter", "height", "plantStatus", "live", "permanent", "growthForm", "canopyPosition",
                      "measurementHeight", "basalMeasurementHeight", "changedMeasurementLocation",
                      "opportunity_source_missing"), names(h))
  h[order(.date_num(h$date), .chr(h$eventID), .chr(h$tempStemID)), keep, drop = FALSE]
}

# One plant's event-aware diameter trajectory. Full-plot multi-bole tree events
# use equivalent diameter sqrt(sum(d^2)). Multi-stem basal trajectories are
# withheld because shrub/sapling tempStemID cannot be aligned across years.
tree_trajectory <- function(trees, id, col, plots = NULL) {
  if (is.null(trees) || is.null(id) || is.null(col)) return(NULL)
  h <- trees[!is.na(trees$individualID) & trees$individualID == id &
               !is.na(trees$date), , drop = FALSE]
  if (!nrow(h) || !(col %in% names(h))) return(NULL)
  if (length(unique(stats::na.omit(h$plotID))) > 1) return(NULL)
  h <- .ensure_event_columns(h)
  spec <- if (identical(col, "basalStemDiameter")) SIZE_SHRUB else SIZE_FOREST
  if (!is.null(plots)) {
    h <- .supported_history(h, plots, spec, allow_legacy = TRUE)
    if (is.null(h) || !nrow(h)) return(NULL)
  }
  h$.d <- h[[col]]
  live_ok <- if ("live" %in% names(h)) h$live %in% TRUE else rep(TRUE, nrow(h))
  g <- h[h$growthForm %in% spec$forms & is.finite(h$.d) & h$.d > 0 & h$.d >= spec$min & live_ok, , drop = FALSE]
  if (!nrow(g)) return(NULL)
  # A line is itself a change claim. Apply the same point-of-measurement guard
  # used by tree_growth(), not merely its numeric rate filter, so the chart and
  # evidence chips cannot imply like-for-like change after the field point moved.
  if (!is.null(plots)) {
    point_check <- tree_growth(h, spec, plots)
    if (!is.null(point_check) && nrow(point_check) &&
        any(point_check$mh_change %in% TRUE)) return(NULL)
  }
  per_date <- g %>% dplyr::group_by(.data$eventID) %>%
    dplyr::summarise(dbh = sqrt(sum(.data$.d^2, na.rm = TRUE)),
                     date = as.Date(min(.date_num(.data$date)), origin = "1970-01-01"),
                     n_stems = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(.data$date, .data$eventID)
  if (identical(spec$channel, "shrub_sapling_basal") && any(per_date$n_stems > 1)) return(NULL)
  list(per_date = as.data.frame(per_date),
       raw = data.frame(eventID = g$eventID, date = g$date, d = g$.d)[order(.date_num(g$date), .chr(g$eventID)), , drop = FALSE],
       contract_id = VEG_CONTRACT_ID)
}

# Summary of comparable growth for a channel, with its gate. Comparable means
# an unchanged measurement point, at least 0.9 years between censuses, and a
# change inside the -2 to +5 cm/yr review screen (outside it is kept in the
# data and counted, not summarised).
growth_summary <- function(g, plots = NULL) {
  empty <- list(status = "none", n_plants = 0L, n_plots = 0L, n_tower = 0L,
                median = NA_real_, q25 = NA_real_, q75 = NA_real_,
                n_mh_excluded = 0L, n_offscreen = 0L, n_short_interval = 0L,
                share_decrease = NA_real_)
  if (is.null(g) || !nrow(g)) return(empty)
  short <- if ("short_interval" %in% names(g)) g$short_interval %in% TRUE else rep(FALSE, nrow(g))
  mh <- g$mh_change %in% TRUE
  fin <- is.finite(g$growth_cm_yr)
  usable <- !mh & !short & fin
  on_screen <- usable & g$growth_cm_yr >= VEG_GROWTH_SCREEN[1] & g$growth_cm_yr <= VEG_GROWTH_SCREEN[2]
  clean <- g[on_screen, , drop = FALSE]
  plots_in <- unique(.chr(clean$plotID))
  ptype <- if (!is.null(plots) && all(c("plotID", "plotType") %in% names(plots)))
    tolower(.chr(plots$plotType[match(plots_in, plots$plotID)])) else rep(NA_character_, length(plots_in))
  out <- empty
  out$n_plants <- nrow(clean); out$n_plots <- length(plots_in)
  out$n_tower <- sum(ptype %in% "tower")
  out$n_mh_excluded <- sum(mh); out$n_short_interval <- sum(short & !mh)
  out$n_offscreen <- sum(usable & !on_screen)
  out$status <- if (out$n_plants >= VEG_GROWTH_MIN_PLANTS && out$n_plots >= VEG_GROWTH_MIN_PLOTS) "reported" else "insufficient"
  if (identical(out$status, "reported")) {
    out$median <- stats::median(clean$growth_cm_yr)
    qq <- stats::quantile(clean$growth_cm_yr, c(0.25, 0.75), names = FALSE)
    out$q25 <- qq[1]; out$q75 <- qq[2]
    out$share_decrease <- mean(clean$growth_cm_yr < -0.1)
  }
  out
}

# ---------------------------------------------------------------------------
# Per-plant QC flags from its history, ranked. Every flag is "verify", not "wrong".
# Diameter decreases can reflect biology, damage, a changed measurement point,
# or other measurement differences. Keep the observation, flag it, and explain.
# ---------------------------------------------------------------------------
tree_qc_flags <- function(hist, spec = SIZE_FOREST, plots = NULL) {
  flags <- list(); add <- function(level, text) flags[[length(flags) + 1L]] <<- list(level = level, text = text)
  spikes <- .size_spike_rows(hist, spec)
  if (nrow(spikes)) {
    add("warn", sprintf(
      "%s %s out of line with this plant's other records or with physical bounds (%s). %s in every summary but %s left out of the matching tallest or biggest headline; verify, not wrong.",
      paste(sprintf("The %s record (%s, %s)", ifelse(is.na(spikes$year), "undated", spikes$year),
                    ifelse(is.na(spikes$height_m), "no height", paste(spikes$height_m, "m")),
                    ifelse(is.na(spikes$diameter_cm), "no diameter", paste(spikes$diameter_cm, "cm"))), collapse = "; "),
      if (nrow(spikes) == 1L) "is" else "are",
      paste(unique(spikes$screen_reason), collapse = "; "),
      if (nrow(spikes) == 1L) "It stays" else "They stay",
      if (nrow(spikes) == 1L) "is" else "are"))
  }
  if (!is.null(hist) && nrow(hist) &&
      "opportunity_source_missing" %in% names(hist)) {
    missing <- hist$opportunity_source_missing %in% TRUE
    if (any(missing)) {
      held_events <- if ("eventID" %in% names(hist)) {
        dplyr::n_distinct(hist$eventID[missing])
      } else {
        sum(missing)
      }
      add("info", sprintf(
        "%d recorded event%s %s preserved but held from comparisons because no published plot-opportunity row accompanies it.",
        held_events, if (held_events == 1L) "" else "s",
        if (held_events == 1L) "is" else "are"
      ))
    }
  }
  h <- .supported_history(hist, plots, spec, allow_legacy = TRUE)
  if (is.null(h) || !nrow(h)) {
    add("info", "No supported measurement-bearing event is available for a longitudinal check.")
    return(flags)
  }
  dated <- is.finite(.date_num(h$date))
  if (any(!dated)) {
    add("info", sprintf(
      "%d supported measurement row%s lack%s a usable date and %s excluded from longitudinal ordering.",
      sum(!dated), if (sum(!dated) == 1L) "" else "s",
      if (sum(!dated) == 1L) "s" else "",
      if (sum(!dated) == 1L) "is" else "are"
    ))
    h <- h[dated, , drop = FALSE]
  }
  if (!nrow(h)) {
    add("info", "No dated supported event is available for a longitudinal check.")
    return(flags)
  }
  if (dplyr::n_distinct(h$eventID) < 2) {
    add("info", "Measured in one event, so a longitudinal growth check does not apply."); return(flags)
  }
  st <- .chr(h$plantStatus); h$.live <- grepl("^Live", st); h$.dead <- grepl("Dead|Downed", st, ignore.case = TRUE)
  ev <- h %>% dplyr::group_by(.data$eventID) %>%
    dplyr::summarise(date = as.Date(min(.date_num(.data$date)), origin = "1970-01-01"),
                     live = any(.data$.live), dead = all(.data$.dead), .groups = "drop") %>%
    dplyr::arrange(.data$date, .data$eventID)
  if (any(ev$live & cumsum(ev$dead) > 0))
    add("high", "Status returned to Live after an event where every recorded stem was dead; verify the tag and event records.")

  if (!"live" %in% names(h)) h$live <- grepl("^Live", h$plantStatus)
  if (!"permanent" %in% names(h)) h$permanent <- grepl("^NEON", h$individualID)
  if (!"scientificName" %in% names(h)) h$scientificName <- NA_character_
  g <- tree_growth(h, spec, plots)
  if (is.null(g) && identical(spec$channel, "shrub_sapling_basal")) {
    nstem <- h %>% dplyr::count(.data$eventID, name = "n")
    if (any(nstem$n > 1)) add("info", "Multi-stem basal trajectory withheld: tempStemID cannot be aligned reliably across years for shrubs/saplings.")
    return(flags)
  }
  if (!is.null(g) && nrow(g)) {
    if (any(g$mh_change)) add("info", "The diameter measurement point changed between events, so that increment is kept for QC but excluded from growth summaries.")
    clean <- g[!g$mh_change, , drop = FALSE]
    if (any(clean$growth_cm_yr > 5, na.rm = TRUE)) add("warn", sprintf("Diameter grew >5 cm/yr (max %.1f cm/yr); verify the measurement point and tag.", max(clean$growth_cm_yr, na.rm = TRUE)))
    if (any(clean$growth_cm_yr < -0.1, na.rm = TRUE)) add("info", "Diameter decreased across comparable events; this can be real, but should be verified against the field record.")
  }
  flags
}

# ---------------------------------------------------------------------------
# Per-SPECIES structure (composition, leaderboard, the "champion plant").
# ---------------------------------------------------------------------------
species_structure <- function(snap, plots, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  per <- stand_by_plot(snap, plots, spec)
  if (is.null(per)) return(NULL)
  # Keep unidentified and coarsely identified stems in the physical totals; an
  # unresolved taxon is not a reason to erase a measured stem.
  s <- .selected_stems(snap, per, spec, live = TRUE)
  if (is.null(s) || !nrow(s)) return(NULL)
  s$.taxon_label <- .taxon_name(s)
  if (!"family" %in% names(s)) s$family <- NA_character_
  ba <- s; ba$.d <- ba[[spec$col]]; ba <- ba[is.finite(ba$.d) & ba$.d > 0, ]
  ba$ba_m2 <- pi * (ba$.d / 200)^2
  area <- per[per$supported %in% TRUE, c("plotID", "eventID", "area_ha"), drop = FALSE]
  by_plot <- ba %>%
    dplyr::group_by(.data$.taxon_label, .data$plotID, .data$eventID) %>%
    dplyr::summarise(measured_ba_m2 = sum(.data$ba_m2), .groups = "drop") %>%
    dplyr::left_join(area, by = c("plotID", "eventID")) %>%
    dplyr::mutate(plot_ba_m2_ha = .data$measured_ba_m2 / .data$area_ha)
  n_supported <- dplyr::n_distinct(paste(area$plotID, area$eventID, sep = "\r"))
  ba_by <- by_plot %>% dplyr::group_by(.data$.taxon_label) %>%
    dplyr::summarise(
      measured_ba_m2 = sum(.data$measured_ba_m2),
      ba_m2_ha = sum(.data$plot_ba_m2_ha) / n_supported,
      .groups = "drop")
  # Plants per taxon count every plant with a stem of that taxon (plotID +
  # plant_id), the same rule as the bundle's taxa index, so a multi-bole plant
  # whose boles carry two taxa counts under both. Sizes come from the same
  # stems: the screened maxima, and the mean of each plant's largest stem.
  s$.plant_key <- .plant_id_key(s)
  s$.d <- .num(s[[spec$col]])
  s$.d_ok <- if ("size_diameter_ok" %in% names(s)) s$size_diameter_ok %in% TRUE else rep(TRUE, nrow(s))
  s$.h_ok <- if ("size_height_ok" %in% names(s)) s$size_height_ok %in% TRUE else rep(TRUE, nrow(s))
  if (!"height" %in% names(s)) s$height <- NA_real_
  plant_size <- s %>% dplyr::group_by(.data$.taxon_label, .data$.plant_key) %>%
    dplyr::summarise(plant_d = smax(.data$.d), .groups = "drop") %>%
    dplyr::group_by(.data$.taxon_label) %>%
    dplyr::summarise(mean_dbh = smean(.data$plant_d), .groups = "drop")
  s %>% dplyr::group_by(.data$.taxon_label) %>%
    dplyr::summarise(plants = dplyr::n_distinct(.data$.plant_key),
                     family = mode_chr(.data$family),
                     max_dbh = smax(.data$.d[.data$.d_ok]),
                     max_ht = smax(.data$height[.data$.h_ok]),
                     .groups = "drop") %>%
    dplyr::left_join(plant_size, by = ".taxon_label") %>%
    dplyr::left_join(ba %>% dplyr::group_by(.data$.taxon_label) %>%
                       dplyr::summarise(stems = dplyr::n(), .groups = "drop"), by = ".taxon_label") %>%
    dplyr::left_join(ba_by, by = ".taxon_label") %>%
    dplyr::rename(scientificName = .taxon_label) %>%
    dplyr::arrange(dplyr::desc(.data$ba_m2_ha))
}

# ---------------------------------------------------------------------------
# Per-PLOT (stand) summary — feeds the Map + a stand profile.
# ---------------------------------------------------------------------------
plot_summary_veg <- function(snap, plots, spec = SIZE_FOREST) {
  per <- stand_by_plot(snap, plots, spec); if (is.null(per)) return(NULL)
  s <- .selected_stems(snap, per, spec, live = TRUE)
  if (is.null(s) || !nrow(s)) {
    sp <- data.frame(plotID = character(), eventID = character(),
                     n_taxa = integer(), n_species = integer(),
                     tallest = numeric(), biggest = numeric(),
                     dominant = character(), stringsAsFactors = FALSE)
  } else {
    s$.d <- s[[spec$col]]
    s$.taxon_label <- .taxon_name(s)
    s$.is_species <- if ("is_species" %in% names(s)) s$is_species %in% TRUE else FALSE
    s$.d_ok <- if ("size_diameter_ok" %in% names(s)) s$size_diameter_ok %in% TRUE else rep(TRUE, nrow(s))
    s$.h_ok <- if ("size_height_ok" %in% names(s)) s$size_height_ok %in% TRUE else rep(TRUE, nrow(s))
    sp <- s %>% dplyr::group_by(.data$plotID, .data$eventID) %>%
      dplyr::summarise(n_taxa = dplyr::n_distinct(.data$.taxon_label[!is.na(.data$.taxon_label) & nzchar(.data$.taxon_label)]),
                       n_species = dplyr::n_distinct(.data$.taxon_label[.data$.is_species & !is.na(.data$.taxon_label) & nzchar(.data$.taxon_label)]),
                       tallest = smax(.data$height[.data$.h_ok]),
                       biggest = smax(.data$.d[.data$.d_ok]),
                       dominant = { i <- which.max(ifelse(.data$.d_ok, .data$.d, NA_real_)); if (length(i)) .data$.taxon_label[i] else NA_character_ },
                       .groups = "drop")
  }
  out <- per %>% dplyr::left_join(sp, by = c("plotID", "eventID"))
  out$n_taxa[out$supported %in% TRUE & is.na(out$n_taxa)] <- 0L
  out$n_species[out$supported %in% TRUE & is.na(out$n_species)] <- 0L
  out %>% dplyr::arrange(dplyr::desc(.data$ba_ha))
}

# ---------------------------------------------------------------------------
# Analysis-ready exports: tidy, typed, unit-bearing column names, self-identifying
# (a downloaded record joins with no app context). One row per preserved
# apparent-individual source uid, with the plot-scoped stem-event locator and
# its conflict audit retained separately.
# ---------------------------------------------------------------------------
col_or_na <- function(d, nm) if (nm %in% names(d)) d[[nm]] else NA
.receipt_value <- function(meta, field, fallback = "unverified / legacy HOLD") {
  value <- if (!is.null(meta)) meta[[field]] else NULL
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) fallback else as.character(value[[1]])
}
with_export_receipt <- function(data, meta = NULL) {
  if (is.null(data)) return(NULL)
  # Receipt identity is canonical and must occur exactly once. Some preserved
  # source/context tables already carry a site column; replace reserved receipt
  # fields instead of emitting duplicate CSV headers with check.names = FALSE.
  reserved <- c("site", "source_product", "source_release", "source_digest")
  data <- data[, !(names(data) %in% reserved), drop = FALSE]
  n <- nrow(data)
  receipt <- if (!is.null(meta)) meta$source_receipt else NULL
  digest <- if (!is.null(receipt)) receipt$raw_source_digest else NULL
  data.frame(
    site = rep(.receipt_value(meta, "site"), n),
    source_product = rep(.receipt_value(meta, "product"), n),
    source_release = rep(.receipt_value(meta, "release"), n),
    source_digest = rep(if (is.null(digest) || !length(digest)) "unverified / legacy HOLD" else as.character(digest[[1]]), n),
    data, check.names = FALSE, stringsAsFactors = FALSE)
}
.iso_date <- function(x) {
  if (!length(x) || all(is.na(x))) return(rep(NA_character_, length(x)))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.numeric(x)) return(format(as.Date(x, origin = "1970-01-01"), "%Y-%m-%d"))
  format(suppressWarnings(as.Date(x)), "%Y-%m-%d")
}
tidy_trees_export <- function(trees, meta = NULL) {
  if (is.null(trees) || !nrow(trees)) return(NULL)
  d <- .ensure_event_columns(trees)
  if (!"tempStemID" %in% names(d)) d$tempStemID <- NA_character_
  d <- d[order(d$plotID, d$individualID, .date_num(d$date), .chr(d$eventID), .chr(d$tempStemID)), , drop = FALSE]
  out <- data.frame(
    contract_id = VEG_CONTRACT_ID,
    source_record_key = col_or_na(d, "source_uid"),
    mapping_source_record_key = col_or_na(d, "mapping_source_uid"),
    protocol_stem_key = paste(
      .chr(d$plotID), .chr(d$eventID), .chr(d$individualID),
      .chr(d$tempStemID), sep = "::"
    ),
    protocol_key_group_n = col_or_na(d, "protocol_key_group_n"),
    protocol_key_conflict = col_or_na(d, "protocol_key_conflict"),
    opportunity_source_missing = col_or_na(d, "opportunity_source_missing"),
    plant_key = paste(.chr(d$plotID), .chr(d$individualID), sep = "::"),
    plant_id = col_or_na(d, "plant_id"),
    superseded_in_event = col_or_na(d, "superseded_in_event"),
    duplicate_of_uid = col_or_na(d, "duplicate_of_uid"),
    plotID = d$plotID,
    eventID = d$eventID,
    individualID = d$individualID,
    tempStemID = d$tempStemID,
    subplotID = col_or_na(d, "subplotID"),
    mapping_created_date = .iso_date(col_or_na(d, "mappingCreatedDate")),
    taxonID = col_or_na(d, "taxonID"),
    taxon_label = .taxon_name(d),
    taxon_resolution = col_or_na(d, "taxon_resolution"),
    scientificName = col_or_na(d, "scientificName"),
    genus = col_or_na(d, "genus"),
    family = col_or_na(d, "family"),
    taxonRank = col_or_na(d, "taxonRank"),
    recordType = col_or_na(d, "recordType"),
    identificationQualifier = col_or_na(d, "identificationQualifier"),
    mappingDataQF = col_or_na(d, "mappingDataQF"),
    growthForm = col_or_na(d, "growthForm"),
    date = .iso_date(d$date),
    year = col_or_na(d, "year"),
    dbh_cm = col_or_na(d, "stemDiameter"),
    basal_stem_diam_cm = col_or_na(d, "basalStemDiameter"),
    height_m = col_or_na(d, "height"),
    max_crown_diam_m = col_or_na(d, "maxCrownDiameter"),
    measurement_height_cm = col_or_na(d, "measurementHeight"),
    basal_measurement_height_cm = col_or_na(d, "basalMeasurementHeight"),
    changed_measurement_location = col_or_na(d, "changedMeasurementLocation"),
    tagStatus = col_or_na(d, "tagStatus"),
    dendrometerCondition = col_or_na(d, "dendrometerCondition"),
    heightQualifier = col_or_na(d, "heightQualifier"),
    dataQF = col_or_na(d, "dataQF"),
    canopy_position = col_or_na(d, "canopyPosition"),
    plant_status = col_or_na(d, "plantStatus"),
    live = col_or_na(d, "live"),
    permanent = col_or_na(d, "permanent"),
    is_species = col_or_na(d, "is_species"),
    stringsAsFactors = FALSE)
  # The shared size screen travels with the number: a FALSE row is kept in
  # every summary but left out of the tallest/biggest extremes.
  screen <- vst_size_screen(d)
  out$size_plausible <- screen$size_plausible
  out$size_screen_reason <- screen$size_screen_reason
  # Preserve source QC/flag fields without guessing their meaning. Exact raw
  # names make them traceable to the NEON table and prevent silent omission.
  qn <- grep("(quality|qualifier|qf$|flag$|status$|condition$|remark)",
             names(d), value = TRUE, ignore.case = TRUE)
  mapped_source <- c(
    "recordType", "identificationQualifier", "mappingDataQF", "growthForm",
    "plantStatus", "changedMeasurementLocation", "tagStatus",
    "dendrometerCondition", "heightQualifier", "dataQF", "canopyPosition"
  )
  qn <- setdiff(qn, c(names(out), mapped_source))
  if (length(qn)) out <- cbind(out, d[, qn, drop = FALSE])
  with_export_receipt(out, meta)
}
plots_export <- function(snap, plots, spec = SIZE_FOREST, meta = NULL) {
  ps <- plot_summary_veg(snap, plots, spec); if (is.null(ps)) return(NULL)
  out <- data.frame(
    contract_id = VEG_CONTRACT_ID,
    channel = spec$channel, size_metric = spec$metric_kind,
    plotID = ps$plotID, eventID = ps$eventID, event_date = .iso_date(ps$date),
    plotType = ps$plotType, nlcdClass = ps$nlcdClass,
    lat = ps$lat, lng = ps$lng, sampled_area_m2 = ps$area_use,
    support_status = ps$support_status, support_reason = ps$support_reason,
    n_held_events = ps$n_held_events, n_later_held = ps$n_later_held,
    held_reasons = ps$held_reasons, later_held_reasons = ps$later_held_reasons,
    supported = ps$supported, sampled_absence = ps$sampled_absence,
    tree_channel_complete = col_or_na(ps, "tree_channel_complete"),
    shrub_channel_complete = col_or_na(ps, "shrub_channel_complete"),
    disturbance_classes = col_or_na(ps, "disturbance_classes"),
    n_status_lost_burned = col_or_na(ps, "n_status_lost_burned"),
    n_status_removed = col_or_na(ps, "n_status_removed"),
    ba_m2_ha = ps$ba_ha, density_stems_ha = ps$density_ha,
    qmd_cm = ps$qmd, n_live_stems = ps$stems, n_live_plants = ps$plants,
    n_taxa = ps$n_taxa, n_species = ps$n_species,
    tallest_m = ps$tallest, biggest_diam_cm = ps$biggest,
    largest_stem_taxon = ps$dominant,
    issue_ids = veg_issue_ids((meta %||% list())$site %||% NA_character_, ps$date),
    stringsAsFactors = FALSE)
  with_export_receipt(out, meta)
}
veg_codebook <- function() {
  rows <- list(
    c("site","all exports","character","NEON site code","Site associated with the exported row."),
    c("source_product","all exports","character","DP1.10098.001","NEON data product from the validated bundle metadata."),
    c("source_release","all exports","character","RELEASE-2026","Exact official source release; any unverified fallback is a release failure."),
    c("source_digest","all exports","character","SHA-256","Digest of the staged raw official-release source family."),
    c("contract_id","trees_long/plot_summary_latest","character",VEG_CONTRACT_ID,"Versioned metric and export contract. Join or compare outputs only when this value agrees."),
    c("source_record_key","trees_long/plot_opportunity_source","character","published uid","Unique published source-row identity. It is preserved even when a protocol locator conflicts."),
    c("mapping_source_record_key","trees_long","character","published uid","Published mapping/tagging source-row identity used for the selected taxonomic identity. Blank only when no mapping/tagging row matched the physical plant."),
    c("protocol_stem_key","trees_long","character","plotID::eventID::individualID::tempStemID","Plot-scoped display of the protocol stem locator. The published three-field locator should be unique, but plot scoping prevents known cross-plot tag collisions from contaminating another plot; true within-plot anomalies are preserved and flagged."),
    c("protocol_key_group_n","trees_long/plot_opportunity_source","integer","records","Number of published source records sharing the relevant protocol locator."),
    c("protocol_key_conflict","trees_long/plot_opportunity_source","logical","TRUE/FALSE","TRUE when the protocol locator is not unique; affected physical-channel summaries are held."),
    c("size_plausible","trees_long","logical","TRUE/FALSE","Shared size screen (R/size_plausibility.R, size-screen-v1). FALSE rows are kept in every count, basal-area and growth summary and raise the warn-level size_spike QC flag: verify, not wrong. A failing height is left out of the tallest extremes, a failing diameter out of the biggest extremes, and a height-to-DBH ratio failure out of both; size_screen_reason names the rule."),
    c("size_screen_reason","trees_long","character","rule labels","Why size_plausible is FALSE: a 3x and 5 m / 5 cm spike against the same plant's other events, a height-to-DBH ratio above 2.5 m per cm on DBH >= 10 cm, a shrub or sapling over 10 m on a basal diameter under 5 cm, or a height above 100 m. Blank when plausible."),
    c("opportunity_source_missing","trees_long/plot_event_contexts_all","logical","TRUE/FALSE","TRUE when this measurement event has no matching published vst_perplotperyear plotID + eventID row. It is always held from absence, area scaling, and derived summaries."),
    c("plant_key","trees_long","character","plotID::individualID","Canonical bole identity in this app. individualID alone is not unique across plots. Growth is computed per bole."),
    c("plant_id","trees_long","character","NEON plant tag","Plant-level tag. For a lettered secondary bole of a multi-bole tree (individualID ending in one letter whose parent tag exists in the same plot) it is the parent tag; otherwise it equals individualID. Plant counts, the status summary and the mortality cohort use plotID + plant_id."),
    c("superseded_in_event","trees_long","logical","TRUE/FALSE","TRUE when a later-dated row of the same individual in the same event supersedes this row. Superseded rows are kept in this table and left out of every summary."),
    c("duplicate_of_uid","trees_long","character","published uid","source_record_key of the identical row this row duplicates; blank normally. Duplicate rows are kept in this table and left out of every summary."),
    c("plotID","trees_long/plot_summary_latest/plot_event_contexts_all","character","","NEON plot identifier; part of the plant and plot-event keys."),
    c("eventID","trees_long/plot_summary_latest/plot_event_contexts_all","character","","NEON sampling event identifier. Plot-event identity is plotID x eventID; date alone is never used as the event key."),
    c("individualID","trees_long","character","","NEON plant tag. Always pair with plotID; TEMP.PLA ids are not stable remeasurement identities."),
    c("tempStemID","trees_long","character","","Stem-within-event locator. It is not stable across years for multi-bole shrubs/saplings and can be missing or conflicting in documented source anomalies."),
    c("subplotID","trees_long","character","subplot_area[_nested]","NEON RELEASE-2026 subplotID from vst_apparentindividual: subplot number plus sampled area in m2, plus the nested subplot when there is one (for example 41_25_1). It can change between events for the same plant (NEON issue 81910)."),
    c("mapping_created_date","trees_long","date (ISO)","YYYY-MM-DD","Creation date used to select the mapping/tagging identity record."),
    c("taxonID","trees_long","character","","Published taxon identifier where available."),
    c("taxon_label","trees_long","character","","Best available explicit label, including unresolved/coarse identifications; physical records are never dropped for missing species names."),
    c("taxon_resolution","trees_long","character","","Resolution class for taxon_label."),
    c("scientificName","trees_long","character","","Identified taxon; may be revised across bouts."),
    c("family","trees_long","character","","Taxonomic family."),
    c("taxonRank","trees_long","character","species/genus/...","Rank of the identification; is_species = TRUE only at species or finer."),
    c("recordType","trees_long","character","published NEON value","Mapping/tagging record type preserved from the selected identity row."),
    c("identificationQualifier","trees_long","character","published NEON value","Identification qualifier preserved from the selected mapping/tagging identity row."),
    c("mappingDataQF","trees_long","character","published NEON value","Published data-quality flag from the selected mapping/tagging identity row; preserved for review, not used as a blanket exclusion."),
    c("growthForm","trees_long","character","single bole tree/multi-bole tree/small tree/single shrub/small shrub/sapling/...","NEON growth form. Tree-DBH metrics admit only full-plot tree forms; basal metrics admit only compatible shrub/sapling forms. Small tree is not silently assigned across channels."),
    c("date","trees_long","date (ISO)","YYYY-MM-DD","Measurement date; useful for ordering but not an event identity."),
    c("year","trees_long","integer","","Calendar year of the bout."),
    c("dbh_cm","trees_long","numeric","cm","Diameter at breast height. Used only for the full-plot tree-DBH channel."),
    c("basal_stem_diam_cm","trees_long","numeric","cm","Basal stem diameter. Used only for the nested shrub/sapling basal channel."),
    c("height_m","trees_long","numeric","m","Plant height; often NA (not every stem is measured for height)."),
    c("max_crown_diam_m","trees_long","numeric","m","Maximum crown/canopy diameter (where measured)."),
    c("measurement_height_cm","trees_long","numeric","cm","Height on the stem at which dbh_cm was taken; a change makes an increment apples-to-oranges."),
    c("basal_measurement_height_cm","trees_long","numeric","cm","Height of a basal-diameter measurement; changes are guarded in longitudinal estimates."),
    c("changed_measurement_location","trees_long","character","noChange/boleChange/...","NEON reason code if the measurement point moved."),
    c("tagStatus","trees_long","character","published NEON value","Published tag status preserved for record-level review; it is not a blanket row-exclusion rule."),
    c("dendrometerCondition","trees_long","character","published NEON value","Published dendrometer condition preserved for record-level review."),
    c("heightQualifier","trees_long","character","published NEON value","Published height qualifier preserved for record-level review."),
    c("dataQF","trees_long","character","published NEON value","Published apparent-individual data-quality flag preserved for review; required-metric validity is evaluated independently."),
    c("canopy_position","trees_long","character","","Crown position class."),
    c("plant_status","trees_long","character","Live*/Standing dead/Downed/Lost.../Removed/...","NEON status string."),
    c("live","trees_long","logical","TRUE/FALSE","Derived = grepl('^Live', plant_status)."),
    c("permanent","trees_long","logical","TRUE/FALSE","Derived = id starts with 'NEON'; growth metrics use permanent ids only."),
    c("is_species","trees_long","logical","TRUE/FALSE","Derived = identified to species or finer (unambiguous)."),
    c("channel","plot_summary_latest","character","tree_dbh/shrub_sapling_basal","Sampling and measurement channel. Never pool or rank ba_m2_ha across these physically different channels."),
    c("size_metric","plots","character","bole-DBH basal area, boles 10 cm and over (breast height)/basal-diameter cover (stem base)","Physical meaning of ba_m2_ha on this row."),
    c("event_date","plots","date (ISO)","YYYY-MM-DD","Date of the selected latest supported plot event."),
    c("plotType","plots","character","distributed/tower","NEON plot design class. The app reports a sampled-plot summary; restrict to distributed plots for the spatially balanced design stratum."),
    c("nlcdClass","plots","character","","NEON land-cover class (NLCD) at the plot."),
    c("lat","plots","numeric","decimal degrees","Plot centroid latitude (WGS84 decimal degrees)."),
    c("lng","plots","numeric","decimal degrees","Plot centroid longitude (WGS84 decimal degrees)."),
    c("sampled_area_m2","plots","numeric","m^2","Positive event-specific sampled area for this channel. No temporal median or minimum-area cutoff is applied; valid 40 m2 areas are retained."),
    c("support_status","plots","character",paste(c(VEG_CONTRACT$supported_status, VEG_CONTRACT$held_status), collapse = "/"),"Sampling-opportunity disposition. sampled_absence is an observed zero; every held_* value is unsupported, not zero. supported_legacy_schema (a pre-2020-03-10 event with records and blank sampling fields) is used for change only, never for structure."),
    c("support_reason","plots","character","","Human-readable reason for the support disposition."),
    c("n_held_events","plots","integer","events","Count of held plot-event contexts for this plot across the preserved event history."),
    c("n_later_held","plots","integer","events","Count of held attempts later than the selected latest supported census."),
    c("held_reasons","plots","character","","Distinct reasons across held plot-event contexts for this plot."),
    c("later_held_reasons","plots","character","","Distinct reasons for held attempts later than the selected census."),
    c("supported","plots","logical","TRUE/FALSE","TRUE only for sampled_with_records or sampled_absence with a positive event-specific area."),
    c("sampled_absence","plots","logical","TRUE/FALSE","Explicit protocol-supported absence. Metric values are zero. FALSE with supported=FALSE must remain NA."),
    c("ba_m2_ha","plots","numeric","m^2/ha","Live cross-sectional area per hectare for this channel. NA means held/unsupported; zero means explicit sampled absence or a supported census with no eligible live stems."),
    c("density_stems_ha","plots","numeric","stems/ha","Live stem density per hectare for this plot."),
    c("qmd_cm","plots","numeric","cm","Stem-weighted quadratic mean diameter: sqrt(sum(diameter^2) / number of live stem rows)."),
    c("n_live_stems","plots","integer","stems","Count of eligible live apparent-individual rows; multi-stem plants contribute multiple stems."),
    c("n_live_plants","plots","integer","plants","Count of distinct plotID x plant_id plants; a multi-bole tree counts once."),
    c("partial_event_rule_id","plot_event_contexts_all","character","VST-PARTIAL-01/02/03/04 or blank","Rule from the reviewed curation file scripts/curation/vst_partial_event_completeness.csv for a dataCollected = partial event; blank for every other event."),
    c("tree_channel_complete","plot_summary_latest/plot_event_contexts_all","logical","TRUE/FALSE/NA","TRUE when the reviewed curation found a dataCollected = partial event complete for the tree channel, so that channel reads it as all-growth-forms; every other support check still applies."),
    c("shrub_channel_complete","plot_summary_latest/plot_event_contexts_all","logical","TRUE/FALSE/NA","TRUE when the reviewed curation found a dataCollected = partial event complete for the shrub/sapling channel, so that channel reads it as all-growth-forms; every other support check still applies."),
    c("disturbance_classes","plot_summary_latest/plot_event_contexts_all","character","fire_wild;burn_prescribed;fire_unspecified;wind_hurricane;flood;harvest_logging;brush_mowing","Disturbance classes recorded for the plot event, semicolon-separated. fire_unspecified means a fire was recorded without saying whether it was prescribed or wild. Blank when none was recorded; blank is not evidence of no disturbance."),
    c("disturbance_curation_id","plot_event_contexts_all","character","curation id","Identifier of the reviewed curation that assigned disturbance_classes; blank when none applies."),
    c("n_status_lost_burned","plot_summary_latest/plot_event_contexts_all","integer","records","Count of rows in this plot event with plantStatus Lost, burned. These fates are censored, not deaths."),
    c("n_status_removed","plot_summary_latest/plot_event_contexts_all","integer","records","Count of rows in this plot event with plantStatus Removed. These fates are censored, not deaths."),
    c("n_taxa","plots","integer","","Count of distinct live explicit/coarse taxon labels in the plot. Supported zeros are 0; held opportunities remain NA."),
    c("n_species","plots","integer","","Count of distinct live species-or-finer labels (`is_species = TRUE`) in the plot. Supported zeros are 0; held opportunities remain NA."),
    c("tallest_m","plots","numeric","m","Height of the tallest live plant in the plot among rows that pass the shared size screen."),
    c("biggest_diam_cm","plots","numeric","cm","Largest eligible live stem diameter in the plot among records whose diameter passes the shared size screen: DBH for tree_dbh, basal stem diameter for shrub_sapling_basal."),
    c("issue_ids","plots","character","NEON issue ids; semicolon-separated","NEON issue-log entries (DP1.10098.001 changeLogs, curated in R/neon_issue_log.R) whose site and date range cover this plot event. Blank when none applies."),
    c("largest_stem_taxon","plot_summary_latest","character","","Taxon label attached to the largest eligible live stem in the selected plot event; not an ecological-dominance estimate."),
    c("date","plot_event_contexts_all","date (ISO)","YYYY-MM-DD","Published vst_perplotperyear date for a source-backed context; NA for a measurement-only context. It is ordering context, not event identity."),
    c("opportunity_source_uid","plot_event_contexts_all","character","published uid","Deterministically selected source row retained only to carry nominal fields when a plot-event has multiple source records; a conflict is always held. NA for measurement-only contexts."),
    c("opportunity_source_record_count","plot_event_contexts_all","integer","records","Number of published vst_perplotperyear source rows sharing the plotID + eventID key."),
    c("opportunity_key_conflict","plot_event_contexts_all","logical","TRUE/FALSE","TRUE when multiple published opportunity rows share one plot-event key. Both physical channels are held rather than selecting a denominator as truth."),
    c("opportunity_source_uids","plot_event_contexts_all","character","semicolon-separated published uids","Complete uid inventory for source rows sharing this plot-event key; NA when no published opportunity source row exists."),
    c("measurement_record_count_all","plot_event_contexts_all","integer","records","Count of all preserved vst_apparentindividual rows sharing this plotID + eventID, including growth forms outside the two registered summary channels."),
    c("measurement_date_min","plot_event_contexts_all","date (ISO)","YYYY-MM-DD","Earliest published apparent-individual measurement date in this context; explicitly measurement-sourced and not an opportunity date."),
    c("measurement_date_max","plot_event_contexts_all","date (ISO)","YYYY-MM-DD","Latest published apparent-individual measurement date in this context; explicitly measurement-sourced and not an opportunity date."),
    c("measurement_date_distinct_n","plot_event_contexts_all","integer","dates","Number of distinct nonmissing apparent-individual measurement dates in this context."),
    c("year","plot_event_contexts_all","integer","calendar year","Calendar year of the published opportunity row; NA for a measurement-only context."),
    c("eventType","plot_event_contexts_all","character","published event type","Published NEON event classification retained for protocol review."),
    c("samplingImpractical","plot_event_contexts_all","character","ok / protocol reason","Published opportunity field. Values other than protocol-supported ok can cause a held state."),
    c("dataCollected","plot_event_contexts_all","character","allGrowthForms/dendrometerOnly/...","Published collection-scope field; dendrometer-only events are not scaled as census measurements."),
    c("treesPresent","plot_event_contexts_all","character","published presence state","Published tree-channel presence/absence evidence used with record consistency checks."),
    c("shrubsPresent","plot_event_contexts_all","character","published presence state","Published shrub/sapling-channel presence/absence evidence used with record consistency checks."),
    c("treePresence","plot_event_contexts_all","character","normalized present/absent/unknown","Normalized tree-channel presence evidence derived from the published opportunity fields."),
    c("shrubSaplingPresence","plot_event_contexts_all","character","normalized present/absent/unknown","Normalized shrub/sapling-channel presence evidence derived from the published opportunity fields."),
    c("area_trees","plot_event_contexts_all","numeric m^2",">0 or NA","Exact event-specific tree sampled area; NA is not zero."),
    c("area_shrub","plot_event_contexts_all","numeric m^2",">0 or NA","Exact event-specific shrub/sapling sampled area; NA is not zero."),
    c("tree_support","plot_event_contexts_all","character",paste(c(VEG_CONTRACT$supported_status, setdiff(VEG_CONTRACT$held_status, "held_snapshot_event_mismatch")), collapse = "/"),"Tree-channel plot-event context state. Only sampled_absence is zero; source-missing contexts are held."),
    c("tree_support_reason","plot_event_contexts_all","character","","Reason for the tree-channel opportunity state."),
    c("tree_supported","plot_event_contexts_all","logical","TRUE/FALSE","TRUE only when tree_support is sampled_with_records or sampled_absence; positive area is separately required at point of use."),
    c("shrub_support","plot_event_contexts_all","character",paste(c(VEG_CONTRACT$supported_status, setdiff(VEG_CONTRACT$held_status, "held_snapshot_event_mismatch")), collapse = "/"),"Shrub/sapling-channel plot-event context state. Only sampled_absence is zero; source-missing contexts are held."),
    c("shrub_support_reason","plot_event_contexts_all","character","","Reason for the shrub/sapling-channel opportunity state."),
    c("shrub_supported","plot_event_contexts_all","logical","TRUE/FALSE","TRUE only when shrub_support is sampled_with_records or sampled_absence; positive area is separately required at point of use."),
    c("tree_records","plot_event_contexts_all","integer records","≥0","Apparent-individual rows matched to this plot event in the tree channel."),
    c("shrub_records","plot_event_contexts_all","integer records","≥0","Apparent-individual rows matched to this plot event in the shrub/sapling basal channel."),
    c("tree_invalid_metric_records","plot_event_contexts_all","integer records","≥0","Eligible live tree-channel rows whose DBH is missing, non-finite, non-positive, or below 10 cm. Any such row holds the event unless an earlier protocol or presence-conflict state takes precedence."),
    c("shrub_invalid_metric_records","plot_event_contexts_all","integer records","≥0","Eligible live shrub/sapling-channel rows whose basal stem diameter is missing, non-finite, or non-positive. Any such row holds the event unless an earlier protocol or presence-conflict state takes precedence."),
    c("tree_identity_conflict_keys","plot_event_contexts_all","integer protocol locators","≥0","Count of non-unique plotID + eventID + individualID + tempStemID locators involving the tree-DBH channel. Any positive count holds that channel event."),
    c("shrub_identity_conflict_keys","plot_event_contexts_all","integer protocol locators","≥0","Count of non-unique plotID + eventID + individualID + tempStemID locators involving the shrub/sapling basal channel. Any positive count holds that channel event."),
    c("event_key","plot_event_contexts_all","character","plotID::eventID","Deterministic display form of the plot-event context key; published source fields remain authoritative where present."))
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("column", "table", "type", "allowed_values", "definition")
  out$table[out$table == "plots"] <- "plot_summary_latest"
  out
}

.dictionary_type <- function(value) {
  if (inherits(value, "Date")) return("date (ISO)")
  if (inherits(value, "POSIXt")) return("datetime (ISO)")
  if (is.logical(value)) return("logical")
  if (is.integer(value)) return("integer")
  if (is.numeric(value)) return("numeric")
  "character"
}

.dictionary_has <- function(dictionary, table, column) {
  rows <- dictionary$column %in% column
  if (!any(rows)) return(FALSE)
  targets <- strsplit(as.character(dictionary$table[rows]), "/", fixed = TRUE)
  any(vapply(targets, function(x) "all exports" %in% x || table %in% x, logical(1)))
}

# Complete the static science dictionary from the exact frames being emitted.
# NEON source-compatible QF/opportunity columns can expand between releases; they
# remain preserved, explicitly named, and documented instead of silently dropped.
complete_veg_codebook <- function(dictionary = veg_codebook(), exports) {
  stopifnot(is.data.frame(dictionary), is.list(exports), !is.null(names(exports)))
  additions <- list()
  for (table in sort(names(exports))) {
    frame <- exports[[table]]
    if (is.null(frame)) next
    frame <- as.data.frame(frame, stringsAsFactors = FALSE)
    for (column in sort(names(frame))) {
      if (.dictionary_has(dictionary, table, column)) next
      source_field <- grepl("(quality|qualifier|QF$|Flag$|flag$|status|condition|remark)",
                            column, ignore.case = TRUE)
      definition <- if (source_field) {
        sprintf("Preserved source-compatible review field `%s` from the validated NEON bundle. It is retained for traceability and is not an automatic row-deletion rule unless SCIENCE-CONTRACT.md names it.", column)
      } else {
        sprintf("Preserved source-compatible `%s` field from the validated NEON bundle; see the DP1.10098.001 data dictionary for the upstream definition.", column)
      }
      additions[[length(additions) + 1L]] <- data.frame(
        column = column, table = table, type = .dictionary_type(frame[[column]]),
        allowed_values = "", definition = definition, stringsAsFactors = FALSE)
    }
  }
  if (length(additions)) dictionary <- dplyr::bind_rows(dictionary, dplyr::bind_rows(additions))
  dictionary
}

assert_veg_codebook <- function(dictionary, exports) {
  missing <- character(0)
  for (table in names(exports)) {
    frame <- exports[[table]]
    if (is.null(frame)) next
    absent <- names(frame)[!vapply(names(frame), function(column) {
      .dictionary_has(dictionary, table, column)
    }, logical(1))]
    if (length(absent)) missing <- c(missing, paste0(table, ":", absent))
  }
  if (length(missing)) stop("export dictionary lacks emitted fields: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Data dictionary for the QC flag tables / QC-report CSV emitted by tree_qc_site().
# Documents EVERY column the QC rows can carry (with units + NA-semantics) so the
# downloaded flag CSVs are self-describing and the full individualID can be joined
# back to trees_long.
qc_dictionary <- function(report = NULL) {
  rows <- list(
    c("contract_id","character",VEG_CONTRACT_ID,"Versioned science contract for the QC derivation."),
    c("level","character","high/warn/info","Review priority: high = incompatible with a registered analysis contract; warn = unusual and worth a second look; info = context retained but excluded from the affected summary."),
    c("issue","character","","Human-readable flag name (e.g. 'Implausible diameter jump (>5 cm/yr)')."),
    c("flag","character","resurrection/jump/shrink/mh/size_spike","Stable machine key for the flag type (present on per-flag downloads)."),
    c("dead_eventID","character","","First all-dead event of a plant later recorded Live (resurrection only)."),
    c("dead_date","character","YYYY-MM-DD","Date of that all-dead event (resurrection only)."),
    c("live_eventID","character","","First Live event after it (resurrection only)."),
    c("live_date","character","YYYY-MM-DD","Date of that Live event (resurrection only)."),
    c("date_start","character","YYYY-MM-DD","Date of the earlier supported event in the compared pair (jump, shrink, mh)."),
    c("date_later","character","YYYY-MM-DD","Date of the later supported event in the compared pair (jump, shrink, mh)."),
    c("years","numeric","years","Elapsed time between the two compared events (jump, shrink)."),
    c("mh_start_cm","numeric","cm","Measurement height on the stem at the earlier event (mh only)."),
    c("mh_later_cm","numeric","cm","Measurement height on the stem at the later event (mh only)."),
    c("plotID","character","","NEON plot identifier; pair with individualID."),
    c("plant_key","character","plotID::individualID","Canonical join key for the flagged plant."),
    c("individualID","character","","FULL NEON tag. It is not a sufficient join key without plotID."),
    c("plant","character","","Short display id (site/domain prefix stripped); for reading only, NOT a join key."),
    c("species","character","","Identified taxon for the flagged plant (may be NA if unidentified)."),
    c("start_cm","numeric","cm","Earlier supported-bout diameter (DBH for tree_dbh, basal stem diameter for shrub_sapling_basal). NA for the status-reversal flag."),
    c("later_cm","numeric","cm","Later supported-bout diameter, same measure as start_cm. NA for the status-reversal flag."),
    c("year","integer","","Calendar year of the flagged record (size_spike only)."),
    c("height_m","numeric","m","Recorded height of the flagged record (size_spike only)."),
    c("diameter_cm","numeric","cm","Recorded channel diameter of the flagged record: DBH for tree forms, basal stem diameter for shrub and sapling forms (size_spike only)."),
    c("screen_reason","character","rule labels","Which size-screen rule the record failed (size_spike only)."),
    c("height_reference_m","numeric","m","Median of the same plant's other intact live heights, one per event, that the height spike rule compared against; NA when there was no other event (size_spike only)."),
    c("diameter_reference_cm","numeric","cm","Median of the same plant's other diameters, one per event, that the diameter spike rule compared against; NA when there was no other event (size_spike only)."),
    c("cm_per_yr","numeric","cm/yr","Annualized diameter change between the two supported bouts (later_cm − start_cm over the like-for-like interval). Positive = increase, negative = decrease. NA where not applicable (status reversal, measurement-height moved)."))
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("column", "type", "units_or_values", "definition")
  if (!is.null(report)) {
    report <- as.data.frame(report, stringsAsFactors = FALSE)
    missing <- sort(setdiff(names(report), out$column))
    if (length(missing)) {
      extra <- lapply(missing, function(column) data.frame(
        column = column, type = .dictionary_type(report[[column]]), units_or_values = "",
        definition = sprintf("Export receipt or preserved QC field `%s`; join source identifiers to the validated bundle family and review fields to trees_long.csv.", column),
        stringsAsFactors = FALSE))
      out <- dplyr::bind_rows(out, dplyr::bind_rows(extra))
    }
  }
  out
}

assert_qc_dictionary <- function(dictionary, report) {
  missing <- setdiff(names(report), dictionary$column)
  if (length(missing)) stop("QC dictionary lacks emitted fields: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# live vs dead snapshot composition (an honest crude status ratio, not a rate).
# Reduced PER INDIVIDUAL across all snapshot stems: live if ANY stem is live, dead
# only if a stem is dead/downed and none live. "Lost track / removed" (lost tag,
# removed, no-longer-qualifies) is a DATA state, split out so it isn't read as
# biological mortality. Scoped to the paradigm's growth forms.
status_summary <- function(snap, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(NULL)
  forms <- spec$forms %||% TREE_FORMS
  snap <- .current_rows(snap)
  s <- .option_a_tree_rows(snap[snap$growthForm %in% forms, , drop = FALSE], spec)
  if (!nrow(s)) return(NULL)
  s$.plant_key <- .plant_id_key(s)   # one count per plant (plotID + plant_id)
  state <- veg_status_state(s$plantStatus)
  s$.live <- state %in% "live"
  s$.dead <- state %in% "dead"
  s$.lost <- state %in% "censored"
  per <- s %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::summarise(
      any_live = any(.data$.live),
      all_dead = all(.data$.dead),
      any_lost = any(.data$.lost),
      .groups = "drop")
  per$cls <- dplyr::case_when(
    per$any_live ~ "Live",
    per$all_dead ~ "Dead / standing dead",
    per$any_lost ~ "Lost track / removed",
    TRUE ~ "Other / unknown")
  lvl <- c("Live", "Dead / standing dead", "Lost track / removed", "Other / unknown")
  out <- per %>% dplyr::count(.data$cls, name = "n")
  out$cls <- factor(out$cls, levels = lvl)
  out[order(out$cls), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Compound annual mortality for permanent composite plant identities. Each event
# becomes one state: live if any stem is live, dead only if every observed stem
# is dead/downed, and censored for lost/removed/unknown. Uncertainty uses a
# delete-one-plot jackknife, not an independence-assuming plant-level binomial CI.
# ---------------------------------------------------------------------------
# Plant status, explicitly (hk, 2026-10-21). Dead means one of NEON's three
# dead states; every "Lost, ...", "Removed" and "No longer qualifies" record is
# a censored (unknown) fate, never a death.
VEG_DEAD_STATUS <- c("standing dead", "dead, broken bole", "downed")
veg_status_state <- function(status) {
  s <- tolower(trimws(.chr(status)))
  dead <- Reduce(`|`, lapply(VEG_DEAD_STATUS, function(x) startsWith(ifelse(is.na(s), "", s), x)))
  ifelse(is.na(s) | !nzchar(s), NA_character_,
         ifelse(grepl("^live", s), "live", ifelse(dead, "dead", "censored")))
}
# Mortality gates (hk): a rate needs at least 5 plot clusters (NEON's annual
# tower subset is 5 plots) and 30 trackable plants (one death then moves the
# rate by about 1%/yr over 3 years).
VEG_MORT_MIN_PLOTS <- 5L
VEG_MORT_MIN_N0 <- 30L

# Compound annual cohort mortality (Sheil-May) from supported censuses. The
# cohort is permanent plants live at their first supported census with a known
# later fate; time at risk ends at the first all-dead census; a fate after a
# censored (lost, removed) census is unknown; plants recorded Live after an
# all-dead census are held out (see the QC flag). Returns NULL when no cohort
# exists, else a list whose status is "reported", "insufficient_clusters" or
# "insufficient_plants"; rate and interval are NA unless reported. The 95%
# interval is a Korn-Graubard cluster beta interval with plots as clusters.
stand_mortality <- function(trees, spec = SIZE_FOREST, plots = NULL) {
  if (is.null(trees) || !nrow(trees) || !"date" %in% names(trees)) return(NULL)
  d <- .supported_history(trees, plots, spec, allow_legacy = TRUE)
  if (is.null(d) || !nrow(d)) return(NULL)
  forms <- spec$forms %||% TREE_FORMS
  d <- d[d$growthForm %in% forms & is.finite(.date_num(d$date)), , drop = FALSE]
  if ("permanent" %in% names(d)) d <- d[d$permanent %in% TRUE, , drop = FALSE]
  if (is.null(d) || !nrow(d) || !"plantStatus" %in% names(d)) return(NULL)
  d$.state <- veg_status_state(d$plantStatus)
  d$.event_date <- .date_num(d$date)
  # The cohort is PLANTS (plotID + plant_id): a lettered secondary bole and its
  # parent are one plant. Live at an event if any of its rows is live; dead only
  # if every observed row is an explicit dead state; otherwise censored.
  d$.plant_id <- .plant_id_vec(d)
  d$.plant_key <- .plant_id_key(d)
  ev <- d %>% dplyr::group_by(.data$plotID, .data$.plant_id, .data$.plant_key, .data$eventID) %>%
    dplyr::summarise(date = as.Date(min(.data$.event_date), origin = "1970-01-01"),
                     any_live = any(.data$.state %in% "live"),
                     all_dead = all(.data$.state %in% "dead"), .groups = "drop")
  ev$individualID <- ev$.plant_id
  ev$state <- ifelse(ev$any_live, "live", ifelse(ev$all_dead, "dead", "censored"))
  ev <- ev[order(ev$.plant_key, ev$date, .chr(ev$eventID)), , drop = FALSE]
  n_censored_events <- sum(ev$state == "censored")
  bad <- ev %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::summarise(resurrected = any(.data$state == "live" & cumsum(.data$state == "dead") > 0), .groups = "drop")
  ev <- ev[!(ev$.plant_key %in% bad$.plant_key[bad$resurrected %in% TRUE]), , drop = FALSE]
  # Unknown after the first censored census; time at risk stops at the first
  # all-dead census (repeat dead records add no time at risk).
  ev <- ev %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::mutate(after_censor = cumsum(.data$state == "censored") > 0,
                  after_death = dplyr::lag(cumsum(.data$state == "dead") > 0, default = FALSE)) %>%
    dplyr::ungroup()
  known <- ev[ev$state %in% c("live", "dead") & !ev$after_censor & !ev$after_death, , drop = FALSE]
  if (!nrow(known)) return(NULL)
  per <- known %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::filter(dplyr::n_distinct(.data$eventID) >= 2) %>%
    dplyr::summarise(plotID = dplyr::first(.data$plotID), individualID = dplyr::first(.data$individualID),
                     first_state = dplyr::first(.data$state), last_state = dplyr::last(.data$state),
                     t = as.numeric(dplyr::last(.data$date) - dplyr::first(.data$date)) / 365.25,
                     d0 = dplyr::first(.data$date), d1 = dplyr::last(.data$date),
                     .groups = "drop")
  coh <- per[per$first_state == "live" & per$t > 0, , drop = FALSE]
  n0 <- nrow(coh)
  if (!n0) return(NULL)
  coh$death <- coh$last_state == "dead"
  D <- sum(coh$death); q <- D / n0; tbar <- mean(coh$t)
  plots_in <- unique(coh$plotID); K <- length(plots_in)
  ptype <- if (!is.null(plots) && all(c("plotID", "plotType") %in% names(plots)))
    tolower(.chr(plots$plotType[match(plots_in, plots$plotID)])) else rep(NA_character_, K)
  status <- if (K < VEG_MORT_MIN_PLOTS) "insufficient_clusters" else
    if (n0 < VEG_MORT_MIN_N0) "insufficient_plants" else "reported"
  to_rate <- function(x) 100 * (1 - (1 - x)^(1 / tbar))
  rate <- lo <- hi <- deff <- NA_real_
  if (identical(status, "reported") && is.finite(tbar) && tbar > 0) {
    by_plot <- split(coh$death, coh$plotID)
    d_p <- vapply(by_plot, sum, numeric(1)); n_p <- vapply(by_plot, length, numeric(1))
    v <- K / (K - 1) * sum((d_p - q * n_p)^2) / n0^2
    base_n <- if (q > 0 && q < 1 && v > 0) min(n0, q * (1 - q) / v) else n0
    neff <- base_n * (stats::qnorm(0.975) / stats::qt(0.975, K - 1))^2
    q_lo <- if (D == 0) 0 else stats::qbeta(0.025, neff * q, neff * (1 - q) + 1)
    q_hi <- if (q >= 1) 1 else stats::qbeta(0.975, neff * q + 1, neff * (1 - q))
    rate <- to_rate(q); lo <- to_rate(q_lo); hi <- to_rate(q_hi)
    deff <- if (q > 0 && q < 1 && v > 0) v / (q * (1 - q) / n0) else NA_real_
  }
  list(status = status,
       rate_pct = if (is.finite(rate)) round(rate, 2) else NA_real_,
       lo = if (is.finite(lo)) round(lo, 2) else NA_real_,
       hi = if (is.finite(hi)) round(hi, 2) else NA_real_,
       n0 = n0, deaths = D, n_plots = K,
       n_tower = sum(ptype %in% "tower"), n_distributed = sum(ptype %in% "distributed"),
       t_yrs = round(tbar, 1),
       yr0 = as.integer(format(min(coh$d0), "%Y")), yr1 = as.integer(format(max(coh$d1), "%Y")),
       deff = if (is.finite(deff)) round(deff, 2) else NA_real_,
       n_censored_events = n_censored_events,
       ci_method = "Korn-Graubard cluster beta interval (plots as clusters)",
       contract_id = VEG_CONTRACT_ID)
}

# Why annual mortality is or is not reported, in plain words, with the plot
# count and the real hold reason. NEON added samplingImpractical on 2020-03-10
# (changelog 16306). Earlier events with records are supported_legacy_schema:
# admitted to growth and mortality under the changelog's legacy-schema
# assumption, never to structure. Earlier events without records stay held.
VEG_SAMPLING_IMPRACTICAL_START <- as.Date("2020-03-10")
mortality_context <- function(plots, spec = SIZE_FOREST) {
  out <- list(k2 = 0L, n_supported_plots = 0L, n_legacy = 0L, legacy_y0 = NA_integer_, legacy_y1 = NA_integer_,
              pre2020_held = 0L, other_held = 0L, other_reason = NA_character_)
  if (is.null(plots) || !nrow(plots) || !(spec$support %in% names(plots))) return(out)
  rec_col <- if (identical(spec$channel, "shrub_sapling_basal")) "shrub_records" else "tree_records"
  p <- as.data.frame(plots)
  rec <- if (rec_col %in% names(p)) .num(p[[rec_col]]) else rep(NA_real_, nrow(p))
  status <- .chr(p[[spec$support]])
  when <- if ("date" %in% names(p)) suppressWarnings(as.Date(p$date)) else as.Date(rep(NA_character_, nrow(p)))
  yr <- if ("year" %in% names(p)) suppressWarnings(as.integer(p$year)) else as.integer(format(when, "%Y"))
  bearing <- is.finite(rec) & rec > 0
  legacy <- bearing & status %in% VEG_LEGACY_SCHEMA_STATUS
  # Plots with two or more censuses count legacy events, as the rate does.
  sup <- bearing & status %in% c("sampled_with_records", VEG_LEGACY_SCHEMA_STATUS)
  per_plot <- table(.chr(p$plotID)[sup])
  out$k2 <- as.integer(sum(per_plot >= 2L))
  out$n_supported_plots <- as.integer(length(per_plot))
  out$n_legacy <- as.integer(sum(legacy))
  if (any(legacy & !is.na(yr))) { out$legacy_y0 <- min(yr[legacy], na.rm = TRUE); out$legacy_y1 <- max(yr[legacy], na.rm = TRUE) }
  pre_date <- (!is.na(when) & when <= VEG_SAMPLING_IMPRACTICAL_START) | (is.na(when) & !is.na(yr) & yr < 2020L)
  out$pre2020_held <- as.integer(sum(!bearing & status %in% "held_opportunity_unknown" & pre_date))
  other <- bearing & !sup
  out$other_held <- as.integer(sum(other))
  if (any(other)) {
    top <- sort(table(status[other]), decreasing = TRUE)
    label <- gsub("_", " ", sub("^held_", "", names(top)))
    # Say "mostly" only for a true majority; otherwise give the counts.
    out$other_reason <- if (length(top) == 1L) label[1] else if (top[[1]] > sum(top) / 2) {
      paste("mostly", label[1])
    } else {
      keep <- seq_len(min(3L, length(top)))
      paste0(paste(sprintf("%d %s", as.integer(top[keep]), label[keep]), collapse = ", "),
             if (length(top) > 3L) ", and others" else "")
    }
  }
  out
}
.plural <- function(n, one, many = paste0(one, "s")) if (identical(as.integer(n), 1L)) one else many
.pre2020_sentence <- function(ctx) {
  n <- ctx$n_legacy %||% 0L; m <- ctx$pre2020_held %||% 0L
  held <- if (m > 0L) sprintf("%d pre-March-2020 plot-%s without records %s held", m,
                              .plural(m, "census", "censuses"), if (m == 1L) "stays" else "stay") else ""
  if (n > 0L) {
    span <- if (is.na(ctx$legacy_y0)) "" else if (ctx$legacy_y0 == ctx$legacy_y1) sprintf(" (%d)", ctx$legacy_y0) else sprintf(" (%d-%d)", ctx$legacy_y0, ctx$legacy_y1)
    return(sprintf(" %d pre-March-2020 plot-%s with records%s %s used for growth and mortality under NEON's legacy-schema assumption (changelog 16306)%s.",
                   n, .plural(n, "census", "censuses"), span, if (n == 1L) "is" else "are",
                   if (m > 0L) sprintf("; %d without records %s held", m, if (m == 1L) "stays" else "stay") else ""))
  }
  if (m > 0L) return(paste0(" ", held, "."))
  ""
}
mortality_unavailable_reason <- function(trees, spec = SIZE_FOREST, plots = NULL, site = "this site") {
  ctx <- mortality_context(plots, spec)
  other <- if (ctx$other_held > 0L) sprintf(" %d other record-bearing plot-%s %s held (%s).",
    ctx$other_held, .plural(ctx$other_held, "census", "censuses"), if (ctx$other_held == 1L) "is" else "are",
    ctx$other_reason) else ""
  if (ctx$k2 == 0L) {
    lead <- if (ctx$n_supported_plots == 0L) {
      sprintf("Annual mortality is not reported: no supported plot-census at %s has measured plants in this channel.", site)
    } else {
      sprintf("Annual mortality is not reported: no plot at %s has two or more supported censuses in this channel (%d %s one).",
              site, ctx$n_supported_plots, .plural(ctx$n_supported_plots, "plot has", "plots have"))
    }
    return(paste0(lead, .pre2020_sentence(ctx), other))
  }
  paste0(sprintf("Annual mortality is not reported: %d %s at %s %s at least two supported censuses, but no permanent plant there was live at the first with a known later fate.",
                 ctx$k2, .plural(ctx$k2, "plot"), site, if (ctx$k2 == 1L) "has" else "have"),
         .pre2020_sentence(ctx))
}

# Number formatting for helper-built copy (mirrors server.R fmt_num/fmt_count).
.vh_num <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("\u2013")
  formatC(round(x[[1]], digits), format = "f", digits = digits, big.mark = ",")
}
.vh_count <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("\u2013")
  format(round(x[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# The mortality banner sentence for a stand_mortality() result (hk copy).
# Returns list(status, text); text is plain (escape it before HTML).
mortality_text <- function(mr, trees = NULL, spec = SIZE_FOREST, plots = NULL, site = "this site") {
  if (is.null(mr)) return(list(status = "none", text = mortality_unavailable_reason(trees, spec, plots, site)))
  ctx <- mortality_context(plots, spec)
  pl <- function(n, one, many = paste0(one, "s")) sprintf("%s %s", .vh_count(n), .plural(n, one, many))
  window <- if (is.na(mr$yr0 %||% NA)) "" else if (mr$yr0 == mr$yr1) sprintf(", %d", mr$yr0) else sprintf(", %d-%d", mr$yr0, mr$yr1)
  tower <- if (isTRUE(mr$n_distributed == 0L && mr$n_tower > 0L)) ", all tower plots" else ""
  text <- switch(mr$status,
    insufficient_clusters = sprintf("Annual mortality is not reported: %s in %s (%d died), too few plot clusters for a rate (needs %d).",
      pl(mr$n0, "trackable plant"), pl(mr$n_plots, "plot"), mr$deaths, VEG_MORT_MIN_PLOTS),
    insufficient_plants = sprintf("Annual mortality is not reported: %s in %s (%d died), too few trackable plants for a rate (needs %d).",
      pl(mr$n0, "trackable plant"), pl(mr$n_plots, "plot"), mr$deaths, VEG_MORT_MIN_N0),
    if (mr$deaths > 0) sprintf("Annual mortality about %s%%/yr (95%% interval %s to %s): %s of %s died over a mean %s years in %s%s%s.",
      .vh_num(mr$rate_pct, 2), .vh_num(mr$lo, 2), .vh_num(mr$hi, 2), .vh_count(mr$deaths), pl(mr$n0, "trackable plant"),
      .vh_num(mr$t_yrs, 1), pl(mr$n_plots, "plot"), window, tower)
    else sprintf("No deaths among %s in %s over a mean %s years%s%s; 95%% upper bound %s%%/yr.",
      pl(mr$n0, "trackable plant"), pl(mr$n_plots, "plot"), .vh_num(mr$t_yrs, 1), window, tower, .vh_num(mr$hi, 2)))
  list(status = mr$status,
       text = paste0(text, " Lost and unknown fates are censored.", .pre2020_sentence(ctx)))
}

# The growth banner sentence for a growth_summary() result (hk copy).
growth_text <- function(gs, spec = SIZE_FOREST) {
  if (is.null(gs) || identical(gs$status, "none")) return(NULL)
  pl <- function(n, one, many = paste0(one, "s")) sprintf("%s %s", .vh_count(n), .plural(n, one, many))
  extra <- paste0(
    if (gs$n_mh_excluded > 0) sprintf(" %s with a changed measurement point %s excluded.", pl(gs$n_mh_excluded, "pair"), if (gs$n_mh_excluded == 1) "is" else "are") else "",
    if (gs$n_short_interval > 0) sprintf(" %s remeasured under 0.9 years apart %s excluded.", pl(gs$n_short_interval, "pair"), if (gs$n_short_interval == 1) "is" else "are") else "",
    if (gs$n_offscreen > 0) sprintf(" %s outside the -2 to +5 cm/yr review screen %s kept in the data but not summarised.", pl(gs$n_offscreen, "change"), if (gs$n_offscreen == 1) "is" else "are") else "")
  if (!identical(gs$status, "reported")) {
    return(paste0(sprintf("%s comparable remeasured %s in %s: too few for a summary (needs %d in %d plots).",
      .vh_count(gs$n_plants), if (gs$n_plants == 1) spec$noun else spec$nouns, pl(gs$n_plots, "plot"),
      VEG_GROWTH_MIN_PLANTS, VEG_GROWTH_MIN_PLOTS), extra))
  }
  paste0(sprintf("Median annualized %s change %s cm/yr (IQR %s to %s) across %s comparable remeasured %s in %s. About %d%% decreased between censuses; decreases can reflect biology, damage or measurement differences, so they are kept and flagged.",
    spec$size_lab, .vh_num(gs$median, 2), .vh_num(gs$q25, 2), .vh_num(gs$q75, 2),
    .vh_count(gs$n_plants), spec$nouns, pl(gs$n_plots, "plot"), round(100 * gs$share_decrease)), extra)
}

# Rows that fail the shared size screen in this channel's growth forms, shaped
# for the QC report (one row per flagged record, full plant key for joins).
.size_spike_rows <- function(trees, spec = SIZE_FOREST) {
  empty <- data.frame(plotID = character(), plant_key = character(), individualID = character(),
                      plant = character(), species = character(), year = integer(),
                      height_m = numeric(), diameter_cm = numeric(), screen_reason = character(),
                      height_reference_m = numeric(), diameter_reference_cm = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(trees) || !nrow(trees) || !"growthForm" %in% names(trees)) return(empty)
  d <- as.data.frame(trees)[trees$growthForm %in% spec$forms, , drop = FALSE]
  if (!nrow(d)) return(empty)
  screen_columns <- c("size_plausible", "size_screen_reason",
                      "size_height_reference_m", "size_diameter_reference_cm")
  sc <- if (all(screen_columns %in% names(d))) d[, screen_columns, drop = FALSE] else vst_size_screen(d)
  bad <- !(sc$size_plausible %in% TRUE)
  if (!any(bad)) return(empty)
  d <- d[bad, , drop = FALSE]; why <- sc$size_screen_reason[bad]
  yr <- if ("year" %in% names(d)) suppressWarnings(as.integer(d$year)) else
    if ("date" %in% names(d)) suppressWarnings(as.integer(format(as.Date(d$date), "%Y"))) else rep(NA_integer_, nrow(d))
  out <- data.frame(plotID = .chr(d$plotID),
    plant_key = paste(.chr(d$plotID), .chr(d$individualID), sep = "::"),
    individualID = .chr(d$individualID),
    plant = sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", .chr(d$individualID)),
    species = if ("scientificName" %in% names(d)) .chr(d$scientificName) else NA_character_,
    year = yr, height_m = round(.num(d$height), 1), diameter_cm = round(.num(d[[spec$col]]), 1),
    screen_reason = why,
    height_reference_m = round(.num(sc$size_height_reference_m[bad]), 1),
    diameter_reference_cm = round(.num(sc$size_diameter_reference_cm[bad]), 1),
    stringsAsFactors = FALSE)
  out[order(out$plotID, out$individualID, out$year), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# SITE-LEVEL data-quality scan (the small-mammal QC signature) — ranked
# "verify, not wrong" flags across all of a site's remeasured plants, each with
# the offending individuals so the UI can show an inspector + a downloadable CSV.
# Returns list(flags = list of {level,key,label,why,n,rows}, n_flag, report).
# rows = a tidy data.frame per flag (individualID, species, the evidence columns).
# ---------------------------------------------------------------------------
tree_qc_site <- function(trees, spec = SIZE_FOREST, plots = NULL) {
  size_rows <- .size_spike_rows(trees, spec)
  trees <- .supported_history(trees, plots, spec, allow_legacy = TRUE)
  has_history <- !is.null(trees) && nrow(trees) > 0L
  if (!has_history && !nrow(size_rows)) return(NULL)
  flags <- list()
  add <- function(level, key, label, why, rows) {
    if (is.null(rows) || !nrow(rows)) return(invisible())
    rows <- cbind(flag = key, rows, stringsAsFactors = FALSE)
    flags[[length(flags) + 1L]] <<- list(level = level, key = key, label = label, why = why,
      n = nrow(rows), rows = rows) }
  short <- function(x) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(x))

  # 0) Size screen (every event of the channel, held or supported): a size out
  # of line with the plant's own records or with physical bounds. Kept in every
  # summary, left out of the tallest/biggest extremes. Verify, not wrong.
  if (nrow(size_rows)) add("warn", "size_spike", "Size out of line with the plant's records",
    "A height or diameter more than 3x this plant's other records and over 5 m or 5 cm above them (heights compared on intact live records only), a height above 2.5 m per cm of DBH on DBH 10 cm or more, a shrub or sapling over 10 m on a basal diameter under 5 cm or none, or a height above 100 m. The record stays in every count and basal-area summary; the failing measure is left out of the tallest or biggest headlines. Verify the units, decimal point and tag against the field record.",
    size_rows)
  if (!has_history) trees <- NULL

  # 1) Recorded Live AFTER an all-dead EVENT — incompatible with this mortality
  # contract; aggregate stems before
  # looking through time so one dead bole beside a live bole is not a false flag.
  d <- if (has_history) .ensure_event_columns(trees) else NULL
  if (!is.null(d)) d <- d[is.finite(.date_num(d$date)) & !is.na(d$plantStatus) & d$growthForm %in% spec$forms, , drop = FALSE]
  if (!is.null(d) && "permanent" %in% names(d)) d <- d[d$permanent %in% TRUE, , drop = FALSE]
  if (!is.null(d) && nrow(d)) {
    d$.plant_key <- .plant_key(d); d$.event_date <- .date_num(d$date)
    d$.live <- grepl("^Live", d$plantStatus)
    d$.dead <- grepl("Dead|Downed", d$plantStatus, ignore.case = TRUE)
    pd <- d %>% dplyr::group_by(.data$plotID, .data$individualID, .data$.plant_key, .data$eventID) %>%
      dplyr::summarise(date = as.Date(min(.data$.event_date), origin = "1970-01-01"),
        scientificName = .first_known(.data$scientificName),
        live = any(.data$.live), dead = all(.data$.dead),
        .groups = "drop")
    pd <- pd[order(pd$.plant_key, pd$date, .chr(pd$eventID)), , drop = FALSE]
    res <- pd %>% dplyr::group_by(.data$.plant_key) %>%
      dplyr::summarise(plotID = dplyr::first(.data$plotID), individualID = dplyr::first(.data$individualID),
        scientificName = .first_known(.data$scientificName),
        resurrected = any(.data$live & cumsum(.data$dead) > 0),
        # the triggering pair: the first all-dead event and the first live event after it
        dead_i = { i <- which(.data$dead)[1]; if (length(i) && !is.na(i)) i else NA_integer_ },
        dead_eventID = if (is.na(dead_i)) NA_character_ else .chr(.data$eventID[dead_i]),
        dead_date = if (is.na(dead_i)) as.Date(NA) else .data$date[dead_i],
        live_i = { j <- if (is.na(dead_i)) integer() else which(.data$live & seq_along(.data$live) > dead_i); if (length(j)) j[1] else NA_integer_ },
        live_eventID = if (is.na(live_i)) NA_character_ else .chr(.data$eventID[live_i]),
        live_date = if (is.na(live_i)) as.Date(NA) else .data$date[live_i],
        .groups = "drop")
    rr <- res[res$resurrected %in% TRUE, , drop = FALSE]
    # Clonal shrubs resprout after top-kill by fire or browsing, so on the
    # shrub channel a reversal is a warn-level check, not a contract breach.
    shrub_channel <- identical(spec$channel, "shrub_sapling_basal")
    if (nrow(rr)) add(if (shrub_channel) "warn" else "high", "resurrection", "Live status recorded after Dead",
      if (shrub_channel) "Shrubs can resprout after being top-killed by fire or browsing, so a live record after an all-dead one can be real; verify identity, status coding and the event records, and note that the mortality rate censors these plants."
      else "This status reversal is incompatible with the mortality state contract; verify identity, status coding, and the event records before interpretation.",
      data.frame(plotID = rr$plotID, plant_key = paste(rr$plotID, rr$individualID, sep = "::"),
        individualID = as.character(rr$individualID), plant = short(rr$individualID),
        species = rr$scientificName,
        dead_eventID = rr$dead_eventID, dead_date = .iso_date(rr$dead_date),
        live_eventID = rr$live_eventID, live_date = .iso_date(rr$live_date),
        stringsAsFactors = FALSE))
  }

  # growth-derived flags (per permanent individual, like-for-like increments)
  g <- if (has_history) tree_growth(trees, spec, plots) else NULL
  if (!is.null(g) && nrow(g)) {
    jump <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 5 & !g$mh_change, , drop = FALSE]
    if (nrow(jump)) add("high", "jump", "Implausible diameter jump (>5 cm/yr)",
      "A diameter increase faster than ~5 cm/yr is outside the display screen; verify the measurement point, tag, units, and source record before interpretation.",
      data.frame(plotID = jump$plotID, plant_key = paste(jump$plotID, jump$individualID, sep = "::"),
        individualID = as.character(jump$individualID), plant = short(jump$individualID),
        species = jump$scientificName,
        start_cm = round(jump$d0, 1), later_cm = round(jump$d1, 1), cm_per_yr = jump$growth_cm_yr,
        date_start = .iso_date(jump$date0), date_later = .iso_date(jump$date1), years = round(jump$yrs, 2),
        stringsAsFactors = FALSE))
    shrink <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr < -2 & !g$mh_change, , drop = FALSE]
    if (nrow(shrink)) add("warn", "shrink", "Large shrink (< −2 cm/yr)",
      "A decrease steeper than 2 cm/yr can reflect biology, damage, or measurement differences; verify the measurement point, tag, units, and source record.",
      data.frame(plotID = shrink$plotID, plant_key = paste(shrink$plotID, shrink$individualID, sep = "::"),
        individualID = as.character(shrink$individualID), plant = short(shrink$individualID),
        species = shrink$scientificName,
        start_cm = round(shrink$d0, 1), later_cm = round(shrink$d1, 1), cm_per_yr = shrink$growth_cm_yr,
        date_start = .iso_date(shrink$date0), date_later = .iso_date(shrink$date1), years = round(shrink$yrs, 2),
        stringsAsFactors = FALSE))
    mh <- g[g$mh_change %in% TRUE, , drop = FALSE]
    if (nrow(mh)) add("info", "mh", "Measurement height moved between events",
      "The point on the stem where diameter is taken changed, so the before/after increment isn't apples-to-apples, and these are kept but excluded from growth stats.",
      data.frame(plotID = mh$plotID, plant_key = paste(mh$plotID, mh$individualID, sep = "::"),
        individualID = as.character(mh$individualID), plant = short(mh$individualID),
        species = mh$scientificName,
        start_cm = round(mh$d0, 1), later_cm = round(mh$d1, 1),
        mh_start_cm = round(mh$mh0, 1), mh_later_cm = round(mh$mh1, 1),
        date_start = .iso_date(mh$date0), date_later = .iso_date(mh$date1),
        stringsAsFactors = FALSE))
  }
  ord <- c(high = 1L, warn = 2L, info = 3L)
  flags <- flags[order(vapply(flags, function(f) ord[[f$level]], integer(1)))]
  # per-flag row frames have different columns (jump/shrink carry sizes, resurrection
  # doesn't) — bind_rows fills the gaps with NA so the report never errors.
  report <- if (length(flags)) dplyr::bind_rows(lapply(flags, function(f) {
    r <- f$rows; r$flag <- NULL
    cbind(level = f$level, issue = f$label, r, stringsAsFactors = FALSE) })) else data.frame()
  if (nrow(report)) report <- cbind(contract_id = VEG_CONTRACT_ID, report, stringsAsFactors = FALSE)
  list(flags = flags, n_flag = length(flags), report = report, contract_id = VEG_CONTRACT_ID,
       n_history = if (has_history) nrow(trees) else 0L,
       n_growth_pairs = if (is.null(g)) 0L else nrow(g))
}

# The QC report frame for a download: the scan's report, or a zero-row frame
# with every documented column when nothing was flagged, so a clean site's
# qc_report.csv has the same columns as a flagged one.
qc_report_frame <- function(q) {
  if (!is.null(q) && is.data.frame(q$report) && nrow(q$report)) return(q$report)
  dict <- qc_dictionary()
  cols <- lapply(seq_len(nrow(dict)), function(i) {
    switch(dict$type[[i]], numeric = numeric(), integer = integer(), logical = logical(), character())
  })
  names(cols) <- dict$column
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}
