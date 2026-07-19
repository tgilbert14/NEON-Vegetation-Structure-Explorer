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
mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }
smax <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) }   # NA (not -Inf) on empty
smean <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else mean(x) }
short_tree <- function(id) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(id))   # NEON.PLA.D01.000123 -> 000123
short_plot <- function(p) sub("^[A-Z]{4}_", "", as.character(p))

VEG_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v2"
VEG_CONTRACT <- list(
  id = VEG_CONTRACT_ID,
  product = "DP1.10098.001",
  release = "RELEASE-2026",
  plant_key = c("plotID", "individualID"),
  event_key = c("plotID", "eventID"),
  source_record_key = "source_uid",
  protocol_stem_locator = c("plotID", "eventID", "individualID", "tempStemID"),
  opportunity_source_record_key = "source_record_key",
  supported_status = c("sampled_with_records", "sampled_absence"),
  held_status = c("held_sampling_impractical", "held_dendrometer_only",
                  "held_missing_area", "held_opportunity_unknown",
                  "held_presence_record_conflict", "held_metric_invalid",
                  "held_identity_conflict", "held_snapshot_event_mismatch"),
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
  channel = "tree_dbh", metric_kind = "bole-DBH basal area (breast height)",
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

# Compatibility fallback only. A v2 bundle supplies its display channel in meta;
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
.event_key <- function(d) paste(.chr(d$plotID), .chr(d$eventID), sep = "\r")
.ensure_event_columns <- function(d) {
  if (is.null(d)) return(d)
  if (!"plotID" %in% names(d)) d$plotID <- NA_character_
  if (!"eventID" %in% names(d)) {
    # Legacy bundles predate event preservation. Date is only a compatibility
    # token here; opportunity-aware stand estimates require a v2 eventID.
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
  out <- trees %>%
    dplyr::group_by(.data$.plant_key) %>%
    dplyr::filter(.data$.event_date == max(.data$.event_date, na.rm = TRUE)) %>%
    dplyr::filter(.data$.event_sort == max(.data$.event_sort, na.rm = TRUE)) %>%
    dplyr::ungroup()
  out$.plant_key <- NULL; out$.event_date <- NULL; out$.event_sort <- NULL
  attr(out, "contract_id") <- VEG_CONTRACT_ID
  out
}
one_per_tree <- function(snap, spec = SIZE_FOREST) {
  if (is.null(snap) || !nrow(snap)) return(snap)
  snap <- .ensure_event_columns(snap)
  snap$.size <- snap[[spec$col]]
  snap$.plant_key <- .plant_key(snap)
  out <- snap %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::slice_max(.data$.size, n = 1, with_ties = FALSE, na_rm = FALSE) %>% dplyr::ungroup()
  out$.size <- NULL; out$.plant_key <- NULL; out
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
.supported_history <- function(trees, plots, spec = SIZE_FOREST) {
  if (is.null(trees) || !nrow(trees) || is.null(plots) || !nrow(plots)) return(NULL)
  t <- .ensure_event_columns(as.data.frame(trees))
  p <- .ensure_event_columns(as.data.frame(plots))
  if (!(spec$support %in% names(p)) || !(spec$area %in% names(p))) return(NULL)
  support <- .chr(p[[spec$support]])
  area <- .num(p[[spec$area]])
  keep <- support == "sampled_with_records" & is.finite(area) & area > 0 &
    !is.na(p$plotID) & nzchar(.chr(p$plotID)) & !is.na(p$eventID) & nzchar(.chr(p$eventID))
  keys <- unique(.event_key(p[keep, , drop = FALSE]))
  if (!length(keys)) return(NULL)
  out <- t[.event_key(t) %in% keys, , drop = FALSE]
  if (!nrow(out)) NULL else out
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
    s$.plant_key <- .plant_key(s)
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
                      "later_held_reasons"), names(selected))
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
  list(ba_ha = mean(ok$ba_ha, na.rm = TRUE),
       ba_se = se(ok$ba_ha),
       density_ha = mean(ok$density_ha, na.rm = TRUE),
       density_se = se(ok$density_ha),
       qmd = if (n_stems > 0) sqrt(sum(ok$sumD2, na.rm = TRUE) / n_stems) else NA_real_,
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
  if (is.null(s) || !nrow(s)) {
    if (is.null(per) || !any(per$supported %in% TRUE)) return(NULL)
    out <- data.frame(cls = factor(bk$labs, levels = bk$labs), stems = 0L)
  } else {
    s$cls <- cut(s[[spec$col]], breaks = bk$brks, labels = bk$labs, right = FALSE)
    out <- s %>% dplyr::count(.data$cls, name = "stems") %>%
      tidyr::complete(cls = factor(bk$labs, levels = bk$labs), fill = list(stems = 0))
  }
  # area-standardize to stems/ha when plots are supplied (raw counts are
  # sampling-effort-dependent). Pooled over the sampled area of contributing plots.
  if (!is.null(per)) {
    tot_ha <- sum(per$area_ha[per$supported %in% TRUE], na.rm = TRUE)
    out$stems_ha <- if (is.finite(tot_ha) && tot_ha > 0) out$stems / tot_ha else NA_real_
  }
  out
}

# ---------------------------------------------------------------------------
# Per-plant growth: diameter increment between the first and last live bouts,
# annualised. De-pseudoreplicated (one rate per plant). Negative increments
# (shrinkage = measurement error / bole damage) are flagged, not deleted. Uses
# the paradigm's diameter (DBH for trees, basal for shrubs).
# ---------------------------------------------------------------------------
tree_growth <- function(trees, spec = SIZE_FOREST, plots = NULL) {
  g <- .supported_history(trees, plots, spec)
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
  loc <- tolower(trimws(.chr(g$changedMeasurementLocation)))
  g$.pom_changed <- !is.na(loc) & nzchar(loc) & !(loc %in% c("no", "false", "0", "none", "nochange", "no change"))
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
      mh_change = any(.data$location_changed) || any(.data$mh_span > 0.1) ||
                  (is.finite(dplyr::first(.data$mh)) && is.finite(dplyr::last(.data$mh)) &&
                   abs(dplyr::last(.data$mh) - dplyr::first(.data$mh)) > 0.1),
      .groups = "drop") %>%
    dplyr::filter(.data$yrs > 0) %>%
    dplyr::mutate(growth_cm_yr = (.data$d1 - .data$d0) / .data$yrs,
                  shrank = .data$growth_cm_yr < -0.1,
                  contract_id = VEG_CONTRACT_ID)
  out$.plant_key <- NULL
  if (!nrow(out)) NULL else out
}

# one plant's full preserved measurement history (shown as records on the card)
tree_history <- function(trees, id, plotID = NULL) {
  if (is.null(trees) || is.null(id)) return(NULL)
  h <- trees[trees$individualID == id & !is.na(trees$date), , drop = FALSE]
  if (!is.null(plotID)) h <- h[h$plotID == plotID, , drop = FALSE]
  if (!nrow(h)) return(NULL)
  if (length(unique(stats::na.omit(h$plotID))) > 1) return(NULL)
  h <- .ensure_event_columns(h)
  keep <- intersect(c("plotID", "eventID", "individualID", "tempStemID", "date", "year", "scientificName",
                      "stemDiameter", "basalStemDiameter", "height", "plantStatus", "live", "permanent", "growthForm", "canopyPosition",
                      "measurementHeight", "basalMeasurementHeight", "changedMeasurementLocation"), names(h))
  h[order(.date_num(h$date), .chr(h$eventID), .chr(h$tempStemID)), keep, drop = FALSE]
}

# One plant's event-aware diameter trajectory. Full-plot multi-bole tree events
# use equivalent diameter sqrt(sum(d^2)). Multi-stem basal trajectories are
# withheld because shrub/sapling tempStemID cannot be aligned across years.
tree_trajectory <- function(trees, id, col) {
  if (is.null(trees) || is.null(id) || is.null(col)) return(NULL)
  h <- trees[trees$individualID == id & !is.na(trees$date), , drop = FALSE]
  if (!nrow(h) || !(col %in% names(h))) return(NULL)
  if (length(unique(stats::na.omit(h$plotID))) > 1) return(NULL)
  h <- .ensure_event_columns(h)
  spec <- if (identical(col, "basalStemDiameter")) SIZE_SHRUB else SIZE_FOREST
  h$.d <- h[[col]]
  live_ok <- if ("live" %in% names(h)) h$live %in% TRUE else rep(TRUE, nrow(h))
  g <- h[h$growthForm %in% spec$forms & is.finite(h$.d) & h$.d > 0 & h$.d >= spec$min & live_ok, , drop = FALSE]
  if (!nrow(g)) return(NULL)
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

# ---------------------------------------------------------------------------
# Per-plant QC flags from its history, ranked. Every flag is "verify", not "wrong".
# Diameter decreases can reflect biology, damage, a changed measurement point,
# or other measurement differences. Keep the observation, flag it, and explain.
# ---------------------------------------------------------------------------
tree_qc_flags <- function(hist, spec = SIZE_FOREST, plots = NULL) {
  flags <- list(); add <- function(level, text) flags[[length(flags) + 1L]] <<- list(level = level, text = text)
  h <- .supported_history(hist, plots, spec)
  if (is.null(h) || !nrow(h)) {
    add("info", "No supported measurement-bearing event is available for a longitudinal check.")
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
  one <- one_per_tree(s, spec)
  if (!nrow(one)) return(NULL)
  one$.d <- one[[spec$col]]
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
  one$.plant_key <- .plant_key(one)
  ba$.plant_key <- .plant_key(ba)
  one %>% dplyr::group_by(.data$.taxon_label) %>%
    dplyr::summarise(plants = dplyr::n_distinct(.data$.plant_key),
                     family = mode_chr(.data$family),
                     max_dbh = smax(.data$.d),
                     max_ht = smax(.data$height),
                     mean_dbh = smean(.data$.d),
                     .groups = "drop") %>%
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
    sp <- s %>% dplyr::group_by(.data$plotID, .data$eventID) %>%
      dplyr::summarise(n_taxa = dplyr::n_distinct(.data$.taxon_label[!is.na(.data$.taxon_label) & nzchar(.data$.taxon_label)]),
                       n_species = dplyr::n_distinct(.data$.taxon_label[.data$.is_species & !is.na(.data$.taxon_label) & nzchar(.data$.taxon_label)]),
                       tallest = smax(.data$height),
                       biggest = smax(.data$.d),
                       dominant = { i <- which.max(.data$.d); if (length(i)) .data$.taxon_label[i] else NA_character_ },
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
    protocol_stem_key = paste(
      .chr(d$plotID), .chr(d$eventID), .chr(d$individualID),
      .chr(d$tempStemID), sep = "::"
    ),
    protocol_key_group_n = col_or_na(d, "protocol_key_group_n"),
    protocol_key_conflict = col_or_na(d, "protocol_key_conflict"),
    plant_key = paste(.chr(d$plotID), .chr(d$individualID), sep = "::"),
    plotID = d$plotID,
    eventID = d$eventID,
    individualID = d$individualID,
    tempStemID = d$tempStemID,
    subplotID = col_or_na(d, "subplotID"),
    mapping_eventID = col_or_na(d, "mappingEventID"),
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
    ba_m2_ha = ps$ba_ha, density_stems_ha = ps$density_ha,
    qmd_cm = ps$qmd, n_live_stems = ps$stems, n_live_plants = ps$plants,
    n_taxa = ps$n_taxa, n_species = ps$n_species,
    tallest_m = ps$tallest, biggest_diam_cm = ps$biggest,
    largest_stem_taxon = ps$dominant, stringsAsFactors = FALSE)
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
    c("protocol_stem_key","trees_long","character","plotID::eventID::individualID::tempStemID","Plot-scoped display of the protocol stem locator. The published three-field locator should be unique, but plot scoping prevents known cross-plot tag collisions from contaminating another plot; true within-plot anomalies are preserved and flagged."),
    c("protocol_key_group_n","trees_long/plot_opportunity_source","integer","records","Number of published source records sharing the relevant protocol locator."),
    c("protocol_key_conflict","trees_long/plot_opportunity_source","logical","TRUE/FALSE","TRUE when the protocol locator is not unique; affected physical-channel summaries are held."),
    c("plant_key","trees_long","character","plotID::individualID","Canonical plant identity in this app. individualID alone is not unique across plots."),
    c("plotID","trees_long/plot_summary_latest/plot_opportunities_all","character","","NEON plot identifier; part of the plant and plot-event keys."),
    c("eventID","trees_long/plot_summary_latest/plot_opportunities_all","character","","NEON sampling event identifier. Plot-event identity is plotID x eventID; date alone is never used as the event key."),
    c("individualID","trees_long","character","","NEON plant tag. Always pair with plotID; TEMP.PLA ids are not stable remeasurement identities."),
    c("tempStemID","trees_long","character","","Stem-within-event locator. It is not stable across years for multi-bole shrubs/saplings and can be missing or conflicting in documented source anomalies."),
    c("subplotID","trees_long","character","","Nested subplot within the plot."),
    c("mapping_eventID","trees_long","character","","Mapping/tagging event chosen by the deterministic latest-created identity join."),
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
    c("size_metric","plots","character","bole-DBH basal area (breast height)/basal-diameter cover (stem base)","Physical meaning of ba_m2_ha on this row."),
    c("event_date","plots","date (ISO)","YYYY-MM-DD","Date of the selected latest supported plot event."),
    c("plotType","plots","character","distributed/tower","NEON plot design class. The app reports a sampled-plot summary; restrict to distributed plots for the spatially balanced design stratum."),
    c("nlcdClass","plots","character","","NEON land-cover class (NLCD) at the plot."),
    c("lat","plots","numeric","decimal degrees","Plot centroid latitude (WGS84 decimal degrees)."),
    c("lng","plots","numeric","decimal degrees","Plot centroid longitude (WGS84 decimal degrees)."),
    c("sampled_area_m2","plots","numeric","m^2","Positive event-specific sampled area for this channel. No temporal median or minimum-area cutoff is applied; valid 40 m2 areas are retained."),
    c("support_status","plots","character",paste(c(VEG_CONTRACT$supported_status, VEG_CONTRACT$held_status), collapse = "/"),"Sampling-opportunity disposition. sampled_absence is an observed zero; every held_* value is unsupported, not zero."),
    c("support_reason","plots","character","","Human-readable reason for the support disposition."),
    c("n_held_events","plots","integer","events","Count of held opportunities for this plot across the preserved event history."),
    c("n_later_held","plots","integer","events","Count of held attempts later than the selected latest supported census."),
    c("held_reasons","plots","character","","Distinct reasons across held opportunities for this plot."),
    c("later_held_reasons","plots","character","","Distinct reasons for held attempts later than the selected census."),
    c("supported","plots","logical","TRUE/FALSE","TRUE only for sampled_with_records or sampled_absence with a positive event-specific area."),
    c("sampled_absence","plots","logical","TRUE/FALSE","Explicit protocol-supported absence. Metric values are zero. FALSE with supported=FALSE must remain NA."),
    c("ba_m2_ha","plots","numeric","m^2/ha","Live cross-sectional area per hectare for this channel. NA means held/unsupported; zero means explicit sampled absence or a supported census with no eligible live stems."),
    c("density_stems_ha","plots","numeric","stems/ha","Live stem density per hectare for this plot."),
    c("qmd_cm","plots","numeric","cm","Stem-weighted quadratic mean diameter: sqrt(sum(diameter^2) / number of live stem rows)."),
    c("n_live_stems","plots","integer","stems","Count of eligible live apparent-individual rows; multi-stem plants contribute multiple stems."),
    c("n_live_plants","plots","integer","plants","Count of distinct plotID x individualID plant keys."),
    c("n_taxa","plots","integer","","Count of distinct live explicit/coarse taxon labels in the plot. Supported zeros are 0; held opportunities remain NA."),
    c("n_species","plots","integer","","Count of distinct live species-or-finer labels (`is_species = TRUE`) in the plot. Supported zeros are 0; held opportunities remain NA."),
    c("tallest_m","plots","numeric","m","Height of the tallest live plant in the plot."),
    c("biggest_diam_cm","plots","numeric","cm","Largest eligible live stem diameter in the plot: DBH for tree_dbh, basal stem diameter for shrub_sapling_basal."),
    c("largest_stem_taxon","plot_summary_latest","character","","Taxon label attached to the largest eligible live stem in the selected plot event; not an ecological-dominance estimate."),
    c("date","plot_opportunities_all","date (ISO)","YYYY-MM-DD","Published plot-event measurement date; ordering context, not the event identity."),
    c("opportunity_source_uid","plot_opportunities_all","character","published uid","Deterministically selected source row retained only to carry nominal fields when a plot-event has multiple source records; a conflict is always held."),
    c("opportunity_source_record_count","plot_opportunities_all","integer","records","Number of published vst_perplotperyear source rows sharing the plotID + eventID key."),
    c("opportunity_key_conflict","plot_opportunities_all","logical","TRUE/FALSE","TRUE when multiple published opportunity rows share one plot-event key. Both physical channels are held rather than selecting a denominator as truth."),
    c("opportunity_source_uids","plot_opportunities_all","character","semicolon-separated published uids","Complete uid inventory for source rows sharing this plot-event key."),
    c("year","plot_opportunities_all","integer","calendar year","Calendar year of the plot-event opportunity."),
    c("eventType","plot_opportunities_all","character","published event type","Published NEON event classification retained for protocol review."),
    c("samplingImpractical","plot_opportunities_all","character","ok / protocol reason","Published opportunity field. Values other than protocol-supported ok can cause a held state."),
    c("dataCollected","plot_opportunities_all","character","allGrowthForms/dendrometerOnly/...","Published collection-scope field; dendrometer-only events are not scaled as census measurements."),
    c("treesPresent","plot_opportunities_all","character","published presence state","Published tree-channel presence/absence evidence used with record consistency checks."),
    c("shrubsPresent","plot_opportunities_all","character","published presence state","Published shrub/sapling-channel presence/absence evidence used with record consistency checks."),
    c("treePresence","plot_opportunities_all","character","normalized present/absent/unknown","Normalized tree-channel presence evidence derived from the published opportunity fields."),
    c("shrubSaplingPresence","plot_opportunities_all","character","normalized present/absent/unknown","Normalized shrub/sapling-channel presence evidence derived from the published opportunity fields."),
    c("area_trees","plot_opportunities_all","numeric m^2",">0 or NA","Exact event-specific tree sampled area; NA is not zero."),
    c("area_shrub","plot_opportunities_all","numeric m^2",">0 or NA","Exact event-specific shrub/sapling sampled area; NA is not zero."),
    c("tree_support","plot_opportunities_all","character",paste(c(VEG_CONTRACT$supported_status, setdiff(VEG_CONTRACT$held_status, "held_snapshot_event_mismatch")), collapse = "/"),"Tree-channel opportunity state. Only sampled_absence is zero."),
    c("tree_support_reason","plot_opportunities_all","character","","Reason for the tree-channel opportunity state."),
    c("tree_supported","plot_opportunities_all","logical","TRUE/FALSE","TRUE only when tree_support is sampled_with_records or sampled_absence; positive area is separately required at point of use."),
    c("shrub_support","plot_opportunities_all","character",paste(c(VEG_CONTRACT$supported_status, setdiff(VEG_CONTRACT$held_status, "held_snapshot_event_mismatch")), collapse = "/"),"Shrub/sapling-channel opportunity state. Only sampled_absence is zero."),
    c("shrub_support_reason","plot_opportunities_all","character","","Reason for the shrub/sapling-channel opportunity state."),
    c("shrub_supported","plot_opportunities_all","logical","TRUE/FALSE","TRUE only when shrub_support is sampled_with_records or sampled_absence; positive area is separately required at point of use."),
    c("tree_records","plot_opportunities_all","integer records","≥0","Apparent-individual rows matched to this plot event in the tree channel."),
    c("shrub_records","plot_opportunities_all","integer records","≥0","Apparent-individual rows matched to this plot event in the shrub/sapling basal channel."),
    c("tree_invalid_metric_records","plot_opportunities_all","integer records","≥0","Eligible live tree-channel rows whose DBH is missing, non-finite, non-positive, or below 10 cm. Any such row holds the event unless an earlier protocol or presence-conflict state takes precedence."),
    c("shrub_invalid_metric_records","plot_opportunities_all","integer records","≥0","Eligible live shrub/sapling-channel rows whose basal stem diameter is missing, non-finite, or non-positive. Any such row holds the event unless an earlier protocol or presence-conflict state takes precedence."),
    c("tree_identity_conflict_keys","plot_opportunities_all","integer protocol locators","≥0","Count of non-unique plotID + eventID + individualID + tempStemID locators involving the tree-DBH channel. Any positive count holds that channel event."),
    c("shrub_identity_conflict_keys","plot_opportunities_all","integer protocol locators","≥0","Count of non-unique plotID + eventID + individualID + tempStemID locators involving the shrub/sapling basal channel. Any positive count holds that channel event."),
    c("event_key","plot_opportunities_all","character","plotID::eventID","Deterministic display form of the opportunity key; the source fields remain authoritative."))
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
    c("flag","character","resurrection/jump/shrink/mh","Stable machine key for the flag type (present on per-flag downloads)."),
    c("plotID","character","","NEON plot identifier; pair with individualID."),
    c("plant_key","character","plotID::individualID","Canonical join key for the flagged plant."),
    c("individualID","character","","FULL NEON tag. It is not a sufficient join key without plotID."),
    c("plant","character","","Short display id (site/domain prefix stripped); for reading only, NOT a join key."),
    c("species","character","","Identified taxon for the flagged plant (may be NA if unidentified)."),
    c("start_cm","numeric","cm","Earlier supported-bout diameter (DBH for tree_dbh, basal stem diameter for shrub_sapling_basal). NA for the status-reversal flag."),
    c("later_cm","numeric","cm","Later supported-bout diameter, same measure as start_cm. NA for the status-reversal flag."),
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
  s <- snap[snap$growthForm %in% forms, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  s$.plant_key <- .plant_key(s)
  st <- .chr(s$plantStatus)
  s$.live <- !is.na(st) & grepl("^Live", st)
  s$.dead <- !is.na(st) & grepl("Dead|Downed", st, ignore.case = TRUE)
  s$.lost <- !is.na(st) & grepl("Lost|Removed|No longer", st, ignore.case = TRUE)
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
stand_mortality <- function(trees, spec = SIZE_FOREST, plots = NULL) {
  if (is.null(trees) || !nrow(trees) || !"date" %in% names(trees)) return(NULL)
  d <- .supported_history(trees, plots, spec)
  if (is.null(d) || !nrow(d)) return(NULL)
  forms <- spec$forms %||% TREE_FORMS
  d <- d[d$growthForm %in% forms & is.finite(.date_num(d$date)), , drop = FALSE]
  if ("permanent" %in% names(d)) d <- d[d$permanent %in% TRUE, , drop = FALSE]
  if (is.null(d) || !nrow(d) || !"plantStatus" %in% names(d)) return(NULL)
  st <- .chr(d$plantStatus)
  d$.live <- !is.na(st) & grepl("^Live", st)
  d$.dead <- !is.na(st) & grepl("Dead|Downed", st, ignore.case = TRUE)
  d$.lost <- !is.na(st) & grepl("Lost|Removed|No longer", st, ignore.case = TRUE)
  d$.event_date <- .date_num(d$date)
  d$.plant_key <- .plant_key(d)
  ev <- d %>% dplyr::group_by(.data$plotID, .data$individualID, .data$.plant_key, .data$eventID) %>%
    dplyr::summarise(date = as.Date(min(.data$.event_date), origin = "1970-01-01"),
                     any_live = any(.data$.live),
                     all_dead = all(.data$.dead),
                     any_lost = any(.data$.lost), .groups = "drop")
  ev$state <- ifelse(ev$any_live, "live", ifelse(ev$all_dead, "dead", "censored"))
  ev <- ev[order(ev$.plant_key, ev$date, .chr(ev$eventID)), , drop = FALSE]
  bad <- ev %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::summarise(resurrected = any(.data$state == "live" & cumsum(.data$state == "dead") > 0), .groups = "drop")
  ev <- ev[!(ev$.plant_key %in% bad$.plant_key[bad$resurrected %in% TRUE]), , drop = FALSE]
  ev <- ev %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::mutate(after_censor = cumsum(.data$state == "censored") > 0) %>% dplyr::ungroup()
  known <- ev[ev$state %in% c("live", "dead") & !ev$after_censor, , drop = FALSE]
  if (!nrow(known)) return(NULL)
  per <- known %>% dplyr::group_by(.data$.plant_key) %>%
    dplyr::filter(dplyr::n_distinct(.data$eventID) >= 2) %>%
    dplyr::summarise(plotID = dplyr::first(.data$plotID), individualID = dplyr::first(.data$individualID),
                     first_state = dplyr::first(.data$state), last_state = dplyr::last(.data$state),
                     t = as.numeric(dplyr::last(.data$date) - dplyr::first(.data$date)) / 365.25,
                     .groups = "drop")
  coh <- per[per$first_state == "live" & per$t > 0, , drop = FALSE]
  n0 <- nrow(coh); if (n0 < 10) return(NULL)
  coh$death <- coh$last_state == "dead"
  estimate <- function(x) {
    if (!nrow(x)) return(NA_real_)
    tt <- mean(x$t, na.rm = TRUE); q <- mean(x$death)
    if (!is.finite(tt) || tt <= 0) return(NA_real_)
    100 * (1 - (1 - q)^(1 / tt))
  }
  rate <- estimate(coh); if (!is.finite(rate)) return(NULL)
  pl <- unique(coh$plotID); jk <- numeric()
  if (length(pl) >= 3) jk <- vapply(pl, function(p) estimate(coh[coh$plotID != p, , drop = FALSE]), numeric(1))
  jk <- jk[is.finite(jk)]
  if (length(jk) >= 3) {
    se <- sqrt((length(jk) - 1) / length(jk) * sum((jk - mean(jk))^2))
    ci <- pmax(0, pmin(100, rate + c(-1, 1) * 1.96 * se))
  } else ci <- c(NA_real_, NA_real_)
  list(rate_pct = round(rate, 2), n0 = n0, deaths = sum(coh$death), t_yrs = round(mean(coh$t), 1),
       lo = if (is.finite(ci[1])) round(ci[1], 2) else NA_real_,
       hi = if (is.finite(ci[2])) round(ci[2], 2) else NA_real_,
       n_plots = length(pl), n_censored_events = sum(ev$state == "censored"),
       ci_method = if (length(jk) >= 3) "delete-one-plot jackknife" else "not reported (<3 plot clusters)",
       contract_id = VEG_CONTRACT_ID)
}

# ---------------------------------------------------------------------------
# SITE-LEVEL data-quality scan (the small-mammal QC signature) — ranked
# "verify, not wrong" flags across all of a site's remeasured plants, each with
# the offending individuals so the UI can show an inspector + a downloadable CSV.
# Returns list(flags = list of {level,key,label,why,n,rows}, n_flag, report).
# rows = a tidy data.frame per flag (individualID, species, the evidence columns).
# ---------------------------------------------------------------------------
tree_qc_site <- function(trees, spec = SIZE_FOREST, plots = NULL) {
  trees <- .supported_history(trees, plots, spec)
  if (is.null(trees) || !nrow(trees)) return(NULL)
  flags <- list()
  add <- function(level, key, label, why, rows) {
    if (is.null(rows) || !nrow(rows)) return(invisible())
    rows <- cbind(flag = key, rows, stringsAsFactors = FALSE)
    flags[[length(flags) + 1L]] <<- list(level = level, key = key, label = label, why = why,
      n = nrow(rows), rows = rows) }
  short <- function(x) sub("^NEON\\.PLA\\.D[0-9]{2}\\.", "", as.character(x))

  # 1) Recorded Live AFTER an all-dead EVENT — incompatible with this mortality
  # contract; aggregate stems before
  # looking through time so one dead bole beside a live bole is not a false flag.
  d <- .ensure_event_columns(trees)
  d <- d[is.finite(.date_num(d$date)) & !is.na(d$plantStatus) & d$growthForm %in% spec$forms, , drop = FALSE]
  if ("permanent" %in% names(d)) d <- d[d$permanent %in% TRUE, , drop = FALSE]
  if (nrow(d)) {
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
        resurrected = any(.data$live & cumsum(.data$dead) > 0), .groups = "drop")
    rr <- res[res$resurrected %in% TRUE, , drop = FALSE]
    if (nrow(rr)) add("high", "resurrection", "Live status recorded after Dead",
      "This status reversal is incompatible with the mortality state contract; verify identity, status coding, and the event records before interpretation.",
      data.frame(plotID = rr$plotID, plant_key = paste(rr$plotID, rr$individualID, sep = "::"),
        individualID = as.character(rr$individualID), plant = short(rr$individualID),
        species = rr$scientificName, stringsAsFactors = FALSE))
  }

  # growth-derived flags (per permanent individual, like-for-like increments)
  g <- tree_growth(trees, spec, plots)
  if (!is.null(g) && nrow(g)) {
    jump <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr > 5 & !g$mh_change, , drop = FALSE]
    if (nrow(jump)) add("high", "jump", "Implausible diameter jump (>5 cm/yr)",
      "A diameter increase faster than ~5 cm/yr is outside the display screen; verify the measurement point, tag, units, and source record before interpretation.",
      data.frame(plotID = jump$plotID, plant_key = paste(jump$plotID, jump$individualID, sep = "::"),
        individualID = as.character(jump$individualID), plant = short(jump$individualID),
        species = jump$scientificName,
        start_cm = round(jump$d0, 1), later_cm = round(jump$d1, 1), cm_per_yr = jump$growth_cm_yr, stringsAsFactors = FALSE))
    shrink <- g[is.finite(g$growth_cm_yr) & g$growth_cm_yr < -2 & !g$mh_change, , drop = FALSE]
    if (nrow(shrink)) add("warn", "shrink", "Large shrink (< −2 cm/yr)",
      "A decrease steeper than 2 cm/yr can reflect biology, damage, or measurement differences; verify the measurement point, tag, units, and source record.",
      data.frame(plotID = shrink$plotID, plant_key = paste(shrink$plotID, shrink$individualID, sep = "::"),
        individualID = as.character(shrink$individualID), plant = short(shrink$individualID),
        species = shrink$scientificName,
        start_cm = round(shrink$d0, 1), later_cm = round(shrink$d1, 1), cm_per_yr = shrink$growth_cm_yr, stringsAsFactors = FALSE))
    mh <- g[g$mh_change %in% TRUE, , drop = FALSE]
    if (nrow(mh)) add("info", "mh", "Measurement height moved between events",
      "The point on the stem where diameter is taken changed, so the before/after increment isn't apples-to-apples, and these are kept but excluded from growth stats.",
      data.frame(plotID = mh$plotID, plant_key = paste(mh$plotID, mh$individualID, sep = "::"),
        individualID = as.character(mh$individualID), plant = short(mh$individualID),
        species = mh$scientificName,
        start_cm = round(mh$d0, 1), later_cm = round(mh$d1, 1), stringsAsFactors = FALSE))
  }
  ord <- c(high = 1L, warn = 2L, info = 3L)
  flags <- flags[order(vapply(flags, function(f) ord[[f$level]], integer(1)))]
  # per-flag row frames have different columns (jump/shrink carry sizes, resurrection
  # doesn't) — bind_rows fills the gaps with NA so the report never errors.
  report <- if (length(flags)) dplyr::bind_rows(lapply(flags, function(f) {
    r <- f$rows; r$flag <- NULL
    cbind(level = f$level, issue = f$label, r, stringsAsFactors = FALSE) })) else data.frame()
  if (nrow(report)) report <- cbind(contract_id = VEG_CONTRACT_ID, report, stringsAsFactors = FALSE)
  list(flags = flags, n_flag = length(flags), report = report, contract_id = VEG_CONTRACT_ID)
}
