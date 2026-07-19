#!/usr/bin/env Rscript

# Deterministic fixture gate for the DP1.10098.001 v2 ingestion contract.
# It runs without network/data artifacts and exercises the scientific failure
# modes that a candidate release must resolve before bundle promotion.

Sys.setenv(VST_BUNDLE_FUNCTIONS_ONLY = "true", VST_NEON_RELEASE = "RELEASE-2026")
source("scripts/bundle_veg_data.R")

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    stop(sprintf("%s; actual=%s expected=%s", message,
                 paste(actual, collapse = ","), paste(expected, collapse = ",")),
         call. = FALSE)
  }
}

assert_close <- function(actual, expected, message, tolerance = 1e-12) {
  if (length(actual) != length(expected) ||
      any(!is.finite(actual) != !is.finite(expected)) ||
      any(abs(actual - expected) > tolerance, na.rm = TRUE)) {
    stop(sprintf("%s; actual=%s expected=%s", message,
                 paste(actual, collapse = ","), paste(expected, collapse = ",")),
         call. = FALSE)
  }
}

assert_error <- function(expression, pattern, message) {
  caught <- tryCatch({
    force(expression)
    NULL
  }, error = function(error) error)
  if (is.null(caught) || !grepl(pattern, conditionMessage(caught), fixed = TRUE)) {
    stop(message, call. = FALSE)
  }
}

mapping <- data.frame(
  plotID = c("P1", "P1", "P2", "P4", "P6"),
  individualID = c("A", "A", "A", "D", "S"),
  date = as.Date(c("2025-01-01", "2024-01-01", "2024-02-01",
                   "2024-01-01", "2024-01-01")),
  createdDate = c("2020-01-01T00:00:00Z", "2024-01-01T00:00:00Z",
                  "2024-02-01T00:00:00Z", "2024-01-01T00:00:00Z",
                  "2024-01-01T00:00:00Z"),
  taxonID = c("OLD", "NEW", "OTHER", "DEND", "SHRUB"),
  scientificName = c("Old name", "Acer rubrum", "Betula lenta",
                     "Quercus alba", NA),
  genus = c("Old", "Acer", "Betula", "Quercus", "Vaccinium"),
  family = c("Oldaceae", "Sapindaceae", "Betulaceae", "Fagaceae", "Ericaceae"),
  taxonRank = rep("species", 5),
  recordType = rep("mapped and tagged", 5),
  identificationQualifier = rep(NA_character_, 5),
  dataQF = rep(NA_character_, 5),
  stringsAsFactors = FALSE
)

apparent <- data.frame(
  eventID = c("E1", "E1", "E2", "E3", "E6", "E7"),
  plotID = c("P1", "P1", "P1", "P2", "P4", "P6"),
  individualID = c("A", "A", "A", "A", "D", "S"),
  tempStemID = c("1", "2", "1", "1", "1", "1"),
  subplotID = c("31", "31", "31", "31", "31", "31_40_1"),
  date = as.Date(c("2024-06-01", "2024-06-01", "2024-06-01",
                   "2024-07-01", "2024-07-01", "2024-07-01")),
  growthForm = c("single bole tree", "single bole tree", "single bole tree",
                 "single bole tree", "single bole tree", "single shrub"),
  plantStatus = rep("Live", 6),
  stemDiameter = c(12, 11, 13, 15.123, 20, NA),
  basalStemDiameter = c(NA, NA, NA, NA, NA, 2),
  height = c(8, NA, 8.5, 9, 11, 1.2),
  maxCrownDiameter = c(3, NA, 3.2, 4, 5, 1),
  ninetyCrownDiameter = c(2, NA, 2.1, 3, 4, 0.8),
  canopyPosition = c("open", NA, "open", "partial", "open", "understory"),
  measurementHeight = c(130, 130, 130, 130, 130, 10),
  basalMeasurementHeight = c(NA, NA, NA, NA, NA, 10),
  changedMeasurementLocation = rep("noChange", 6),
  tagStatus = rep("ok", 6),
  dendrometerCondition = c(NA, NA, NA, NA, "ok", NA),
  heightQualifier = rep(NA_character_, 6),
  dataQF = rep(NA_character_, 6),
  measurementErrorQF = c(NA, NA, NA, NA, "reviewed", NA),
  stringsAsFactors = FALSE
)

perplot <- data.frame(
  eventID = paste0("E", 1:8),
  plotID = paste0("P", 1:8),
  date = as.Date(c("2024-06-01", "2024-06-01", "2024-07-01", "2024-07-01",
                   "2024-07-01", "2024-07-01", "2024-07-01", "2024-07-01")),
  eventType = rep("distributed", 8),
  plotType = rep("distributed", 8),
  nlcdClass = rep("forest", 8),
  decimalLatitude = 40 + seq_len(8) / 100,
  decimalLongitude = -70 - seq_len(8) / 100,
  samplingImpractical = c("ok", "ok", "ok", "ok", "access denied", "ok", "ok", "ok"),
  dataCollected = c("allGrowthForms", "allGrowthForms", "allGrowthForms",
                    "allGrowthForms", "allGrowthForms", "dendrometerOnly",
                    "allGrowthForms", "allGrowthForms"),
  treesPresent = c("present - sampled", "present - sampled", "present - sampled",
                   "not present", "not present", "present - sampled",
                   "not present", "not present"),
  shrubsPresent = c("not present", "not present", "not present", "not present",
                    "not present", "not present", "present - sampled", "not present"),
  totalSampledAreaTrees = c(400, 40, 40, 40, 40, 40, 40, 40),
  totalSampledAreaShrubSapling = rep(40, 8),
  nestedSubplotAreaShrubSapling = rep(10, 8),
  stringsAsFactors = FALSE
)
# Correct event/plot relationships for measurement-bearing fixture rows.
perplot$plotID <- c("P1", "P1", "P2", "P3", "P5", "P4", "P6", "P7")

# A mixed-validity event must fail closed as a whole. The first live tree has a
# valid DBH and a published dataQF; the second live tree lacks DBH. dataQF is
# preserved for review but is not itself an exclusion rule.
mapping <- rbind(mapping, data.frame(
  plotID = c("P9", "P9"), individualID = c("Q", "R"),
  date = as.Date(c("2025-01-01", "2025-01-01")),
  createdDate = c("2025-01-01T00:00:00Z", "2025-01-01T00:00:00Z"),
  taxonID = c("VALID", "INVALID"),
  scientificName = c("Validus metricus", "Invalidus metricus"),
  genus = c("Validus", "Invalidus"), family = c("Validaceae", "Invalidaceae"),
  taxonRank = c("species", "species"),
  recordType = c("mapped and tagged", "mapped and tagged"),
  identificationQualifier = c(NA_character_, NA_character_),
  dataQF = c(NA_character_, NA_character_), stringsAsFactors = FALSE
))
apparent <- rbind(apparent, data.frame(
  eventID = c("E9", "E9"), plotID = c("P9", "P9"),
  individualID = c("Q", "R"), tempStemID = c("1", "1"),
  subplotID = c("31", "31"), date = as.Date(c("2025-07-01", "2025-07-01")),
  growthForm = c("single bole tree", "single bole tree"),
  plantStatus = c("Live", "Live"), stemDiameter = c(14, NA_real_),
  basalStemDiameter = c(NA_real_, NA_real_), height = c(7, 6),
  maxCrownDiameter = c(2, 2), ninetyCrownDiameter = c(1.5, 1.5),
  canopyPosition = c("partial", "partial"), measurementHeight = c(130, 130),
  basalMeasurementHeight = c(NA_real_, NA_real_),
  changedMeasurementLocation = c("noChange", "noChange"),
  tagStatus = c("ok", "ok"), dendrometerCondition = c(NA_character_, NA_character_),
  heightQualifier = c(NA_character_, NA_character_),
  dataQF = c("legacyData", NA_character_),
  measurementErrorQF = c(NA_character_, NA_character_), stringsAsFactors = FALSE
))
perplot <- rbind(perplot, data.frame(
  eventID = "E9", plotID = "P9", date = as.Date("2025-07-01"),
  eventType = "distributed", plotType = "distributed", nlcdClass = "forest",
  decimalLatitude = 40.09, decimalLongitude = -70.09,
  samplingImpractical = "ok", dataCollected = "allGrowthForms",
  treesPresent = "present - sampled", shrubsPresent = "not present",
  totalSampledAreaTrees = 400, totalSampledAreaShrubSapling = 40,
  nestedSubplotAreaShrubSapling = 10, stringsAsFactors = FALSE
))

raw <- list(
  vst_mappingandtagging = mapping,
  vst_apparentindividual = apparent,
  vst_perplotperyear = perplot
)
bundle <- vst_build_site_from_tables("TEST", raw)

assert_equal(bundle$meta$contract_id, "NEON-VST-DP1.10098.001-v2",
             "contract ID changed")
assert_equal(bundle$meta$release, "RELEASE-2026", "release is not explicit")
assert_equal(nrow(bundle$trees[bundle$trees$eventID == "E1", ]), 2L,
             "multi-stem event was collapsed")
assert_equal(sort(unique(bundle$trees$eventID[bundle$trees$plotID == "P1"])),
             c("E1", "E2"), "same-date distinct events were collapsed")
assert_equal(unique(bundle$trees$scientificName[
  bundle$trees$plotID == "P1" & bundle$trees$individualID == "A"
]), "Acer rubrum", "most-recent mapping record was not selected")
assert_equal(unique(bundle$trees$scientificName[
  bundle$trees$plotID == "P2" & bundle$trees$individualID == "A"
]), "Betula lenta", "individual identity leaked across plots")
assert_true("measurementErrorQF" %in% names(bundle$trees),
            "published QC fields were dropped")
assert_equal(bundle$plots$area_trees[bundle$plots$eventID == "E1"], 400,
             "first event-specific area changed")
assert_equal(bundle$plots$area_trees[bundle$plots$eventID == "E2"], 40,
             "valid 40 m2 event area was filtered or median-collapsed")
assert_equal(bundle$plots$tree_support[bundle$plots$eventID == "E4"],
             "sampled_absence", "explicit sampled absence is not represented as zero")
assert_equal(bundle$plots$tree_support[bundle$plots$eventID == "E5"],
             "held_sampling_impractical", "impractical event was treated as sampled")
assert_equal(bundle$plots$tree_support[bundle$plots$eventID == "E6"],
             "held_dendrometer_only", "dendrometer-only event was treated as scalable")
assert_equal(bundle$plots$shrub_support[bundle$plots$eventID == "E7"],
             "sampled_with_records", "supported shrub record event was held")
assert_equal(bundle$plots$shrub_support[bundle$plots$eventID == "E8"],
             "sampled_absence", "explicit shrub-channel absence is not represented as zero")
assert_equal(bundle$plots$tree_records[bundle$plots$eventID == "E9"], 2L,
             "mixed-validity event lost a channel record")
assert_equal(bundle$plots$tree_invalid_metric_records[bundle$plots$eventID == "E9"], 1L,
             "invalid required DBH was not counted exactly")
assert_equal(bundle$plots$tree_support[bundle$plots$eventID == "E9"],
             "held_metric_invalid", "mixed-validity event did not fail closed")
assert_true(!bundle$plots$tree_supported[bundle$plots$eventID == "E9"],
            "metric-invalid event was marked supported")
assert_equal(bundle$trees$dataQF[
  bundle$trees$eventID == "E9" & bundle$trees$individualID == "Q"
], "legacyData", "published dataQF was discarded or treated as a row filter")
precedence <- vst_support_status(400, "ok", "allGrowthForms", "absent", 2L, 1L)
assert_equal(precedence$status, "held_presence_record_conflict",
             "metric-invalid state improperly outranked a presence-record conflict")
assert_true("Unresolved taxon (SHRUB)" %in% bundle$contract$index$taxa$taxon_label,
            "coarse or unidentified taxa were dropped from the canonical index")
assert_true(!vst_is_species_rank(NA_character_, "Acer rubrum"),
            "missing taxon rank was incorrectly promoted to species-level")
assert_true(!vst_is_species_rank("speciesGroup", "Acer rubrum group"),
            "a species-group identification was incorrectly counted as species-or-finer")
assert_true(!"Invalidus metricus" %in% bundle$contract$index$taxa$taxon_label,
            "taxon index included a held metric-invalid event")
assert_true(all(bundle$contract$index$taxa$year_min == 2024L &
                bundle$contract$index$taxa$year_max == 2024L),
            "taxon year range escaped the latest supported eligible snapshots")
assert_equal(bundle$contract$index$site$biggest_diam_cm, 15.123,
             "canonical site metrics were rounded before presentation")
assert_true(is.finite(bundle$contract$index$site$ba_ha),
            "canonical site measured-area summary is missing")
assert_equal(bundle$contract$index$site$ba_ha,
             bundle$contract$channel_summary$tree_dbh$ba_ha,
             "site index does not copy the chosen canonical channel summary")
assert_equal(bundle$contract$index$site$metric_kind,
             "bole-DBH basal area (breast height)",
             "site index lost the physical meaning of its measured-area value")

# Cross-consumer numeric parity: the runtime helpers, canonical embedded site
# row, and embedded taxon/search rows must agree on the same synthetic bundle.
source("R/veg_helpers.R")
runtime_snapshot <- tree_snapshot(bundle$trees, bundle$plots, SIZE_FOREST)
runtime_stand <- stand_site(runtime_snapshot, bundle$plots, SIZE_FOREST)
assert_close(runtime_stand$ba_ha, bundle$contract$index$site$ba_ha,
             "runtime and canonical site measured-area values differ")
assert_close(runtime_stand$density_ha, bundle$contract$index$site$density_ha,
             "runtime and canonical site density values differ")
assert_close(runtime_stand$qmd, bundle$contract$index$site$qmd_cm,
             "runtime and canonical site QMD values differ")
runtime_taxa <- species_structure(runtime_snapshot, bundle$plots, SIZE_FOREST)
index_taxa <- bundle$contract$index$taxa[
  bundle$contract$index$taxa$channel == "tree_dbh", , drop = FALSE]
assert_equal(sort(runtime_taxa$scientificName), sort(index_taxa$taxon_label),
             "runtime and canonical taxon sets differ")
for (taxon in index_taxa$taxon_label) {
  runtime_row <- runtime_taxa[runtime_taxa$scientificName == taxon, , drop = FALSE]
  index_row <- index_taxa[index_taxa$taxon_label == taxon, , drop = FALSE]
  assert_close(runtime_row$ba_m2_ha, index_row$ba_m2_ha,
               paste("runtime and canonical taxon values differ for", taxon))
  assert_equal(as.integer(runtime_row$stems), as.integer(index_row$n_stems),
               paste("runtime and canonical stem counts differ for", taxon))
}

shrub_snapshot <- tree_snapshot(bundle$trees, bundle$plots, SIZE_SHRUB)
shrub_plots <- plot_summary_veg(shrub_snapshot, bundle$plots, SIZE_SHRUB)
assert_equal(shrub_plots$n_taxa[shrub_plots$eventID == "E7"], 1L,
             "coarse shrub identification was not counted as a taxon")
assert_equal(shrub_plots$n_species[shrub_plots$eventID == "E7"], 0L,
             "coarse shrub identification was mislabeled as a species")
assert_equal(shrub_plots$n_taxa[shrub_plots$eventID == "E8"], 0L,
             "supported sampled absence did not retain a zero taxon count")
assert_true(is.na(shrub_plots$n_taxa[shrub_plots$eventID == "E9"]),
            "held opportunity received a synthetic zero taxon count")

held_only <- vst_contract_payload(
  "HOLD", bundle$trees[bundle$trees$eventID == "E9", , drop = FALSE],
  bundle$plots[bundle$plots$eventID == "E9", , drop = FALSE], 40.09, -70.09
)$index$site
assert_equal(held_only$primary_channel, "unavailable",
             "held-only site was assigned a measurement channel")
unavailable_fields <- c(
  "n_supported_plots", "n_record_plots", "n_stems", "n_individuals",
  "n_species", "n_taxa", "n_sampled_absence", "ba_ha", "density_ha",
  "qmd_cm", "tallest_m", "biggest_diam_cm", "n_trees", "n_plots"
)
assert_true(all(vapply(held_only[unavailable_fields], function(value) is.na(value[[1L]]),
                       logical(1))),
            "unavailable site index synthesized zero-valued derived metrics")

latest_p1 <- vst_latest_supported_events(bundle$plots, "tree_dbh")
latest_p1 <- latest_p1[latest_p1$plotID == "P1", ]
assert_equal(latest_p1$eventID, "E2", "latest event tie-break is not deterministic")
assert_equal(latest_p1$area_trees, 40, "latest supported event did not retain its area")

duplicate_raw <- raw
duplicate_raw$vst_apparentindividual <- rbind(apparent, apparent[1, ])
assert_error(
  vst_build_site_from_tables("TEST", duplicate_raw),
  "violates unique key",
  "duplicate apparent-table primary key did not fail the build"
)

cat("PASS: event-keyed bundle contract fixtures\n")
