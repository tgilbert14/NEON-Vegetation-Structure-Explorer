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
  basalStemDiameterMsrmntHeight = c(NA, NA, NA, NA, NA, 10),
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
  treesPresent = c("Y", "Yes", "present - sampled", "N", "No", "Y", "N", "N"),
  shrubsPresent = c("N", "No", "not present", "N", "N", "N", "Y", "N"),
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
  basalStemDiameterMsrmntHeight = c(NA_real_, NA_real_),
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
  treesPresent = "Y", shrubsPresent = "N",
  totalSampledAreaTrees = 400, totalSampledAreaShrubSapling = 40,
  nestedSubplotAreaShrubSapling = 10, stringsAsFactors = FALSE
))

mapping$uid <- sprintf("mapping-source-%02d", seq_len(nrow(mapping)))
apparent$uid <- sprintf("apparent-source-%02d", seq_len(nrow(apparent)))
perplot$uid <- sprintf("opportunity-source-%02d", seq_len(nrow(perplot)))

raw <- list(
  vst_mappingandtagging = mapping,
  vst_apparentindividual = apparent,
  vst_perplotperyear = perplot
)
bundle <- vst_build_site_from_tables("TEST", raw)

assert_equal(bundle$meta$contract_id, "NEON-VST-DP1.10098.001-v2",
             "contract ID changed")
assert_equal(bundle$meta$release, "RELEASE-2026", "release is not explicit")
assert_equal(bundle$contract$source_record_key, "source_uid",
             "published source-row identity changed")
assert_equal(bundle$contract$mapping_source_record_key, "mapping_source_uid",
             "mapping/tagging source-row identity changed")
assert_equal(bundle$contract$protocol_stem_locator,
             c("plotID", "eventID", "individualID", "tempStemID"),
             "plot-scoped protocol stem locator changed")
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
assert_equal(unique(bundle$trees$mapping_source_uid[
  bundle$trees$plotID == "P1" & bundle$trees$individualID == "A"
]), "mapping-source-02", "selected mapping/tagging uid was not preserved")
assert_equal(bundle$trees$basalMeasurementHeight[bundle$trees$eventID == "E7"],
             10, "RELEASE-2026 basal measurement-height alias was dropped")
assert_true("measurementErrorQF" %in% names(bundle$trees),
            "published QC fields were dropped")
assert_equal(
  vst_presence_state(c("Y", "Yes", "N", "No", "Present - sampled",
                       "not present", NA_character_, "")),
  c("present", "present", "absent", "absent", "present", "absent",
    "unknown", "unknown"),
  "reviewed RELEASE-2026 presence vocabulary was not normalized exactly"
)
assert_equal(
  vst_shrub_presence(c("N", "N", "Y"), c("N", "Y", "N")),
  c("absent", "present", "unknown"),
  "nested shrub/sapling Y/N presence algebra changed"
)
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

# Both directions of the reviewed Y/N presence-record consistency rule fail
# closed. N plus records is not an observed zero; Y without records is not an
# inferred absence.
yn_absent_with_records_raw <- raw
yn_absent_with_records_raw$vst_perplotperyear$treesPresent[
  yn_absent_with_records_raw$vst_perplotperyear$eventID == "E1"
] <- "N"
yn_absent_with_records <- vst_build_site_from_tables(
  "TEST", yn_absent_with_records_raw
)
assert_equal(
  yn_absent_with_records$plots$tree_support[
    yn_absent_with_records$plots$eventID == "E1"
  ],
  "held_presence_record_conflict",
  "RELEASE-2026 N plus records did not fail closed"
)
yn_present_without_records_raw <- raw
yn_present_without_records_raw$vst_perplotperyear$treesPresent[
  yn_present_without_records_raw$vst_perplotperyear$eventID == "E4"
] <- "Y"
yn_present_without_records <- vst_build_site_from_tables(
  "TEST", yn_present_without_records_raw
)
assert_equal(
  yn_present_without_records$plots$tree_support[
    yn_present_without_records$plots$eventID == "E4"
  ],
  "held_presence_record_conflict",
  "RELEASE-2026 Y without records did not fail closed"
)
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
assert_true(!vst_is_species_rank("species", NA_character_),
            "missing scientific name was incorrectly promoted to species-level")
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

# Measurement rows without a published vst_perplotperyear row remain visible as
# measurement-only context, but can never invent effort, absence, or area.
missing_opportunity_raw <- raw
missing_opportunity_raw$vst_perplotperyear <- perplot[
  perplot$eventID != "E7", , drop = FALSE
]
missing_opportunity_bundle <- vst_build_site_from_tables(
  "TEST", missing_opportunity_raw
)
missing_context <- missing_opportunity_bundle$plots[
  missing_opportunity_bundle$plots$eventID == "E7", , drop = FALSE
]
assert_equal(nrow(missing_context), 1L,
             "measurement-only event did not receive one audit context")
assert_true(missing_context$opportunity_source_missing %in% TRUE,
            "measurement-only context lost its explicit source-missing flag")
assert_equal(missing_context$opportunity_source_record_count, 0L,
             "measurement-only context invented an opportunity source count")
assert_true(is.na(missing_context$opportunity_source_uid) &
              is.na(missing_context$opportunity_source_uids),
            "measurement-only context invented published source identity")
assert_true(is.na(missing_context$date) & is.na(missing_context$year) &
              is.na(missing_context$area_trees) &
              is.na(missing_context$area_shrub),
            "measurement-only context invented date, year, or sampled area")
assert_equal(missing_context$measurement_record_count_all, 1L,
             "measurement-only context lost its preserved row count")
assert_equal(missing_context$measurement_date_min, as.Date("2024-07-01"),
             "measurement-sourced minimum date was not kept separately")
assert_equal(missing_context$measurement_date_max, as.Date("2024-07-01"),
             "measurement-sourced maximum date was not kept separately")
assert_equal(missing_context$measurement_date_distinct_n, 1L,
             "measurement-only context date count changed")
assert_equal(missing_context$tree_support,
             "held_opportunity_source_missing",
             "missing opportunity source did not hold the tree channel")
assert_equal(missing_context$shrub_support,
             "held_opportunity_source_missing",
             "missing opportunity source did not hold the shrub channel")
assert_equal(
  nrow(missing_opportunity_bundle$opportunity_source), nrow(perplot) - 1L,
  "measurement-only context contaminated the published opportunity source table"
)
assert_true(any(missing_opportunity_bundle$trees$eventID == "E7"),
            "measurement rows were dropped with their missing opportunity source")
assert_true(all(missing_opportunity_bundle$trees$opportunity_source_missing ==
                  (missing_opportunity_bundle$trees$eventID == "E7")),
            "measurement-row source-missing flags disagree with the context")
assert_true(!"Unresolved taxon (SHRUB)" %in%
              missing_opportunity_bundle$contract$index$taxa$taxon_label,
            "measurement-only context leaked into a supported taxon index")
assert_equal(
  missing_opportunity_bundle$contract$index$site$n_measurement_only_contexts,
  1L, "site index did not count measurement-only contexts"
)
assert_equal(
  missing_opportunity_bundle$meta$n_measurement_records_without_opportunity_source,
  1L, "site metadata did not count preserved source-unmatched measurements"
)

# Measurement-derived date evidence remains exact for multi-row/multi-date
# contexts, while an all-undated context reports zero distinct dates and no
# invented bounds.
rich_missing_raw <- missing_opportunity_raw
extra_dated <- rich_missing_raw$vst_apparentindividual[
  rich_missing_raw$vst_apparentindividual$eventID == "E7", , drop = FALSE
]
extra_dated$tempStemID <- "2"
extra_dated$date <- as.Date("2024-08-15")
extra_dated$uid <- "apparent-source-extra-dated"
extra_undated <- extra_dated
extra_undated$eventID <- "E10"
extra_undated$plotID <- "P10"
extra_undated$individualID <- "U"
extra_undated$tempStemID <- "1"
extra_undated$date <- as.Date(NA)
extra_undated$uid <- "apparent-source-extra-undated"
rich_missing_raw$vst_apparentindividual <- rbind(
  rich_missing_raw$vst_apparentindividual, extra_dated, extra_undated
)
rich_missing_bundle <- vst_build_site_from_tables("TEST", rich_missing_raw)
rich_e7 <- rich_missing_bundle$plots[
  rich_missing_bundle$plots$eventID == "E7", , drop = FALSE
]
rich_e10 <- rich_missing_bundle$plots[
  rich_missing_bundle$plots$eventID == "E10", , drop = FALSE
]
assert_equal(rich_e7$measurement_record_count_all, 2L,
             "multi-row measurement-only count changed")
assert_equal(rich_e7$measurement_date_min, as.Date("2024-07-01"),
             "multi-date measurement-only minimum changed")
assert_equal(rich_e7$measurement_date_max, as.Date("2024-08-15"),
             "multi-date measurement-only maximum changed")
assert_equal(rich_e7$measurement_date_distinct_n, 2L,
             "multi-date measurement-only distinct count changed")
assert_equal(rich_e10$measurement_record_count_all, 1L,
             "undated measurement-only row count changed")
assert_true(is.na(rich_e10$measurement_date_min) &
              is.na(rich_e10$measurement_date_max),
            "undated measurement-only context invented date bounds")
assert_equal(rich_e10$measurement_date_distinct_n, 0L,
             "undated measurement-only context invented a date count")
assert_true(is.na(rich_e7$nestedSubplotAreaShrubSapling) &
              is.na(rich_e10$nestedSubplotAreaShrubSapling),
            "measurement-only contexts invented an additional published area field")

# Mutation checks prove the independent DQA gate rejects altered summaries,
# fractional counts, a source-key alias, and invented extra opportunity fields.
dqa_environment <- new.env(parent = globalenv())
sys.source("scripts/write_data_quality_audit.R", envir = dqa_environment)
dqa_probe <- rich_missing_bundle
dqa_probe$meta$source_receipt <- list(
  provenance_class = "official-release", product = "DP1.10098.001",
  neon_release = "RELEASE-2026",
  release_doi = "https://doi.org/10.48443/pypa-qf12",
  source_receipt_id = paste0(
    "VST-DP1.10098.001-RELEASE-2026-sha256-", paste(rep("a", 64), collapse = "")
  ),
  raw_source_digest = paste(rep("a", 64), collapse = ""),
  source_normalization = "portable-vectors+published-uid-byte-order-v1"
)
selected_source_parity <- vst_selected_source_parity(
  dqa_probe$plots, dqa_probe$opportunity_source
)
assert_true(
  isTRUE(selected_source_parity$ok) &&
    length(selected_source_parity$fields) == 0L,
  "selected opportunity source parity rejected an unmodified bundle"
)
invisible(dqa_environment$vst_dqa_site_rows(dqa_probe, "TEST"))
fractional_probe <- dqa_probe
fractional_probe$plots$measurement_record_count_all[
  fractional_probe$plots$eventID == "E7"
] <- 2.5
assert_error(
  dqa_environment$vst_dqa_site_rows(fractional_probe, "TEST"),
  "exact nonnegative integers",
  "DQA accepted a fractional measurement record count"
)
date_probe <- dqa_probe
date_probe$plots$measurement_date_max[date_probe$plots$eventID == "E7"] <-
  as.Date("2024-08-16")
assert_error(
  dqa_environment$vst_dqa_site_rows(date_probe, "TEST"),
  "count/date summaries differ",
  "DQA accepted a corrupted measurement date bound"
)
uid_probe <- dqa_probe
uid_probe$opportunity_source$source_record_key[[1L]] <- "not-the-published-uid"
assert_error(
  dqa_environment$vst_dqa_site_rows(uid_probe, "TEST"),
  "opportunity source uids",
  "DQA accepted a source-record alias that differs from published uid"
)
area_probe <- dqa_probe
area_probe$plots$area_trees[area_probe$plots$eventID == "E1"] <- 401
assert_error(
  dqa_environment$vst_dqa_site_rows(area_probe, "TEST"),
  "selected opportunity source row",
  "DQA accepted an area that differs from its selected opportunity source row"
)
invented_probe <- dqa_probe
invented_probe$plots$nestedSubplotAreaShrubSapling[
  invented_probe$plots$eventID == "E7"
] <- 10
assert_error(
  dqa_environment$vst_dqa_site_rows(invented_probe, "TEST"),
  "invent published opportunity fields",
  "DQA accepted invented additional opportunity metadata"
)

dqa_n_with_records <- dqa_probe
dqa_n_with_records$plots$treesPresent[
  dqa_n_with_records$plots$eventID == "E1"
] <- "N"
dqa_n_with_records$opportunity_source$treesPresent[
  dqa_n_with_records$opportunity_source$eventID == "E1"
] <- "N"
assert_error(
  dqa_environment$vst_dqa_site_rows(dqa_n_with_records, "TEST"),
  "support states, reasons, counts, or presence differ",
  "DQA did not independently reject RELEASE-2026 N plus records"
)
dqa_y_without_records <- dqa_probe
dqa_y_without_records$plots$treesPresent[
  dqa_y_without_records$plots$eventID == "E4"
] <- "Y"
dqa_y_without_records$opportunity_source$treesPresent[
  dqa_y_without_records$opportunity_source$eventID == "E4"
] <- "Y"
assert_error(
  dqa_environment$vst_dqa_site_rows(dqa_y_without_records, "TEST"),
  "support states, reasons, counts, or presence differ",
  "DQA did not independently reject RELEASE-2026 Y without records"
)
dqa_live_probe <- dqa_probe
dqa_live_probe$trees$live[[1L]] <- !dqa_live_probe$trees$live[[1L]]
assert_error(
  dqa_environment$vst_dqa_site_rows(dqa_live_probe, "TEST"),
  "row-derived invariants differ",
  "DQA accepted live inconsistent with plantStatus"
)

# Cross-consumer numeric parity: the runtime helpers, canonical embedded site
# row, and embedded taxon/search rows must agree on the same synthetic bundle.
# The reusable parity module deliberately sources the runtime consumer rather
# than the bundle builder's summary functions.
source("scripts/derived_parity.R")
held_history <- tree_history(
  rich_missing_bundle$trees[rich_missing_bundle$trees$eventID == "E7", , drop = FALSE],
  "S"
)
assert_true(all(held_history$opportunity_source_missing %in% TRUE),
            "plant history dropped the source-missing evidence flag")
undated_history <- tree_history(rich_missing_bundle$trees, "U")
assert_equal(nrow(undated_history), 1L,
             "plant history dropped an undated preserved measurement")
assert_true(is.na(undated_history$date),
            "plant history invented a date for an undated measurement")
assert_true(is.null(tree_trajectory(
  rich_missing_bundle$trees, "S", "basalStemDiameter",
  rich_missing_bundle$plots
)), "plant trajectory connected a held measurement-only event")
runtime_snapshot <- tree_snapshot(bundle$trees, bundle$plots, SIZE_FOREST)
runtime_stand <- stand_site(runtime_snapshot, bundle$plots, SIZE_FOREST)
assert_close(runtime_stand$ba_ha, bundle$contract$index$site$ba_ha,
             "runtime and canonical site measured-area values differ")
assert_close(runtime_stand$density_ha, bundle$contract$index$site$density_ha,
             "runtime and canonical site density values differ")
assert_close(runtime_stand$qmd, bundle$contract$index$site$qmd_cm,
             "runtime and canonical site QMD values differ")

# The selected plot event is atomic: legitimate stem rows from that event are
# retained even when their row dates differ or one row date is missing.
atomic_trees <- bundle$trees[bundle$trees$eventID == "E1", , drop = FALSE]
atomic_plots <- bundle$plots[bundle$plots$eventID == "E1", , drop = FALSE]
atomic_trees$date <- as.Date(c("2024-06-01", "2024-06-02"))
atomic_trees$year <- 2024L
atomic_dated <- tree_snapshot(atomic_trees, atomic_plots, SIZE_FOREST)
assert_equal(
  sort(as.character(atomic_dated$source_uid)),
  sort(as.character(atomic_trees$source_uid)),
  "event-atomic snapshot dropped a stem with a differing row date"
)
atomic_trees$date[[2L]] <- as.Date(NA)
atomic_trees$year[[2L]] <- NA_integer_
atomic_undated <- tree_snapshot(atomic_trees, atomic_plots, SIZE_FOREST)
assert_equal(
  sort(as.character(atomic_undated$source_uid)),
  sort(as.character(atomic_trees$source_uid)),
  "event-atomic snapshot dropped an undated stem from the selected event"
)
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

parity_problems <- vst_parity_bundle_problems(bundle, "TEST")
assert_equal(length(parity_problems), 0L,
             paste("consumer parity rejected an unmodified bundle:",
                   paste(parity_problems, collapse = "; ")))

assert_row_derivation_rejected <- function(probe, message) {
  probe_problems <- vst_parity_bundle_problems(probe, "TEST")
  assert_true(any(grepl(
    "row-derived invariants differ", probe_problems, fixed = TRUE
  )), message)
}
rebuild_corrupted_contract <- function(probe) {
  probe$contract <- vst_contract_payload(
    "TEST", probe$trees, probe$plots,
    vst_safe_median(probe$plots$lat), vst_safe_median(probe$plots$lng)
  )
  probe$meta$primary_channel <-
    probe$contract$index$site$primary_channel[[1L]]
  probe$meta$structure_type <-
    probe$contract$index$site$structure_type[[1L]]
  probe$meta$years <- sort(unique(probe$trees$year[is.finite(probe$trees$year)]))
  probe
}
live_row_probe <- bundle
live_row_probe$trees$live[[1L]] <- !live_row_probe$trees$live[[1L]]
live_row_probe <- rebuild_corrupted_contract(live_row_probe)
assert_row_derivation_rejected(
  live_row_probe,
  "consumer parity accepted coherent live/summary corruption inconsistent with plantStatus"
)
year_row_probe <- bundle
year_row_probe$trees$year[[1L]] <- year_row_probe$trees$year[[1L]] + 1L
year_row_probe <- rebuild_corrupted_contract(year_row_probe)
assert_row_derivation_rejected(
  year_row_probe,
  "consumer parity accepted coherent year/summary corruption inconsistent with date"
)
taxonomy_row_probe <- bundle
taxonomy_row_probe$trees$taxon_label[[1L]] <- "Coherently corrupted taxon"
taxonomy_row_probe$trees$is_species[[1L]] <-
  !taxonomy_row_probe$trees$is_species[[1L]]
taxonomy_row_probe$trees$taxon_resolution[[1L]] <- "coherently-corrupted"
taxonomy_row_probe <- rebuild_corrupted_contract(taxonomy_row_probe)
assert_row_derivation_rejected(
  taxonomy_row_probe,
  "consumer parity accepted coherent taxonomy/summary corruption inconsistent with raw taxonomy"
)
identity_row_probe <- bundle
identity_row_probe$trees$permanent[[1L]] <-
  !identity_row_probe$trees$permanent[[1L]]
identity_row_probe$trees$plant_key[[1L]] <- "corrupted-plant-key"
identity_row_probe$trees$event_key[[1L]] <- "corrupted-event-key"
identity_row_probe <- rebuild_corrupted_contract(identity_row_probe)
assert_row_derivation_rejected(
  identity_row_probe,
  "consumer parity accepted corrupted permanence and composite identities"
)

site_summary_probe <- bundle
site_summary_probe$contract$index$site$ba_ha <-
  site_summary_probe$contract$index$site$ba_ha + 1
assert_true(any(grepl(
  "embedded site index field ba_ha differs",
  vst_parity_bundle_problems(site_summary_probe, "TEST"), fixed = TRUE
)), "consumer parity accepted a corrupted embedded site summary")

channel_summary_probe <- bundle
channel_summary_probe$contract$channel_summary$tree_dbh$n_stems <-
  channel_summary_probe$contract$channel_summary$tree_dbh$n_stems + 1L
assert_true(any(grepl(
  "embedded channel summary field n_stems differs",
  vst_parity_bundle_problems(channel_summary_probe, "TEST"), fixed = TRUE
)), "consumer parity accepted a corrupted embedded channel summary")

taxon_summary_probe <- bundle
taxon_summary_probe$contract$index$taxa$n_stems[[1L]] <-
  taxon_summary_probe$contract$index$taxa$n_stems[[1L]] + 1L
assert_true(any(grepl(
  "embedded taxon index field n_stems differs",
  vst_parity_bundle_problems(taxon_summary_probe, "TEST"), fixed = TRUE
)), "consumer parity accepted a corrupted embedded taxon summary")

# Exercise the family/index comparator independently of file I/O. The release
# verifier runs the same path over all 42 actual bundles and committed indexes.
fixture_bundles <- list(TEST = bundle)
fixture_family <- vst_parity_family_projection(fixture_bundles, "TEST")
fixture_search <- list(
  sites = fixture_family$sites,
  channel_sites = fixture_family$channel_sites,
  taxa = fixture_family$taxa
)
family_problems <- vst_parity_family_problems(
  fixture_bundles, fixture_family$sites, fixture_search, "TEST"
)
assert_equal(length(family_problems), 0L,
             "consumer family parity rejected its positive fixture")
family_search_probe <- fixture_search
family_search_probe$channel_sites$n_stems[[1L]] <-
  family_search_probe$channel_sites$n_stems[[1L]] + 1L
assert_true(any(grepl(
  "search channel rows field n_stems differs",
  vst_parity_family_problems(
    fixture_bundles, fixture_family$sites, family_search_probe, "TEST"
  ), fixed = TRUE
)), "consumer family parity accepted a corrupted search channel row")

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

# The documented three-field locator can recur in another plot. Plot scope must
# keep that tag reuse from creating a false within-event conflict.
cross_plot_apparent <- apparent[c(1L, 4L), , drop = FALSE]
cross_plot_apparent$eventID <- "E-CROSS"
cross_plot_apparent$tempStemID <- "1"
cross_plot_apparent$uid <- c("cross-plot-source-1", "cross-plot-source-2")
cross_plot_table <- vst_measurement_table(
  cross_plot_apparent, vst_latest_mapping(mapping)
)
assert_true(!any(cross_plot_table$protocol_key_conflict),
            "cross-plot tag reuse was mislabeled as a within-plot identity conflict")

duplicate_raw <- raw
duplicate_measurement <- apparent[1, , drop = FALSE]
duplicate_measurement$uid <- "apparent-source-conflict"
duplicate_raw$vst_apparentindividual <- rbind(apparent, duplicate_measurement)
duplicate_bundle <- vst_build_site_from_tables("TEST", duplicate_raw)
assert_equal(nrow(duplicate_bundle$trees), nrow(apparent) + 1L,
             "conflicting apparent source row was silently deleted")
assert_true(all(duplicate_bundle$trees$protocol_key_conflict[
  duplicate_bundle$trees$eventID == "E1" &
    duplicate_bundle$trees$individualID == "A" &
    duplicate_bundle$trees$tempStemID == "1"
]), "non-unique protocol stem locator was not flagged on every source row")
assert_equal(duplicate_bundle$plots$tree_identity_conflict_keys[
  duplicate_bundle$plots$eventID == "E1"
], 1L, "tree-channel identity conflict was not counted by protocol locator")
assert_equal(duplicate_bundle$plots$tree_support[
  duplicate_bundle$plots$eventID == "E1"
], "held_identity_conflict", "ambiguous measurement identity did not hold the channel event")
assert_true(duplicate_bundle$plots$shrub_support[
  duplicate_bundle$plots$eventID == "E1"
] != "held_identity_conflict", "tree identity conflict leaked into the shrub channel")

conflicting_measurement_raw <- raw
conflicting_measurement <- apparent[1, , drop = FALSE]
conflicting_measurement$uid <- "apparent-source-metric-conflict"
conflicting_measurement$stemDiameter <- 99
conflicting_measurement$plantStatus <- "Standing dead 5"
conflicting_measurement_raw$vst_apparentindividual <- rbind(
  apparent, conflicting_measurement
)
conflicting_measurement_bundle <- vst_build_site_from_tables(
  "TEST", conflicting_measurement_raw
)
assert_equal(conflicting_measurement_bundle$plots$tree_support[
  conflicting_measurement_bundle$plots$eventID == "E1"
], "held_identity_conflict", "same-date metric/status conflict was adjudicated")

different_date_raw <- raw
different_date_measurement <- apparent[1, , drop = FALSE]
different_date_measurement$uid <- "apparent-source-date-conflict"
different_date_measurement$date <- as.Date("2025-06-01")
different_date_raw$vst_apparentindividual <- rbind(apparent, different_date_measurement)
different_date_bundle <- vst_build_site_from_tables("TEST", different_date_raw)
assert_equal(different_date_bundle$plots$tree_support[
  different_date_bundle$plots$eventID == "E1"
], "held_identity_conflict", "different-date locator conflict was treated as a revision")

shrub_duplicate_raw <- raw
shrub_duplicate <- apparent[apparent$eventID == "E7", , drop = FALSE]
shrub_duplicate$uid <- "shrub-source-conflict"
shrub_duplicate_raw$vst_apparentindividual <- rbind(apparent, shrub_duplicate)
shrub_duplicate_bundle <- vst_build_site_from_tables("TEST", shrub_duplicate_raw)
assert_equal(shrub_duplicate_bundle$plots$shrub_support[
  shrub_duplicate_bundle$plots$eventID == "E7"
], "held_identity_conflict", "shrub identity conflict did not hold the shrub channel")
assert_true(shrub_duplicate_bundle$plots$tree_support[
  shrub_duplicate_bundle$plots$eventID == "E7"
] != "held_identity_conflict", "shrub identity conflict leaked into the tree channel")

# Measurement-identity conflict is recorded but does not obscure an earlier
# protocol/presence reason; opportunity-source ambiguity always comes first.
sampling_precedence_raw <- duplicate_raw
sampling_precedence_raw$vst_perplotperyear$samplingImpractical[
  sampling_precedence_raw$vst_perplotperyear$eventID == "E1"
] <- "access denied"
sampling_precedence_bundle <- vst_build_site_from_tables(
  "TEST", sampling_precedence_raw
)
assert_equal(sampling_precedence_bundle$plots$tree_support[
  sampling_precedence_bundle$plots$eventID == "E1"
], "held_sampling_impractical", "measurement identity hid sampling impracticality")

presence_precedence_raw <- duplicate_raw
presence_precedence_raw$vst_perplotperyear$treesPresent[
  presence_precedence_raw$vst_perplotperyear$eventID == "E1"
] <- "N"
presence_precedence_bundle <- vst_build_site_from_tables(
  "TEST", presence_precedence_raw
)
assert_equal(presence_precedence_bundle$plots$tree_support[
  presence_precedence_bundle$plots$eventID == "E1"
], "held_presence_record_conflict", "measurement identity hid a presence-record conflict")

metric_precedence_raw <- raw
metric_conflict <- apparent[1, , drop = FALSE]
metric_conflict$uid <- "apparent-source-invalid-conflict"
metric_conflict$stemDiameter <- NA_real_
metric_precedence_raw$vst_apparentindividual <- rbind(apparent, metric_conflict)
metric_precedence_bundle <- vst_build_site_from_tables("TEST", metric_precedence_raw)
assert_equal(metric_precedence_bundle$plots$tree_support[
  metric_precedence_bundle$plots$eventID == "E1"
], "held_identity_conflict", "metric-invalid state improperly outranked identity conflict")

duplicate_opportunity_raw <- raw
duplicate_opportunity <- perplot[perplot$eventID == "E2", , drop = FALSE]
duplicate_opportunity$uid <- "opportunity-source-conflict"
duplicate_opportunity$totalSampledAreaTrees <- 999
duplicate_opportunity_raw$vst_perplotperyear <- rbind(perplot, duplicate_opportunity)
duplicate_opportunity_bundle <- vst_build_site_from_tables("TEST", duplicate_opportunity_raw)
assert_equal(nrow(duplicate_opportunity_bundle$opportunity_source), nrow(perplot) + 1L,
             "conflicting opportunity source row was silently deleted")
assert_true(duplicate_opportunity_bundle$plots$opportunity_key_conflict[
  duplicate_opportunity_bundle$plots$eventID == "E2"
], "non-unique opportunity key was not flagged")
assert_equal(duplicate_opportunity_bundle$plots$tree_support[
  duplicate_opportunity_bundle$plots$eventID == "E2"
], "held_identity_conflict", "ambiguous opportunity identity did not hold the channel event")
assert_equal(duplicate_opportunity_bundle$plots$shrub_support[
  duplicate_opportunity_bundle$plots$eventID == "E2"
], "held_identity_conflict", "ambiguous opportunity identity did not hold both channels")

opportunity_precedence_raw <- duplicate_opportunity_raw
opportunity_precedence_raw$vst_perplotperyear$samplingImpractical[
  opportunity_precedence_raw$vst_perplotperyear$eventID == "E2"
] <- "access denied"
opportunity_precedence_bundle <- vst_build_site_from_tables(
  "TEST", opportunity_precedence_raw
)
assert_equal(opportunity_precedence_bundle$plots$tree_support[
  opportunity_precedence_bundle$plots$eventID == "E2"
], "held_identity_conflict", "opportunity ambiguity lost first-precedence hold status")

ambiguous_mapping <- mapping[2L, , drop = FALSE]
ambiguous_mapping$uid <- "mapping-source-ambiguous"
ambiguous_mapping$scientificName <- "Acer saccharum"
assert_error(
  vst_latest_mapping(rbind(mapping, ambiguous_mapping)),
  "ambiguous latest mapping rows",
  "same-created-time mapping/tagging tie was resolved by row order"
)
duplicate_mapping_uid <- mapping
duplicate_mapping_uid$uid[[2L]] <- duplicate_mapping_uid$uid[[1L]]
assert_error(
  vst_latest_mapping(duplicate_mapping_uid),
  "violates unique key",
  "duplicate mapping/tagging source uid did not fail the build"
)
blank_mapping_uid <- mapping
blank_mapping_uid$uid[[1L]] <- ""
assert_error(
  vst_latest_mapping(blank_mapping_uid),
  "blank uid",
  "blank mapping/tagging source uid did not fail the build"
)

duplicate_uid_raw <- raw
duplicate_uid_raw$vst_apparentindividual <- rbind(apparent, apparent[1, ])
assert_error(
  vst_build_site_from_tables("TEST", duplicate_uid_raw),
  "violates unique key",
  "duplicate published uid did not fail the build"
)

blank_uid_raw <- raw
blank_uid_raw$vst_apparentindividual$uid[[1L]] <- ""
assert_error(
  vst_build_site_from_tables("TEST", blank_uid_raw),
  "blank uid",
  "blank apparent source uid did not fail the build"
)

duplicate_opportunity_uid_raw <- raw
duplicate_opportunity_uid_raw$vst_perplotperyear <- rbind(perplot, perplot[1, ])
assert_error(
  vst_build_site_from_tables("TEST", duplicate_opportunity_uid_raw),
  "violates unique key",
  "duplicate opportunity source uid did not fail the build"
)

blank_opportunity_uid_raw <- raw
blank_opportunity_uid_raw$vst_perplotperyear$uid[[1L]] <- ""
assert_error(
  vst_build_site_from_tables("TEST", blank_opportunity_uid_raw),
  "blank uid",
  "blank opportunity source uid did not fail the build"
)

cat("PASS: event-keyed bundle contract fixtures\n")
