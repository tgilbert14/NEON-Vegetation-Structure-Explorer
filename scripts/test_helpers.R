# ===========================================================================
# test_helpers.R — smoke + regression tests for R/veg_helpers.R, exercising
# BOTH size paradigms (forest = DBH, shrubland = basal diameter). Run with any R:
#   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/test_helpers.R
# Fails loudly (stop()) on a regression so it can gate a commit / CI run.
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))
source("R/veg_helpers.R")

check <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg); cat("  ok -", msg, "\n") }

run_site <- function(site, spec) {
  b <- readRDS(file.path("data/sites", paste0(site, ".rds")))
  tr <- b$trees; pl <- b$plots
  stype <- b$meta$structure_type %||% "forest"
  cat(sprintf("\n=== %s (%s; spec=%s) — trees %d, plots %d ===\n",
              site, stype, spec$type, nrow(tr), nrow(pl)))
  snap <- tree_snapshot(tr, pl, spec); one <- one_per_tree(live_only(snap), spec)
  woody <- woody_only(one, spec)
  cat("  live plants (woody_only):", nrow(woody), "| size col:", spec$col, "\n")
  st <- stand_site(snap, pl, spec)
  sc <- size_class(snap, pl, spec)
  ss <- status_summary(snap, spec)
  cat("  stand_site ba/ha:", if (is.null(st)) "NA" else st$ba_ha,
      "| size_class rows:", if (is.null(sc)) 0 else nrow(sc),
      "| status rows:", if (is.null(ss)) 0 else nrow(ss), "\n")
  check(nrow(woody) > 0, sprintf("%s has live %s by %s", site, spec$nouns, spec$size_lab))
  check(!is.null(ss) && sum(ss$n) > 0, sprintf("%s status_summary is non-empty", site))
  # the size column the spec points at must actually carry data for this site
  check(any(is.finite(woody[[spec$col]]) & woody[[spec$col]] > 0),
        sprintf("%s %s column is populated", site, spec$col))
  invisible(list(snap = snap, st = st, ss = ss))
}

cat("################  FOREST  ################\n")
run_site("HARV", SIZE_FOREST)
cat("\n################  SHRUBLAND  ################\n")
# JORN/SRER are desert shrublands; pick whichever is bundled
shrub_site <- if (file.exists("data/sites/SRER.rds")) "SRER" else "JORN"
run_site(shrub_site, SIZE_SHRUB)

# ---------------------------------------------------------------------------
# REGRESSION: status_summary() must scope strictly to the registered, disjoint
# growth forms. Small-tree DBH and unknown growth forms are preserved in the
# long table but withheld from both summaries pending a dedicated channel.
# ---------------------------------------------------------------------------
cat("\n################  REGRESSION: status_summary form-scoping  ################\n")
syn <- data.frame(
  individualID = c("t1", "t2", "s1", "s2", "x1", "d1"),
  growthForm   = c("single bole tree", "small tree", "single shrub", "small shrub", NA, "single bole tree"),
  plantStatus  = c("Live", "Live", "Live", "Live", "Live", "Standing dead 5"),
  stringsAsFactors = FALSE)

check(!is.null(SIZE_FOREST$forms) && length(SIZE_FOREST$forms) > 0, "SIZE_FOREST$forms is defined")
check(!is.null(SIZE_SHRUB$forms)  && length(SIZE_SHRUB$forms)  > 0, "SIZE_SHRUB$forms is defined")

sf <- status_summary(syn, SIZE_FOREST)   # t1,d1 -> 2 (incl. 1 dead)
sh <- status_summary(syn, SIZE_SHRUB)    # s1,s2 -> 2 (all live)
cat("  forest counts:\n"); print(sf)
cat("  shrub counts:\n");  print(sh)

# Neither channel may silently absorb small-tree or unknown growth-form rows.
check(sum(sf$n) == 2, "forest scoping keeps only registered full-plot tree forms")
check(sum(sh$n) == 2, "shrub scoping keeps only registered shrub/sapling basal forms")
# forest must see the standing-dead TREE; shrub must NOT (it's a single-bole tree, excluded)
check(sum(sf$n[grepl("Dead", sf$cls)]) == 1, "forest scoping includes the standing-dead tree")
check(sum(sh$n[grepl("Dead", sh$cls)]) == 0, "shrub scoping excludes the single-bole-tree (different form set)")

cat("\n################  REGRESSION: export dictionaries  ################\n")
export_fixture <- list(
  trees_long = data.frame(site = "HARV", contract_id = VEG_CONTRACT_ID,
                          dataQF = "legacyData", stringsAsFactors = FALSE),
  plot_summary_latest = data.frame(site = "HARV", plotID = "HARV_001",
                                   n_taxa = 1L, stringsAsFactors = FALSE),
  plot_event_contexts_all = data.frame(site = "HARV", plotID = "HARV_001",
                                      eventID = "EV1", customOpportunityFlag = "ok",
                                      stringsAsFactors = FALSE),
  plot_opportunity_source = data.frame(
                                      site = "HARV", source_record_key = "opp-uid-1",
                                      eventID = "EV1", customSourceFlag = "review",
                                      stringsAsFactors = FALSE)
)
cb <- complete_veg_codebook(veg_codebook(), export_fixture)
check(isTRUE(assert_veg_codebook(cb, export_fixture)),
      "data dictionary covers every emitted canonical and preserved source field")
check(any(cb$column == "dataQF" & cb$table == "trees_long"),
      "preserved QF fields receive explicit dictionary rows")
check(any(cb$column == "customOpportunityFlag" & cb$table == "plot_event_contexts_all"),
      "new opportunity fields receive explicit dictionary rows")
check(any(cb$column == "customSourceFlag" & cb$table == "plot_opportunity_source"),
      "preserved opportunity-source fields receive explicit dictionary rows")

tree_export_fixture <- data.frame(
  source_uid = "apparent-uid-1", protocol_stem_key = "fixture",
  mapping_source_uid = "mapping-uid-1",
  protocol_key_group_n = 1L, protocol_key_conflict = FALSE,
  opportunity_source_missing = TRUE,
  eventID = "EV1", plotID = "HARV_001", individualID = "NEON.PLA.D01.1",
  tempStemID = "1", date = as.Date("2025-01-01"), year = 2025L,
  taxonRank = "species", scientificName = "Acer rubrum", growthForm = "single bole tree",
  plantStatus = "Live", live = TRUE, permanent = TRUE, is_species = TRUE,
  stemDiameter = 12, basalStemDiameter = NA_real_, height = 8,
  recordType = "mapped and tagged", identificationQualifier = "reviewed",
  mappingDataQF = "mapping-review", tagStatus = "ok",
  dendrometerCondition = "not applicable", heightQualifier = "estimated",
  dataQF = "legacyData", measurementErrorQF = "reviewed",
  stringsAsFactors = FALSE
)
tree_export_result <- tidy_trees_export(tree_export_fixture, list(
  site = "HARV", product = VEG_CONTRACT$product, release = VEG_CONTRACT$release,
  source_receipt = list(raw_source_digest = paste(rep("a", 64), collapse = ""))
))
required_review_fields <- c(
  "source_record_key", "mapping_source_record_key", "protocol_stem_key",
  "protocol_key_group_n",
  "protocol_key_conflict", "opportunity_source_missing",
  "recordType", "identificationQualifier", "mappingDataQF", "tagStatus",
  "dendrometerCondition", "heightQualifier", "dataQF", "measurementErrorQF"
)
check(all(required_review_fields %in% names(tree_export_result)),
      "trees_long preserves every registered identity and measurement review field")
check(identical(tree_export_result$opportunity_source_missing, TRUE),
      "trees_long preserves the measurement-row opportunity-source flag")
check(identical(tree_export_result$mapping_source_record_key, "mapping-uid-1"),
      "trees_long preserves the selected mapping/tagging source uid")

qc_fixture <- data.frame(site = "HARV", source_digest = paste(rep("a", 64), collapse = ""),
                         contract_id = VEG_CONTRACT_ID, level = "info",
                         issue = "fixture", later_cm = 10, stringsAsFactors = FALSE)
qcb <- qc_dictionary(qc_fixture)
check(isTRUE(assert_qc_dictionary(qcb, qc_fixture)),
      "QC dictionary covers emitted receipt and evidence fields")

# ---------------------------------------------------------------------------
# REGRESSION (suite audit 2026-10-18, VEG-01): shared size-plausibility screen.
# Rows are kept; implausible measures leave only the matching size extreme and
# raise a warn-level size_spike flag.
# ---------------------------------------------------------------------------
cat("\n################  REGRESSION: size-plausibility screen  ################\n")
fx <- function(case, plotID, individualID, eventID, growthForm, plantStatus, height, stemDiameter, basalStemDiameter = NA)
  data.frame(case = case, plotID = plotID, individualID = individualID, eventID = eventID,
             growthForm = growthForm, plantStatus = plantStatus, height = height,
             stemDiameter = stemDiameter, basalStemDiameter = basalStemDiameter, stringsAsFactors = FALSE)
tree <- "single bole tree"
size_fx <- rbind(
  fx("tall01932", "TALL_011", "NEON.PLA.D08.TALL.01932", c("e2015", "e2020", "e2023"), tree, "Live", c(8.9, 85.6, 11.3), c(11.0, 11.4, 11.8)),
  fx("bart05414", "BART_001", "NEON.PLA.D01.BART.05414", c("e2016", "e2017", "e2018"), tree, "Live", c(18.3, 119.8, 18.8), c(22.1, 22.5, 22.6)),
  fx("harv_dead_snap", "HARV_002", "NEON.PLA.D01.HARV.01179", c("e2014", "e2015", "e2019"), tree,
     c("Live", "Dead, broken bole", "Dead, broken bole"), c(16.0, 1.8, 1.7), c(29.4, 28.1, 28.8)),
  fx("tall_live_snap", "TALL_015", "NEON.PLA.D08.TALL.01194", c("e2015", "e2019", "e2023"), tree,
     c("Live", "Live", "Live, broken bole"), c(21.0, 21.5, 2.1), c(19.9, 20.2, 20.5)),
  fx("two_event_snap", "TALL_016", "NEON.PLA.D08.TALL.09001", c("e2015", "e2023"), tree,
     c("Live", "Live, physically damaged"), c(18.0, 2.0), c(25.0, 25.5)),
  fx("height_only", "TALL_017", "NEON.PLA.D08.TALL.09002", c("e2015", "e2019", "e2023"), tree, "Live", c(20.0, 70.0, 21.0), c(60.0, 61.0, 62.0)),
  fx("diameter_only", "YELL_002", "NEON.PLA.D12.YELL.00475", c("e2019", "e2021", "e2024"), tree, "Standing dead", c(21.7, 20.9, 20.4), c(139.0, 45.5, 44.9)),
  fx("wref", "WREF_001", "W1", "e2019", tree, "Live", 70.0, 150),
  fx("plantation", "PLAN_001", "P1", "e2019", tree, "Live", 25.0, 12),
  fx("yell_sapling", "YELL_001", "Y1", "e2021", "sapling", "Live", 30.0, NA, 1.4),
  fx("srer_shrub", "SRER_001", "S1", c("e2019", "e2024"), "single shrub", "Live", c(7.0, 7.4), NA, c(71.2, 73.5)))
size_fx$live <- grepl("^Live", size_fx$plantStatus)
size_fx$year <- as.integer(sub("^e", "", size_fx$eventID))
size_fx$date <- as.Date(paste0(size_fx$year, "-06-01"))
sc <- vst_size_screen(size_fx)
at <- function(case, eventID = NULL) which(size_fx$case == case & (is.null(eventID) | size_fx$eventID %in% eventID))
check(identical(sc$size_plausible[at("tall01932")], c(TRUE, FALSE, TRUE)),
      "TALL.01932 85.6 m on an 11.4 cm oak fails; its 8.9 m and 11.3 m events pass")
check(grepl("height spike", sc$size_screen_reason[at("tall01932", "e2020")]) &&
        grepl("2.5 m per cm of DBH", sc$size_screen_reason[at("tall01932", "e2020")]),
      "the 85.6 m record names both the spike and the height-to-DBH rule")
check(identical(sc$size_height_reference_m[at("tall01932", "e2020")], 10.1),
      "the flagged height carries its reference: the median of the plant's other heights (10.1 m)")
check(!sc$size_plausible[at("bart05414", "e2017")] && grepl("above 100 m", sc$size_screen_reason[at("bart05414", "e2017")]),
      "BART 119.8 m fails the 100 m cap")
check(all(sc$size_plausible[at("harv_dead_snap")]),
      "a live 16 m tree that later snaps to a 1.8 m dead broken bole is not a height spike")
check(all(sc$size_plausible[at("tall_live_snap")]),
      "intact 21 m heights are not spikes against a later 2.1 m 'Live, broken bole' record")
check(all(sc$size_plausible[at("two_event_snap")]),
      "with two events, an intact 18 m height is not a spike against a 'Live, physically damaged' 2 m record")
check(!sc$size_height_ok[at("height_only", "e2019")] && sc$size_diameter_ok[at("height_only", "e2019")],
      "a height spike on a sound DBH leaves only the tallest extremes")
check(sc$size_height_ok[at("diameter_only", "e2019")] && !sc$size_diameter_ok[at("diameter_only", "e2019")],
      "a 139 cm DBH spike (YELL.00475 scale) leaves only the biggest extremes")
check(sc$size_plausible[at("wref")], "a 70 m tree on a 150 cm DBH (WREF Douglas-fir scale) passes")
check(sc$size_plausible[at("plantation")], "a 25 m plantation conifer on a 12 cm DBH (ratio 2.08) passes")
check(!sc$size_plausible[at("yell_sapling")] && !sc$size_height_ok[at("yell_sapling")],
      "a 30 m sapling on a 1.4 cm basal diameter fails the shrub rule on its height")
check(all(sc$size_plausible[at("srer_shrub")]), "a stable 71-74 cm desert shrub basal diameter passes")
screened_fx <- with_size_screen(size_fx)
check(identical(smax(size_screened(screened_fx[screened_fx$case == "tall01932", ], "height")$height), 11.3),
      "the tallest extreme skips the flagged height")
check(identical(smax(size_screened(screened_fx[screened_fx$case == "height_only", ], "diameter")$stemDiameter), 62),
      "the biggest extreme keeps a sound diameter whose height failed")
check(nrow(screened_fx) == nrow(size_fx) && nrow(size_screened(screened_fx)) == sum(sc$size_plausible),
      "the screen never drops rows from the source table")
size_rows <- .size_spike_rows(screened_fx, SIZE_FOREST)
check(nrow(size_rows) == 4L && setequal(size_rows$height_m, c(85.6, 119.8, 70.0, 21.7)),
      "size_spike QC rows list the four tree-channel failures")
check(all(c("height_reference_m", "diameter_reference_cm") %in% names(size_rows)) &&
        identical(size_rows$diameter_reference_cm[size_rows$diameter_cm %in% 139], 45.2),
      "size_spike rows carry the other-event reference the rule compared against")
size_q <- tree_qc_site(screened_fx, SIZE_FOREST, NULL)
check(!is.null(size_q) && any(vapply(size_q$flags, function(f) identical(f$key, "size_spike") && identical(f$level, "warn"), logical(1))),
      "tree_qc_site raises a warn-level size_spike flag even without supported history")
check(isTRUE(assert_qc_dictionary(qc_dictionary(size_q$report), size_q$report)),
      "QC dictionary covers the size_spike columns")
plant_flags <- tree_qc_flags(screened_fx[screened_fx$case == "tall01932", ], SIZE_FOREST, NULL)
check(any(vapply(plant_flags, function(f) identical(f$level, "warn") &&
                   grepl("2020 record \\(85.6 m, 11.4 cm\\) is out of line", f$text) &&
                   grepl("It stays in every summary but is left out", f$text), logical(1))),
      "the plant card names the 2020 record and says verify, not wrong")
export_fx <- tidy_trees_export(cbind(size_fx, source_uid = paste0("u", seq_len(nrow(size_fx)))), NULL)
check(all(c("size_plausible", "size_screen_reason") %in% names(export_fx)) && sum(!export_fx$size_plausible) == 5L,
      "trees_long carries the screen result with the number")

# ---------------------------------------------------------------------------
# REGRESSION (VEG-03 part A): the mortality banner names the plot count and
# the real hold reason; the false "needs 2 censuses" line is gone.
# ---------------------------------------------------------------------------
cat("\n################  REGRESSION: mortality reason copy  ################\n")
mort_plots <- data.frame(
  plotID = c("SRER_001", "SRER_001", "SRER_002", "SRER_003"),
  eventID = c("e2016", "e2021", "e2019", "e2024"),
  date = as.Date(c("2016-05-01", "2021-05-01", "2019-05-01", "2024-05-01")),
  year = c(2016L, 2021L, 2019L, 2024L),
  shrub_records = c(12L, 10L, 8L, 9L),
  shrub_support = c("held_opportunity_unknown", "sampled_with_records", "held_opportunity_unknown", "held_identity_conflict"),
  shrub_support_reason = c("samplingImpractical is missing", "supported", "samplingImpractical is missing", "conflict"),
  stringsAsFactors = FALSE)
why <- mortality_unavailable_reason(NULL, SIZE_SHRUB, mort_plots, "SRER")
cat("  ", why, "\n")
check(grepl("no plot at SRER has two or more supported censuses in this channel \\(1 plot has one\\)", why),
      "mortality reason states the supported plot count")
check(grepl("2 earlier record-bearing plot-censuses \\(2016-2019\\) are held, not zero, because samplingImpractical is blank on these pre-March-2020 records \\(NEON added the field on 2020-03-10\\)", why),
      "mortality reason names the pre-2020 samplingImpractical hold and its years")
check(grepl("1 other record-bearing plot-census is held \\(identity conflict\\)", why),
      "mortality reason counts a single other hold without 'mostly'")
check(!grepl("needs ≥2 censuses|—", why), "no false 'needs 2 censuses' line and no em dash")
held_mix <- function(status) data.frame(plotID = paste0("P", seq_along(status)), eventID = "e2022",
  date = as.Date("2022-06-01"), year = 2022L, shrub_records = 5L, shrub_support = status,
  shrub_support_reason = "x", stringsAsFactors = FALSE)
majority <- mortality_unavailable_reason(NULL, SIZE_SHRUB, held_mix(c("held_identity_conflict", "held_identity_conflict", "held_missing_area")), "X")
tie <- mortality_unavailable_reason(NULL, SIZE_SHRUB, held_mix(c("held_identity_conflict", "held_missing_area")), "X")
check(grepl("3 other record-bearing plot-censuses are held \\(mostly identity conflict\\)", majority),
      "'mostly' only for a true majority")
check(grepl("\\(1 identity conflict, 1 missing area\\)", tie) && !grepl("mostly", tie),
      "a tie lists the counts instead of claiming 'mostly'")
scope <- mortality_scope_note(list(n_plots = 2L, yr0 = 2020L, yr1 = 2024L, lo = NA_real_), mort_plots, SIZE_SHRUB)
check(grepl("From 2 plots, 2020-2024\\. No interval: fewer than three plots\\.", scope),
      "a reported rate carries its plot count, window and the withheld-interval reason")

cat("\nALL TESTS PASSED\n")
