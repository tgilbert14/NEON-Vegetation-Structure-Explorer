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

# ---------------------------------------------------------------------------
# REGRESSION (hk, 2026-10-21): mortality and growth gates, Korn-Graubard
# interval, explicit dead states, first-death time at risk, size classes that
# sum to the headline density, and QMD from the plot means.
# ---------------------------------------------------------------------------
cat("\n################  REGRESSION: hk statistics gates  ################\n")
mr_rep <- list(status = "reported", rate_pct = 1.9, lo = 0.5, hi = 3.9, n0 = 160L, deaths = 12L, n_plots = 5L,
               n_tower = 5L, n_distributed = 0L, t_yrs = 3.1, yr0 = 2020L, yr1 = 2023L)
txt <- mortality_text(mr_rep, NULL, SIZE_FOREST, NULL, "GUAN")$text
check(grepl("Annual mortality about 1.90%/yr \\(95% interval 0.50 to 3.90\\): 12 of 160 trackable plants died over a mean 3.1 years in 5 plots, 2020-2023, all tower plots\\.", txt),
      "a reported rate names its interval, cohort, plot count, window and tower-only design")
txt0 <- mortality_text(modifyList(mr_rep, list(deaths = 0L, rate_pct = 0, lo = 0, hi = 1.1, n_tower = 2L, n_distributed = 3L)),
                       NULL, SIZE_FOREST, NULL, "X")$text
check(grepl("No deaths among 160 trackable plants in 5 plots over a mean 3.1 years, 2020-2023; 95% upper bound 1.10%/yr\\.", txt0),
      "a cohort with no deaths reports its upper bound, not a zero rate")
txtk <- mortality_text(modifyList(mr_rep, list(status = "insufficient_clusters", n_plots = 2L, deaths = 3L)), NULL, SIZE_FOREST, NULL, "X")$text
check(grepl("Annual mortality is not reported: 160 trackable plants in 2 plots \\(3 died\\), too few plot clusters for a rate \\(needs 5\\)\\.", txtk),
      "fewer than 5 plot clusters withholds the rate and says why")
txtn <- mortality_text(modifyList(mr_rep, list(status = "insufficient_plants", n0 = 12L, deaths = 1L)), NULL, SIZE_FOREST, NULL, "X")$text
check(grepl("12 trackable plants in 5 plots \\(1 died\\), too few trackable plants for a rate \\(needs 30\\)\\.", txtn),
      "fewer than 30 trackable plants withholds the rate and says why")

# synthetic cohort: 6 tower plots x 6 permanent trees, censuses 2020 and 2023
mk <- function(plots) {
  rows <- list()
  for (p in plots) for (i in 1:6) {
    id <- sprintf("NEON.PLA.D00.SYN.%s%02d", sub("P", "", p), i)
    status23 <- if (p == "P1" && i <= 2) "Standing dead" else if (p == "P2" && i == 1) "Dead, broken bole" else
      if (p == "P3" && i == 1) "Lost, presumed dead" else "Live"
    rows[[length(rows) + 1]] <- data.frame(plotID = p, individualID = id, eventID = "E20", date = as.Date("2020-06-01"),
      growthForm = "single bole tree", plantStatus = "Live", stemDiameter = 20, permanent = TRUE, stringsAsFactors = FALSE)
    rows[[length(rows) + 1]] <- data.frame(plotID = p, individualID = id, eventID = "E23", date = as.Date("2023-06-01"),
      growthForm = "single bole tree", plantStatus = status23, stemDiameter = 20.5, permanent = TRUE, stringsAsFactors = FALSE)
    if (p == "P2" && i == 1) rows[[length(rows) + 1]] <- data.frame(plotID = p, individualID = id, eventID = "E24",
      date = as.Date("2024-06-01"), growthForm = "single bole tree", plantStatus = "Standing dead", stemDiameter = 20.5,
      permanent = TRUE, stringsAsFactors = FALSE)
  }
  tr <- do.call(rbind, rows); tr$live <- grepl("^Live", tr$plantStatus)
  ev <- unique(tr[, c("plotID", "eventID", "date")])
  pl <- data.frame(ev, tree_support = "sampled_with_records", tree_support_reason = "x", area_trees = 400,
                   plotType = "tower", stringsAsFactors = FALSE)
  list(trees = tr, plots = pl)
}
syn <- mk(paste0("P", 1:6))
mr <- stand_mortality(syn$trees, SIZE_FOREST, syn$plots)
check(identical(mr$status, "reported") && mr$n_plots == 6L && mr$n_tower == 6L,
      "6 tower plots and 35 trackable plants report a rate")
check(mr$n0 == 35L && mr$deaths == 3L,
      "'Lost, presumed dead' is censored, not a death (35 trackable plants, 3 deaths)")
check(isTRUE(all.equal(mr$t_yrs, round(as.numeric(as.Date("2023-06-01") - as.Date("2020-06-01")) / 365.25, 1))),
      "time at risk ends at the first all-dead census, not at a repeat dead record a year later")
expected <- round(100 * (1 - (1 - 3 / 35)^(1 / (as.numeric(as.Date("2023-06-01") - as.Date("2020-06-01")) / 365.25))), 2)
check(identical(mr$rate_pct, expected) && mr$lo > 0 && mr$lo < mr$rate_pct && mr$hi > mr$rate_pct,
      "the Korn-Graubard interval brackets the Sheil-May rate with a positive lower bound when deaths occurred")
check(identical(stand_mortality(syn$trees[syn$trees$plotID %in% paste0("P", 1:4), ], SIZE_FOREST, syn$plots)$status, "insufficient_clusters"),
      "4 plot clusters withhold the rate")
check(identical(veg_status_state(c("Live, broken bole", "Lost, presumed dead", "Lost, burned", "Downed", "Standing dead", "No longer qualifies")),
                c("live", "censored", "censored", "dead", "dead", "censored")),
      "dead means NEON's three dead states only")

# growth gate
gfx <- function(n, plots) data.frame(plotID = rep_len(plots, n), individualID = paste0("G", seq_len(n)),
  growth_cm_yr = seq(0.1, 0.5, length.out = n), mh_change = FALSE, short_interval = FALSE, stringsAsFactors = FALSE)
gs <- growth_summary(gfx(25, c("A", "B", "C")))
check(identical(gs$status, "reported") && gs$n_plants == 25L && gs$n_plots == 3L,
      "25 comparable plants in 3 plots report a median")
check(identical(growth_summary(gfx(19, c("A", "B", "C")))$status, "insufficient") &&
        identical(growth_summary(gfx(30, c("A", "B")))$status, "insufficient"),
      "fewer than 20 plants or fewer than 3 plots withhold the median")
gx <- gfx(25, c("A", "B", "C")); gx$short_interval[1:3] <- TRUE; gx$mh_change[4] <- TRUE; gx$growth_cm_yr[5] <- 7
gsx <- growth_summary(gx)
check(gsx$n_plants == 20L && gsx$n_short_interval == 3L && gsx$n_mh_excluded == 1L && gsx$n_offscreen == 1L,
      "short intervals, moved measurement points and off-screen changes are excluded and counted")
check(grepl("too few for a summary \\(needs 20 in 3 plots\\)", growth_text(growth_summary(gfx(8, c("A", "B"))), SIZE_FOREST)),
      "the withheld growth note says what is missing")

# size classes sum to the headline density; QMD comes from the plot means
for (site_spec in list(list("HARV", SIZE_FOREST), list("SRER", SIZE_SHRUB))) {
  b <- readRDS(file.path("data/sites", paste0(site_spec[[1]], ".rds")))
  snap <- tree_snapshot(b$trees, b$plots, site_spec[[2]])
  st <- stand_site(snap, b$plots, site_spec[[2]]); sc <- size_class(snap, b$plots, site_spec[[2]])
  check(isTRUE(all.equal(sum(sc$stems_ha), st$density_ha, tolerance = 1e-9)),
        sprintf("%s size-class stems/ha sum to the headline density", site_spec[[1]]))
  check(isTRUE(all.equal(st$qmd, sqrt(40000 * st$ba_ha / (pi * st$density_ha)))),
        sprintf("%s QMD equals sqrt(40000 BA / (pi density))", site_spec[[1]]))
}

# ---------------------------------------------------------------------------
# REGRESSION (contract v3, Option A): lettered secondary boles under 10 cm no
# longer hold their plot-event, but stay out of every tree-channel estimand.
# ---------------------------------------------------------------------------
cat("\n################  REGRESSION: contract v3 Option A estimands  ################\n")
oa <- data.frame(plotID = "GUAN_002",
  eventID = rep(c("E20", "E23"), each = 4),
  individualID = rep(c("NEON.PLA.D04.GUAN.01946", "NEON.PLA.D04.GUAN.01946A",
                       "NEON.PLA.D04.GUAN.01946B", "NEON.PLA.D04.GUAN.01946C"), 2),
  growthForm = "multi-bole tree",
  plantStatus = c("Live", "Live", "Live", "Live", "Live", "Live", "Standing dead", "Live"),
  stemDiameter = c(13.8, 8.9, 8.5, 10.2, 14.1, 9.1, 8.7, 10.6), stringsAsFactors = FALSE)
kept <- .option_a_tree_rows(oa, SIZE_FOREST)
check(setequal(unique(kept$individualID), c("NEON.PLA.D04.GUAN.01946", "NEON.PLA.D04.GUAN.01946C")) && nrow(kept) == 4L,
      "Option A keeps the main bole and secondary boles >= 10 cm, and drops secondary boles under 10 cm")
check(identical(.option_a_tree_rows(oa, SIZE_SHRUB), oa), "the shrub channel is untouched by the tree-class rule")
oa_status <- status_summary(oa[oa$eventID == "E23", ], SIZE_FOREST)
check(sum(oa_status$n) == 2L,
      "the status snapshot counts the parent and its 10.6 cm bole, not the dead 8.7 cm or live 9.1 cm boles")

cat("\nALL TESTS PASSED\n")
