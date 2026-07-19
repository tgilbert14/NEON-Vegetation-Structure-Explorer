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
  plot_opportunities_all = data.frame(site = "HARV", plotID = "HARV_001",
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
check(any(cb$column == "customOpportunityFlag" & cb$table == "plot_opportunities_all"),
      "new opportunity fields receive explicit dictionary rows")
check(any(cb$column == "customSourceFlag" & cb$table == "plot_opportunity_source"),
      "preserved opportunity-source fields receive explicit dictionary rows")

tree_export_fixture <- data.frame(
  source_uid = "apparent-uid-1", protocol_stem_key = "fixture",
  protocol_key_group_n = 1L, protocol_key_conflict = FALSE,
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
  "source_record_key", "protocol_stem_key", "protocol_key_group_n",
  "protocol_key_conflict",
  "recordType", "identificationQualifier", "mappingDataQF", "tagStatus",
  "dendrometerCondition", "heightQualifier", "dataQF", "measurementErrorQF"
)
check(all(required_review_fields %in% names(tree_export_result)),
      "trees_long preserves every registered identity and measurement review field")

qc_fixture <- data.frame(site = "HARV", source_digest = paste(rep("a", 64), collapse = ""),
                         contract_id = VEG_CONTRACT_ID, level = "info",
                         issue = "fixture", later_cm = 10, stringsAsFactors = FALSE)
qcb <- qc_dictionary(qc_fixture)
check(isTRUE(assert_qc_dictionary(qcb, qc_fixture)),
      "QC dictionary covers emitted receipt and evidence fields")

cat("\nALL TESTS PASSED\n")
