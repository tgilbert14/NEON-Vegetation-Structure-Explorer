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
  stype <- b$meta$structure_type %||% classify_structure(tree_snapshot(tr))
  cat(sprintf("\n=== %s (%s; spec=%s) — trees %d, plots %d ===\n",
              site, stype, spec$type, nrow(tr), nrow(pl)))
  snap <- tree_snapshot(tr); one <- one_per_tree(live_only(snap), spec)
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
# REGRESSION: status_summary() must SCOPE by the spec's growth forms. A prior
# bug referenced spec$forms when no spec defined it -> `%in% NULL` kept only
# NA-growthForm rows. This synthetic snapshot fails that bug loudly.
# ---------------------------------------------------------------------------
cat("\n################  REGRESSION: status_summary form-scoping  ################\n")
syn <- data.frame(
  individualID = c("t1", "t2", "s1", "s2", "x1", "d1"),
  growthForm   = c("single bole tree", "small tree", "single shrub", "small shrub", NA, "single bole tree"),
  plantStatus  = c("Live", "Live", "Live", "Live", "Live", "Standing dead 5"),
  stringsAsFactors = FALSE)

check(!is.null(SIZE_FOREST$forms) && length(SIZE_FOREST$forms) > 0, "SIZE_FOREST$forms is defined")
check(!is.null(SIZE_SHRUB$forms)  && length(SIZE_SHRUB$forms)  > 0, "SIZE_SHRUB$forms is defined")

sf <- status_summary(syn, SIZE_FOREST)   # keeps tree forms + NA: t1,t2,d1,x1 -> 4 (incl. 1 dead)
sh <- status_summary(syn, SIZE_SHRUB)    # keeps shrub forms + NA: s1,s2,t2,x1 -> 4 (all live; "single bole tree" d1 excluded)
cat("  forest counts:\n"); print(sf)
cat("  shrub counts:\n");  print(sh)

# the bug would scope to ONLY the NA row (x1) -> sum == 1 for both
check(sum(sf$n) == 4, "forest scoping keeps tree-form + NA individuals (4), not only NA (1)")
check(sum(sh$n) == 4, "shrub scoping keeps shrub-form + NA individuals (4), not only NA (1)")
# forest must see the standing-dead TREE; shrub must NOT (it's a single-bole tree, excluded)
check(sum(sf$n[grepl("Dead", sf$cls)]) == 1, "forest scoping includes the standing-dead tree")
check(sum(sh$n[grepl("Dead", sh$cls)]) == 0, "shrub scoping excludes the single-bole-tree (different form set)")

cat("\nALL TESTS PASSED\n")
