# ===========================================================================
# refresh_data.R — pull NEON Vegetation structure (DP1.10098.001) for EVERY
# site NEON publishes and rebuild data/sites/*.rds + data/site_index.rds.
#
# Self-contained for CI (.github/workflows/refresh-data.yml on ubuntu-latest):
# fetches the three source tables per site into ../veg-data-fetch/<SITE>_raw.rds
# (the shape scripts/bundle_veg_data.R expects), then reuses that tested bundler
# + the app-matched index builder. NEON's loadByProduct is referenced by a
# COMPUTED package name so the rsconnect scanner never pins neonUtilities.
#
# Robustness (the workflow `rm -f data/sites/*.rds` BEFORE this runs, so a half
# pull must NOT silently ship a shrunken dataset):
#  * Site list comes from the NEON API (self-maintaining); falls back to the
#    full known list if the API directory call fails.
#  * Each site is wrapped in tryCatch — a transient single-site failure is
#    skipped & logged, not fatal (one bad site can't abort the whole refresh).
#  * include.provisional is tried, then retried WITHOUT it (older neonUtilities
#    lacks the arg); no start/end dates (some sites have no data in a fixed
#    window — e.g. WOOD).
#  * A MASS-FAILURE GUARD stops() before bundling if too few sites succeeded, so
#    the job fails (and the destructive PR step never runs) instead of opening a
#    PR that deletes 30 sites.
#
# Local (Windows): use R-4.1.1 (R-4.5.x can crash on loadByProduct). On Linux/CI
# any recent R is fine. Set NEON_TOKEN in the environment to speed the pull.
#   Rscript scripts/refresh_data.R
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

DPID  <- "DP1.10098.001"
RAW   <- file.path("..", "veg-data-fetch")
dir.create(RAW, showWarnings = FALSE, recursive = TRUE)

# Full known site list (the 42 NEON publishes as of 2026-06) — used as a fallback
# if the API directory call fails so the refresh still covers everything.
KNOWN <- c("ABBY","BART","BLAN","BONA","CLBJ","CPER","DCFS","DEJU","DELA","DSNY",
           "GRSM","GUAN","HARV","HEAL","JERC","JORN","KONZ","LAJA","LENO","MLBS",
           "MOAB","NIWO","NOGP","ONAQ","ORNL","OSBS","PUUM","RMNP","SCBI","SERC",
           "SJER","SOAP","SRER","STEI","TALL","TEAK","TREE","UKFS","UNDE","WOOD",
           "WREF","YELL")
SITES <- tryCatch({
  j <- jsonlite::fromJSON(sprintf("https://data.neonscience.org/api/v0/products/%s", DPID),
                          simplifyVector = FALSE)
  api <- sort(vapply(j$data$siteCodes, function(s) s$siteCode, ""))
  if (length(api) >= 30) sort(union(api, KNOWN)) else KNOWN
}, error = function(e) { cat("API site list failed (", conditionMessage(e), ") — using known list.\n"); KNOWN })
cat("Refreshing", length(SITES), "sites:\n  ", paste(SITES, collapse = ", "), "\n\n")

.NEON_PKG <- paste0("neon", "Utilities")          # computed name -> scanner can't pin it
if (!requireNamespace(.NEON_PKG, quietly = TRUE))
  stop("neonUtilities is required to refresh data (install it in this R / CI runner).")
loadByProduct <- get("loadByProduct", asNamespace(.NEON_PKG))

tok  <- Sys.getenv("NEON_TOKEN")
want <- c("vst_mappingandtagging", "vst_apparentindividual", "vst_perplotperyear")

# one site: try with include.provisional, retry without it (older neonUtilities),
# no date bounds; returns TRUE on a saved raw file, FALSE on any failure.
fetch_site <- function(s) {
  base <- list(dpID = DPID, site = s, package = "basic", check.size = FALSE, progress = FALSE)
  if (nzchar(tok)) base$token <- tok
  dl <- tryCatch(do.call(loadByProduct, c(base, list(include.provisional = TRUE))),
    error = function(e) tryCatch(do.call(loadByProduct, base),
      error = function(e2) { cat("  ERROR", s, ":", conditionMessage(e2), "\n"); NULL }))
  if (is.null(dl)) return(FALSE)
  miss <- setdiff(want, names(dl))
  if (length(miss)) { cat("  SKIP", s, "- missing tables:", paste(miss, collapse = ", "), "\n"); return(FALSE) }
  saveRDS(dl[want], file.path(RAW, paste0(s, "_raw.rds")))
  cat(sprintf("  %s: %d tagged-stem rows, %d measurement rows\n",
              s, nrow(dl$vst_mappingandtagging), nrow(dl$vst_apparentindividual)))
  TRUE
}

ok <- 0L
for (s in SITES) { cat("=== fetching", s, "===\n"); if (isTRUE(fetch_site(s))) ok <- ok + 1L }

# Mass-failure guard: refuse to bundle (and thus refuse to open a shrinking PR)
# unless most sites pulled. The workflow already deleted data/sites/*.rds, so a
# stop() here fails the job and the PR step never runs — far safer than shipping
# a dataset missing 30 sites.
floor_n <- max(30L, as.integer(ceiling(0.75 * length(SITES))))
cat(sprintf("\n%d / %d sites fetched (floor = %d).\n", ok, length(SITES), floor_n))
if (ok < floor_n)
  stop(sprintf("Only %d/%d sites fetched (< %d) — aborting before bundle so the refresh PR can't delete sites.",
               ok, length(SITES), floor_n))

# Reuse the tested bundler (raw -> data/sites/*.rds + demo + a first index;
# classifies forest/shrubland, carries basal/crown/area_shrub cols), then
# overwrite the index with the app-matched definitions.
source("scripts/bundle_veg_data.R")
source("scripts/build_site_index.R")
cat("REFRESH DONE — commit data/ (manifest.json only if the file/package set changed).\n")
