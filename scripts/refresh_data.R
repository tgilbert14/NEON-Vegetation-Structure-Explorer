# ===========================================================================
# refresh_data.R — pull NEON Vegetation structure (DP1.10098.001) for the
# bundled sites and rebuild data/sites/*.rds + data/site_index.rds.
#
# Self-contained for CI (.github/workflows/refresh-data.yml on ubuntu-latest):
# it fetches the three source tables per site into ../veg-data-fetch/<SITE>_raw.rds
# (the exact shape scripts/bundle_veg_data.R already expects), then reuses that
# tested bundler + the app-matched index builder. NEON's loadByProduct is
# referenced by a COMPUTED package name so the rsconnect scanner never pins
# neonUtilities into the lean deploy manifest.
#
# Local (Windows): use R-4.1.1 (R-4.5.x can crash on loadByProduct). On Linux/CI
# any recent R is fine. Set NEON_TOKEN in the environment to speed the pull.
#   Rscript scripts/refresh_data.R
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

SITES <- c("HARV", "WREF", "SCBI")
DPID  <- "DP1.10098.001"
RAW   <- file.path("..", "veg-data-fetch")
dir.create(RAW, showWarnings = FALSE, recursive = TRUE)

.NEON_PKG <- paste0("neon", "Utilities")          # computed name -> scanner can't pin it
if (!requireNamespace(.NEON_PKG, quietly = TRUE))
  stop("neonUtilities is required to refresh data (install it in this R / CI runner).")
loadByProduct <- get("loadByProduct", asNamespace(.NEON_PKG))

tok  <- Sys.getenv("NEON_TOKEN")
want <- c("vst_mappingandtagging", "vst_apparentindividual", "vst_perplotperyear")

for (s in SITES) {
  cat("=== fetching", s, "===\n")
  args <- list(dpID = DPID, site = s, package = "basic",
               include.provisional = TRUE, check.size = FALSE, progress = FALSE)
  if (nzchar(tok)) args$token <- tok
  dl <- do.call(loadByProduct, args)
  miss <- setdiff(want, names(dl))
  if (length(miss)) stop(sprintf("%s: missing tables from the pull: %s", s, paste(miss, collapse = ", ")))
  keep <- dl[want]                                # exactly what bundle_veg_data.R reads
  saveRDS(keep, file.path(RAW, paste0(s, "_raw.rds")))
  cat(sprintf("  %s: %d tagged-stem rows, %d measurement rows\n",
              s, nrow(dl$vst_mappingandtagging), nrow(dl$vst_apparentindividual)))
}

# Reuse the tested bundler (raw -> data/sites/*.rds + demo + a first index),
# then overwrite the index with the app-matched definitions.
source("scripts/bundle_veg_data.R")
source("scripts/build_site_index.R")
cat("REFRESH DONE — commit data/ (manifest.json only if the file/package set changed).\n")
