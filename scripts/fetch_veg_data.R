# ===========================================================================
# fetch_veg_data.R — pull NEON Vegetation structure (DP1.10098.001) for EVERY
# site that publishes it (42 sites) and save raw loadByProduct dumps to
# ../veg-data-fetch/<SITE>_raw.rds for later bundling. Skips sites already
# downloaded, so it is safe to re-run / resume.
#
# Run with R-4.1.1 (neonUtilities crashes on R-4.5.x):
#   & "C:\Program Files\R\R-4.1.1\bin\Rscript.exe" scripts/fetch_veg_data.R
#
# Optional: put your NEON API token in a file called .neon_token in the
# project root to avoid rate-limiting.
# ===========================================================================
tok <- tryCatch(trimws(readLines(".neon_token", warn = FALSE))[1], error = function(e) "")
suppressPackageStartupMessages({ library(neonUtilities); library(jsonlite) })

outdir <- "../veg-data-fetch"
dir.create(outdir, showWarnings = FALSE)

# Pull the authoritative availability list from the NEON API, then fetch all of
# them. Arid / shrub / grassland sites are ordered FIRST (they measure woody
# structure as shrubs, not trees — see docs) so they're available soonest.
ARID_FIRST <- c("SRER","JORN","ONAQ","MOAB","CPER","WOOD","DCFS","NOGP",
                "KONZ","CLBJ","DSNY")
avail <- tryCatch({
  j <- jsonlite::fromJSON("https://data.neonscience.org/api/v0/products/DP1.10098.001",
                          simplifyVector = FALSE)
  sort(vapply(j$data$siteCodes, function(s) s$siteCode, ""))
}, error = function(e) character(0))
if (!length(avail)) stop("Could not read site availability from the NEON API.")

SITES <- c(intersect(ARID_FIRST, avail), setdiff(avail, ARID_FIRST))
cat("NEON publishes veg structure at", length(avail), "sites. Fetch order:\n  ",
    paste(SITES, collapse = ", "), "\n\n")

for (s in SITES) {
  outfile <- file.path(outdir, paste0(s, "_raw.rds"))
  if (file.exists(outfile)) { cat("=== SKIP", s, "(already downloaded) ===\n"); next }
  cat("=== fetching", s, "===\n"); flush.console()
  res <- tryCatch(
    loadByProduct(dpID = "DP1.10098.001", site = s,
                  startdate = "2015-01", enddate = "2024-12",
                  package = "basic", check.size = FALSE,
                  token = if (nzchar(tok)) tok else NA),
    error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) {
    saveRDS(res, outfile)
    cat("  saved", s, "\n")
    for (tb in c("vst_mappingandtagging","vst_apparentindividual","vst_perplotperyear","vst_shrubgroup"))
      if (!is.null(res[[tb]])) cat("  ", tb, "rows:", nrow(res[[tb]]), "\n")
    flush.console()
  }
}
cat("ALL DONE — now run: Rscript scripts/bundle_veg_data.R\n")
