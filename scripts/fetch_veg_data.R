# ===========================================================================
# fetch_veg_data.R — pull NEON Vegetation structure (DP1.10098.001) for an
# expanded set of forest sites and save raw loadByProduct dumps to
# ../veg-data-fetch/<SITE>_raw.rds for later bundling.
#
# Run with R-4.1.1 (neonUtilities crashes on R-4.5.x):
#   & "C:\Program Files\R\R-4.1.1\bin\Rscript.exe" scripts/fetch_veg_data.R
#
# Optional: put your NEON API token in a file called .neon_token in the
# project root to avoid rate-limiting. Skips sites already downloaded.
# ===========================================================================
tok <- tryCatch(trimws(readLines(".neon_token", warn = FALSE))[1], error = function(e) "")
suppressPackageStartupMessages(library(neonUtilities))

outdir <- "../veg-data-fetch"
dir.create(outdir, showWarnings = FALSE)

# Diverse forest sites spanning US biomes.
# Add / remove freely — bundle_veg_data.R reads whatever is in veg-data-fetch/.
SITES <- c(
  "HARV",   # Harvard Forest, MA          — NE mixed hardwood (flagship)
  "BART",   # Bartlett Experimental, NH   — northern hardwood-spruce
  "SCBI",   # Smithsonian CBI, VA         — mid-Atlantic deciduous (ForestGEO)
  "GRSM",   # Great Smoky Mountains, TN   — most biodiverse temperate forest in NA
  "ORNL",   # Oak Ridge NL, TN            — southeastern mixed forest
  "TALL",   # Talladega NF, AL            — longleaf pine restoration
  "JERC",   # Jones Ecological RC, GA     — longleaf pine-wiregrass
  "OSBS",   # Ordway-Swisher BS, FL       — FL sand scrub / longleaf
  "DELA",   # Dead Lake, AL               — bottomland hardwood / baldcypress
  "RMNP",   # Rocky Mountain NP, CO       — subalpine Engelmann spruce-fir
  "WREF",   # Wind River, WA              — old-growth Douglas-fir (2nd tallest site)
  "SJER"    # San Joaquin ER, CA          — California blue-oak woodland
)

for (s in SITES) {
  outfile <- file.path(outdir, paste0(s, "_raw.rds"))
  if (file.exists(outfile)) {
    cat("=== SKIP", s, "(already downloaded) ===\n"); next
  }
  cat("=== fetching", s, "===\n"); flush.console()
  res <- tryCatch(
    loadByProduct(dpID = "DP1.10098.001", site = s,
                  startdate = "2015-01", enddate = "2024-12",
                  package = "basic", check.size = FALSE,
                  token = if (nzchar(tok)) tok else NA),
    error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) {
    saveRDS(res, outfile)
    cat("  saved", s, "— tables:", paste(names(res), collapse = ", "), "\n")
    for (tb in c("vst_mappingandtagging", "vst_apparentindividual", "vst_perplotperyear"))
      if (!is.null(res[[tb]])) cat("  ", tb, "rows:", nrow(res[[tb]]), "\n")
    flush.console()
  }
}
cat("ALL DONE — now run: Rscript scripts/bundle_veg_data.R\n")
