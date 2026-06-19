# ===========================================================================
# build_site_index.R — rebuild data/site_index.rds from the committed per-site
# bundles, using the SAME definitions the app's hero uses, so the picker site
# cards and the in-app headline numbers always agree (live trees >=10 cm DBH as
# individuals; tree species >=10 cm; tallest live tree; biggest live DBH).
#
# No raw NEON pull needed — derives from data/sites/<SITE>.rds. Run with any R
# (readRDS/saveRDS + dplyr), e.g. via PowerShell:
#   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/build_site_index.R
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))
source("R/veg_helpers.R")

sites <- sub("\\.rds$", "", list.files("data/sites", pattern = "\\.rds$"))
rows <- lapply(sites, function(s) {
  b <- tryCatch(readRDS(file.path("data/sites", paste0(s, ".rds"))), error = function(e) NULL)
  if (is.null(b) || is.null(b$trees) || !nrow(b$trees)) return(NULL)
  snap <- tree_snapshot(b$trees)
  one  <- one_per_tree(live_only(snap))
  tree_sp <- species_level_only(trees_only(one))
  data.frame(
    site = s,
    n_trees = nrow(trees_only(one)),                                  # live trees >=10 cm (individuals)
    n_species = dplyr::n_distinct(tree_sp$scientificName),            # tree species >=10 cm
    tallest_m = round(smax(live_only(snap)$height), 1),
    biggest_dbh_cm = round(smax(trees_only(live_only(snap))$stemDiameter), 1),
    lat = b$meta$lat %||% NA_real_, lng = b$meta$lng %||% NA_real_,
    stringsAsFactors = FALSE)
})
idx <- dplyr::bind_rows(rows)
saveRDS(idx, "data/site_index.rds", compress = "xz")
cat("site_index rebuilt (definitions matched to the app hero):\n"); print(idx); cat("DONE\n")
