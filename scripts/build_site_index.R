# ===========================================================================
# build_site_index.R — rebuild data/site_index.rds from the committed per-site
# bundles, using the SAME definitions the app's hero uses, so the picker site
# cards and the in-app headline numbers always agree.
#
# ADAPTIVE per site: forest sites count live trees >=10 cm DBH and report biggest
# DBH; shrubland sites count live shrubs (any basal diameter) and report biggest
# basal diameter. structure_type + size_metric are stored so the picker cards and
# hero can label each site correctly.
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
  stype <- b$meta$structure_type %||% classify_structure(tree_snapshot(b$trees))
  spec  <- size_spec(stype)
  snap <- tree_snapshot(b$trees)
  one  <- one_per_tree(live_only(snap), spec)
  woody <- woody_only(one, spec)                                   # live plants at/above threshold
  woody_sp <- species_level_only(woody)
  data.frame(
    site = s,
    structure_type = stype,
    size_metric = if (identical(stype, "shrubland")) "basal ø" else "DBH",
    n_trees = nrow(woody),                                         # live plants (trees or shrubs)
    n_species = dplyr::n_distinct(woody_sp$scientificName),        # species among them
    tallest_m = round(smax(live_only(snap)$height), 1),
    biggest_diam_cm = round(smax(woody_only(live_only(snap), spec)[[spec$col]]), 1),
    lat = b$meta$lat %||% NA_real_, lng = b$meta$lng %||% NA_real_,
    stringsAsFactors = FALSE)
})
idx <- dplyr::bind_rows(rows)
saveRDS(idx, "data/site_index.rds", compress = "xz")
cat("site_index rebuilt (adaptive forest/shrubland defs, matched to the app hero):\n")
print(idx[, c("site","structure_type","n_trees","n_species","tallest_m","biggest_diam_cm")]); cat("DONE\n")
