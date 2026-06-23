# ===========================================================================
# build_search_index.R — build the small, bundled, PRECOMPUTED "Search the
# network" index from the committed per-site bundles (data/sites/<SITE>.rds).
# NOT a live fetch. Writes data/search_index.rds, a tiny list loaded once at
# boot (like site_index) and filtered in memory by the Search tab.
#
# search_index = list(taxa = <tidy taxon-occurrence table>, sites = <site_index>,
#                     built = <Date>):
#   taxa — ONE ROW PER (scientificName, site): the display name, the site, the
#          site's structure_type, the per-site MEASURE for that species (live
#          basal area m2/ha for that taxon at that site, computed with the SAME
#          area-scaled stand machinery the app uses, and the live stem count),
#          plus year_min/year_max. Species-level only (is_species), live stems.
#   sites — the existing site_index (reused verbatim) so the THRESHOLD query
#          (site basal area > X, tallest, biggest stem) needs no recompute.
#
# The per-taxon basal area is built per plot then area-standardized to the site
# (stem basal area summed per plot / that plot's sampled area, averaged across
# plots), so it is the species' share of stand basal area in m2/ha — an honest
# within-site index, NOT an absolute cross-site ranking (forest DBH basal area
# and shrubland basal cover are different measurements; see veg_helpers codebook).
#
# Run with any R that can readRDS the bundles + dplyr (arrow loaded defensively
# in case a bundle still carries ALTREP string cols). Via PowerShell:
#   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/build_search_index.R
# ===========================================================================
suppressWarnings(suppressMessages({
  library(dplyr)
  try(library(arrow), silent = TRUE)   # materialize any ALTREP string cols on read
}))
source("R/veg_helpers.R")

sites <- sub("\\.rds$", "", list.files("data/sites", pattern = "\\.rds$"))
SITE_INDEX <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)

# per-taxon, area-standardized live basal area (m2/ha) at a site, summed across
# the same per-plot sampled areas the stand metrics use, plus live stem count.
taxon_rows_for_site <- function(s) {
  b <- tryCatch(readRDS(file.path("data/sites", paste0(s, ".rds"))), error = function(e) NULL)
  if (is.null(b) || is.null(b$trees) || !nrow(b$trees)) return(NULL)
  stype <- b$meta$structure_type %||% classify_structure(tree_snapshot(b$trees))
  spec  <- size_spec(stype)
  snap  <- tree_snapshot(b$trees)
  # live, species-identified, countable woody plants under this site's paradigm
  s_live <- woody_only(live_only(species_level_only(snap)), spec)
  if (is.null(s_live) || !nrow(s_live)) return(NULL)
  s_live$.d <- s_live[[spec$col]]
  s_live <- s_live[is.finite(s_live$.d) & s_live$.d > 0, , drop = FALSE]
  if (!nrow(s_live)) return(NULL)
  s_live$ba_m2 <- pi * (s_live$.d / 200)^2

  # total sampled area (ha) across plots that hold countable live woody plants —
  # the same denominator stand_by_plot uses, so the per-taxon m2/ha shares the
  # stand's area basis.
  acol <- if (spec$area %in% names(b$plots)) spec$area else "area_trees"
  pa <- b$plots[, c("plotID", acol)]; names(pa)[2] <- "area_use"
  pa$area_ha <- pa$area_use / 10000
  pa <- pa[is.finite(pa$area_ha) & pa$area_ha > 0.005, , drop = FALSE]
  contrib_plots <- intersect(unique(s_live$plotID), pa$plotID)
  tot_ha <- sum(pa$area_ha[pa$plotID %in% contrib_plots], na.rm = TRUE)
  if (!is.finite(tot_ha) || tot_ha <= 0) tot_ha <- NA_real_

  by_sp <- s_live %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      family   = mode_chr(.data$family),
      n_stems  = dplyr::n_distinct(.data$individualID),
      ba_m2    = sum(.data$ba_m2, na.rm = TRUE),
      .groups  = "drop")
  yrs <- range(b$trees$year[!is.na(b$trees$year)])
  by_sp %>% dplyr::transmute(
    scientificName = .data$scientificName,
    family         = .data$family,
    site           = s,
    structure_type = stype,
    size_metric    = if (identical(stype, "shrubland")) "basal cover" else "DBH basal area",
    n_stems        = .data$n_stems,
    ba_m2_ha       = if (is.na(tot_ha)) NA_real_ else round(.data$ba_m2 / tot_ha, 2),
    year_min       = if (all(is.finite(yrs))) yrs[1] else NA_integer_,
    year_max       = if (all(is.finite(yrs))) yrs[2] else NA_integer_)
}

taxa <- dplyr::bind_rows(lapply(sites, taxon_rows_for_site))
taxa <- taxa[!is.na(taxa$scientificName) & taxa$scientificName != "", , drop = FALSE]
taxa <- taxa[order(taxa$scientificName, -taxa$ba_m2_ha), , drop = FALSE]

search_index <- list(taxa = taxa, sites = SITE_INDEX, built = Sys.Date())
saveRDS(search_index, "data/search_index.rds", compress = "xz")

cat(sprintf("search_index built: %d taxon-site rows, %d distinct taxa, %d sites | file %s\n",
            nrow(taxa), dplyr::n_distinct(taxa$scientificName), dplyr::n_distinct(taxa$site),
            format(file.size("data/search_index.rds"), big.mark = ",")))
cat("sample:\n"); print(utils::head(taxa, 6)); cat("DONE\n")
