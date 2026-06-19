# ===========================================================================
# Bundle NEON Vegetation structure (DP1.10098.001) into lean per-site .rds.
# Reads raw loadByProduct dumps from ../veg-data-fetch/<SITE>_raw.rds (built by
# the mammal app's scripts/fetch_veg_demo.R with R-4.1.1) and writes
# data/sites/<SITE>.rds + a data-sample demo + data/site_index.rds.
# Run with any R (just readRDS/saveRDS). See docs/data-bundling-pattern.md.
#
# Each bundle = list(trees=, plots=, meta=):
#   trees — ONE ROW PER individual x measurement bout (the growth career): join of
#           vst_mappingandtagging (identity/species, 1 row/individual) and
#           vst_apparentindividual (the repeated measurements). Cols: individualID,
#           plotID, year, date, scientificName, genus, family, taxonRank, is_species,
#           growthForm, plantStatus, live, stemDiameter (DBH cm @130), basalStemDiameter,
#           height (m), canopyPosition.
#   plots — vst_perplotperyear: plotID, year, plotType, nlcdClass, lat, lng,
#           area_trees (totalSampledAreaTrees, m^2 — the density/basal-area denominator).
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

RAW <- "../veg-data-fetch"
SITES <- c("HARV", "WREF", "SCBI")
DEMO  <- "HARV"   # Harvard Forest — NEON's flagship mixed-hardwood forest

is_species_rank <- function(rank, sci) {
  ok <- is.na(rank) | rank %in% c("species", "subspecies", "variety", "speciesGroup")
  amb <- grepl("\\bsp\\.?$", ifelse(is.na(sci), "", sci)) | grepl("/", ifelse(is.na(sci), "", sci), fixed = TRUE)
  ok & !amb
}

build_site <- function(site) {
  f <- file.path(RAW, paste0(site, "_raw.rds"))
  if (!file.exists(f)) { cat("  MISSING", f, "\n"); return(NULL) }
  r <- readRDS(f)
  m <- tibble::as_tibble(r$vst_mappingandtagging)
  a <- tibble::as_tibble(r$vst_apparentindividual)
  pp <- tibble::as_tibble(r$vst_perplotperyear)
  num <- function(x) suppressWarnings(as.numeric(x))

  ident <- m %>% dplyr::distinct(.data$individualID, .keep_all = TRUE) %>%
    dplyr::transmute(individualID, scientificName, genus, family, taxonRank)

  trees <- a %>%
    dplyr::transmute(
      individualID, plotID, subplotID,
      year = as.integer(substr(as.character(date), 1, 4)), date = as.Date(date),
      growthForm, plantStatus,
      live = grepl("^Live", plantStatus),
      stemDiameter = num(stemDiameter), basalStemDiameter = num(basalStemDiameter),
      height = num(height), canopyPosition,
      measurementHeight = num(measurementHeight),
      changedMeasurementLocation = changedMeasurementLocation,
      permanent = grepl("^NEON", individualID)) %>%   # TEMP.PLA ids aren't stable across years
    dplyr::left_join(ident, by = "individualID") %>%
    dplyr::filter(!is.na(.data$year)) %>%
    dplyr::mutate(is_species = is_species_rank(.data$taxonRank, .data$scientificName))

  plots <- pp %>%
    dplyr::transmute(plotID, year = as.integer(substr(as.character(date), 1, 4)),
                     plotType, nlcdClass,
                     lat = num(decimalLatitude), lng = num(decimalLongitude),
                     area_trees = num(totalSampledAreaTrees)) %>%
    dplyr::filter(!is.na(.data$year)) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(plotType = mode_chr(.data$plotType), nlcdClass = mode_chr(.data$nlcdClass),
                     lat = stats::median(.data$lat, na.rm = TRUE), lng = stats::median(.data$lng, na.rm = TRUE),
                     area_trees = stats::median(.data$area_trees, na.rm = TRUE), .groups = "drop")

  meta <- list(site = site,
               lat = stats::median(plots$lat, na.rm = TRUE), lng = stats::median(plots$lng, na.rm = TRUE),
               years = sort(unique(trees$year)))
  list(trees = trees, plots = plots, meta = meta)
}

mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }

dir.create("data/sites", showWarnings = FALSE, recursive = TRUE); dir.create("data-sample", showWarnings = FALSE)
idx_rows <- list()
for (s in SITES) {
  cat("=== bundling", s, "===\n")
  b <- build_site(s); if (is.null(b)) next
  saveRDS(b, file.path("data/sites", paste0(s, ".rds")), compress = "xz")
  if (identical(s, DEMO)) saveRDS(b, file.path("data-sample", "demo.rds"), compress = "xz")
  tr <- b$trees
  # latest measurement per individual = the current snapshot
  snap <- tr %>% dplyr::group_by(.data$individualID) %>% dplyr::slice_max(.data$date, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
  live <- snap[snap$live %in% TRUE & snap$is_species, ]
  idx_rows[[s]] <- data.frame(
    site = s,
    n_trees = nrow(live[live$growthForm %in% c("single bole tree", "multi-bole tree", "small tree"), ]),
    n_species = length(unique(live$scientificName)),
    tallest_m = round(max(snap$height, na.rm = TRUE), 1),
    biggest_dbh_cm = round(max(snap$stemDiameter, na.rm = TRUE), 1),
    lat = b$meta$lat, lng = b$meta$lng, stringsAsFactors = FALSE)
  cat(sprintf("  %s: %d live trees, %d species | tallest %.1f m, biggest %.1f cm | %d tree-bouts | size %s\n",
      s, idx_rows[[s]]$n_trees, idx_rows[[s]]$n_species, idx_rows[[s]]$tallest_m, idx_rows[[s]]$biggest_dbh_cm,
      nrow(tr), format(file.size(file.path("data/sites", paste0(s, ".rds"))), big.mark = ",")))
}
idx <- dplyr::bind_rows(idx_rows)
saveRDS(idx, "data/site_index.rds", compress = "xz")
cat("\nsite_index:\n"); print(idx); cat("DONE\n")
