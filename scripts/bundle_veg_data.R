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
# Auto-detect all <SITE>_raw.rds files present — add sites by fetching more.
SITES <- sort(sub("_raw\\.rds$", "", basename(list.files(RAW, pattern = "_raw\\.rds$"))))
if (length(SITES) == 0) stop("No *_raw.rds files found in ", RAW, " — run scripts/fetch_veg_data.R first.")
cat("Sites found in veg-data-fetch:", paste(SITES, collapse = ", "), "\n")
DEMO  <- if ("HARV" %in% SITES) "HARV" else SITES[1]  # Harvard Forest preferred demo

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
  # ensure optional columns exist so transmute never errors on a site that lacks one
  for (cc in c("maxCrownDiameter", "basalStemDiameter", "measurementHeight"))
    if (!cc %in% names(a)) a[[cc]] <- NA
  for (cc in c("totalSampledAreaTrees", "totalSampledAreaShrubSapling"))
    if (!cc %in% names(pp)) pp[[cc]] <- NA

  ident <- m %>% dplyr::distinct(.data$individualID, .keep_all = TRUE) %>%
    dplyr::transmute(individualID, scientificName, genus, family, taxonRank)

  trees <- a %>%
    dplyr::transmute(
      individualID, plotID, subplotID,
      year = as.integer(substr(as.character(date), 1, 4)), date = as.Date(date),
      growthForm, plantStatus,
      live = grepl("^Live", plantStatus),
      stemDiameter = num(stemDiameter), basalStemDiameter = num(basalStemDiameter),
      height = num(height), maxCrownDiameter = num(maxCrownDiameter), canopyPosition,
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
                     area_trees = num(totalSampledAreaTrees),
                     area_shrub = num(totalSampledAreaShrubSapling)) %>%
    dplyr::filter(!is.na(.data$year)) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(plotType = mode_chr(.data$plotType), nlcdClass = mode_chr(.data$nlcdClass),
                     lat = stats::median(.data$lat, na.rm = TRUE), lng = stats::median(.data$lng, na.rm = TRUE),
                     area_trees = stats::median(.data$area_trees, na.rm = TRUE),
                     area_shrub = stats::median(.data$area_shrub, na.rm = TRUE), .groups = "drop")

  # classify the site by BASAL AREA — which growth form occupies more cross-sectional
  # area (so a forest with a dense shrub understory stays a forest). Trees use DBH
  # (>=10 cm), shrubs use basal stem diameter. Drives the app's adaptive paradigm.
  TREE_F  <- c("single bole tree", "multi-bole tree", "small tree")
  snap0 <- trees %>% dplyr::group_by(.data$individualID) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>% dplyr::ungroup()
  lv <- snap0[snap0$live %in% TRUE & !is.na(snap0$growthForm), ]
  is_tree  <- lv$growthForm %in% TREE_F & is.finite(lv$stemDiameter) & lv$stemDiameter >= 10
  is_shrub <- lv$growthForm %in% c("single shrub", "small shrub") & is.finite(lv$basalStemDiameter) & lv$basalStemDiameter > 0
  tree_ba  <- sum(pi * (lv$stemDiameter[is_tree] / 200)^2, na.rm = TRUE)
  shrub_ba <- sum(pi * (lv$basalStemDiameter[is_shrub] / 200)^2, na.rm = TRUE)
  stype <- if (tree_ba == 0 && shrub_ba == 0) "forest" else if (tree_ba >= shrub_ba) "forest" else "shrubland"

  meta <- list(site = site, structure_type = stype,
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
    biggest_diam_cm = round(max(snap$stemDiameter, na.rm = TRUE), 1),
    lat = b$meta$lat, lng = b$meta$lng, stringsAsFactors = FALSE)
  cat(sprintf("  %s: %d live trees, %d species | tallest %.1f m, biggest %.1f cm | %d tree-bouts | size %s\n",
      s, idx_rows[[s]]$n_trees, idx_rows[[s]]$n_species, idx_rows[[s]]$tallest_m, idx_rows[[s]]$biggest_diam_cm,
      nrow(tr), format(file.size(file.path("data/sites", paste0(s, ".rds"))), big.mark = ",")))
}
idx <- dplyr::bind_rows(idx_rows)
saveRDS(idx, "data/site_index.rds", compress = "xz")
cat("\nsite_index:\n"); print(idx); cat("DONE\n")
