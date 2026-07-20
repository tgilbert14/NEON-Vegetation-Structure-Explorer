#!/usr/bin/env Rscript

# Release gate for the exact 42-site v2 derived family. Support decisions are
# independently checked by verify_bundle.R; this script then uses the deployed
# consumer helpers to rebuild every embedded and network-facing site, channel,
# and taxon summary from preserved rows.

source("scripts/vegetation_inventory.R")
source("scripts/derived_parity.R")

site_dir <- Sys.getenv("VST_SITE_DIR", unset = "data/sites")
site_index_path <- Sys.getenv("VST_SITE_INDEX", unset = "data/site_index.rds")
search_index_path <- Sys.getenv(
  "VST_SEARCH_INDEX", unset = "data/search_index.rds"
)

sites <- vst_assert_site_inventory(site_dir)
bundles <- stats::setNames(lapply(sites, function(site) {
  path <- file.path(site_dir, paste0(site, ".rds"))
  bundle <- readRDS(path)
  if (!is.list(bundle) ||
      !identical(as.character(bundle$meta$site %||% ""), site) ||
      !identical(as.character(bundle$meta$contract_id %||% ""),
                 VST_PARITY_CONTRACT_ID)) {
    stop(site, " is not an exact v2 parity input", call. = FALSE)
  }
  bundle
}), sites)

site_index <- readRDS(site_index_path)
search_index <- readRDS(search_index_path)
vst_assert_derived_parity(
  bundles, site_index, search_index, sort(VST_EXPECTED_SITES)
)
cat(sprintf(
  paste0(
    "Derived consumer parity OK: %d bundles, %d site rows, ",
    "%d channel rows, %d taxon rows.\n"
  ),
  length(bundles), nrow(site_index), nrow(search_index$channel_sites),
  nrow(search_index$taxa)
))
