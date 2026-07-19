#!/usr/bin/env Rscript

# Copy the canonical site rows embedded by bundle_veg_data.R. This script does
# not own a second stand definition; a missing/mismatched v2 contract is fatal.

suppressWarnings(suppressMessages(library(dplyr)))
source("scripts/vegetation_inventory.R")

VST_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v2"
SITE_DIR <- Sys.getenv("VST_SITE_DIR", unset = "data/sites")
SITE_INDEX_OUT <- Sys.getenv("VST_SITE_INDEX_OUT", unset = "data/site_index.rds")
sites <- vst_assert_site_inventory(SITE_DIR)
receipts <- list()

rows <- lapply(sites, function(site) {
  path <- file.path(SITE_DIR, paste0(site, ".rds"))
  bundle <- readRDS(path)
  if (is.null(bundle$contract) || !identical(bundle$contract$id, VST_CONTRACT_ID)) {
    stop(site, " bundle lacks canonical contract ", VST_CONTRACT_ID, call. = FALSE)
  }
  row <- bundle$contract$index$site
  if (!is.data.frame(row) || nrow(row) != 1L || !identical(row$site[[1L]], site) ||
      !identical(row$contract_id[[1L]], VST_CONTRACT_ID)) {
    stop(site, " bundle has an invalid canonical site-index payload", call. = FALSE)
  }
  receipts[[site]] <<- bundle$meta$source_receipt %||% NULL
  row
})

index <- dplyr::bind_rows(rows)
if (nrow(index) != length(VST_EXPECTED_SITES) ||
    !identical(sort(as.character(index$site)), sort(VST_EXPECTED_SITES))) {
  stop("site index did not account for the complete registered site family",
       call. = FALSE)
}
attr(index, "contract_id") <- VST_CONTRACT_ID
if (any(vapply(receipts, Negate(is.null), logical(1)))) {
  if (length(receipts) != length(VST_EXPECTED_SITES) ||
      !all(vapply(receipts, Negate(is.null), logical(1)))) {
    stop("source receipt is present on only part of the site family", call. = FALSE)
  }
  attr(index, "source_receipt") <- vst_receipts_identical(receipts)
}

dir.create(dirname(SITE_INDEX_OUT), recursive = TRUE, showWarnings = FALSE)
saveRDS(index, SITE_INDEX_OUT, compress = "xz")
cat(sprintf("site_index copied from %d canonical %s bundles\n",
            nrow(index), VST_CONTRACT_ID))
