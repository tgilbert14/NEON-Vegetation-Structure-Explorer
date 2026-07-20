#!/usr/bin/env Rscript

# Copy canonical channel-specific taxon summaries embedded in the site bundles.
# Tree DBH basal area and shrub/sapling basal-diameter cover remain separate;
# `ba_m2_ha` is retained only as a compatibility alias within each row's named
# physical channel and must never be ranked across channels.

suppressWarnings(suppressMessages(library(dplyr)))
source("scripts/vegetation_inventory.R")

VST_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v2"
SITE_DIR <- Sys.getenv("VST_SITE_DIR", unset = "data/sites")
SITE_INDEX_PATH <- Sys.getenv("VST_SITE_INDEX", unset = "data/site_index.rds")
SEARCH_INDEX_OUT <- Sys.getenv("VST_SEARCH_INDEX_OUT", unset = "data/search_index.rds")
sites <- vst_assert_site_inventory(SITE_DIR)
site_index <- readRDS(SITE_INDEX_PATH)

if (!is.data.frame(site_index) ||
    !identical(sort(as.character(site_index$site)), sort(VST_EXPECTED_SITES)) ||
    !identical(attr(site_index, "contract_id"), VST_CONTRACT_ID) ||
    any(site_index$contract_id != VST_CONTRACT_ID)) {
  stop("site_index is missing, incomplete, or not built from the canonical v2 contract",
       call. = FALSE)
}
source_receipt <- attr(site_index, "source_receipt")
receipts <- list()
channel_rows <- list()

taxa <- dplyr::bind_rows(lapply(sites, function(site) {
  bundle <- readRDS(file.path(SITE_DIR, paste0(site, ".rds")))
  if (is.null(bundle$contract) || !identical(bundle$contract$id, VST_CONTRACT_ID)) {
    stop(site, " bundle lacks canonical contract ", VST_CONTRACT_ID, call. = FALSE)
  }
  receipts[[site]] <<- bundle$meta$source_receipt %||% NULL
  summaries <- bundle$contract$channel_summary %||% list()
  channel_rows[[site]] <<- dplyr::bind_rows(lapply(
    c("tree_dbh", "shrub_sapling_basal"), function(channel) {
      summary <- summaries[[channel]] %||% NULL
      if (!is.list(summary) || !identical(as.character(summary$channel), channel)) {
        stop(site, " bundle lacks canonical channel summary for ", channel,
             call. = FALSE)
      }
      supported <- as.integer(summary$n_supported_plots) > 0L
      data.frame(
        site = site,
        contract_id = VST_CONTRACT_ID,
        channel = channel,
        channel_label = if (identical(channel, "tree_dbh"))
          "Tree DBH" else "Shrub & sapling basal",
        is_default_channel = identical(bundle$meta$primary_channel, channel),
        support_status = if (supported)
          "supported_sampled_context" else "held_no_supported_event",
        n_supported_plots = as.integer(summary$n_supported_plots),
        n_record_plots = as.integer(summary$n_record_plots),
        n_stems = as.integer(summary$n_stems),
        n_individuals = as.integer(summary$n_individuals),
        n_species = as.integer(summary$n_species),
        n_taxa = as.integer(summary$n_taxa),
        n_sampled_absence = as.integer(summary$n_sampled_absence),
        ba_ha = as.numeric(summary$ba_ha),
        density_ha = as.numeric(summary$density_ha),
        qmd_cm = as.numeric(summary$qmd_cm),
        metric_kind = as.character(summary$metric_kind),
        tallest_m = as.numeric(summary$tallest_m),
        biggest_diam_cm = as.numeric(summary$biggest_diam_cm),
        inference_scope = "latest supported event per sampled plot within this physical channel",
        stringsAsFactors = FALSE
      )
    }
  ))
  rows <- bundle$contract$index$taxa
  if (is.null(rows) || !nrow(rows)) return(NULL)
  if (any(rows$site != site) || any(rows$contract_id != VST_CONTRACT_ID)) {
    stop(site, " bundle has an invalid canonical taxon-index payload", call. = FALSE)
  }
  rows
}))

channel_sites <- dplyr::bind_rows(channel_rows)
channel_sites <- channel_sites[
  order(channel_sites$site, match(channel_sites$channel,
                                  c("tree_dbh", "shrub_sapling_basal"))),
  , drop = FALSE
]
expected_channel_keys <- as.vector(outer(
  sort(VST_EXPECTED_SITES), c("tree_dbh", "shrub_sapling_basal"),
  function(site, channel) paste(site, channel, sep = "\r")
))
actual_channel_keys <- paste(channel_sites$site, channel_sites$channel, sep = "\r")
if (nrow(channel_sites) != length(expected_channel_keys) ||
    anyDuplicated(actual_channel_keys) ||
    !identical(sort(actual_channel_keys), sort(expected_channel_keys))) {
  stop("channel-site index must contain exactly one row per registered site and physical channel",
       call. = FALSE)
}

if (nrow(taxa)) {
  taxa <- taxa[
    order(taxa$taxon_label, taxa$site, taxa$channel,
          -taxa$mean_plot_basal_m2_ha, na.last = TRUE), , drop = FALSE
  ]
}
if (any(vapply(receipts, Negate(is.null), logical(1)))) {
  if (length(receipts) != length(VST_EXPECTED_SITES) ||
      !all(vapply(receipts, Negate(is.null), logical(1)))) {
    stop("source receipt is present on only part of the site family", call. = FALSE)
  }
  bundle_receipt <- vst_receipts_identical(receipts)
  if (!identical(bundle_receipt, source_receipt)) {
    stop("site_index receipt does not match the canonical bundle family", call. = FALSE)
  }
}

search_index <- list(
  contract_id = VST_CONTRACT_ID,
  taxa = taxa,
  sites = site_index,
  channel_sites = channel_sites,
  built = as.Date(NA),
  metric_guard = "compare values only within the same physical channel"
)
if (!is.null(source_receipt)) search_index$source_receipt <- source_receipt
dir.create(dirname(SEARCH_INDEX_OUT), recursive = TRUE, showWarnings = FALSE)
saveRDS(search_index, SEARCH_INDEX_OUT, compress = "xz")

cat(sprintf(
  "search_index copied from canonical bundles: %d taxon-site-channel rows; %d site-channel summaries; %d sites; contract %s\n",
  nrow(taxa), nrow(channel_sites), length(sites), VST_CONTRACT_ID
))
