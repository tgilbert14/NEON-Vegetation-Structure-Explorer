# ===========================================================================
# NEON Vegetation Structure Explorer — global.R
# A NEONize sibling (Desert Data Labs) for the Vegetation structure product
# (DP1.10098.001): individual tagged stems remeasured over years. Chrome +
# bundling spine + pin-card interaction ported from the Small Mammal Tracker /
# Plant Diversity siblings; the analysis layer is woody-structure-native.
# ===========================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons)
  library(dplyr); library(tidyr); library(stringr); library(tibble)
  library(plotly); library(leaflet); library(DT)
  library(shinyjs); library(shinycssloaders); library(RColorBrewer); library(htmltools)
})

source("R/site_metadata.R", local = FALSE)
source("R/veg_helpers.R", local = FALSE)
source("R/report_pdf.R", local = FALSE)
source("R/map_picker.R", local = FALSE)   # reusable national site-picker map (flagship front door)

NEON_DPID <- "DP1.10098.001"   # Vegetation structure
.NEON_PKG <- paste0("neon", "Utilities")
LIVE_FETCH <- (Sys.getenv("VST_LIVE", "0") != "0") && requireNamespace(.NEON_PKG, quietly = TRUE)

# ---- bundled per-site data: list(trees, plots, meta) ----------------------
SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/demo.rds"
DEMO_META <- list(site = "HARV", label = "HARV · Harvard Forest · demo")

read_bundle <- function(f) {
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) { warning(sprintf("read_bundle('%s'): %s", f, conditionMessage(e))); NULL })
  if (is.null(out)) return(NULL)
  if (is.data.frame(out)) return(out)                  # site_index
  if (!is.list(out) || !is.data.frame(out$trees) || !is.data.frame(out$plots) ||
      is.null(out$meta)) NULL else out
}
load_site_bundle <- function(site) read_bundle(file.path(SITE_DIR, paste0(site, ".rds")))
load_demo <- function() { b <- load_site_bundle(DEMO_META$site); if (!is.null(b)) b else read_bundle(DEMO_PATH) }

# ---- exact v2 front-door and bundle release gate -------------------------
# Keep this runtime gate independent of the build scripts: those scripts are not
# part of the deployed app surface, and an index is not trusted merely because it
# can be read. A partial, legacy, or receipt-mismatched family must expose no
# derived counts, map summaries, search results, or site views.
VEG_EXPECTED_SITES <- c(
  "ABBY", "BART", "BLAN", "BONA", "CLBJ", "CPER", "DCFS", "DEJU",
  "DELA", "DSNY", "GRSM", "GUAN", "HARV", "HEAL", "JERC", "JORN",
  "KONZ", "LAJA", "LENO", "MLBS", "MOAB", "NIWO", "NOGP", "ONAQ",
  "ORNL", "OSBS", "PUUM", "RMNP", "SCBI", "SERC", "SJER", "SOAP",
  "SRER", "STEI", "TALL", "TEAK", "TREE", "UKFS", "UNDE", "WOOD",
  "WREF", "YELL"
)
VEG_RECEIPT_FIELDS <- c(
  "schema_version", "provenance_class", "product", "neon_release",
  "release_doi", "query_start", "query_end", "source_receipt_id",
  "raw_source_digest", "neon_utilities_version", "built_at", "builder_commit"
)

.veg_scalar_chr <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  trimws(as.character(x))
}
.veg_gate_result <- function(ok, reason = character(0), receipt = NULL) {
  reason <- as.character(reason)
  reason <- unique(reason[!is.na(reason) & nzchar(reason)])
  list(ok = isTRUE(ok) && !length(reason), reason = reason, receipt = receipt)
}

source_receipt_check <- function(receipt) {
  reason <- character(0)
  if (!is.list(receipt)) {
    return(.veg_gate_result(FALSE, "source receipt is missing or is not a list"))
  }
  missing <- setdiff(VEG_RECEIPT_FIELDS, names(receipt))
  if (length(missing)) reason <- c(reason, paste0("source receipt lacks ", paste(missing, collapse = ", ")))
  values <- stats::setNames(vapply(VEG_RECEIPT_FIELDS, function(field) {
    .veg_scalar_chr(receipt[[field]])
  }, character(1)), VEG_RECEIPT_FIELDS)
  blank <- names(values)[is.na(values) | !nzchar(values)]
  if (length(blank)) reason <- c(reason, paste0("source receipt has blank ", paste(blank, collapse = ", ")))
  exact <- c(
    schema_version = "1",
    provenance_class = "official-release",
    product = VEG_CONTRACT$product,
    neon_release = VEG_CONTRACT$release,
    release_doi = "https://doi.org/10.48443/pypa-qf12"
  )
  mismatch <- names(exact)[is.na(values[names(exact)]) | values[names(exact)] != exact]
  if (length(mismatch)) reason <- c(reason, paste0("source receipt has unexpected ", paste(mismatch, collapse = ", ")))
  digest <- values[["raw_source_digest"]]
  if (is.na(digest) || !grepl("^[0-9a-f]{64}$", digest)) {
    reason <- c(reason, "source receipt raw_source_digest is not a lowercase SHA-256")
  }
  expected_id <- sprintf("VST-%s-%s-sha256-%s", VEG_CONTRACT$product,
                         VEG_CONTRACT$release, digest)
  if (is.na(values[["source_receipt_id"]]) ||
      !identical(values[["source_receipt_id"]], expected_id)) {
    reason <- c(reason, "source receipt ID does not bind product, release, and raw digest")
  }
  if (is.na(values[["builder_commit"]]) ||
      !grepl("^[0-9a-f]{40}$", values[["builder_commit"]])) {
    reason <- c(reason, "source receipt builder_commit is not a full Git commit")
  }
  if (is.na(values[["built_at"]]) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", values[["built_at"]])) {
    reason <- c(reason, "source receipt built_at is not an ISO date")
  }
  query <- values[c("query_start", "query_end")]
  full_release <- all(query == "FULL_RELEASE")
  bounded <- all(grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", query)) && query[[2]] >= query[[1]]
  if (!isTRUE(full_release) && !isTRUE(bounded)) {
    reason <- c(reason, "source receipt query bounds are neither FULL_RELEASE nor a valid closed month range")
  }
  .veg_gate_result(!length(reason), reason, receipt)
}
valid_source_receipt <- function(receipt) isTRUE(source_receipt_check(receipt)$ok)

site_index_check <- function(index) {
  reason <- character(0)
  if (!is.data.frame(index)) {
    return(.veg_gate_result(FALSE, "site index is missing or is not a data frame"))
  }
  required <- c(
    "site", "contract_id", "primary_channel", "structure_type", "size_metric",
    "n_trees", "n_plots", "n_species", "n_taxa", "tallest_m", "biggest_diam_cm",
    "lat", "lng", "n_supported_plots", "n_record_plots", "n_stems",
    "n_individuals", "n_sampled_absence", "ba_ha", "density_ha", "qmd_cm",
    "metric_kind", "support_status", "inference_scope"
  )
  missing <- setdiff(required, names(index))
  if (length(missing)) reason <- c(reason, paste0("site index lacks ", paste(missing, collapse = ", ")))
  sites <- if ("site" %in% names(index)) as.character(index$site) else character(0)
  if (nrow(index) != length(VEG_EXPECTED_SITES) || anyDuplicated(sites) ||
      !identical(sort(sites), sort(VEG_EXPECTED_SITES))) {
    reason <- c(reason, "site index does not contain the exact registered 42-site family")
  }
  if (!identical(attr(index, "contract_id"), VEG_CONTRACT_ID)) {
    reason <- c(reason, "site index contract attribute is not the exact v2 contract")
  }
  if (!"contract_id" %in% names(index) ||
      any(is.na(index$contract_id)) || any(as.character(index$contract_id) != VEG_CONTRACT_ID)) {
    reason <- c(reason, "site index contains a non-v2 contract row")
  }
  if ("primary_channel" %in% names(index) &&
      any(is.na(index$primary_channel) |
          !as.character(index$primary_channel) %in% c("tree_dbh", "shrub_sapling_basal", "unavailable"))) {
    reason <- c(reason, "site index contains an unknown primary physical channel")
  }
  if ("support_status" %in% names(index) &&
      any(is.na(index$support_status) |
          !as.character(index$support_status) %in% c("supported_sampled_context", "held_no_supported_event"))) {
    reason <- c(reason, "site index contains an unknown support state")
  }
  if (length(sites) && any(is.na(match(sites, neon_sites$site)))) {
    reason <- c(reason, "site index contains a site without registered front-door metadata")
  }
  receipt <- attr(index, "source_receipt")
  receipt_check <- source_receipt_check(receipt)
  if (!receipt_check$ok) reason <- c(reason, receipt_check$reason)
  .veg_gate_result(!length(reason), reason, if (receipt_check$ok) receipt else NULL)
}
valid_site_index <- function(index) isTRUE(site_index_check(index)$ok)

search_index_check <- function(index, site_index, source_receipt) {
  reason <- character(0)
  if (!is.list(index)) {
    return(.veg_gate_result(FALSE, "search index is missing or is not a list"))
  }
  required <- c("contract_id", "taxa", "sites", "channel_sites", "built",
                "metric_guard", "source_receipt")
  missing <- setdiff(required, names(index))
  if (length(missing)) reason <- c(reason, paste0("search index lacks ", paste(missing, collapse = ", ")))
  if (!identical(.veg_scalar_chr(index$contract_id), VEG_CONTRACT_ID)) {
    reason <- c(reason, "search index is not the exact v2 contract")
  }
  if (!is.data.frame(index$taxa) || !is.data.frame(index$sites) ||
      !is.data.frame(index$channel_sites)) {
    reason <- c(reason, "search index taxa/sites/channel_sites are not data frames")
  }
  if (is.data.frame(index$sites) && is.data.frame(site_index) &&
      !isTRUE(all.equal(index$sites, site_index, check.attributes = TRUE))) {
    reason <- c(reason, "search index site rows differ from the canonical site index")
  }
  if (is.data.frame(index$taxa) && nrow(index$taxa)) {
    taxa_required <- c(
      "site", "contract_id", "channel", "taxon_label", "scientificName",
      "is_species", "n_stems", "ba_m2_ha", "year_min", "year_max"
    )
    taxa_missing <- setdiff(taxa_required, names(index$taxa))
    if (length(taxa_missing)) {
      reason <- c(reason, paste0("search taxa lack ", paste(taxa_missing, collapse = ", ")))
    } else {
      if (any(is.na(index$taxa$contract_id)) ||
          any(as.character(index$taxa$contract_id) != VEG_CONTRACT_ID)) {
        reason <- c(reason, "search index contains a non-v2 taxon row")
      }
      if (any(!as.character(index$taxa$site) %in% VEG_EXPECTED_SITES)) {
        reason <- c(reason, "search index contains a taxon row outside the registered site family")
      }
      if (any(!as.character(index$taxa$channel) %in% c("tree_dbh", "shrub_sapling_basal"))) {
        reason <- c(reason, "search index contains an unknown physical channel")
      }
      if (any(is.na(index$taxa$is_species))) {
        reason <- c(reason, "search index contains an unknown species-resolution state")
      }
    }
  }
  if (is.data.frame(index$channel_sites)) {
    channel_required <- c(
      "site", "contract_id", "channel", "channel_label", "is_default_channel",
      "support_status", "n_supported_plots", "n_record_plots", "n_stems",
      "n_individuals", "n_species", "n_taxa", "n_sampled_absence", "ba_ha",
      "density_ha", "qmd_cm", "metric_kind", "tallest_m",
      "biggest_diam_cm", "inference_scope"
    )
    channel_missing <- setdiff(channel_required, names(index$channel_sites))
    if (length(channel_missing)) {
      reason <- c(reason, paste0("search channel sites lack ",
                                 paste(channel_missing, collapse = ", ")))
    } else {
      channel_keys <- paste(index$channel_sites$site,
                            index$channel_sites$channel, sep = "\r")
      expected_keys <- as.vector(outer(
        sort(VEG_EXPECTED_SITES), c("tree_dbh", "shrub_sapling_basal"),
        function(site, channel) paste(site, channel, sep = "\r")
      ))
      if (nrow(index$channel_sites) != length(expected_keys) ||
          anyDuplicated(channel_keys) ||
          !identical(sort(channel_keys), sort(expected_keys))) {
        reason <- c(reason, "search channel sites do not contain the exact registered site x channel grid")
      }
      if (any(is.na(index$channel_sites$contract_id)) ||
          any(as.character(index$channel_sites$contract_id) != VEG_CONTRACT_ID)) {
        reason <- c(reason, "search channel sites contain a non-v2 contract row")
      }
      if (any(is.na(index$channel_sites$is_default_channel))) {
        reason <- c(reason, "search channel sites contain an unknown default-channel state")
      }
      allowed_support <- c("supported_sampled_context", "held_no_supported_event")
      if (any(is.na(index$channel_sites$support_status)) ||
          any(!as.character(index$channel_sites$support_status) %in% allowed_support)) {
        reason <- c(reason, "search channel sites contain an unknown support state")
      }
    }
  }
  if (length(index$built) != 1L || !inherits(index$built, "Date") || !is.na(index$built)) {
    reason <- c(reason, "search index build field is not deterministic NA_Date_")
  }
  receipt_check <- source_receipt_check(index$source_receipt)
  if (!receipt_check$ok) reason <- c(reason, receipt_check$reason)
  if (!is.null(source_receipt) && !identical(index$source_receipt, source_receipt)) {
    reason <- c(reason, "search index source receipt differs from the site index")
  }
  .veg_gate_result(!length(reason), reason, if (receipt_check$ok) index$source_receipt else NULL)
}
valid_search_index <- function(index, site_index, source_receipt) {
  isTRUE(search_index_check(index, site_index, source_receipt)$ok)
}

SITE_INDEX_CANDIDATE <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)
SITE_INDEX_CHECK <- site_index_check(SITE_INDEX_CANDIDATE)
SITE_SOURCE_RECEIPT <- SITE_INDEX_CHECK$receipt

# Tiny list(taxa, sites, channel_sites, built) built by
# scripts/build_search_index.R from the
# committed bundles. The Search tab filters this in memory — no live fetch.
SEARCH_INDEX_CANDIDATE <- tryCatch(readRDS("data/search_index.rds"), error = function(e) NULL)
SEARCH_INDEX_CHECK <- if (SITE_INDEX_CHECK$ok) {
  search_index_check(SEARCH_INDEX_CANDIDATE, SITE_INDEX_CANDIDATE, SITE_SOURCE_RECEIPT)
} else {
  .veg_gate_result(FALSE, "search index is held because the canonical site index failed")
}
VEG_FAMILY_READY <- isTRUE(SITE_INDEX_CHECK$ok) && isTRUE(SEARCH_INDEX_CHECK$ok)
VEG_FAMILY_HOLD_REASON <- unique(c(SITE_INDEX_CHECK$reason, SEARCH_INDEX_CHECK$reason))
SITE_INDEX <- if (VEG_FAMILY_READY) SITE_INDEX_CANDIDATE else NULL
SEARCH_INDEX <- if (VEG_FAMILY_READY) SEARCH_INDEX_CANDIDATE else NULL
BUNDLED <- if (VEG_FAMILY_READY) as.character(SITE_INDEX$site) else character(0)

bundle_contract_check <- function(bundle, expected_site = NULL,
                                  expected_receipt = SITE_SOURCE_RECEIPT,
                                  require_family = TRUE) {
  reason <- character(0)
  if (isTRUE(require_family) && !isTRUE(VEG_FAMILY_READY)) {
    reason <- c(reason, "the exact v2 index family is on hold")
  }
  if (!is.list(bundle) || !is.data.frame(bundle$trees) ||
      !is.data.frame(bundle$plots) || !is.list(bundle$meta) ||
      !is.list(bundle$contract)) {
    return(.veg_gate_result(FALSE, c(reason, "bundle lacks trees, plots, meta, or embedded contract")))
  }
  meta <- bundle$meta
  contract <- bundle$contract
  site <- .veg_scalar_chr(meta$site)
  if (is.na(site) || !site %in% VEG_EXPECTED_SITES) {
    reason <- c(reason, "bundle metadata does not identify a registered site")
  }
  if (!is.null(expected_site) && !identical(site, .veg_scalar_chr(expected_site))) {
    reason <- c(reason, "bundle metadata identifies a different requested site")
  }
  if (!identical(.veg_scalar_chr(meta$contract_id), VEG_CONTRACT_ID) ||
      !identical(.veg_scalar_chr(contract$id), VEG_CONTRACT_ID) ||
      !identical(suppressWarnings(as.integer(contract$version)), 2L)) {
    reason <- c(reason, "bundle does not carry the exact embedded v2 contract")
  }
  if (!identical(as.character(contract$plant_key), VEG_CONTRACT$plant_key) ||
      !identical(as.character(contract$event_key), VEG_CONTRACT$event_key) ||
      !identical(as.character(contract$stem_key), VEG_CONTRACT$stem_key) ||
      !setequal(as.character(contract$support_status$supported), VEG_CONTRACT$supported_status) ||
      !identical(.veg_scalar_chr(contract$support_status$zero), VEG_CONTRACT$zero_status) ||
      !setequal(as.character(contract$support_status$held),
                setdiff(VEG_CONTRACT$held_status, "held_snapshot_event_mismatch"))) {
    reason <- c(reason, "bundle identity keys or support vocabulary differ from the v2 contract")
  }
  if (!identical(.veg_scalar_chr(meta$product), VEG_CONTRACT$product) ||
      !identical(.veg_scalar_chr(contract$product), VEG_CONTRACT$product) ||
      !identical(.veg_scalar_chr(meta$release), VEG_CONTRACT$release) ||
      !identical(.veg_scalar_chr(contract$release), VEG_CONTRACT$release)) {
    reason <- c(reason, "bundle product or official release differs from the v2 contract")
  }
  channel <- .veg_scalar_chr(meta$primary_channel)
  if (is.na(channel) || !channel %in% c("tree_dbh", "shrub_sapling_basal", "unavailable")) {
    reason <- c(reason, "bundle metadata has an unknown primary physical channel")
  }
  embedded <- contract$index$site
  if (!is.data.frame(embedded) || nrow(embedded) != 1L ||
      !all(c("site", "contract_id", "primary_channel") %in% names(embedded)) ||
      !identical(.veg_scalar_chr(embedded$site), site) ||
      !identical(.veg_scalar_chr(embedded$contract_id), VEG_CONTRACT_ID) ||
      !identical(.veg_scalar_chr(embedded$primary_channel), channel)) {
    reason <- c(reason, "bundle embedded site index is missing or inconsistent")
  }
  if (isTRUE(require_family) && isTRUE(VEG_FAMILY_READY) &&
      is.data.frame(embedded) && nrow(embedded) == 1L && !is.na(site)) {
    canonical <- SITE_INDEX[SITE_INDEX$site == site, , drop = FALSE]
    if (nrow(canonical) != 1L ||
        !isTRUE(all.equal(as.data.frame(embedded), as.data.frame(canonical),
                          check.attributes = FALSE))) {
      reason <- c(reason, "bundle embedded site row differs from the canonical site index")
    }
  }
  if (!is.data.frame(contract$index$taxa)) {
    reason <- c(reason, "bundle embedded taxon index is not a data frame")
  } else if (nrow(contract$index$taxa)) {
    taxa <- contract$index$taxa
    if (!all(c("site", "contract_id") %in% names(taxa)) ||
        any(is.na(taxa$site) | as.character(taxa$site) != site) ||
        any(is.na(taxa$contract_id) | as.character(taxa$contract_id) != VEG_CONTRACT_ID)) {
      reason <- c(reason, "bundle embedded taxon rows are inconsistent with its site or contract")
    }
  }
  receipt_check <- source_receipt_check(meta$source_receipt)
  if (!receipt_check$ok) reason <- c(reason, receipt_check$reason)
  if (is.null(expected_receipt) || !identical(meta$source_receipt, expected_receipt)) {
    reason <- c(reason, "bundle source receipt differs from the canonical index family")
  }
  .veg_gate_result(!length(reason), reason, if (receipt_check$ok) meta$source_receipt else NULL)
}
valid_veg_bundle <- function(bundle, expected_site = NULL) {
  isTRUE(bundle_contract_check(bundle, expected_site = expected_site)$ok)
}

site_table <- if (length(BUNDLED)) {
  m <- neon_sites[match(BUNDLED, neon_sites$site), ]
  idx_cols <- intersect(c("contract_id", "primary_channel", "structure_type", "size_metric",
                          "metric_kind", "support_status", "n_trees", "n_stems", "n_plots",
                          "n_supported_plots", "n_sampled_absence", "n_species", "n_taxa", "ba_ha",
                          "density_ha", "qmd_cm", "tallest_m", "biggest_diam_cm"), names(SITE_INDEX))
  out <- cbind(m, SITE_INDEX[match(m$site, SITE_INDEX$site), idx_cols])
  if (!"primary_channel" %in% names(out)) out$primary_channel <- "unavailable"
  if (!"structure_type" %in% names(out)) out$structure_type <- "unknown"
  if (!"size_metric" %in% names(out)) out$size_metric <- "unavailable"
  out$channel_label <- ifelse(out$primary_channel == "tree_dbh", "Tree DBH channel",
    ifelse(out$primary_channel == "shrub_sapling_basal", "Shrub & sapling basal channel", "Held / unavailable"))
  out
} else neon_sites[0, ]

# ---- theme: DDL desert-night creative system ------------------------------
# Matches the DDL suite cover + the Small Mammal Tracker sibling: teal primary,
# coral accent, gold highlight on a dark sky — carried by the chart layer. The
# app DEFAULTS to LIGHT (ui.R input_dark_mode mode="light"); these DDL values
# drive the plotly markers/lines, which read crisp in both modes. Key NAMES are
# KEPT (server.R references DDL$navy/$gold/$bark/etc.), VALUES remapped to the
# desert palette so every chart re-themes from this one edit.
DDL <- list(
  navy = "#102018", navy2 = "#16412a", cardinal = "#c98a4c", gold = "#ffd24a",
  gold2 = "#e0b43a", sky = "#2f8fc4", green = "#4eb86a", green2 = "#2f8a52",
  bark = "#c98a4c", ink = "#eaf4ec", muted = "#a4c0aa", bg = "#0a140e",
  paper = "#102018", line = "rgba(255,255,255,0.12)",
  live = "#4eb86a", dead = "#c98a4c", rust = "#c98a4c")   # rust = reserved bark true-error tone

# Light "desert-day" base (DEFAULT). styles.css [data-bs-theme="dark"] carries
# the full desert-night system; both modes show the dark command-band hero +
# dark stat info-boxes (the "light page, dark hero" look).
# A system stack keeps cold starts and the full interface independent of a font CDN.
app_font_stack <- bslib::font_collection(
  "Aptos", "Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
)
app_theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = "#16261c",
  primary = "#2f8a52", secondary = "#b07a3c",
  success = "#3f9a52", info = "#2f8fc4", warning = "#c79a1c", danger = "#b07a3c",
  base_font = app_font_stack, heading_font = app_font_stack, "border-radius" = "10px")

asset_url <- function(path) {
  f <- file.path("www", path)
  v <- if (file.exists(f)) as.integer(as.numeric(file.mtime(f))) else 0L
  sprintf("%s?v=%s", path, v)
}

spin <- function(x, img = NULL) shinycssloaders::withSpinner(x, color = DDL$green, type = 6)
info_pop <- function(title, ..., placement = "auto")
  bslib::popover(
    tags$span(class = "info-dot", tabindex = "0", role = "button",
              `aria-label` = paste0("More info: ", title),
              bsicons::bs_icon("info-circle", `aria-hidden` = "true")),
    ..., title = title, placement = placement)
insight_banner <- function(icon, ..., tone = "navy")
  div(class = paste("chart-insight", paste0("ci-", tone)), bsicons::bs_icon(icon), div(class = "ci-text", ...))
# Auto-picks DARK text (#16261c) on a bright fill (gold/canopy/bark) and white on
# a dark fill via a luminance check, so the badge reads in both themes.
glow_badge <- function(label, color = "#2f8a52", glow = color) {
  txt <- tryCatch({
    rc <- grDevices::col2rgb(color)
    if ((0.299 * rc[1] + 0.587 * rc[2] + 0.114 * rc[3]) / 255 > 0.6) "#16261c" else "#ffffff"
  }, error = function(e) "#ffffff")
  span(class = "glow-badge", style = sprintf("color:%s; background:%s; border-color:%s;", txt, color, color), label)
}
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon), tags$span(class = "ch-title", " ", title), ...)
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

# Code-native measurement mark used in the chrome and loading state. It depicts
# a trunk, a diameter tape, and repeated-observation contours without inventing
# a species, measurement value, tag number, or site record.
MEASURE_MARK <- htmltools::HTML(paste0(
  '<svg class="measure-mark" viewBox="0 0 72 72" aria-hidden="true" focusable="false">',
  '<path class="mm-echo mm-echo-a" d="M22 62c8-11 8-38 7-53"/>',
  '<path class="mm-echo mm-echo-b" d="M50 62c-8-11-8-38-7-53"/>',
  '<path class="mm-trunk" d="M27 65c5-16 4-41 2-57h14c-2 16-3 41 2 57z"/>',
  '<path class="mm-tape" d="M24 39c8 3 16 3 24 0"/>',
  '<rect class="mm-tag" x="43" y="34" width="8" height="10" rx="1.5"/>',
  '<circle class="mm-rivet" cx="47" cy="37" r="1"/>',
  '</svg>'))
