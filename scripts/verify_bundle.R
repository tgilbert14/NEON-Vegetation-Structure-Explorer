#!/usr/bin/env Rscript

# Non-network release gate for the complete Vegetation Structure runtime family.
# Legacy bytes remain diagnosable, but only the event-keyed v2 contract can be a
# release candidate. The gate never turns a missing opportunity into an absence.

suppressPackageStartupMessages({
  library(dplyr)
  library(digest)
})
source("scripts/vegetation_inventory.R")
source("R/veg_helpers.R")

VST_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v2"
VST_SUPPORTED <- c("sampled_with_records", "sampled_absence")
VST_HELD <- c(
  "held_sampling_impractical", "held_dendrometer_only", "held_missing_area",
  "held_opportunity_unknown", "held_presence_record_conflict",
  "held_metric_invalid"
)
VST_STATUS <- c(VST_SUPPORTED, VST_HELD)

problems <- character(0)
note <- function(message) problems <<- c(problems, message)
nonblank <- function(value) !is.na(value) & nzchar(trimws(as.character(value)))
release_key <- function(data, columns) {
  parts <- lapply(columns, function(column) {
    value <- as.character(data[[column]])
    value[is.na(value)] <- "<NA>"
    paste0(nchar(value), ":", value)
  })
  do.call(paste, c(parts, sep = "\u001f"))
}
read_checked <- function(path) {
  result <- tryCatch(readRDS(path), error = function(error) error)
  if (inherits(result, "error")) {
    note(sprintf("%s failed to load: %s", path, conditionMessage(result)))
    return(NULL)
  }
  result
}
event_counts <- function(trees, plots, keep) {
  result <- integer(nrow(plots))
  keep[is.na(keep)] <- FALSE
  if (!nrow(trees) || !any(keep)) return(result)
  counts <- table(release_key(trees[keep, , drop = FALSE], c("plotID", "eventID")))
  matched <- match(release_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}
presence_state <- function(value) {
  value <- tolower(trimws(as.character(value)))
  result <- rep("unknown", length(value))
  result[grepl("not present|absent", value)] <- "absent"
  result[grepl("^present", value)] <- "present"
  result[is.na(value) | !nzchar(value)] <- "unknown"
  result
}
shrub_presence_state <- function(trees_present, shrubs_present) {
  tree_state <- presence_state(trees_present)
  shrub_state <- presence_state(shrubs_present)
  ifelse(shrub_state == "present", "present",
         ifelse(shrub_state == "absent" & tree_state == "absent",
                "absent", "unknown"))
}
expected_support_status <- function(area, sampling_impractical, data_collected,
                                    presence, records, invalid_metric) {
  sampling <- gsub("[^a-z]", "", tolower(trimws(as.character(sampling_impractical))))
  collected <- gsub("[^a-z]", "", tolower(trimws(as.character(data_collected))))
  result <- rep(NA_character_, length(area))
  for (i in seq_along(area)) {
    if (is.na(sampling[[i]]) || !nzchar(sampling[[i]])) {
      result[[i]] <- "held_opportunity_unknown"
    } else if (!identical(sampling[[i]], "ok")) {
      result[[i]] <- "held_sampling_impractical"
    } else if (identical(collected[[i]], "dendrometeronly")) {
      result[[i]] <- "held_dendrometer_only"
    } else if (!identical(collected[[i]], "allgrowthforms")) {
      result[[i]] <- "held_opportunity_unknown"
    } else if (!is.finite(area[[i]]) || area[[i]] <= 0) {
      result[[i]] <- "held_missing_area"
    } else if (records[[i]] > 0L && identical(presence[[i]], "absent")) {
      result[[i]] <- "held_presence_record_conflict"
    } else if (invalid_metric[[i]] > 0L) {
      result[[i]] <- "held_metric_invalid"
    } else if (records[[i]] > 0L) {
      result[[i]] <- "sampled_with_records"
    } else if (identical(presence[[i]], "absent")) {
      result[[i]] <- "sampled_absence"
    } else if (identical(presence[[i]], "present")) {
      result[[i]] <- "held_presence_record_conflict"
    } else {
      result[[i]] <- "held_opportunity_unknown"
    }
  }
  result
}
read_hash_ledger <- function(path, expected_names, label) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  valid <- grepl("^[0-9a-f]{64} [^ /]+$", lines)
  if (length(lines) != length(expected_names) || !all(valid)) {
    note(sprintf("%s checksum ledger must contain exactly %d strict '<sha256> <basename>' entries",
                 label, length(expected_names)))
    return(NULL)
  }
  parsed <- regexec("^([0-9a-f]{64}) ([^ /]+)$", lines)
  fields <- regmatches(lines, parsed)
  hashes <- vapply(fields, `[[`, character(1), 2L)
  names <- vapply(fields, `[[`, character(1), 3L)
  if (!identical(names, expected_names)) {
    note(sprintf("%s checksum ledger is not in exact registered basename order", label))
    return(NULL)
  }
  list(lines = lines, hashes = hashes, names = names)
}
read_family_digest <- function(path, label) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  if (length(lines) != 1L || !grepl("^[0-9a-f]{64}$", lines[[1L]])) {
    note(sprintf("%s family checksum ledger must contain one lowercase SHA-256", label))
    return(NULL)
  }
  lines[[1L]]
}
read_file_bytes <- function(path) {
  size <- file.info(path)$size
  if (is.na(size)) stop("cannot determine file size: ", path, call. = FALSE)
  readBin(path, what = "raw", n = size)
}

expected <- sort(VST_EXPECTED_SITES)
site_files <- list.files("data/sites", pattern = "[.]rds$", full.names = TRUE)
site_codes <- sort(sub("[.]rds$", "", basename(site_files)))
if (!identical(site_codes, expected)) {
  note(sprintf("site inventory mismatch: missing=[%s] extra=[%s]",
               paste(setdiff(expected, site_codes), collapse = ","),
               paste(setdiff(site_codes, expected), collapse = ",")))
}

legacy_tree_required <- c(
  "individualID", "plotID", "year", "date", "scientificName", "family",
  "taxonRank", "is_species", "growthForm", "plantStatus", "live",
  "stemDiameter", "basalStemDiameter", "height", "permanent"
)
legacy_plot_required <- c(
  "plotID", "plotType", "nlcdClass", "lat", "lng", "area_trees", "area_shrub"
)
v2_tree_required <- c(
  "plotID", "eventID", "individualID", "tempStemID", "date", "year",
  "growthForm", "plantStatus", "live", "stemDiameter", "basalStemDiameter",
  "measurementHeight", "basalMeasurementHeight", "changedMeasurementLocation",
  "tagStatus", "dendrometerCondition", "heightQualifier", "dataQF",
  "plant_key", "event_key", "stem_key"
)
v2_plot_required <- c(
  "plotID", "eventID", "date", "year", "eventType", "plotType",
  "samplingImpractical", "dataCollected", "treesPresent", "shrubsPresent",
  "treePresence", "shrubSaplingPresence", "area_trees", "area_shrub",
  "tree_records", "shrub_records", "tree_invalid_metric_records",
  "shrub_invalid_metric_records", "tree_support", "tree_support_reason",
  "shrub_support", "shrub_support_reason", "tree_supported",
  "shrub_supported", "event_key"
)

bundles <- list()
receipts <- list()
denominator_mismatches <- list()
v2_sites <- character(0)

for (path in site_files) {
  site <- sub("[.]rds$", "", basename(path))
  bundle <- read_checked(path)
  if (is.null(bundle)) next
  bundles[[site]] <- bundle
  if (!is.list(bundle) || !all(c("trees", "plots", "meta") %in% names(bundle))) {
    note(sprintf("%s is not a trees/plots/meta bundle", site))
    next
  }
  contract_id <- as.character(bundle$meta$contract_id %||% "")
  is_v2 <- identical(contract_id, VST_CONTRACT_ID)
  if (is_v2) v2_sites <- c(v2_sites, site)
  if (nzchar(contract_id) && !is_v2)
    note(sprintf("%s has unknown contract_id %s", site, contract_id))

  if (!is.data.frame(bundle$trees)) {
    note(sprintf("%s trees table is not a data frame", site))
    next
  }
  if (!is_v2 && !nrow(bundle$trees)) {
    note(sprintf("%s legacy trees table is empty", site))
    next
  }
  if (!is.data.frame(bundle$plots) || !nrow(bundle$plots)) {
    note(sprintf("%s plots table is empty or not a data frame", site))
    next
  }
  tree_required <- unique(c(legacy_tree_required, if (is_v2) v2_tree_required))
  plot_required <- unique(c(legacy_plot_required, if (is_v2) v2_plot_required))
  missing_trees <- setdiff(tree_required, names(bundle$trees))
  missing_plots <- setdiff(plot_required, names(bundle$plots))
  if (length(missing_trees))
    note(sprintf("%s trees lacks: %s", site, paste(missing_trees, collapse = ",")))
  if (length(missing_plots))
    note(sprintf("%s plots lacks: %s", site, paste(missing_plots, collapse = ",")))
  allowed_structure <- if (is_v2) c("forest", "shrubland", "unknown") else c("forest", "shrubland")
  if (!is.list(bundle$meta) || !identical(as.character(bundle$meta$site), site) ||
      !bundle$meta$structure_type %in% allowed_structure)
    note(sprintf("%s metadata is incomplete or identifies another site", site))
  receipts[site] <- list(bundle$meta$source_receipt %||% NULL)

  if (is_v2 && !length(missing_trees) && !length(missing_plots)) {
    contract <- bundle$contract %||% NULL
    if (!is.list(contract) || !identical(as.character(contract$id), VST_CONTRACT_ID) ||
        !identical(as.integer(contract$version), 2L)) {
      note(sprintf("%s lacks the exact embedded v2 contract", site))
    } else {
      if (!identical(as.character(contract$stem_key),
                     c("eventID", "individualID", "tempStemID")))
        note(sprintf("%s contract changes the official stem key", site))
      if (!identical(as.character(contract$event_key), c("plotID", "eventID")))
        note(sprintf("%s contract changes the opportunity-event key", site))
      if (!setequal(as.character(contract$support_status$supported), VST_SUPPORTED) ||
          !setequal(as.character(contract$support_status$held), VST_HELD))
        note(sprintf("%s contract support vocabulary differs from v2", site))
      embedded_site <- contract$index$site %||% NULL
      if (!is.data.frame(embedded_site) || nrow(embedded_site) != 1L ||
          !identical(as.character(embedded_site$site), site) ||
          !identical(as.character(embedded_site$contract_id), VST_CONTRACT_ID))
        note(sprintf("%s embedded site index is missing or inconsistent", site))
      if (!is.data.frame(contract$index$taxa %||% data.frame()))
        note(sprintf("%s embedded taxa index is not a data frame", site))
    }
    if (!identical(as.character(bundle$meta$product), "DP1.10098.001") ||
        !identical(as.character(bundle$meta$release), "RELEASE-2026"))
      note(sprintf("%s metadata does not identify DP1.10098.001 RELEASE-2026", site))

    trees <- bundle$trees
    plots <- bundle$plots
    if (nrow(trees)) {
      if (any(!nonblank(trees$eventID)) || any(!nonblank(trees$plotID)) ||
          any(!nonblank(trees$individualID)))
        note(sprintf("%s has blank measurement identity fields", site))
      if (anyDuplicated(release_key(trees, c("eventID", "individualID", "tempStemID"))))
        note(sprintf("%s duplicates eventID + individualID + tempStemID", site))
    }
    if (any(!nonblank(plots$eventID)) || any(!nonblank(plots$plotID)))
      note(sprintf("%s has blank opportunity identity fields", site))
    if (anyDuplicated(release_key(plots, c("eventID", "plotID"))))
      note(sprintf("%s duplicates eventID + plotID opportunity rows", site))
    if (nrow(trees)) {
      orphan <- setdiff(release_key(trees, c("plotID", "eventID")),
                        release_key(plots, c("plotID", "eventID")))
      if (length(orphan))
        note(sprintf("%s has %d measurement event keys without opportunity rows", site,
                     length(unique(orphan))))
    }

    tree_form <- tolower(trimws(as.character(trees$growthForm))) %in%
      c("single bole tree", "multi-bole tree")
    shrub_form <- tolower(trimws(as.character(trees$growthForm))) %in%
      c("single shrub", "small shrub", "sapling")
    tree_diameter <- suppressWarnings(as.numeric(trees$stemDiameter))
    shrub_diameter <- suppressWarnings(as.numeric(trees$basalStemDiameter))
    live <- trees$live %in% TRUE
    recomputed <- list(
      tree = list(
        records = event_counts(trees, plots, tree_form),
        invalid = event_counts(
          trees, plots,
          tree_form & live & !(is.finite(tree_diameter) & tree_diameter > 0 &
                               tree_diameter >= 10)
        ),
        presence = presence_state(plots$treesPresent)
      ),
      shrub = list(
        records = event_counts(trees, plots, shrub_form),
        invalid = event_counts(
          trees, plots,
          shrub_form & live & !(is.finite(shrub_diameter) & shrub_diameter > 0)
        ),
        presence = shrub_presence_state(plots$treesPresent, plots$shrubsPresent)
      )
    )
    if (!identical(as.character(plots$treePresence), recomputed$tree$presence))
      note(sprintf("%s tree presence state differs from published opportunity fields", site))
    if (!identical(as.character(plots$shrubSaplingPresence),
                   recomputed$shrub$presence))
      note(sprintf("%s shrub presence state differs from published opportunity fields", site))

    for (channel in c("tree", "shrub")) {
      status <- plots[[paste0(channel, "_support")]]
      reason <- plots[[paste0(channel, "_support_reason")]]
      supported <- plots[[paste0(channel, "_supported")]]
      area <- plots[[if (channel == "tree") "area_trees" else "area_shrub"]]
      records <- plots[[paste0(channel, "_records")]]
      invalid <- plots[[paste0(channel, "_invalid_metric_records")]]
      if (any(is.na(status)) || any(!status %in% VST_STATUS))
        note(sprintf("%s %s channel uses an unknown support status", site, channel))
      if (any(!nonblank(reason)))
        note(sprintf("%s %s channel has blank support reasons", site, channel))
      if (!identical(as.logical(supported), status %in% VST_SUPPORTED))
        note(sprintf("%s %s_supported disagrees with support status", site, channel))
      supported_rows <- status %in% VST_SUPPORTED
      if (any(supported_rows & (!is.finite(area) | area <= 0), na.rm = TRUE))
        note(sprintf("%s %s supported rows lack positive event area", site, channel))
      if (any(!is.finite(records) | records < 0 | records != as.integer(records), na.rm = TRUE))
        note(sprintf("%s %s record counts are invalid", site, channel))
      if (any(!is.finite(invalid) | invalid < 0 | invalid != as.integer(invalid), na.rm = TRUE))
        note(sprintf("%s %s invalid-metric counts are invalid", site, channel))
      if (!identical(as.integer(records), recomputed[[channel]]$records))
        note(sprintf("%s %s record counts differ from preserved measurement rows", site, channel))
      if (!identical(as.integer(invalid), recomputed[[channel]]$invalid))
        note(sprintf("%s %s invalid-metric counts differ from preserved live measurement rows", site, channel))
      expected_status <- expected_support_status(
        area, plots$samplingImpractical, plots$dataCollected,
        recomputed[[channel]]$presence, recomputed[[channel]]$records,
        recomputed[[channel]]$invalid
      )
      if (!identical(as.character(status), expected_status))
        note(sprintf("%s %s support states violate fail-closed precedence", site, channel))
      if (any(supported_rows & invalid != 0L, na.rm = TRUE))
        note(sprintf("%s %s supported rows contain invalid required metrics", site, channel))
      if (any(status == "held_metric_invalid" & invalid <= 0L, na.rm = TRUE))
        note(sprintf("%s %s held_metric_invalid rows contain no invalid required metrics", site, channel))
      if (any(status == "sampled_absence" & records != 0L, na.rm = TRUE))
        note(sprintf("%s %s sampled_absence rows contain records", site, channel))
      if (any(status == "sampled_with_records" & records <= 0L, na.rm = TRUE))
        note(sprintf("%s %s sampled_with_records rows contain no records", site, channel))
    }
  } else if (!length(missing_trees) && !length(missing_plots)) {
    # Keep the known legacy mismatch visible without normalizing it into a zero.
    spec <- size_spec(bundle$meta$structure_type)
    snap <- tree_snapshot(bundle$trees)
    eligible <- woody_only(live_only(snap), spec)
    record_plots <- sort(unique(as.character(eligible$plotID[!is.na(eligible$plotID)])))
    denominator_plots <- sort(unique(as.character(bundle$plots$plotID[!is.na(bundle$plots$plotID)])))
    unmatched <- setdiff(record_plots, denominator_plots)
    if (length(unmatched)) denominator_mismatches[[site]] <- unmatched
  }
}

receipt_count <- sum(vapply(receipts, Negate(is.null), logical(1)))
common_receipt <- NULL
if (length(v2_sites)) {
  if (!identical(sort(v2_sites), expected))
    note(sprintf("mixed contract family: only %d/%d bundles use v2", length(v2_sites), length(expected)))
  if (receipt_count != length(expected))
    note(sprintf("source receipt is present on only %d/%d v2 bundles", receipt_count, length(expected)))
  if (receipt_count == length(expected)) {
    common_receipt <- tryCatch(vst_receipts_identical(receipts), error = function(error) {
      note(conditionMessage(error)); NULL
    })
    if (!is.null(common_receipt)) {
      receipt_required <- c(
        "schema_version", "provenance_class", "product", "neon_release",
        "release_doi", "query_start", "query_end", "source_receipt_id",
        "raw_source_digest", "neon_utilities_version", "built_at", "builder_commit"
      )
      missing <- receipt_required[!vapply(receipt_required, function(field) {
        value <- common_receipt[[field]]
        length(value) == 1L && nonblank(value)
      }, logical(1))]
      if (length(missing)) note(sprintf("source receipt lacks: %s", paste(missing, collapse = ",")))
      if (!identical(as.character(common_receipt$provenance_class), "official-release") ||
          !identical(as.character(common_receipt$product), "DP1.10098.001") ||
          !identical(as.character(common_receipt$neon_release), "RELEASE-2026") ||
          !identical(as.character(common_receipt$release_doi),
                     "https://doi.org/10.48443/pypa-qf12"))
        note("source receipt does not identify the reviewed official RELEASE-2026 family")
      if (!grepl("^[0-9a-f]{64}$", as.character(common_receipt$raw_source_digest)))
        note("source receipt raw_source_digest is not SHA-256")
    }
  }
} else if (!receipt_count) {
  unexpected <- setdiff(names(denominator_mismatches), "WOOD")
  if (length(unexpected))
    note(sprintf("legacy family has unexpected unmatched denominator sites: %s",
                 paste(unexpected, collapse = ",")))
  wood <- bundles[["WOOD"]]
  if (is.null(wood)) {
    note("WOOD bundle is missing")
  } else {
    spec <- size_spec(wood$meta$structure_type)
    eligible <- woody_only(live_only(tree_snapshot(wood$trees)), spec)
    matched <- intersect(unique(as.character(eligible$plotID)),
                         unique(as.character(wood$plots$plotID)))
    if (nrow(eligible) == 0L || length(matched) != 0L ||
        length(denominator_mismatches[["WOOD"]]) == 0L)
      note("legacy WOOD must remain the explicit eligible-record/unmatched-plot-ID exception")
  }
} else {
  note("legacy contract family unexpectedly contains source receipts")
}

site_index <- read_checked("data/site_index.rds")
site_index_required <- c(
  "site", "structure_type", "size_metric", "n_trees", "n_plots",
  "n_species", "tallest_m", "biggest_diam_cm", "lat", "lng"
)
if (length(v2_sites)) site_index_required <- unique(c(
  site_index_required, "contract_id", "primary_channel", "n_supported_plots",
  "n_record_plots", "n_stems", "n_individuals", "n_taxa", "n_sampled_absence",
  "ba_ha", "density_ha", "qmd_cm", "metric_kind", "support_status",
  "inference_scope"
))
if (!is.null(site_index)) {
  if (!is.data.frame(site_index) || nrow(site_index) != length(expected))
    note("site_index must be a 42-row data frame")
  missing <- setdiff(site_index_required, names(site_index))
  if (length(missing)) note(sprintf("site_index lacks: %s", paste(missing, collapse = ",")))
  if ("site" %in% names(site_index) &&
      !identical(sort(as.character(site_index$site)), expected))
    note("site_index site set differs from the registered inventory")
  if ("site" %in% names(site_index) && anyDuplicated(as.character(site_index$site)))
    note("site_index contains duplicate sites")
  if (length(v2_sites) && "contract_id" %in% names(site_index) &&
      any(as.character(site_index$contract_id) != VST_CONTRACT_ID))
    note("site_index contains a non-v2 contract row")
  if (length(v2_sites) && !identical(attr(site_index, "contract_id"), VST_CONTRACT_ID))
    note("site_index contract_id attribute differs from v2")
  if (length(v2_sites) && !length(missing)) {
    unavailable <- as.character(site_index$primary_channel) == "unavailable"
    unavailable_fields <- c(
      "n_supported_plots", "n_record_plots", "n_stems", "n_individuals",
      "n_species", "n_taxa", "n_sampled_absence", "ba_ha", "density_ha",
      "qmd_cm", "tallest_m", "biggest_diam_cm", "n_trees", "n_plots"
    )
    if (any(vapply(unavailable_fields, function(field) {
      any(!is.na(site_index[[field]][unavailable]))
    }, logical(1))))
      note("site_index unavailable rows synthesize derived values instead of NA")
    if (any(unavailable & (as.character(site_index$support_status) !=
                           "held_no_supported_event" |
                           as.character(site_index$metric_kind) != "unavailable")))
      note("site_index unavailable rows lack the exact held status/metric label")
  }
  if (receipt_count == length(expected) &&
      !identical(attr(site_index, "source_receipt"), common_receipt))
    note("site_index source receipt differs from the site bundles")
}

search <- read_checked("data/search_index.rds")
if (!is.null(search)) {
  required <- c("taxa", "sites", "built")
  if (length(v2_sites)) required <- c(
    required, "channel_sites", "contract_id", "metric_guard", "source_receipt"
  )
  if (!is.list(search) || !all(required %in% names(search))) {
    note(sprintf("search_index lacks: %s", paste(setdiff(required, names(search)), collapse = ",")))
  } else {
    if (!is.data.frame(search$taxa) || !is.data.frame(search$sites) ||
        (length(v2_sites) && !is.data.frame(search$channel_sites)))
      note("search_index taxa/sites/channel_sites must be data frames")
    if (!is.null(site_index) &&
        !isTRUE(all.equal(as.data.frame(search$sites), as.data.frame(site_index),
                          check.attributes = FALSE)))
      note("search_index sites differ from site_index")
    if (length(search$built) != 1L || !inherits(search$built, "Date") || !is.na(search$built))
      note("search_index built must be deterministic NA_Date_; build time belongs in the source receipt")
    if (length(v2_sites) && !identical(as.character(search$contract_id), VST_CONTRACT_ID))
      note("search_index contract_id differs from v2")
    if (length(v2_sites) && is.data.frame(search$channel_sites)) {
      channel_required <- c(
        "site", "contract_id", "channel", "channel_label", "is_default_channel",
        "support_status", "n_supported_plots", "n_record_plots", "n_stems",
        "n_individuals", "n_species", "n_taxa", "n_sampled_absence", "ba_ha",
        "density_ha", "qmd_cm", "metric_kind", "tallest_m",
        "biggest_diam_cm", "inference_scope"
      )
      channel_missing <- setdiff(channel_required, names(search$channel_sites))
      if (length(channel_missing)) {
        note(sprintf("search_index channel_sites lacks: %s",
                     paste(channel_missing, collapse = ",")))
      } else {
        channel_keys <- paste(search$channel_sites$site,
                              search$channel_sites$channel, sep = "\r")
        expected_channel_keys <- as.vector(outer(
          expected, c("tree_dbh", "shrub_sapling_basal"),
          function(site, channel) paste(site, channel, sep = "\r")
        ))
        if (nrow(search$channel_sites) != length(expected_channel_keys) ||
            anyDuplicated(channel_keys) ||
            !identical(sort(channel_keys), sort(expected_channel_keys))) {
          note("search_index channel_sites differs from the exact site x physical-channel grid")
        }
        if (any(as.character(search$channel_sites$contract_id) != VST_CONTRACT_ID))
          note("search_index channel_sites contains a non-v2 row")
        supported_channel <- search$channel_sites$support_status ==
          "supported_sampled_context"
        if (any(supported_channel &
                (!is.finite(search$channel_sites$n_supported_plots) |
                 search$channel_sites$n_supported_plots <= 0), na.rm = TRUE)) {
          note("search_index supported channel rows lack supported plots")
        }
        if (any(!supported_channel &
                search$channel_sites$support_status != "held_no_supported_event",
                na.rm = TRUE)) {
          note("search_index channel_sites uses an unknown support state")
        }
      }
    }
    if (receipt_count == length(expected) &&
        !identical(search$source_receipt, common_receipt))
      note("search_index source receipt differs from the site bundles")
  }
}

demo <- read_checked("data-sample/demo.rds")
if (!is.null(demo) && !is.null(bundles[["HARV"]]) &&
    !isTRUE(all.equal(demo, bundles[["HARV"]], check.attributes = TRUE)))
  note("data-sample/demo.rds differs from the HARV runtime bundle")

audit_path <- file.path("data/source", "vegetation-data-quality-audit.csv")
if (length(v2_sites)) {
  if (!file.exists(audit_path)) {
    note("v2 family lacks data/source/vegetation-data-quality-audit.csv")
  } else {
    audit_environment <- new.env(parent = globalenv())
    audit_temp <- tempfile(fileext = ".csv")
    tryCatch({
      sys.source("scripts/write_data_quality_audit.R", envir = audit_environment)
      expected_audit <- audit_environment$vst_build_data_quality_audit("data/sites")
      audit_environment$vst_write_data_quality_audit(expected_audit, audit_temp)
      if (!identical(read_file_bytes(audit_path), read_file_bytes(audit_temp))) {
        note("data-quality audit differs from the deterministic 42-site v2 recomputation")
      }
    }, error = function(error) {
      note(sprintf("data-quality audit verification failed: %s",
                   conditionMessage(error)))
    }, finally = {
      unlink(audit_temp)
    })
  }
}

if (receipt_count == length(expected)) {
  ledger_files <- stats::setNames(file.path("data/source", c(
    "vegetation-raw-SHA256SUMS.txt", "vegetation-raw-family-SHA256.txt",
    "vegetation-fetch-runtime.txt", "vegetation-bundle-SHA256SUMS.txt",
    "vegetation-bundle-family-SHA256.txt",
    "vegetation-data-quality-audit.csv",
    "vegetation-data-quality-audit-SHA256.txt"
  )), c(
    "raw", "raw_family", "fetch_runtime", "bundle", "bundle_family",
    "audit", "audit_checksum"
  ))
  missing_ledgers <- ledger_files[!file.exists(ledger_files)]
  if (length(missing_ledgers))
    note(sprintf("refreshed source ledgers are missing: %s",
                 paste(missing_ledgers, collapse = ",")))
  if (!length(missing_ledgers)) {
    raw_names <- paste0(expected, "_raw.rds")
    bundle_names <- paste0(expected, ".rds")
    raw_ledger <- read_hash_ledger(ledger_files[["raw"]], raw_names, "raw source")
    raw_family_ledger <- read_family_digest(
      ledger_files[["raw_family"]], "raw source"
    )
    bundle_ledger <- read_hash_ledger(
      ledger_files[["bundle"]], bundle_names, "bundle"
    )
    bundle_family_ledger <- read_family_digest(
      ledger_files[["bundle_family"]], "bundle"
    )
    audit_ledger <- read_hash_ledger(
      ledger_files[["audit_checksum"]], basename(audit_path),
      "data-quality audit"
    )

    if (!is.null(raw_ledger)) {
      computed_raw_family <- digest::digest(
        paste0(paste0(raw_ledger$lines, collapse = "\n"), "\n"),
        algo = "sha256", serialize = FALSE
      )
      if (!is.null(raw_family_ledger) &&
          !identical(raw_family_ledger, computed_raw_family))
        note("raw family checksum differs from the exact raw checksum ledger")
      if (!is.null(common_receipt) &&
          !identical(as.character(common_receipt$raw_source_digest),
                     computed_raw_family))
        note("embedded raw_source_digest differs from the exact raw checksum ledger")
    }

    if (!is.null(bundle_ledger)) {
      bundle_paths <- file.path("data/sites", bundle_names)
      actual_hashes <- vapply(
        bundle_paths, digest::digest, character(1), algo = "sha256", file = TRUE
      )
      actual_lines <- sprintf("%s %s", unname(actual_hashes), bundle_names)
      if (!identical(bundle_ledger$lines, actual_lines))
        note("bundle checksum ledger differs from the exact committed site bytes")
      computed_bundle_family <- digest::digest(
        paste0(paste0(actual_lines, collapse = "\n"), "\n"),
        algo = "sha256", serialize = FALSE
      )
      if (!is.null(bundle_family_ledger) &&
          !identical(bundle_family_ledger, computed_bundle_family))
        note("bundle family checksum differs from the exact committed site bytes")
    }
    if (!is.null(audit_ledger)) {
      actual_audit_hash <- digest::digest(
        ledger_files[["audit"]], algo = "sha256", file = TRUE
      )
      if (!identical(unname(audit_ledger$hashes), actual_audit_hash))
        note("data-quality audit checksum ledger differs from the exact audit bytes")
    }
  }
}

cat(sprintf("bundles=%d; v2=%d; sourceReceipts=%d; denominatorMismatchSites=%s\n",
            length(bundles), length(v2_sites), receipt_count,
            if (length(denominator_mismatches))
              paste(names(denominator_mismatches), collapse = ",") else "none"))
if (length(problems)) {
  cat("Bundle verification failed:\n", paste0("- ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("Vegetation bundle, event/opportunity, quality-audit, index, receipt, and denominator gates passed.\n")
