#!/usr/bin/env Rscript

# Write a deterministic, human-inspectable quality audit for the exact v2
# Vegetation Structure release candidate. This script reports published
# qualifiers; it does not use dataQF, tagStatus, or changedMeasurementLocation
# as blanket row-exclusion rules.

suppressPackageStartupMessages(library(jsonlite))
source("scripts/vegetation_inventory.R", local = TRUE)

VST_DQA_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v2"
VST_DQA_SCHEMA <- "NEON-VST-data-quality-audit-v2"
VST_DQA_PRODUCT <- "DP1.10098.001"
VST_DQA_RELEASE <- "RELEASE-2026"
VST_DQA_RELEASE_DOI <- "https://doi.org/10.48443/pypa-qf12"
VST_DQA_SUPPORTED <- c("sampled_with_records", "sampled_absence")
VST_DQA_HELD <- c(
  "held_sampling_impractical", "held_dendrometer_only", "held_missing_area",
  "held_opportunity_unknown", "held_presence_record_conflict",
  "held_metric_invalid", "held_identity_conflict"
)
VST_DQA_STATUS <- c(VST_DQA_SUPPORTED, VST_DQA_HELD)
VST_DQA_CHANNELS <- list(
  tree_dbh = list(
    forms = c("single bole tree", "multi-bole tree"),
    support = "tree_support",
    reason = "tree_support_reason",
    invalid = "tree_invalid_metric_records",
    identity = "tree_identity_conflict_keys",
    metric = "stemDiameter",
    minimum = 10
  ),
  shrub_sapling_basal = list(
    forms = c("single shrub", "small shrub", "sapling"),
    support = "shrub_support",
    reason = "shrub_support_reason",
    invalid = "shrub_invalid_metric_records",
    identity = "shrub_identity_conflict_keys",
    metric = "basalStemDiameter",
    minimum = 0
  )
)

vst_dqa_nonblank <- function(value) {
  !is.na(value) & nzchar(trimws(as.character(value)))
}

vst_dqa_scalar <- function(value, label) {
  if (length(value) != 1L || is.na(value) ||
      !nzchar(trimws(as.character(value)))) {
    stop(label, " must be one nonblank scalar", call. = FALSE)
  }
  as.character(value)
}

vst_dqa_byte_order <- function(...) {
  values <- lapply(list(...), function(value) enc2utf8(as.character(value)))
  do.call(order, c(values, list(method = "radix", na.last = TRUE)))
}

vst_dqa_key <- function(data, columns) {
  parts <- lapply(columns, function(column) {
    value <- as.character(data[[column]])
    value[is.na(value)] <- "<NA>"
    paste0(nchar(value), ":", value)
  })
  do.call(paste, c(parts, sep = "\u001f"))
}

vst_dqa_value_counts_json <- function(value) {
  value <- as.character(value)
  value <- value[vst_dqa_nonblank(value)]
  if (!length(value)) return("[]")
  labels <- unique(value)
  labels <- labels[vst_dqa_byte_order(labels)]
  payload <- lapply(labels, function(label) {
    list(value = enc2utf8(label), count = as.integer(sum(value == label)))
  })
  as.character(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE
  ))
}

vst_dqa_held_reasons_json <- function(status, reason) {
  keep <- status %in% VST_DQA_HELD
  status <- as.character(status[keep])
  reason <- as.character(reason[keep])
  if (!length(status)) return("[]")
  if (any(!vst_dqa_nonblank(reason))) {
    stop("held support rows must carry nonblank reasons", call. = FALSE)
  }
  pairs <- unique(data.frame(
    status = status, reason = reason, stringsAsFactors = FALSE
  ))
  pairs <- pairs[vst_dqa_byte_order(pairs$status, pairs$reason), , drop = FALSE]
  payload <- lapply(seq_len(nrow(pairs)), function(index) {
    same <- status == pairs$status[[index]] & reason == pairs$reason[[index]]
    list(
      status = enc2utf8(pairs$status[[index]]),
      reason = enc2utf8(pairs$reason[[index]]),
      count = as.integer(sum(same))
    )
  })
  as.character(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE
  ))
}

vst_dqa_validate_receipt <- function(bundle, site) {
  contract_id <- vst_dqa_scalar(bundle$meta$contract_id %||% NULL,
                                paste0(site, " meta contract_id"))
  if (!identical(contract_id, VST_DQA_CONTRACT_ID)) {
    stop(site, " is not an exact v2 bundle", call. = FALSE)
  }
  contract <- bundle$contract %||% NULL
  if (!is.list(contract) ||
      !identical(as.character(contract$id %||% ""), VST_DQA_CONTRACT_ID) ||
      !identical(as.integer(contract$version %||% NA_integer_), 2L)) {
    stop(site, " lacks the exact embedded v2 contract", call. = FALSE)
  }
  if (!identical(as.character(contract$source_record_key %||% ""),
                 "source_uid") ||
      !identical(as.character(contract$protocol_stem_locator %||% character()),
                 c("plotID", "eventID", "individualID", "tempStemID")) ||
      !identical(as.character(contract$opportunity_source_record_key %||% ""),
                 "source_record_key") ||
      !setequal(as.character(contract$support_status$held %||% character()),
                VST_DQA_HELD)) {
    stop(site, " embedded identity or held-status contract differs from v2",
         call. = FALSE)
  }
  receipt <- bundle$meta$source_receipt %||% NULL
  if (!is.list(receipt)) {
    stop(site, " lacks an official source receipt", call. = FALSE)
  }
  required <- c(
    "provenance_class", "product", "neon_release", "release_doi",
    "source_receipt_id", "raw_source_digest"
  )
  values <- stats::setNames(lapply(required, function(field) {
    vst_dqa_scalar(receipt[[field]] %||% NULL,
                   paste(site, "source receipt", field))
  }), required)
  if (!identical(values$provenance_class, "official-release") ||
      !identical(values$product, VST_DQA_PRODUCT) ||
      !identical(values$neon_release, VST_DQA_RELEASE) ||
      !identical(values$release_doi, VST_DQA_RELEASE_DOI) ||
      !identical(as.character(bundle$meta$product %||% ""), VST_DQA_PRODUCT) ||
      !identical(as.character(bundle$meta$release %||% ""), values$neon_release) ||
      !grepl("^[0-9a-f]{64}$", values$raw_source_digest)) {
    stop(site, " source receipt is not an exact official release receipt",
         call. = FALSE)
  }
  receipt
}

vst_dqa_site_rows <- function(bundle, site) {
  if (!is.list(bundle) || !all(c("trees", "plots", "opportunity_source", "meta", "contract") %in%
                               names(bundle))) {
    stop(site, " is not a complete v2 bundle", call. = FALSE)
  }
  if (!is.data.frame(bundle$trees) || !is.data.frame(bundle$plots) ||
      !is.data.frame(bundle$opportunity_source) ||
      !nrow(bundle$plots)) {
    stop(site, " lacks preserved measurement/opportunity tables", call. = FALSE)
  }
  receipt <- vst_dqa_validate_receipt(bundle, site)
  trees <- bundle$trees
  plots <- bundle$plots
  opportunity_source <- bundle$opportunity_source
  required_tree <- c(
    "growthForm", "live", "stemDiameter", "basalStemDiameter", "dataQF",
    "tagStatus", "changedMeasurementLocation", "source_uid",
    "plotID", "eventID", "individualID", "tempStemID",
    "protocol_stem_key", "protocol_key_group_n", "protocol_key_conflict"
  )
  required_plot <- unique(unlist(lapply(VST_DQA_CHANNELS, function(spec) {
    c(spec$support, spec$reason, spec$invalid, spec$identity)
  }), use.names = FALSE))
  required_plot <- unique(c(
    required_plot, "plotID", "eventID", "opportunity_source_record_count",
    "opportunity_key_conflict", "opportunity_source_uids"
  ))
  missing_tree <- setdiff(required_tree, names(trees))
  missing_plot <- setdiff(required_plot, names(plots))
  missing_source <- setdiff(
    c("source_record_key", "plotID", "eventID", "protocol_key_group_n",
      "protocol_key_conflict"),
    names(opportunity_source)
  )
  if (length(missing_tree) || length(missing_plot) || length(missing_source)) {
    stop(sprintf(
      "%s lacks audit fields: trees=[%s]; plots=[%s]; opportunity_source=[%s]", site,
      paste(missing_tree, collapse = ","), paste(missing_plot, collapse = ","),
      paste(missing_source, collapse = ",")
    ), call. = FALSE)
  }
  if (any(!vst_dqa_nonblank(trees$source_uid)) ||
      anyDuplicated(as.character(trees$source_uid))) {
    stop(site, " has blank or duplicate apparent-individual source uids",
         call. = FALSE)
  }
  if (any(!vst_dqa_nonblank(trees$plotID)) ||
      any(!vst_dqa_nonblank(trees$eventID)) ||
      any(!vst_dqa_nonblank(trees$individualID))) {
    stop(site, " has blank apparent-individual locator fields", call. = FALSE)
  }
  if (any(!vst_dqa_nonblank(opportunity_source$source_record_key)) ||
      anyDuplicated(as.character(opportunity_source$source_record_key))) {
    stop(site, " has blank or duplicate opportunity source uids",
         call. = FALSE)
  }
  if (any(!vst_dqa_nonblank(opportunity_source$plotID)) ||
      any(!vst_dqa_nonblank(opportunity_source$eventID))) {
    stop(site, " has blank opportunity-source locator fields", call. = FALSE)
  }

  protocol_key <- vst_dqa_key(
    trees, c("plotID", "eventID", "individualID", "tempStemID")
  )
  protocol_group_n <- as.integer(table(protocol_key)[protocol_key])
  if (any(!vst_dqa_nonblank(protocol_key)) ||
      !identical(as.character(trees$protocol_stem_key), protocol_key) ||
      !identical(as.integer(trees$protocol_key_group_n), protocol_group_n) ||
      !identical(as.logical(trees$protocol_key_conflict), protocol_group_n > 1L)) {
    stop(site, " has inconsistent apparent-individual locator audit fields",
         call. = FALSE)
  }

  source_event_key <- vst_dqa_key(opportunity_source, c("plotID", "eventID"))
  source_group_n <- as.integer(table(source_event_key)[source_event_key])
  if (!identical(as.integer(opportunity_source$protocol_key_group_n),
                 source_group_n) ||
      !identical(as.logical(opportunity_source$protocol_key_conflict),
                 source_group_n > 1L)) {
    stop(site, " has inconsistent opportunity-source locator audit fields",
         call. = FALSE)
  }
  canonical_event_key <- vst_dqa_key(plots, c("plotID", "eventID"))
  if (anyDuplicated(canonical_event_key)) {
    stop(site, " has duplicate canonical plot-event rows", call. = FALSE)
  }
  if (!setequal(unique(source_event_key), canonical_event_key)) {
    stop(site, " source and canonical opportunity key inventories differ",
         call. = FALSE)
  }
  matched_source_n <- as.integer(table(source_event_key)[canonical_event_key])
  source_uid_sets <- tapply(
    as.character(opportunity_source$source_record_key), source_event_key,
    function(value) paste(sort(unique(value)), collapse = ";")
  )
  matched_source_uids <- unname(source_uid_sets[canonical_event_key])
  if (any(is.na(matched_source_n)) ||
      !identical(as.integer(plots$opportunity_source_record_count),
                 matched_source_n) ||
      !identical(as.logical(plots$opportunity_key_conflict),
                 matched_source_n > 1L) ||
      !identical(as.character(plots$opportunity_source_uids),
                 matched_source_uids)) {
    stop(site, " canonical opportunity conflicts differ from preserved source rows",
         call. = FALSE)
  }

  growth_form <- tolower(trimws(as.character(trees$growthForm)))
  lapply(names(VST_DQA_CHANNELS), function(channel) {
    spec <- VST_DQA_CHANNELS[[channel]]
    status <- as.character(plots[[spec$support]])
    reason <- as.character(plots[[spec$reason]])
    if (any(is.na(status)) || any(!status %in% VST_DQA_STATUS)) {
      stop(site, " ", channel, " contains an unknown support state",
           call. = FALSE)
    }
    if (any(!vst_dqa_nonblank(reason))) {
      stop(site, " ", channel, " contains a blank support reason",
           call. = FALSE)
    }
    invalid_value <- suppressWarnings(as.numeric(plots[[spec$invalid]]))
    if (any(!is.finite(invalid_value)) || any(invalid_value < 0) ||
        any(invalid_value != floor(invalid_value)) ||
        any(invalid_value > .Machine$integer.max)) {
      stop(site, " ", channel, " contains invalid metric-row counts",
           call. = FALSE)
    }
    invalid_by_opportunity <- as.integer(invalid_value)
    identity_value <- suppressWarnings(as.numeric(plots[[spec$identity]]))
    if (any(!is.finite(identity_value)) || any(identity_value < 0) ||
        any(identity_value != floor(identity_value)) ||
        any(identity_value > .Machine$integer.max)) {
      stop(site, " ", channel, " contains invalid identity-conflict counts",
           call. = FALSE)
    }
    identity_by_opportunity <- as.integer(identity_value)

    channel_rows <- trees[
      growth_form %in% tolower(spec$forms), , drop = FALSE
    ]
    live <- channel_rows$live %in% TRUE
    metric <- suppressWarnings(as.numeric(channel_rows[[spec$metric]]))
    valid_metric <- is.finite(metric) & metric > 0 & metric >= spec$minimum
    recomputed_invalid <- as.integer(sum(live & !valid_metric))
    stored_invalid <- as.integer(sum(invalid_by_opportunity))
    if (!identical(stored_invalid, recomputed_invalid)) {
      stop(site, " ", channel,
           " invalid metric-row total differs from preserved live rows",
           call. = FALSE)
    }
    conflict_rows <- channel_rows$protocol_key_conflict %in% TRUE
    recomputed_identity <- if (any(conflict_rows)) {
      length(unique(as.character(channel_rows$protocol_stem_key[conflict_rows])))
    } else {
      0L
    }
    stored_identity <- as.integer(sum(identity_by_opportunity))
    if (!identical(stored_identity, as.integer(recomputed_identity))) {
      stop(site, " ", channel,
           " identity-conflict total differs from preserved source rows",
           call. = FALSE)
    }

    status_counts <- table(factor(status, levels = VST_DQA_STATUS))
    data_qf <- as.character(channel_rows$dataQF)
    tag_status <- as.character(channel_rows$tagStatus)
    tag_nonblank <- vst_dqa_nonblank(tag_status)
    non_ok_tag <- tag_nonblank & tolower(trimws(tag_status)) != "ok"
    changed_location <- as.character(channel_rows$changedMeasurementLocation)
    location_nonblank <- vst_dqa_nonblank(changed_location)
    location_normalized <- gsub(
      "[^a-z]", "", tolower(trimws(changed_location))
    )
    changed <- location_nonblank & location_normalized != "nochange"

    data.frame(
      site = site,
      channel = channel,
      audit_schema = VST_DQA_SCHEMA,
      contract_id = VST_DQA_CONTRACT_ID,
      contract_version = 2L,
      product = VST_DQA_PRODUCT,
      source_release = as.character(receipt$neon_release),
      release_doi = as.character(receipt$release_doi),
      source_receipt_id = as.character(receipt$source_receipt_id),
      raw_source_digest = as.character(receipt$raw_source_digest),
      n_opportunities = as.integer(length(status)),
      n_opportunity_source_records = as.integer(nrow(opportunity_source)),
      n_opportunity_key_conflict_groups = as.integer(sum(plots$opportunity_key_conflict %in% TRUE)),
      n_supported_opportunities = as.integer(sum(status %in% VST_DQA_SUPPORTED)),
      n_explicit_absences = as.integer(sum(status == "sampled_absence")),
      n_held_opportunities = as.integer(sum(status %in% VST_DQA_HELD)),
      n_sampled_with_records = as.integer(status_counts[["sampled_with_records"]]),
      n_sampled_absence = as.integer(status_counts[["sampled_absence"]]),
      n_held_sampling_impractical = as.integer(status_counts[["held_sampling_impractical"]]),
      n_held_dendrometer_only = as.integer(status_counts[["held_dendrometer_only"]]),
      n_held_missing_area = as.integer(status_counts[["held_missing_area"]]),
      n_held_opportunity_unknown = as.integer(status_counts[["held_opportunity_unknown"]]),
      n_held_presence_record_conflict = as.integer(status_counts[["held_presence_record_conflict"]]),
      n_held_metric_invalid = as.integer(status_counts[["held_metric_invalid"]]),
      n_held_identity_conflict = as.integer(status_counts[["held_identity_conflict"]]),
      held_reason_counts = vst_dqa_held_reasons_json(status, reason),
      n_measurement_records = as.integer(nrow(channel_rows)),
      n_live_measurement_records = as.integer(sum(live)),
      n_invalid_metric_records = stored_invalid,
      n_protocol_identity_conflict_keys = stored_identity,
      n_protocol_identity_conflict_records = as.integer(sum(conflict_rows)),
      n_nonblank_dataqf_records = as.integer(sum(vst_dqa_nonblank(data_qf))),
      dataqf_value_counts = vst_dqa_value_counts_json(data_qf),
      dataqf_handling = "preserved_and_counted_not_excluded",
      n_nonblank_tag_status_records = as.integer(sum(tag_nonblank)),
      n_non_ok_tag_status_records = as.integer(sum(non_ok_tag)),
      non_ok_tag_status_value_counts = vst_dqa_value_counts_json(tag_status[non_ok_tag]),
      n_nonblank_changed_measurement_location_records = as.integer(sum(location_nonblank)),
      n_changed_measurement_location_records = as.integer(sum(changed)),
      changed_measurement_location_value_counts = vst_dqa_value_counts_json(changed_location[changed]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
}

vst_build_data_quality_audit <- function(site_dir = "data/sites") {
  sites <- vst_assert_site_inventory(site_dir)
  rows <- list()
  receipts <- list()
  row_index <- 0L
  for (site in sites) {
    path <- file.path(site_dir, paste0(site, ".rds"))
    bundle <- readRDS(path)
    receipt <- vst_dqa_validate_receipt(bundle, site)
    receipts[[site]] <- receipt
    for (row in vst_dqa_site_rows(bundle, site)) {
      row_index <- row_index + 1L
      rows[[row_index]] <- row
    }
  }
  vst_receipts_identical(receipts)
  audit <- do.call(rbind, rows)
  channel_order <- match(audit$channel, names(VST_DQA_CHANNELS))
  site_order <- match(audit$site, sites)
  audit <- audit[order(site_order, channel_order, method = "radix"), , drop = FALSE]
  rownames(audit) <- NULL
  expected_rows <- length(VST_EXPECTED_SITES) * length(VST_DQA_CHANNELS)
  if (nrow(audit) != expected_rows ||
      anyDuplicated(paste(audit$site, audit$channel, sep = "\r"))) {
    stop("data-quality audit must contain one row per registered site and channel",
         call. = FALSE)
  }
  audit
}

vst_write_data_quality_audit <- function(audit, output) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    audit, file = output, sep = ",", row.names = FALSE, col.names = TRUE,
    quote = TRUE, qmethod = "double", na = "", eol = "\n",
    fileEncoding = "UTF-8"
  )
  invisible(output)
}

vst_data_quality_audit_main <- function() {
  site_dir <- Sys.getenv("VST_SITE_DIR", unset = "data/sites")
  output <- Sys.getenv(
    "VST_DATA_QUALITY_AUDIT_OUT",
    unset = "data/source/vegetation-data-quality-audit.csv"
  )
  audit <- vst_build_data_quality_audit(site_dir)
  vst_write_data_quality_audit(audit, output)
  cat(sprintf("wrote %s (%d site-channel rows; no timestamps)\n",
              output, nrow(audit)))
}

vst_dqa_invoked_directly <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  any(basename(sub("^--file=", "", file_argument)) ==
        "write_data_quality_audit.R")
}

if (sys.nframe() == 0L && vst_dqa_invoked_directly()) {
  vst_data_quality_audit_main()
}
