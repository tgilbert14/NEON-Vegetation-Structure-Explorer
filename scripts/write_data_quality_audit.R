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
VST_DQA_SOURCE_NORMALIZATION <- VST_SOURCE_NORMALIZATION
VST_DQA_SUPPORTED <- c("sampled_with_records", "sampled_absence")
VST_DQA_HELD <- c(
  "held_sampling_impractical", "held_dendrometer_only", "held_missing_area",
  "held_opportunity_unknown", "held_presence_record_conflict",
  "held_metric_invalid", "held_identity_conflict",
  "held_opportunity_source_missing"
)
VST_DQA_STATUS <- c(VST_DQA_SUPPORTED, VST_DQA_HELD)
VST_DQA_CHANNELS <- list(
  tree_dbh = list(
    forms = c("single bole tree", "multi-bole tree"),
    support = "tree_support",
    reason = "tree_support_reason",
    supported = "tree_supported",
    records = "tree_records",
    invalid = "tree_invalid_metric_records",
    identity = "tree_identity_conflict_keys",
    area = "area_trees",
    presence = "treePresence",
    metric = "stemDiameter",
    minimum = 10
  ),
  shrub_sapling_basal = list(
    forms = c("single shrub", "small shrub", "sapling"),
    support = "shrub_support",
    reason = "shrub_support_reason",
    supported = "shrub_supported",
    records = "shrub_records",
    invalid = "shrub_invalid_metric_records",
    identity = "shrub_identity_conflict_keys",
    area = "area_shrub",
    presence = "shrubSaplingPresence",
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

vst_dqa_nonnegative_integer <- function(value, label) {
  numeric_value <- suppressWarnings(as.numeric(as.character(value)))
  if (any(!is.finite(numeric_value)) || any(numeric_value < 0) ||
      any(numeric_value != floor(numeric_value)) ||
      any(numeric_value > .Machine$integer.max)) {
    stop(label, " must contain exact nonnegative integers", call. = FALSE)
  }
  as.integer(numeric_value)
}

vst_dqa_date_number <- function(value) {
  if (inherits(value, "Date")) return(as.numeric(value))
  suppressWarnings(as.numeric(as.Date(substr(as.character(value), 1L, 10L))))
}

vst_dqa_presence_state <- function(value) {
  value <- tolower(trimws(as.character(value)))
  result <- rep("unknown", length(value))
  result[value %in% c("n", "no") | grepl("not present|absent", value)] <-
    "absent"
  result[value %in% c("y", "yes") | grepl("^present", value)] <- "present"
  result[is.na(value) | !nzchar(value)] <- "unknown"
  result
}

vst_dqa_shrub_presence_state <- function(trees_present, shrubs_present) {
  tree_state <- vst_dqa_presence_state(trees_present)
  shrub_state <- vst_dqa_presence_state(shrubs_present)
  ifelse(
    shrub_state == "present", "present",
    ifelse(
      shrub_state == "absent" & tree_state == "absent", "absent", "unknown"
    )
  )
}

vst_dqa_expected_species <- function(rank, scientific_name) {
  rank <- tolower(trimws(as.character(rank)))
  scientific_name <- as.character(scientific_name)
  accepted <- !is.na(rank) & rank %in% c(
    "species", "subspecies", "variety", "form"
  )
  safe_name <- ifelse(is.na(scientific_name), "", scientific_name)
  accepted & vst_dqa_nonblank(scientific_name) &
    !grepl("\\bsp\\.?$", safe_name, ignore.case = TRUE) &
    !grepl("/", safe_name, fixed = TRUE)
}

vst_dqa_expected_taxon_label <- function(scientific_name, taxon_id) {
  scientific_name <- as.character(scientific_name)
  taxon_id <- as.character(taxon_id)
  ifelse(
    vst_dqa_nonblank(scientific_name), scientific_name,
    ifelse(
      vst_dqa_nonblank(taxon_id),
      paste0("Unresolved taxon (", taxon_id, ")"),
      "Unresolved taxon"
    )
  )
}

vst_dqa_row_derivation_problems <- function(trees) {
  expected_date <- suppressWarnings(as.Date(substr(as.character(trees$date), 1L, 10L)))
  expected_year <- suppressWarnings(as.integer(format(expected_date, "%Y")))
  expected_live <- grepl(
    "^live", trimws(as.character(trees$plantStatus)), ignore.case = TRUE
  )
  expected_permanent <- grepl("^NEON", as.character(trees$individualID))
  expected_species <- vst_dqa_expected_species(
    trees$taxonRank, trees$scientificName
  )
  expected_label <- vst_dqa_expected_taxon_label(
    trees$scientificName, trees$taxonID
  )
  expected_resolution <- ifelse(
    expected_species & vst_dqa_nonblank(trees$scientificName),
    "species-level", "coarse-or-unresolved"
  )
  mapping_matched <- vst_dqa_nonblank(trees$mapping_source_uid)
  checks <- c(
    date_type = inherits(trees$date, "Date"),
    year = is.integer(trees$year) && identical(trees$year, expected_year),
    live = is.logical(trees$live) && identical(trees$live, expected_live),
    permanent = is.logical(trees$permanent) &&
      identical(trees$permanent, expected_permanent),
    plant_key = is.character(trees$plant_key) && identical(
      trees$plant_key,
      paste(trees$plotID, trees$individualID, sep = "\r")
    ),
    event_key = is.character(trees$event_key) && identical(
      trees$event_key,
      paste(trees$plotID, trees$eventID, sep = "\r")
    ),
    mapping_source_uid_type = is.character(trees$mapping_source_uid),
    mappingMatched = is.logical(trees$mappingMatched) && identical(
      trees$mappingMatched, mapping_matched
    ),
    is_species = is.logical(trees$is_species) &&
      identical(trees$is_species, expected_species),
    taxon_label = is.character(trees$taxon_label) &&
      identical(trees$taxon_label, expected_label),
    taxon_resolution = is.character(trees$taxon_resolution) && identical(
      trees$taxon_resolution, expected_resolution
    )
  )
  problems <- names(checks)[!checks]
  mapping_uid <- as.character(trees$mapping_source_uid[mapping_matched])
  if (length(mapping_uid)) {
    mapping_plant <- paste(
      trees$plotID[mapping_matched], trees$individualID[mapping_matched],
      sep = "\r"
    )
    if (any(tapply(mapping_plant, mapping_uid, function(value) {
      length(unique(value))
    }) != 1L) || any(tapply(mapping_uid, mapping_plant, function(value) {
      length(unique(value))
    }) != 1L)) {
      problems <- c(problems, "mapping_source_uid traceability")
    }
  }
  unique(problems)
}

vst_dqa_event_counts <- function(trees, plots, keep) {
  result <- integer(nrow(plots))
  keep[is.na(keep)] <- FALSE
  if (!nrow(trees) || !any(keep)) return(result)
  counts <- table(vst_dqa_key(
    trees[keep, , drop = FALSE], c("plotID", "eventID")
  ))
  matched <- match(vst_dqa_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}

vst_dqa_identity_counts <- function(trees, plots, keep) {
  result <- integer(nrow(plots))
  keep[is.na(keep)] <- FALSE
  keep <- keep & trees$protocol_key_conflict %in% TRUE
  if (!nrow(trees) || !any(keep)) return(result)
  rows <- unique(data.frame(
    event_key = vst_dqa_key(
      trees[keep, , drop = FALSE], c("plotID", "eventID")
    ),
    protocol_stem_key = as.character(trees$protocol_stem_key[keep]),
    stringsAsFactors = FALSE
  ))
  counts <- table(rows$event_key)
  matched <- match(vst_dqa_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}

vst_dqa_support_decision <- function(area, sampling_impractical, data_collected,
                                     presence, records, invalid_metric,
                                     identity_conflicts, opportunity_conflict,
                                     opportunity_source_missing) {
  sampling <- gsub(
    "[^a-z]", "", tolower(trimws(as.character(sampling_impractical)))
  )
  collected <- gsub(
    "[^a-z]", "", tolower(trimws(as.character(data_collected)))
  )
  status <- reason <- rep(NA_character_, length(area))
  for (i in seq_along(area)) {
    if (isTRUE(opportunity_source_missing[[i]])) {
      status[[i]] <- "held_opportunity_source_missing"
      reason[[i]] <- paste(
        "no published vst_perplotperyear row for this plotID + eventID;",
        "sampling effort, absence, and sampled area are unknown"
      )
    } else if (isTRUE(opportunity_conflict[[i]])) {
      status[[i]] <- "held_identity_conflict"
      reason[[i]] <-
        "multiple published vst_perplotperyear rows share this plot-event key"
    } else if (is.na(sampling[[i]]) || !nzchar(sampling[[i]])) {
      status[[i]] <- "held_opportunity_unknown"
      reason[[i]] <- "samplingImpractical is missing"
    } else if (!identical(sampling[[i]], "ok")) {
      status[[i]] <- "held_sampling_impractical"
      reason[[i]] <- paste0(
        "samplingImpractical=", as.character(sampling_impractical[[i]])
      )
    } else if (identical(collected[[i]], "dendrometeronly")) {
      status[[i]] <- "held_dendrometer_only"
      reason[[i]] <- "dataCollected=dendrometerOnly cannot scale a plot response"
    } else if (!identical(collected[[i]], "allgrowthforms")) {
      status[[i]] <- "held_opportunity_unknown"
      raw_collected <- as.character(data_collected[[i]])
      reason[[i]] <- if (is.na(raw_collected) || !nzchar(raw_collected)) {
        "dataCollected is missing"
      } else {
        paste0("dataCollected=", raw_collected)
      }
    } else if (!is.finite(area[[i]]) || area[[i]] <= 0) {
      status[[i]] <- "held_missing_area"
      reason[[i]] <- "event-specific total sampled area is missing or non-positive"
    } else if (records[[i]] > 0L && identical(presence[[i]], "absent")) {
      status[[i]] <- "held_presence_record_conflict"
      reason[[i]] <- "presence says absent but measurement records exist"
    } else if (is.finite(identity_conflicts[[i]]) &&
               identity_conflicts[[i]] > 0L) {
      status[[i]] <- "held_identity_conflict"
      reason[[i]] <- sprintf(
        paste0(
          "%d channel stem locator%s violate%s plotID + eventID + ",
          "individualID + tempStemID uniqueness"
        ),
        identity_conflicts[[i]],
        if (identity_conflicts[[i]] == 1L) "" else "s",
        if (identity_conflicts[[i]] == 1L) "s" else ""
      )
    } else if (invalid_metric[[i]] > 0L) {
      status[[i]] <- "held_metric_invalid"
      reason[[i]] <- sprintf(
        "%d live channel record%s lack%s a finite positive threshold-compatible diameter",
        invalid_metric[[i]], if (invalid_metric[[i]] == 1L) "" else "s",
        if (invalid_metric[[i]] == 1L) "s" else ""
      )
    } else if (records[[i]] > 0L) {
      status[[i]] <- "sampled_with_records"
      reason[[i]] <- "supported all-growth-forms event with measurement records"
    } else if (identical(presence[[i]], "absent")) {
      status[[i]] <- "sampled_absence"
      reason[[i]] <- "explicit plot-scale absence in a supported event"
    } else if (identical(presence[[i]], "present")) {
      status[[i]] <- "held_presence_record_conflict"
      reason[[i]] <- "presence says sampled/present but no measurement records exist"
    } else {
      status[[i]] <- "held_opportunity_unknown"
      reason[[i]] <- "no records and presence does not establish sampled absence"
    }
  }
  list(status = status, reason = reason)
}

VST_DQA_CONTEXT_DERIVED_FIELDS <- c(
  "plotID", "eventID", "opportunity_source_uid",
  "opportunity_source_record_count", "opportunity_key_conflict",
  "opportunity_source_uids", "opportunity_source_missing",
  "measurement_record_count_all", "measurement_date_min",
  "measurement_date_max", "measurement_date_distinct_n",
  "treePresence", "shrubSaplingPresence", "tree_records", "shrub_records",
  "small_tree_records", "tree_invalid_metric_records",
  "shrub_invalid_metric_records", "tree_identity_conflict_keys",
  "shrub_identity_conflict_keys", "tree_support", "tree_support_reason",
  "shrub_support", "shrub_support_reason", "tree_supported",
  "shrub_supported", "event_key"
)

vst_dqa_byte_order <- function(...) {
  values <- lapply(list(...), vst_utf8_byte_key)
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
      !identical(as.character(contract$mapping_source_record_key %||% ""),
                 "mapping_source_uid") ||
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
    "source_receipt_id", "raw_source_digest", "source_normalization"
  )
  values <- stats::setNames(lapply(required, function(field) {
    vst_dqa_scalar(receipt[[field]] %||% NULL,
                   paste(site, "source receipt", field))
  }), required)
  if (!identical(values$provenance_class, "official-release") ||
      !identical(values$product, VST_DQA_PRODUCT) ||
      !identical(values$neon_release, VST_DQA_RELEASE) ||
      !identical(values$release_doi, VST_DQA_RELEASE_DOI) ||
      !identical(values$source_normalization,
                 VST_DQA_SOURCE_NORMALIZATION) ||
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
    "date", "year", "growthForm", "plantStatus", "live", "permanent",
    "stemDiameter", "basalStemDiameter", "dataQF",
    "tagStatus", "changedMeasurementLocation", "source_uid",
    "plotID", "eventID", "individualID", "tempStemID",
    "protocol_stem_key", "protocol_key_group_n", "protocol_key_conflict",
    "opportunity_source_missing", "plant_key", "event_key",
    "mapping_source_uid", "mappingMatched", "scientificName", "taxonID",
    "taxonRank", "is_species", "taxon_label", "taxon_resolution"
  )
  required_plot <- unique(unlist(lapply(VST_DQA_CHANNELS, function(spec) {
    c(
      spec$support, spec$reason, spec$supported, spec$records, spec$invalid,
      spec$identity, spec$area, spec$presence
    )
  }), use.names = FALSE))
  required_plot <- unique(c(
    required_plot, "plotID", "eventID", "opportunity_source_record_count",
    "opportunity_key_conflict", "opportunity_source_uid",
    "opportunity_source_uids", "opportunity_source_missing",
    "measurement_record_count_all", "measurement_date_min",
    "measurement_date_max", "measurement_date_distinct_n",
    "samplingImpractical", "dataCollected", "treesPresent", "shrubsPresent"
  ))
  missing_tree <- setdiff(required_tree, names(trees))
  missing_plot <- setdiff(required_plot, names(plots))
  missing_source <- setdiff(
    c("uid", "source_record_key", "plotID", "eventID", "protocol_key_group_n",
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
  row_derivation_problems <- vst_dqa_row_derivation_problems(trees)
  if (length(row_derivation_problems)) {
    stop(
      site, " row-derived invariants differ from preserved source fields: ",
      paste(row_derivation_problems, collapse = ", "), call. = FALSE
    )
  }
  if (any(!vst_dqa_nonblank(opportunity_source$uid)) ||
      anyDuplicated(as.character(opportunity_source$uid)) ||
      any(!vst_dqa_nonblank(opportunity_source$source_record_key)) ||
      anyDuplicated(as.character(opportunity_source$source_record_key)) ||
      !identical(as.character(opportunity_source$source_record_key),
                 as.character(opportunity_source$uid))) {
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
      !identical(vst_dqa_nonnegative_integer(
        trees$protocol_key_group_n,
        paste(site, "apparent-individual locator group counts")
      ), protocol_group_n) ||
      !identical(as.logical(trees$protocol_key_conflict), protocol_group_n > 1L)) {
    stop(site, " has inconsistent apparent-individual locator audit fields",
         call. = FALSE)
  }

  source_event_key <- vst_dqa_key(opportunity_source, c("plotID", "eventID"))
  source_group_n <- as.integer(table(source_event_key)[source_event_key])
  if (!identical(vst_dqa_nonnegative_integer(
        opportunity_source$protocol_key_group_n,
        paste(site, "opportunity-source locator group counts")
      ), source_group_n) ||
      !identical(as.logical(opportunity_source$protocol_key_conflict),
                 source_group_n > 1L)) {
    stop(site, " has inconsistent opportunity-source locator audit fields",
         call. = FALSE)
  }
  canonical_event_key <- vst_dqa_key(plots, c("plotID", "eventID"))
  if (anyDuplicated(canonical_event_key)) {
    stop(site, " has duplicate canonical plot-event rows", call. = FALSE)
  }
  source_missing <- as.logical(plots$opportunity_source_missing)
  if (any(is.na(source_missing))) {
    stop(site, " has unknown opportunity-source-missing flags", call. = FALSE)
  }
  source_backed_key <- canonical_event_key[!source_missing]
  if (!setequal(unique(source_event_key), source_backed_key)) {
    stop(site, " published source and source-backed context key inventories differ",
         call. = FALSE)
  }
  measurement_event_key <- vst_dqa_key(trees, c("plotID", "eventID"))
  expected_missing_key <- setdiff(
    unique(measurement_event_key), unique(source_event_key)
  )
  if (!setequal(canonical_event_key[source_missing], expected_missing_key) ||
      length(setdiff(unique(measurement_event_key), canonical_event_key))) {
    stop(site, " measurement-only context keys do not match source-key gaps",
         call. = FALSE)
  }
  missing_measurement_n <- as.integer(table(measurement_event_key)[
    canonical_event_key[source_missing]
  ])
  if (any(is.na(missing_measurement_n)) || any(missing_measurement_n < 1L)) {
    stop(site, " has a source-missing context without preserved measurements",
         call. = FALSE)
  }

  matched_source_n <- integer(nrow(plots))
  matched_source_n[!source_missing] <- as.integer(
    table(source_event_key)[source_backed_key]
  )
  source_uid_sets <- tapply(
    as.character(opportunity_source$source_record_key), source_event_key,
    function(value) {
      value <- unique(value)
      paste(value[vst_byte_order(value)], collapse = ";")
    }
  )
  matched_source_uids <- rep(NA_character_, nrow(plots))
  matched_source_uids[!source_missing] <- unname(
    source_uid_sets[source_backed_key]
  )
  selected_source_uid <- as.character(plots$opportunity_source_uid)
  selected_is_known <- mapply(function(uid, uids, missing) {
    if (missing) return(is.na(uid) || !nzchar(trimws(uid)))
    if (is.na(uid) || !nzchar(trimws(uid)) || is.na(uids)) return(FALSE)
    uid %in% strsplit(uids, ";", fixed = TRUE)[[1L]]
  }, selected_source_uid, matched_source_uids, source_missing,
  USE.NAMES = FALSE)
  stored_source_n <- vst_dqa_nonnegative_integer(
    plots$opportunity_source_record_count,
    paste(site, "opportunity source record counts")
  )
  if (!all(selected_is_known) ||
      !identical(stored_source_n, matched_source_n) ||
      !identical(as.logical(plots$opportunity_key_conflict),
                 matched_source_n > 1L) ||
      !identical(as.character(plots$opportunity_source_uids),
                 matched_source_uids)) {
    stop(site, " plot-event context source audit differs from preserved source rows",
         call. = FALSE)
  }
  selected_source_parity <- vst_selected_source_parity(
    plots, opportunity_source
  )
  if (!selected_source_parity$ok) {
    stop(
      site, " canonical context differs from selected opportunity source row: ",
      paste(selected_source_parity$fields, collapse = ","),
      call. = FALSE
    )
  }
  source_derived_fields <- setdiff(
    names(plots), VST_DQA_CONTEXT_DERIVED_FIELDS
  )
  has_invented_value <- vapply(source_derived_fields, function(field) {
    value <- plots[[field]][source_missing]
    if (is.character(value) || is.factor(value)) {
      any(vst_dqa_nonblank(value))
    } else {
      any(!is.na(value))
    }
  }, logical(1))
  if (any(has_invented_value) ||
      any(plots$opportunity_key_conflict[source_missing] %in% TRUE)) {
    stop(site, " source-missing contexts invent published opportunity fields",
         call. = FALSE)
  }

  missing_context_key <- canonical_event_key[source_missing]
  expected_tree_missing <- measurement_event_key %in% missing_context_key
  if (!identical(as.logical(trees$opportunity_source_missing),
                 expected_tree_missing)) {
    stop(site, " measurement-row source-missing flags disagree with contexts",
         call. = FALSE)
  }
  measurement_count <- table(measurement_event_key)
  matched_measurement_count <- as.integer(measurement_count[canonical_event_key])
  matched_measurement_count[is.na(matched_measurement_count)] <- 0L
  stored_measurement_count <- vst_dqa_nonnegative_integer(
    plots$measurement_record_count_all,
    paste(site, "context measurement record counts")
  )
  stored_measurement_date_n <- vst_dqa_nonnegative_integer(
    plots$measurement_date_distinct_n,
    paste(site, "context distinct measurement-date counts")
  )
  if (!inherits(plots$measurement_date_min, "Date") ||
      !inherits(plots$measurement_date_max, "Date")) {
    stop(site, " context measurement date bounds are not Date vectors",
         call. = FALSE)
  }
  expected_date_min <- expected_date_max <- rep(NA_real_, nrow(plots))
  expected_date_n <- integer(nrow(plots))
  if (nrow(trees)) {
    measurement_date_number <- vst_dqa_date_number(trees$date)
    date_min <- tapply(measurement_date_number, measurement_event_key, function(value) {
      value <- value[is.finite(value)]
      if (length(value)) min(value) else NA_real_
    })
    date_max <- tapply(measurement_date_number, measurement_event_key, function(value) {
      value <- value[is.finite(value)]
      if (length(value)) max(value) else NA_real_
    })
    date_n <- tapply(measurement_date_number, measurement_event_key, function(value) {
      as.integer(length(unique(value[is.finite(value)])))
    })
    expected_date_min <- unname(date_min[canonical_event_key])
    expected_date_max <- unname(date_max[canonical_event_key])
    expected_date_n <- as.integer(unname(date_n[canonical_event_key]))
    expected_date_n[is.na(expected_date_n)] <- 0L
  }
  if (!identical(stored_measurement_count, matched_measurement_count) ||
      !identical(vst_dqa_date_number(plots$measurement_date_min),
                 expected_date_min) ||
      !identical(vst_dqa_date_number(plots$measurement_date_max),
                 expected_date_max) ||
      !identical(stored_measurement_date_n, expected_date_n)) {
    stop(site, " context measurement count/date summaries differ from preserved rows",
         call. = FALSE)
  }
  n_missing_measurement_records <- as.integer(
    sum(measurement_event_key %in% missing_context_key)
  )

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
    channel_keep <- growth_form %in% tolower(spec$forms)
    live_all <- grepl(
      "^live", trimws(as.character(trees$plantStatus)), ignore.case = TRUE
    )
    metric_all <- suppressWarnings(as.numeric(trees[[spec$metric]]))
    valid_metric_all <- is.finite(metric_all) & metric_all > 0 &
      metric_all >= spec$minimum
    expected_records <- vst_dqa_event_counts(trees, plots, channel_keep)
    expected_invalid <- vst_dqa_event_counts(
      trees, plots, channel_keep & live_all & !valid_metric_all
    )
    expected_identity <- vst_dqa_identity_counts(
      trees, plots, channel_keep
    )
    records_by_opportunity <- vst_dqa_nonnegative_integer(
      plots[[spec$records]], paste(site, channel, "record counts")
    )
    invalid_by_opportunity <- vst_dqa_nonnegative_integer(
      plots[[spec$invalid]], paste(site, channel, "invalid metric-row counts")
    )
    identity_by_opportunity <- vst_dqa_nonnegative_integer(
      plots[[spec$identity]], paste(site, channel, "identity-conflict counts")
    )
    expected_presence <- if (identical(channel, "tree_dbh")) {
      vst_dqa_presence_state(plots$treesPresent)
    } else {
      vst_dqa_shrub_presence_state(plots$treesPresent, plots$shrubsPresent)
    }
    expected_decision <- vst_dqa_support_decision(
      suppressWarnings(as.numeric(plots[[spec$area]])),
      plots$samplingImpractical, plots$dataCollected, expected_presence,
      expected_records, expected_invalid, expected_identity,
      as.logical(plots$opportunity_key_conflict), source_missing
    )
    support_mismatch <-
      !identical(as.character(plots[[spec$presence]]), expected_presence) ||
      !identical(records_by_opportunity, expected_records) ||
      !identical(invalid_by_opportunity, expected_invalid) ||
      !identical(identity_by_opportunity, expected_identity) ||
      !identical(status, expected_decision$status) ||
      !identical(reason, expected_decision$reason) ||
      !identical(
        as.logical(plots[[spec$supported]]),
        expected_decision$status %in% VST_DQA_SUPPORTED
      )
    if (support_mismatch) {
      stop(
        site, " ", channel,
        " support states, reasons, counts, or presence differ from preserved source rows",
        call. = FALSE
      )
    }

    channel_rows <- trees[channel_keep, , drop = FALSE]
    channel_event_key <- vst_dqa_key(channel_rows, c("plotID", "eventID"))
    n_channel_missing_measurements <- as.integer(
      sum(channel_event_key %in% missing_context_key)
    )
    n_channel_missing_contexts <- as.integer(length(intersect(
      unique(channel_event_key), missing_context_key
    )))
    live <- live_all[channel_keep]
    recomputed_invalid <- as.integer(sum(expected_invalid))
    stored_invalid <- as.integer(sum(invalid_by_opportunity))
    if (!identical(stored_invalid, recomputed_invalid)) {
      stop(site, " ", channel,
           " invalid metric-row total differs from preserved live rows",
           call. = FALSE)
    }
    conflict_rows <- channel_rows$protocol_key_conflict %in% TRUE
    recomputed_identity <- as.integer(sum(expected_identity))
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
      source_normalization = as.character(receipt$source_normalization),
      n_plot_event_contexts = as.integer(length(status)),
      n_published_opportunity_keys = as.integer(length(unique(source_event_key))),
      n_opportunity_source_records = as.integer(nrow(opportunity_source)),
      n_measurement_only_contexts = as.integer(sum(source_missing)),
      n_measurement_records_without_opportunity_source = n_missing_measurement_records,
      n_channel_measurement_only_contexts_with_records = n_channel_missing_contexts,
      n_channel_measurement_records_without_opportunity_source = n_channel_missing_measurements,
      n_opportunity_key_conflict_groups = as.integer(sum(plots$opportunity_key_conflict %in% TRUE)),
      n_supported_contexts = as.integer(sum(status %in% VST_DQA_SUPPORTED)),
      n_explicit_absences = as.integer(sum(status == "sampled_absence")),
      n_held_contexts = as.integer(sum(status %in% VST_DQA_HELD)),
      n_sampled_with_records = as.integer(status_counts[["sampled_with_records"]]),
      n_sampled_absence = as.integer(status_counts[["sampled_absence"]]),
      n_held_sampling_impractical = as.integer(status_counts[["held_sampling_impractical"]]),
      n_held_dendrometer_only = as.integer(status_counts[["held_dendrometer_only"]]),
      n_held_missing_area = as.integer(status_counts[["held_missing_area"]]),
      n_held_opportunity_unknown = as.integer(status_counts[["held_opportunity_unknown"]]),
      n_held_presence_record_conflict = as.integer(status_counts[["held_presence_record_conflict"]]),
      n_held_metric_invalid = as.integer(status_counts[["held_metric_invalid"]]),
      n_held_identity_conflict = as.integer(status_counts[["held_identity_conflict"]]),
      n_held_opportunity_source_missing = as.integer(
        status_counts[["held_opportunity_source_missing"]]
      ),
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
  site_rows <- audit[match(sites, audit$site), , drop = FALSE]
  if (sum(site_rows$n_measurement_only_contexts) != 49L ||
      sum(site_rows$n_measurement_records_without_opportunity_source) != 4365L ||
      sum(site_rows$n_measurement_only_contexts > 0L) != 11L) {
    stop(
      "RELEASE-2026 measurement-only inventory changed from 49 contexts / 4365 records / 11 sites",
      call. = FALSE
    )
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
