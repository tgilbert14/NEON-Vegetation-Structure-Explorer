#!/usr/bin/env Rscript

# Non-network release gate for the complete Vegetation Structure runtime family.
# Legacy bytes remain diagnosable, but only the event-keyed v3 contract can be a
# release candidate. The gate never turns a missing opportunity into an absence.

suppressPackageStartupMessages({
  library(dplyr)
  library(digest)
})
source("scripts/vegetation_inventory.R")
source("R/veg_helpers.R")

VST_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v3"
VST_SUPPORTED <- c("sampled_with_records", "sampled_absence")
VST_HELD <- c(
  "held_sampling_impractical", "held_dendrometer_only", "held_missing_area",
  "held_opportunity_unknown", "held_presence_record_conflict",
  "held_metric_invalid", "held_identity_conflict",
  "held_opportunity_source_missing", "supported_legacy_schema"
)
VST_STATUS <- c(VST_SUPPORTED, VST_HELD)
VST_LEGACY_END <- as.Date("2020-03-10")
VST_LEGACY_REASON <-
  "legacy schema before 2020-03-10: sampling opportunity assumed (NEON changelog 16306)"
VST_CONTEXT_DERIVED_FIELDS <- c(
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
  "shrub_supported", "event_key", "partial_event_rule_id",
  "tree_channel_complete", "shrub_channel_complete", "disturbance_classes",
  "disturbance_curation_id", "n_status_lost_burned", "n_status_removed"
)

problems <- character(0)
note <- function(message) problems <<- c(problems, message)
nonblank <- function(value) !is.na(value) & nzchar(trimws(as.character(value)))
exact_nonnegative_integer <- function(value, label) {
  numeric_value <- suppressWarnings(as.numeric(as.character(value)))
  if (any(!is.finite(numeric_value)) || any(numeric_value < 0) ||
      any(numeric_value != floor(numeric_value)) ||
      any(numeric_value > .Machine$integer.max)) {
    note(sprintf("%s must contain exact nonnegative integers", label))
    return(rep(NA_integer_, length(value)))
  }
  unname(as.integer(numeric_value))
}
date_number <- function(value) {
  if (inherits(value, "Date")) return(unname(as.numeric(value)))
  unname(suppressWarnings(
    as.numeric(as.Date(substr(as.character(value), 1L, 10L)))
  ))
}
expected_species <- function(rank, scientific_name) {
  rank <- tolower(trimws(as.character(rank)))
  scientific_name <- as.character(scientific_name)
  accepted <- !is.na(rank) & rank %in% c(
    "species", "subspecies", "variety", "form"
  )
  safe_name <- ifelse(is.na(scientific_name), "", scientific_name)
  accepted & nonblank(scientific_name) &
    !grepl("\\bsp\\.?$", safe_name, ignore.case = TRUE) &
    !grepl("/", safe_name, fixed = TRUE)
}
expected_taxon_label <- function(scientific_name, taxon_id) {
  scientific_name <- as.character(scientific_name)
  taxon_id <- as.character(taxon_id)
  ifelse(
    nonblank(scientific_name), scientific_name,
    ifelse(
      nonblank(taxon_id), paste0("Unresolved taxon (", taxon_id, ")"),
      "Unresolved taxon"
    )
  )
}
row_derivation_problems <- function(trees) {
  required <- c(
    "date", "year", "plotID", "eventID", "individualID", "plantStatus",
    "live", "permanent", "plant_key", "event_key", "mapping_source_uid",
    "mappingMatched", "scientificName", "taxonID", "taxonRank",
    "is_species", "taxon_label", "taxon_resolution"
  )
  missing <- setdiff(required, names(trees))
  if (length(missing)) {
    return(paste0("missing fields: ", paste(missing, collapse = ",")))
  }
  expected_date <- suppressWarnings(as.Date(substr(as.character(trees$date), 1L, 10L)))
  expected_year <- suppressWarnings(as.integer(format(expected_date, "%Y")))
  expected_live <- grepl(
    "^live", trimws(as.character(trees$plantStatus)), ignore.case = TRUE
  )
  expected_permanent <- grepl("^NEON", as.character(trees$individualID))
  species <- expected_species(trees$taxonRank, trees$scientificName)
  label <- expected_taxon_label(trees$scientificName, trees$taxonID)
  resolution <- ifelse(
    species & nonblank(trees$scientificName),
    "species-level", "coarse-or-unresolved"
  )
  mapping_matched <- nonblank(trees$mapping_source_uid)
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
      identical(trees$is_species, species),
    taxon_label = is.character(trees$taxon_label) &&
      identical(trees$taxon_label, label),
    taxon_resolution = is.character(trees$taxon_resolution) && identical(
      trees$taxon_resolution, resolution
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
identity_conflict_counts <- function(trees, plots, keep) {
  result <- integer(nrow(plots))
  keep[is.na(keep)] <- FALSE
  if (!nrow(trees) || !any(keep) ||
      !all(c("protocol_key_conflict", "protocol_stem_key") %in% names(trees))) {
    return(result)
  }
  keep <- keep & trees$protocol_key_conflict %in% TRUE
  if (!any(keep)) return(result)
  rows <- unique(data.frame(
    event_key = release_key(trees[keep, , drop = FALSE], c("plotID", "eventID")),
    protocol_stem_key = as.character(trees$protocol_stem_key[keep]),
    stringsAsFactors = FALSE
  ))
  counts <- table(rows$event_key)
  matched <- match(release_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}
presence_state <- function(value) {
  value <- tolower(trimws(as.character(value)))
  result <- rep("unknown", length(value))
  result[value %in% c("n", "no") | grepl("not present|absent", value)] <-
    "absent"
  result[value %in% c("y", "yes") | grepl("^present", value)] <- "present"
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
expected_support_decision <- function(area, sampling_impractical, data_collected,
                                      presence, records, invalid_metric,
                                      identity_conflicts,
                                      opportunity_conflict,
                                      opportunity_source_missing,
                                      event_date, curation_note) {
  sampling <- gsub("[^a-z]", "", tolower(trimws(as.character(sampling_impractical))))
  collected <- gsub("[^a-z]", "", tolower(trimws(as.character(data_collected))))
  sampling_blank <- is.na(sampling) | sampling == ""
  collected_blank <- is.na(collected) | collected == ""
  legacy_event <- date_number(event_date) <= as.numeric(VST_LEGACY_END) &
    (sampling_blank | sampling %in% "ok") &
    (collected_blank | collected %in% "allgrowthforms") &
    (sampling_blank | collected_blank) &
    is.na(curation_note)  # published dataCollected = partial is never legacy
  legacy_event[is.na(legacy_event)] <- FALSE
  status <- reason <- rep(NA_character_, length(area))
  for (i in seq_along(area)) {
    if (!isTRUE(opportunity_source_missing[[i]]) &&
        !isTRUE(opportunity_conflict[[i]]) &&
        legacy_event[[i]] && records[[i]] > 0L) {
      # Legacy schema (contract v3): area, presence, identity and metric
      # checks still hold the event; otherwise it is history-only.
      if (!is.finite(area[[i]]) || area[[i]] <= 0) {
        status[[i]] <- "held_missing_area"
        reason[[i]] <- "event-specific total sampled area is missing or non-positive"
      } else if (identical(presence[[i]], "absent")) {
        status[[i]] <- "held_presence_record_conflict"
        reason[[i]] <- "presence says absent but measurement records exist"
      } else if (is.finite(identity_conflicts[[i]]) && identity_conflicts[[i]] > 0L) {
        status[[i]] <- "held_identity_conflict"
        reason[[i]] <- sprintf(
          "%d channel stem locator%s violate%s plotID + eventID + individualID + tempStemID uniqueness",
          identity_conflicts[[i]], if (identity_conflicts[[i]] == 1L) "" else "s",
          if (identity_conflicts[[i]] == 1L) "s" else ""
        )
      } else if (invalid_metric[[i]] > 0L) {
        status[[i]] <- "held_metric_invalid"
        reason[[i]] <- sprintf(
          "%d live channel record%s lack%s a finite positive threshold-compatible diameter",
          invalid_metric[[i]], if (invalid_metric[[i]] == 1L) "" else "s",
          if (invalid_metric[[i]] == 1L) "s" else ""
        )
      } else {
        status[[i]] <- "supported_legacy_schema"
        reason[[i]] <- VST_LEGACY_REASON
      }
    } else if (isTRUE(opportunity_source_missing[[i]])) {
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
    } else if (is.finite(identity_conflicts[[i]]) && identity_conflicts[[i]] > 0L) {
      status[[i]] <- "held_identity_conflict"
      reason[[i]] <- sprintf(
        "%d channel stem locator%s violate%s plotID + eventID + individualID + tempStemID uniqueness",
        identity_conflicts[[i]], if (identity_conflicts[[i]] == 1L) "" else "s",
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
    if (!is.na(curation_note[[i]])) {
      reason[[i]] <- paste0(reason[[i]], "; ", curation_note[[i]])
    }
  }
  data.frame(status = status, reason = reason, stringsAsFactors = FALSE)
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

# --- Contract v3 independent recomputes (cedar's 2026-10-26 rulings) --------
# These restate each rule from its prose, in a different shape from the
# builder, so a builder bug cannot pass by agreeing with itself.
tree_forms <- c("single bole tree", "multi-bole tree")
split_visit_forms <- c("small tree", "sapling", "single shrub", "small shrub")

# C1: per plot, a lettered individual with a tree-form row whose parent tag is
# also an individual in that plot counts with the parent.
expected_plant_id <- function(trees) {
  id <- as.character(trees$individualID)
  out <- id
  form <- tolower(trimws(as.character(trees$growthForm)))
  for (rows in split(seq_len(nrow(trees)), as.character(trees$plotID))) {
    ids <- unique(id[rows])
    with_tree_form <- unique(id[rows][form[rows] %in% tree_forms])
    for (candidate in with_tree_form[grepl("[A-Za-z]$", with_tree_form)]) {
      parent <- substr(candidate, 1L, nchar(candidate) - 1L)
      if (parent %in% ids) out[rows][id[rows] == candidate] <- parent
    }
  }
  out
}

# S4 (a): per individual and event, rows dated before the latest date are
# superseded, unless a small-tree/sapling/shrub stem absent from the latest
# date sits at most 3 days earlier.
expected_superseded <- function(trees) {
  out <- rep(FALSE, nrow(trees))
  day <- date_number(trees$date)
  form <- tolower(trimws(as.character(trees$growthForm)))
  stem <- as.character(trees$tempStemID)
  groups <- split(seq_len(nrow(trees)),
                  release_key(trees, c("plotID", "eventID", "individualID")))
  for (rows in groups[lengths(groups) > 1L]) {
    d <- day[rows]
    if (!any(is.finite(d))) next
    last <- max(d[is.finite(d)])
    last_stems <- stem[rows][is.finite(d) & d == last]
    for (j in which(is.finite(d) & d < last)) {
      split_visit <- form[rows[j]] %in% split_visit_forms &&
        !(stem[rows[j]] %in% last_stems) && last - d[j] <= 3
      out[rows[j]] <- !split_visit
    }
  }
  out
}

# S4 (b): tree-form copies of one individual, event and date with identical
# growthForm, plantStatus, stemDiameter and height point at the copy first in
# source_uid byte order.
expected_duplicate_of <- function(trees) {
  out <- rep(NA_character_, nrow(trees))
  form <- tolower(trimws(as.character(trees$growthForm)))
  rows <- which(form %in% tree_forms)
  if (!length(rows)) return(out)
  key <- release_key(trees[rows, , drop = FALSE], c(
    "plotID", "eventID", "individualID", "date", "growthForm", "plantStatus",
    "stemDiameter", "height"
  ))
  for (group in split(rows, key)) {
    if (length(group) < 2L) next
    uid <- as.character(trees$source_uid[group])
    keep <- uid[order(vst_utf8_byte_key(uid), method = "radix")][[1L]]
    out[group[uid != keep]] <- keep
  }
  out
}

# Reviewed curation files, read without the builder's reader.
read_curation_csv <- function(name, fields) {
  path <- file.path("scripts/curation", name)
  if (!file.exists(path)) {
    note(sprintf("curation file %s is missing", path))
    return(NULL)
  }
  table <- utils::read.csv(path, colClasses = "character", na.strings = character(),
                           check.names = FALSE, stringsAsFactors = FALSE)
  if (!identical(names(table), fields)) {
    note(sprintf("curation file %s has unexpected columns", path))
    return(NULL)
  }
  attr(table, "sha_file") <- digest::digest(path, algo = "sha256", file = TRUE)
  table
}
partial_curation <- read_curation_csv("vst_partial_event_completeness.csv", c(
  "site", "plotID", "eventID", "opportunity_source_uid", "dataCollected",
  "remarks_sha256", "rule_id", "tree_complete", "shrub_complete", "reviewed_by",
  "reviewed_on"
))
disturbance_curation <- read_curation_csv("vst_event_disturbance.csv", c(
  "site", "plotID", "eventID", "source_uid", "remarks_sha256", "classes",
  "timing", "reviewed_by", "reviewed_on"
))
remark_sha <- function(remarks) {
  remarks <- as.character(remarks)
  remarks[is.na(remarks)] <- ""
  vapply(enc2utf8(remarks), digest::digest, character(1), algo = "sha256",
         serialize = FALSE, USE.NAMES = FALSE)
}

# Independent tallest/biggest oracle: rows of the latest supported event per
# plot, screened one candidate at a time from the largest down with the size
# rules restated from R/size_plausibility.R (spike against the plant's other
# events, height-to-DBH ratio, tall thin shrub, 100 m cap).
oracle_extreme <- function(trees, plots, channel, measure) {
  forms <- if (channel == "tree") tree_forms else c("single shrub", "small shrub", "sapling")
  status <- as.character(plots[[paste0(channel, "_support")]])
  supported <- plots[status %in% VST_SUPPORTED, , drop = FALSE]
  if (!nrow(supported)) return(NA_real_)
  supported <- supported[order(as.character(supported$plotID),
                               -date_number(supported$date),
                               -xtfrm(as.character(supported$eventID))), , drop = FALSE]
  latest <- supported[!duplicated(as.character(supported$plotID)), , drop = FALSE]
  form <- tolower(trimws(as.character(trees$growthForm)))
  family <- ifelse(form %in% tree_forms, "tree",
                   ifelse(form %in% c("single shrub", "small shrub", "sapling"), "shrub", "other"))
  height <- suppressWarnings(as.numeric(trees$height))
  dbh <- suppressWarnings(as.numeric(trees$stemDiameter))
  basal <- suppressWarnings(as.numeric(trees$basalStemDiameter))
  own_diameter <- ifelse(family == "tree", dbh, ifelse(family == "shrub", basal, NA_real_))
  minimum <- if (channel == "tree") 10 else 0
  in_snapshot <- release_key(trees, c("plotID", "eventID")) %in%
    release_key(latest, c("plotID", "eventID")) & form %in% forms &
    trees$live %in% TRUE & is.finite(own_diameter) & own_diameter > 0 &
    own_diameter >= minimum & !(trees$superseded_in_event %in% TRUE) &
    is.na(trees$duplicate_of_uid)
  value <- if (measure == "height") height else own_diameter
  candidates <- which(in_snapshot & is.finite(value))
  if (!length(candidates)) return(NA_real_)
  intact <- trees$live %in% TRUE &
    !grepl("broken|physically damaged", as.character(trees$plantStatus), ignore.case = TRUE)
  plant <- paste(trees$plotID, trees$individualID, family, sep = "\r")
  spikes <- function(i) {
    eligible <- if (measure == "height") family != "other" & intact else family != "other"
    if (!eligible[[i]] || !(value[[i]] > 0)) return(FALSE)
    others <- which(plant == plant[[i]] & trees$eventID != trees$eventID[[i]] &
                      eligible & is.finite(value) & value > 0)
    if (!length(others)) return(FALSE)
    reference <- stats::median(tapply(value[others], as.character(trees$eventID[others]), max))
    gap <- 5
    value[[i]] > 3 * reference && value[[i]] - reference > gap
  }
  for (i in candidates[order(-value[candidates])]) {
    ratio <- family[[i]] == "tree" && is.finite(height[[i]]) && is.finite(dbh[[i]]) &&
      dbh[[i]] >= 10 && height[[i]] / dbh[[i]] > 2.5
    fails <- ratio || spikes(i)
    if (measure == "height") {
      fails <- fails || (family[[i]] == "shrub" && height[[i]] > 10 &&
                           (!is.finite(basal[[i]]) || basal[[i]] < 5)) ||
        height[[i]] > 100
    }
    if (!fails) return(value[[i]])
  }
  NA_real_
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
  "source_uid", "protocol_stem_key", "protocol_key_group_n",
  "protocol_key_conflict", "opportunity_source_missing", "plant_key", "event_key",
  "mapping_source_uid", "mappingMatched", "taxonID", "scientificName",
  "taxon_label", "taxon_resolution", "plant_id", "superseded_in_event",
  "duplicate_of_uid", "height"
)
v2_plot_required <- c(
  "opportunity_source_uid", "opportunity_source_record_count",
  "opportunity_key_conflict", "opportunity_source_uids",
  "opportunity_source_missing", "measurement_record_count_all",
  "measurement_date_min", "measurement_date_max",
  "measurement_date_distinct_n",
  "plotID", "eventID", "date", "year", "eventType", "plotType",
  "samplingImpractical", "dataCollected", "treesPresent", "shrubsPresent",
  "treePresence", "shrubSaplingPresence", "area_trees", "area_shrub",
  "tree_records", "shrub_records", "tree_invalid_metric_records",
  "shrub_invalid_metric_records", "tree_identity_conflict_keys",
  "shrub_identity_conflict_keys", "tree_support", "tree_support_reason",
  "shrub_support", "shrub_support_reason", "tree_supported",
  "shrub_supported", "event_key", "partial_event_rule_id",
  "tree_channel_complete", "shrub_channel_complete", "disturbance_classes",
  "disturbance_curation_id", "n_status_lost_burned", "n_status_removed"
)

bundles <- list()
receipts <- list()
denominator_mismatches <- list()
v2_sites <- character(0)
apparent_source_inventory <- character(0)
opportunity_source_inventory <- character(0)
measurement_only_context_total <- 0L
measurement_without_source_total <- 0L
measurement_only_sites <- character(0)

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
  if (is_v2 && (!is.data.frame(bundle$opportunity_source) ||
                !all(c("uid", "source_record_key", "protocol_key_group_n",
                       "protocol_key_conflict") %in% names(bundle$opportunity_source)))) {
    note(sprintf("%s opportunity_source is missing source identity fields", site))
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
        !identical(as.integer(contract$version), 3L)) {
      note(sprintf("%s lacks the exact embedded v3 contract", site))
    } else {
      if (!identical(as.character(contract$source_record_key), "source_uid") ||
          !identical(as.character(contract$mapping_source_record_key),
                     "mapping_source_uid") ||
          !identical(as.character(contract$protocol_stem_locator),
                     c("plotID", "eventID", "individualID", "tempStemID")) ||
          !identical(as.character(contract$opportunity_source_record_key),
                     "source_record_key"))
        note(sprintf("%s contract changes the source-row or protocol-locator identity", site))
      if (!identical(as.character(contract$event_key), c("plotID", "eventID")))
        note(sprintf("%s contract changes the opportunity-event key", site))
      if (!setequal(as.character(contract$support_status$supported), VST_SUPPORTED) ||
          !setequal(as.character(contract$support_status$held), VST_HELD) ||
          !identical(as.character(contract$support_status$history_only),
                     "supported_legacy_schema"))
        note(sprintf("%s contract support vocabulary differs from v3", site))
      if (!identical(as.character(contract$plant_id_key), c("plotID", "plant_id")) ||
          !identical(as.character(contract$legacy_schema$on_or_before), "2020-03-10") ||
          !identical(as.character(contract$legacy_schema$reason), VST_LEGACY_REASON))
        note(sprintf("%s contract plant key or legacy-schema terms differ from v3", site))
      if (is.null(partial_curation) || is.null(disturbance_curation) ||
          !identical(as.character(contract$curation$partial_event_completeness$sha256),
                     attr(partial_curation, "sha_file")) ||
          !identical(as.character(contract$curation$event_disturbance$sha256),
                     attr(disturbance_curation, "sha_file")))
        note(sprintf("%s was not built from the committed curation files", site))
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
    opportunity_source <- bundle$opportunity_source
    if (nrow(trees)) {
      if (any(!nonblank(trees$source_uid)) || any(!nonblank(trees$eventID)) || any(!nonblank(trees$plotID)) ||
          any(!nonblank(trees$individualID)))
        note(sprintf("%s has blank measurement identity fields", site))
      if (anyDuplicated(as.character(trees$source_uid)))
        note(sprintf("%s duplicates published apparent-individual uid", site))
      apparent_source_inventory <- c(
        apparent_source_inventory,
        stats::setNames(as.character(trees$source_uid), rep(site, nrow(trees)))
      )
      protocol_key <- release_key(
        trees, c("plotID", "eventID", "individualID", "tempStemID")
      )
      expected_group_n <- as.integer(table(protocol_key)[protocol_key])
      stored_group_n <- exact_nonnegative_integer(
        trees$protocol_key_group_n,
        paste(site, "apparent-individual locator group counts")
      )
      if (!identical(stored_group_n, expected_group_n) ||
          !identical(as.logical(trees$protocol_key_conflict), expected_group_n > 1L) ||
          !identical(as.character(trees$protocol_stem_key), protocol_key))
        note(sprintf("%s apparent-individual protocol locator audit is inconsistent", site))
      derivation_problems <- row_derivation_problems(trees)
      if (length(derivation_problems)) {
        note(sprintf(
          "%s row-derived invariants differ from preserved source fields: %s",
          site, paste(derivation_problems, collapse = ", ")
        ))
      }
      if (!identical(as.character(trees$plant_id), expected_plant_id(trees)))
        note(sprintf("%s plant_id differs from the parent-tag rule", site))
      if (!identical(as.logical(trees$superseded_in_event), expected_superseded(trees)))
        note(sprintf("%s superseded_in_event differs from the latest-date rule", site))
      if (!identical(as.character(trees$duplicate_of_uid), expected_duplicate_of(trees)))
        note(sprintf("%s duplicate_of_uid differs from the identical-row rule", site))
    }
    if (any(!nonblank(opportunity_source$uid)) ||
        anyDuplicated(as.character(opportunity_source$uid)) ||
        any(!nonblank(opportunity_source$source_record_key)) ||
        anyDuplicated(as.character(opportunity_source$source_record_key)) ||
        !identical(as.character(opportunity_source$source_record_key),
                   as.character(opportunity_source$uid)))
      note(sprintf("%s opportunity source uids are blank or non-unique", site))
    opportunity_source_inventory <- c(
      opportunity_source_inventory,
      stats::setNames(
        as.character(opportunity_source$source_record_key),
        rep(site, nrow(opportunity_source))
      )
    )
    source_event_key <- release_key(opportunity_source, c("eventID", "plotID"))
    expected_source_group_n <- as.integer(table(source_event_key)[source_event_key])
    stored_source_group_n <- exact_nonnegative_integer(
      opportunity_source$protocol_key_group_n,
      paste(site, "opportunity-source locator group counts")
    )
    if (!identical(stored_source_group_n, expected_source_group_n) ||
        !identical(as.logical(opportunity_source$protocol_key_conflict),
                   expected_source_group_n > 1L))
      note(sprintf("%s opportunity source key audit is inconsistent", site))
    if (any(!nonblank(plots$eventID)) || any(!nonblank(plots$plotID)))
      note(sprintf("%s has blank opportunity identity fields", site))
    if (anyDuplicated(release_key(plots, c("eventID", "plotID"))))
      note(sprintf("%s duplicates eventID + plotID opportunity rows", site))
    canonical_keys <- release_key(plots, c("eventID", "plotID"))
    source_missing <- as.logical(plots$opportunity_source_missing)
    if (any(is.na(source_missing)))
      note(sprintf("%s has unknown opportunity-source-missing flags", site))
    source_missing[is.na(source_missing)] <- FALSE
    source_backed_keys <- canonical_keys[!source_missing]
    if (!setequal(unique(source_event_key), source_backed_keys))
      note(sprintf("%s published source and source-backed context inventories differ", site))
    measurement_keys <- release_key(trees, c("eventID", "plotID"))
    expected_missing_keys <- setdiff(unique(measurement_keys), unique(source_event_key))
    if (!setequal(canonical_keys[source_missing], expected_missing_keys) ||
        length(setdiff(unique(measurement_keys), canonical_keys)))
      note(sprintf("%s measurement-only contexts do not equal source-key gaps", site))
    if (any(as.integer(table(measurement_keys)[canonical_keys[source_missing]]) < 1L,
            na.rm = TRUE) ||
        any(is.na(as.integer(table(measurement_keys)[canonical_keys[source_missing]]))))
      note(sprintf("%s has source-missing contexts without measurement rows", site))

    canonical_source_n <- integer(nrow(plots))
    canonical_source_n[!source_missing] <- as.integer(
      table(source_event_key)[source_backed_keys]
    )
    source_uid_sets <- tapply(
      as.character(opportunity_source$source_record_key), source_event_key,
      function(value) {
        value <- unique(value)
        paste(value[vst_byte_order(value)], collapse = ";")
      }
    )
    canonical_uid_sets <- rep(NA_character_, nrow(plots))
    canonical_uid_sets[!source_missing] <- unname(
      source_uid_sets[source_backed_keys]
    )
    selected_source_uid <- as.character(plots$opportunity_source_uid)
    selected_is_known <- mapply(function(uid, uids, missing) {
      if (missing) return(is.na(uid) || !nzchar(trimws(uid)))
      if (is.na(uid) || !nzchar(trimws(uid)) || is.na(uids)) return(FALSE)
      uid %in% strsplit(uids, ";", fixed = TRUE)[[1L]]
    }, selected_source_uid, canonical_uid_sets, source_missing,
    USE.NAMES = FALSE)
    stored_source_n <- exact_nonnegative_integer(
      plots$opportunity_source_record_count,
      paste(site, "opportunity source record counts")
    )
    if (!all(selected_is_known) ||
        !identical(stored_source_n, canonical_source_n) ||
        !identical(as.logical(plots$opportunity_key_conflict), canonical_source_n > 1L) ||
        !identical(as.character(plots$opportunity_source_uids), canonical_uid_sets))
      note(sprintf("%s plot-event context source audit differs from source rows", site))
    selected_source_parity <- vst_selected_source_parity(
      plots, opportunity_source
    )
    if (!selected_source_parity$ok) {
      note(sprintf(
        "%s canonical context differs from selected opportunity source row: %s",
        site, paste(selected_source_parity$fields, collapse = ",")
      ))
    }
    source_derived_fields <- setdiff(names(plots), VST_CONTEXT_DERIVED_FIELDS)
    invented <- vapply(source_derived_fields, function(field) {
      value <- plots[[field]][source_missing]
      if (is.character(value) || is.factor(value)) any(nonblank(value)) else any(!is.na(value))
    }, logical(1))
    if (any(invented) || any(plots$opportunity_key_conflict[source_missing] %in% TRUE))
      note(sprintf("%s source-missing contexts invent published opportunity fields", site))

    expected_tree_missing <- measurement_keys %in% canonical_keys[source_missing]
    if (!identical(as.logical(trees$opportunity_source_missing),
                   expected_tree_missing))
      note(sprintf("%s measurement-row source-missing flags disagree with contexts", site))
    expected_measurement_count <- as.integer(table(measurement_keys)[canonical_keys])
    expected_measurement_count[is.na(expected_measurement_count)] <- 0L
    stored_measurement_count <- exact_nonnegative_integer(
      plots$measurement_record_count_all,
      paste(site, "context measurement record counts")
    )
    stored_measurement_date_n <- exact_nonnegative_integer(
      plots$measurement_date_distinct_n,
      paste(site, "context distinct measurement-date counts")
    )
    if (!inherits(plots$measurement_date_min, "Date") ||
        !inherits(plots$measurement_date_max, "Date")) {
      note(sprintf("%s context measurement date bounds are not Date vectors", site))
    }
    expected_date_min <- expected_date_max <- rep(NA_real_, nrow(plots))
    expected_date_n <- integer(nrow(plots))
    if (nrow(trees)) {
      measurement_date_number <- date_number(trees$date)
      date_min <- tapply(measurement_date_number, measurement_keys, function(value) {
        value <- value[is.finite(value)]
        if (length(value)) min(value) else NA_real_
      })
      date_max <- tapply(measurement_date_number, measurement_keys, function(value) {
        value <- value[is.finite(value)]
        if (length(value)) max(value) else NA_real_
      })
      date_n <- tapply(measurement_date_number, measurement_keys, function(value) {
        as.integer(length(unique(value[is.finite(value)])))
      })
      expected_date_min <- unname(as.numeric(date_min[canonical_keys]))
      expected_date_max <- unname(as.numeric(date_max[canonical_keys]))
      expected_date_n <- as.integer(unname(date_n[canonical_keys]))
      expected_date_n[is.na(expected_date_n)] <- 0L
    }
    measurement_summary_mismatch <- c(
      record_count = !identical(stored_measurement_count, expected_measurement_count),
      date_min = !identical(
        date_number(plots$measurement_date_min), expected_date_min
      ),
      date_max = !identical(
        date_number(plots$measurement_date_max), expected_date_max
      ),
      date_distinct_n = !identical(stored_measurement_date_n, expected_date_n)
    )
    if (any(measurement_summary_mismatch)) {
      note(sprintf(
        "%s context measurement summaries differ from preserved rows: %s",
        site,
        paste(names(measurement_summary_mismatch)[measurement_summary_mismatch],
              collapse = ",")
      ))
    }

    expected_missing_n <- as.integer(sum(source_missing))
    expected_missing_records <- as.integer(sum(measurement_keys %in% canonical_keys[source_missing]))
    measurement_only_context_total <- measurement_only_context_total + expected_missing_n
    measurement_without_source_total <- measurement_without_source_total +
      expected_missing_records
    if (expected_missing_n > 0L) measurement_only_sites <- c(measurement_only_sites, site)
    stored_meta_missing_n <- exact_nonnegative_integer(
      bundle$meta$n_measurement_only_contexts %||% NA_integer_,
      paste(site, "metadata measurement-only context count")
    )
    stored_meta_missing_records <- exact_nonnegative_integer(
      bundle$meta$n_measurement_records_without_opportunity_source %||% NA_integer_,
      paste(site, "metadata measurements-without-source count")
    )
    if (!identical(stored_meta_missing_n,
                   expected_missing_n) ||
        !identical(stored_meta_missing_records, expected_missing_records))
      note(sprintf("%s metadata source-missing counts differ from preserved rows", site))

    tree_form <- tolower(trimws(as.character(trees$growthForm))) %in%
      c("single bole tree", "multi-bole tree")
    shrub_form <- tolower(trimws(as.character(trees$growthForm))) %in%
      c("single shrub", "small shrub", "sapling")
    tree_diameter <- suppressWarnings(as.numeric(trees$stemDiameter))
    shrub_diameter <- suppressWarnings(as.numeric(trees$basalStemDiameter))
    # Contract v3 multi-bole rule (NEON.DOC.000987 Table 8), recomputed here
    # from preserved rows: a lettered tree-form bole is valid at any positive
    # DBH when its parent tag has a tree-form bole >= 10 cm in the same
    # plotID x eventID.
    bole_id <- as.character(trees$individualID)
    bole_plot_event <- paste(as.character(trees$plotID), as.character(trees$eventID),
                             sep = "\r")
    bole_parents <- unique(paste(bole_plot_event, bole_id, sep = "\r")[
      tree_form & is.finite(tree_diameter) & tree_diameter >= 10
    ])
    secondary_bole_ok <- tree_form & !is.na(bole_id) & grepl("[A-Za-z]$", bole_id) &
      paste(bole_plot_event, sub("[A-Za-z]$", "", bole_id), sep = "\r") %in%
        bole_parents
    live <- grepl(
      "^live", trimws(as.character(trees$plantStatus)), ignore.case = TRUE
    )
    recomputed <- list(
      tree = list(
        records = event_counts(trees, plots, tree_form),
        invalid = event_counts(
          trees, plots,
          tree_form & live &
            !((is.finite(tree_diameter) & tree_diameter > 0 & tree_diameter >= 10) |
              (secondary_bole_ok & is.finite(tree_diameter) & tree_diameter > 0))
        ),
        identity_conflicts = identity_conflict_counts(trees, plots, tree_form),
        presence = presence_state(plots$treesPresent)
      ),
      shrub = list(
        records = event_counts(trees, plots, shrub_form),
        invalid = event_counts(
          trees, plots,
          shrub_form & live & !(is.finite(shrub_diameter) & shrub_diameter > 0)
        ),
        identity_conflicts = identity_conflict_counts(trees, plots, shrub_form),
        presence = shrub_presence_state(plots$treesPresent, plots$shrubsPresent)
      )
    )
    if (!identical(as.character(plots$treePresence), recomputed$tree$presence))
      note(sprintf("%s tree presence state differs from published opportunity fields", site))
    if (!identical(as.character(plots$shrubSaplingPresence),
                   recomputed$shrub$presence))
      note(sprintf("%s shrub presence state differs from published opportunity fields", site))

    # S1 and S2 cross-checks against the committed curation files.
    event_id <- paste(plots$plotID, plots$eventID, sep = "\r")
    source_rows <- !(plots$opportunity_source_missing %in% TRUE)
    selected_remarks <- if ("remarks" %in% names(opportunity_source)) {
      as.character(opportunity_source$remarks)[
        match(as.character(plots$opportunity_source_uid),
              as.character(opportunity_source$uid))
      ]
    } else rep(NA_character_, nrow(plots))
    expected_rule <- rep(NA_character_, nrow(plots))
    expected_tree_complete <- expected_shrub_complete <- rep(FALSE, nrow(plots))
    if (!is.null(partial_curation)) {
      rows <- partial_curation[partial_curation$site == site, , drop = FALSE]
      at <- match(paste(rows$plotID, rows$eventID, sep = "\r"), event_id)
      partial_events <- source_rows &
        tolower(trimws(as.character(plots$dataCollected))) %in% "partial"
      if (anyNA(at) || !setequal(at, which(partial_events)) ||
          !identical(rows$opportunity_source_uid, as.character(plots$opportunity_source_uid[at])) ||
          !identical(rows$remarks_sha256, remark_sha(selected_remarks[at])))
        note(sprintf("%s partial-event curation rows are missing, stale or orphaned", site))
      ok_at <- !is.na(at)
      expected_rule[at[ok_at]] <- rows$rule_id[ok_at]
      expected_tree_complete[at[ok_at]] <- rows$tree_complete[ok_at] == "TRUE"
      expected_shrub_complete[at[ok_at]] <- rows$shrub_complete[ok_at] == "TRUE"
    }
    if (!identical(as.character(plots$partial_event_rule_id), expected_rule) ||
        !identical(as.logical(plots$tree_channel_complete), expected_tree_complete) ||
        !identical(as.logical(plots$shrub_channel_complete), expected_shrub_complete))
      note(sprintf("%s partial-event curation columns differ from the committed file", site))
    expected_classes <- expected_curation <- rep(NA_character_, nrow(plots))
    if (!is.null(disturbance_curation)) {
      rows <- disturbance_curation[disturbance_curation$site == site, , drop = FALSE]
      at <- match(paste(rows$plotID, rows$eventID, sep = "\r"), event_id)
      if (anyNA(at) ||
          !identical(rows$source_uid, as.character(plots$opportunity_source_uid[at])) ||
          !identical(rows$remarks_sha256, remark_sha(selected_remarks[at])))
        note(sprintf("%s disturbance curation rows are stale or orphaned", site))
      ok_at <- !is.na(at)
      expected_curation[at[ok_at]] <- "vst-event-disturbance-v1"
      expected_classes[at[ok_at]] <- ifelse(rows$classes[ok_at] == "none", NA_character_,
                                            rows$classes[ok_at])
    }
    if (!identical(as.character(plots$disturbance_classes), expected_classes) ||
        !identical(as.character(plots$disturbance_curation_id), expected_curation))
      note(sprintf("%s disturbance labels differ from the committed file", site))
    status_text <- tolower(trimws(as.character(trees$plantStatus)))
    if (!identical(as.integer(plots$n_status_lost_burned),
                   event_counts(trees, plots, status_text == "lost, burned")) ||
        !identical(as.integer(plots$n_status_removed),
                   event_counts(trees, plots, status_text == "removed")))
      note(sprintf("%s lost-burned or removed status counts differ from preserved rows", site))

    for (channel in c("tree", "shrub")) {
      status <- plots[[paste0(channel, "_support")]]
      reason <- plots[[paste0(channel, "_support_reason")]]
      supported <- plots[[paste0(channel, "_supported")]]
      area <- plots[[if (channel == "tree") "area_trees" else "area_shrub"]]
      records <- plots[[paste0(channel, "_records")]]
      invalid <- plots[[paste0(channel, "_invalid_metric_records")]]
      identity_conflicts <- plots[[paste0(channel, "_identity_conflict_keys")]]
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
      if (any(!is.finite(identity_conflicts) | identity_conflicts < 0 |
              identity_conflicts != as.integer(identity_conflicts), na.rm = TRUE))
        note(sprintf("%s %s identity-conflict counts are invalid", site, channel))
      if (!identical(as.integer(records), recomputed[[channel]]$records))
        note(sprintf("%s %s record counts differ from preserved measurement rows", site, channel))
      if (!identical(as.integer(invalid), recomputed[[channel]]$invalid))
        note(sprintf("%s %s invalid-metric counts differ from preserved live measurement rows", site, channel))
      if (!identical(as.integer(identity_conflicts),
                     recomputed[[channel]]$identity_conflicts))
        note(sprintf("%s %s identity-conflict counts differ from preserved source rows", site, channel))
      complete <- if (channel == "tree") expected_tree_complete else expected_shrub_complete
      expected_decision <- expected_support_decision(
        area, plots$samplingImpractical,
        ifelse(complete, "allGrowthForms", as.character(plots$dataCollected)),
        recomputed[[channel]]$presence, recomputed[[channel]]$records,
        recomputed[[channel]]$invalid,
        recomputed[[channel]]$identity_conflicts,
        plots$opportunity_key_conflict,
        plots$opportunity_source_missing,
        plots$date,
        ifelse(is.na(expected_rule), NA_character_, sprintf(
          "dataCollected=partial reviewed as %s for this channel (%s)",
          ifelse(complete, "complete", "incomplete"), expected_rule
        ))
      )
      if (!identical(as.character(status), expected_decision$status))
        note(sprintf("%s %s support states violate fail-closed precedence", site, channel))
      if (!identical(as.character(reason), expected_decision$reason))
        note(sprintf("%s %s support reasons differ from the recomputed decision", site, channel))
      if (any(supported_rows & invalid != 0L, na.rm = TRUE))
        note(sprintf("%s %s supported rows contain invalid required metrics", site, channel))
      if (any(status == "held_metric_invalid" & invalid <= 0L, na.rm = TRUE))
        note(sprintf("%s %s held_metric_invalid rows contain no invalid required metrics", site, channel))
      if (any(status == "held_identity_conflict" &
              identity_conflicts <= 0L & !plots$opportunity_key_conflict,
              na.rm = TRUE))
        note(sprintf("%s %s held_identity_conflict rows contain no source-key conflict", site, channel))
      if (any(source_missing & status != "held_opportunity_source_missing") ||
          any(!source_missing & status == "held_opportunity_source_missing"))
        note(sprintf("%s %s source-missing status disagrees with context audit", site, channel))
      if (any(status == "sampled_absence" & records != 0L, na.rm = TRUE))
        note(sprintf("%s %s sampled_absence rows contain records", site, channel))
      if (any(status == "sampled_with_records" & records <= 0L, na.rm = TRUE))
        note(sprintf("%s %s sampled_with_records rows contain no records", site, channel))
    }
    # Independent size-extremes oracle against the embedded channel summaries.
    channel_summary <- bundle$contract$channel_summary %||% list()
    for (channel in c("tree", "shrub")) {
      summary_name <- if (channel == "tree") "tree_dbh" else "shrub_sapling_basal"
      stored <- channel_summary[[summary_name]] %||% list()
      for (measure in c("height", "diameter")) {
        field <- if (measure == "height") "tallest_m" else "biggest_diam_cm"
        expected_value <- oracle_extreme(trees, plots, channel, measure)
        stored_value <- suppressWarnings(as.numeric(stored[[field]] %||% NA_real_))
        if (length(stored_value) != 1L ||
            !isTRUE(all.equal(stored_value, expected_value, tolerance = 1e-9)) &&
            !(is.na(stored_value) && is.na(expected_value)))
          note(sprintf("%s %s %s differs from the independent size oracle (stored %s, oracle %s)",
                       site, summary_name, field, format(stored_value), format(expected_value)))
      }
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

source_inventories <- list(
  `apparent-individual` = apparent_source_inventory,
  `plot-opportunity` = opportunity_source_inventory
)
for (label in names(source_inventories)) {
  inventory <- source_inventories[[label]]
  duplicate <- duplicated(unname(inventory)) |
    duplicated(unname(inventory), fromLast = TRUE)
  if (any(duplicate)) {
    values <- unique(unname(inventory[duplicate]))
    examples <- values[seq_len(min(3L, length(values)))]
    note(sprintf(
      "published %s uid is reused across site bundles; examples: %s",
      label, paste(examples, collapse = ",")
    ))
  }
}

if (identical(sort(v2_sites), expected)) {
  if (!identical(measurement_only_context_total, 49L) ||
      !identical(measurement_without_source_total, 4365L) ||
      length(unique(measurement_only_sites)) != 11L) {
    note(sprintf(
      paste0(
        "RELEASE-2026 measurement-only inventory changed: contexts=%d ",
        "records=%d sites=%d; expected 49/4365/11"
      ),
      measurement_only_context_total, measurement_without_source_total,
      length(unique(measurement_only_sites))
    ))
  }
}

receipt_count <- sum(vapply(receipts, Negate(is.null), logical(1)))
common_receipt <- NULL
if (length(v2_sites)) {
  if (!identical(sort(v2_sites), expected))
    note(sprintf("mixed contract family: only %d/%d bundles use v3", length(v2_sites), length(expected)))
  if (receipt_count != length(expected))
    note(sprintf("source receipt is present on only %d/%d v3 bundles", receipt_count, length(expected)))
  if (receipt_count == length(expected)) {
    common_receipt <- tryCatch(vst_receipts_identical(receipts), error = function(error) {
      note(conditionMessage(error)); NULL
    })
    if (!is.null(common_receipt)) {
      receipt_required <- c(
        "schema_version", "provenance_class", "product", "neon_release",
        "release_doi", "query_start", "query_end", "source_receipt_id",
        "raw_source_digest", "neon_utilities_version", "source_normalization",
        "built_at", "builder_commit"
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
                     "https://doi.org/10.48443/pypa-qf12") ||
          !identical(as.character(common_receipt$source_normalization),
                     VST_SOURCE_NORMALIZATION))
        note("source receipt does not identify the reviewed official RELEASE-2026 family")
      if (!identical(as.character(common_receipt$query_start), "FULL_RELEASE") ||
          !identical(as.character(common_receipt$query_end), "FULL_RELEASE"))
        note("release candidate uses a bounded diagnostic query instead of FULL_RELEASE")
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
  "n_measurement_only_contexts",
  "n_measurement_records_without_opportunity_source", "inference_scope"
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
    note("site_index contains a non-v3 contract row")
  if (length(v2_sites) && !identical(attr(site_index, "contract_id"), VST_CONTRACT_ID))
    note("site_index contract_id attribute differs from v3")
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
    diagnostic_fields <- c(
      "n_measurement_only_contexts",
      "n_measurement_records_without_opportunity_source"
    )
    if (any(vapply(diagnostic_fields, function(field) {
      value <- suppressWarnings(as.numeric(site_index[[field]]))
      any(!is.finite(value) | value < 0 | value != floor(value))
    }, logical(1))))
      note("site_index source-missing diagnostics are not nonnegative integers")
    if (sum(site_index$n_measurement_only_contexts) != 49L ||
        sum(site_index$n_measurement_records_without_opportunity_source) != 4365L ||
        sum(site_index$n_measurement_only_contexts > 0L) != 11L)
      note("site_index source-missing diagnostics differ from RELEASE-2026 49/4365/11")
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
      note("search_index contract_id differs from v3")
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
          note("search_index channel_sites contains a non-v3 row")
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
if (!is.null(demo) && !is.null(bundles[["SRER"]]) &&
    !isTRUE(all.equal(demo, bundles[["SRER"]], check.attributes = TRUE)))
  note("data-sample/demo.rds differs from the SRER runtime bundle")

audit_path <- file.path("data/source", "vegetation-data-quality-audit.csv")
if (length(v2_sites)) {
  if (!file.exists(audit_path)) {
    note("v3 family lacks data/source/vegetation-data-quality-audit.csv")
  } else {
    audit_environment <- new.env(parent = globalenv())
    audit_temp <- tempfile(fileext = ".csv")
    tryCatch({
      sys.source("scripts/write_data_quality_audit.R", envir = audit_environment)
      expected_audit <- audit_environment$vst_build_data_quality_audit("data/sites")
      audit_environment$vst_write_data_quality_audit(expected_audit, audit_temp)
      if (!identical(read_file_bytes(audit_path), read_file_bytes(audit_temp))) {
        note("data-quality audit differs from the deterministic 42-site v3 recomputation")
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
    fetch_runtime <- tryCatch(
      vst_read_fetch_runtime(ledger_files[["fetch_runtime"]]),
      error = function(error) {
        note(sprintf("fetch runtime receipt is invalid: %s", conditionMessage(error)))
        NULL
      }
    )
    if (!is.null(fetch_runtime) && !is.null(common_receipt)) {
      expected_runtime <- list(
        product = as.character(common_receipt$product),
        officialNeonRelease = as.character(common_receipt$neon_release),
        releaseDoi = as.character(common_receipt$release_doi),
        queryStart = as.character(common_receipt$query_start),
        queryEnd = as.character(common_receipt$query_end),
        neonUtilities = as.character(common_receipt$neon_utilities_version),
        sourceNormalization = as.character(common_receipt$source_normalization)
      )
      if (!identical(fetch_runtime, expected_runtime) ||
          !identical(fetch_runtime$sourceNormalization,
                     VST_SOURCE_NORMALIZATION)) {
        note("durable fetch runtime differs from the embedded source receipt")
      }
    }
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

cat(sprintf("bundles=%d; v3=%d; sourceReceipts=%d; denominatorMismatchSites=%s\n",
            length(bundles), length(v2_sites), receipt_count,
            if (length(denominator_mismatches))
              paste(names(denominator_mismatches), collapse = ",") else "none"))
if (length(problems)) {
  cat("Bundle verification failed:\n", paste0("- ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("Vegetation bundle, event/opportunity, quality-audit, index, receipt, and denominator gates passed.\n")
