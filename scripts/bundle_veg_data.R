#!/usr/bin/env Rscript

# Build the versioned Vegetation Structure candidate bundle from official raw
# DP1.10098.001 tables. The bundle preserves the source event/stem keys and the
# per-event sampling opportunity needed to distinguish observed zero from
# unsupported inference. Derived indexes are embedded once and copied by the
# index scripts for deterministic bytes; an independent runtime-consumer
# projection verifies every copied summary before a candidate can pass.

suppressWarnings(suppressMessages(library(dplyr)))
source("scripts/vegetation_inventory.R")
source("R/size_plausibility.R")
source("scripts/curation/vst_curation.R")

VST_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v3"
VST_PRODUCT <- "DP1.10098.001"
VST_RELEASE <- Sys.getenv("VST_NEON_RELEASE", unset = "RELEASE-2026")
VST_SUPPORTED_STATUS <- c("sampled_with_records", "sampled_absence")
# Pre-2020-03-10 events whose samplingImpractical / dataCollected fields are
# blank (NEON changelog 16306 added them). With records and every other check
# passing they are admitted to change history only; never to the latest
# supported snapshot, the indexes or the default channel.
VST_LEGACY_STATUS <- "supported_legacy_schema"
VST_LEGACY_SCHEMA_END <- as.Date("2020-03-10")
VST_LEGACY_REASON <-
  "legacy schema before 2020-03-10: sampling opportunity assumed (NEON changelog 16306)"
VST_HELD_STATUS <- c(
  "held_sampling_impractical", "held_dendrometer_only", "held_missing_area",
  "held_opportunity_unknown", "held_presence_record_conflict",
  "held_metric_invalid", "held_identity_conflict",
  "held_opportunity_source_missing", VST_LEGACY_STATUS
)
VST_PERSONNEL_FIELDS <- c("measuredBy", "recordedBy")
VST_TREE_FORMS <- c("single bole tree", "multi-bole tree")
VST_SHRUB_FORMS <- c("single shrub", "small shrub", "sapling")
VST_SMALL_TREE_FORMS <- "small tree"
# S4 row exclusions (snapshot and history; rows stay in the bundle).
VST_SPLIT_VISIT_FORMS <- c("small tree", "sapling", "single shrub", "small shrub")
VST_SPLIT_VISIT_DAYS <- 3
VST_DUPLICATE_FIELDS <- c("growthForm", "plantStatus", "stemDiameter", "height")
# C4 default channel: trees recorded in at least 5 plots and in at least half
# of the supported tree plots.
VST_PRIMARY_TREE_MIN_PLOTS <- 5L
VST_PRIMARY_TREE_MIN_SHARE <- 0.5

vst_field <- function(data, candidates, default = NA, required = FALSE) {
  found <- intersect(candidates, names(data))
  if (!length(found)) {
    if (required) {
      stop(sprintf("required field missing; expected one of [%s]",
                   paste(candidates, collapse = ", ")), call. = FALSE)
    }
    return(rep(default, nrow(data)))
  }
  data[[found[[1L]]]]
}

vst_chr <- function(x) {
  out <- as.character(x)
  out[is.na(x)] <- NA_character_
  out
}

vst_num <- function(x) suppressWarnings(as.numeric(x))

vst_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  suppressWarnings(as.Date(substr(vst_chr(x), 1L, 10L)))
}

vst_datetime_order <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  if (inherits(x, "Date")) return(as.numeric(x) * 86400)
  value <- vst_chr(x)
  parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
  out <- as.numeric(parsed)
  fallback <- suppressWarnings(as.numeric(as.Date(substr(value, 1L, 10L))) * 86400)
  out[!is.finite(out)] <- fallback[!is.finite(out)]
  out
}

vst_nonblank <- function(x) !is.na(x) & nzchar(trimws(vst_chr(x)))

vst_safe_median <- function(x) {
  x <- vst_num(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

vst_safe_max <- function(x) {
  x <- vst_num(x)
  x <- x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}

vst_mode_chr <- function(x) {
  x <- vst_chr(x)
  x <- x[vst_nonblank(x)]
  if (!length(x)) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  sort(names(tab)[tab == max(tab)])[[1L]]
}

vst_key <- function(data, columns) {
  parts <- lapply(columns, function(column) {
    value <- vst_chr(data[[column]])
    value[is.na(value)] <- "<NA>"
    paste0(nchar(value), ":", value)
  })
  do.call(paste, c(parts, sep = "\u001f"))
}

vst_assert_unique <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(sprintf("%s lacks key fields [%s]", label, paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  key <- vst_key(data, columns)
  bad <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(bad)) {
    examples <- unique(key[bad])[seq_len(min(3L, length(unique(key[bad]))))]
    stop(sprintf("%s violates unique key (%s); examples: %s", label,
                 paste(columns, collapse = " + "), paste(examples, collapse = "; ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

vst_is_species_rank <- function(rank, scientific_name) {
  rank <- tolower(trimws(vst_chr(rank)))
  scientific_name <- vst_chr(scientific_name)
  accepted <- !is.na(rank) & rank %in% tolower(c(
    "species", "subspecies", "variety", "form"
  ))
  named <- vst_nonblank(scientific_name)
  ambiguous <- grepl("\\bsp\\.?$", ifelse(is.na(scientific_name), "", scientific_name),
                     ignore.case = TRUE) |
    grepl("/", ifelse(is.na(scientific_name), "", scientific_name), fixed = TRUE)
  accepted & named & !ambiguous
}

vst_row_signature <- function(data) {
  if (!nrow(data)) return(character(0))
  columns <- sort(names(data))
  parts <- lapply(columns, function(column) {
    value <- vst_chr(data[[column]])
    value[is.na(value)] <- "<NA>"
    paste0(column, "=", value)
  })
  do.call(paste, c(parts, sep = "\u001e"))
}

vst_latest_mapping <- function(mapping) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE)
  for (column in c("uid", "plotID", "individualID")) {
    if (!column %in% names(mapping)) {
      stop("vst_mappingandtagging lacks ", column, call. = FALSE)
    }
  }
  if (any(!vst_nonblank(mapping$uid)) || any(!vst_nonblank(mapping$plotID)) ||
      any(!vst_nonblank(mapping$individualID))) {
    stop("vst_mappingandtagging has blank uid, plotID, or individualID",
         call. = FALSE)
  }
  vst_assert_unique(mapping, "uid", "vst_mappingandtagging")

  created_raw <- vst_field(
    mapping, c("createdDate", "date", "recordDate"), required = TRUE
  )
  created_order <- vst_datetime_order(created_raw)
  if (any(!is.finite(created_order))) {
    stop("vst_mappingandtagging has missing or unparseable record dates",
         call. = FALSE)
  }
  if ("eventID" %in% names(mapping) && all(vst_nonblank(mapping$eventID))) {
    vst_assert_unique(mapping, c("plotID", "eventID", "individualID"),
                      "vst_mappingandtagging")
  }

  # A latest-created tie cannot be resolved from source evidence. Choosing by
  # row order, uid, or taxon content would invent a winner, so fail before the
  # mapping/tagging identity is joined to measurement rows.
  mapping_key <- vst_key(mapping, c("plotID", "individualID"))
  latest_created <- tapply(created_order, mapping_key, max)
  latest_row <- created_order == unname(latest_created[mapping_key])
  latest_count <- table(mapping_key[latest_row])
  ambiguous <- names(latest_count)[latest_count > 1L]
  if (length(ambiguous)) {
    examples <- ambiguous[seq_len(min(3L, length(ambiguous)))]
    stop(sprintf(
      paste0(
        "vst_mappingandtagging has ambiguous latest mapping rows for %d ",
        "plotID + individualID key(s); examples: %s"
      ),
      length(ambiguous), paste(examples, collapse = "; ")
    ), call. = FALSE)
  }

  order_rows <- order(vst_chr(mapping$plotID), vst_chr(mapping$individualID),
                      -created_order, vst_utf8_byte_key(mapping$uid),
                      na.last = TRUE)
  ordered <- mapping[order_rows, , drop = FALSE]
  ordered_created <- vst_chr(created_raw)[order_rows]
  composite <- vst_key(ordered, c("plotID", "individualID"))
  keep <- !duplicated(composite)
  selected <- ordered[keep, , drop = FALSE]

  data.frame(
    plotID = vst_chr(selected$plotID),
    individualID = vst_chr(selected$individualID),
    mapping_source_uid = vst_chr(selected$uid),
    mappingCreatedDate = ordered_created[keep],
    mappingEventID = vst_chr(vst_field(selected, c("eventID"))),
    taxonID = vst_chr(vst_field(selected, c("taxonID"))),
    scientificName = vst_chr(vst_field(selected, c("scientificName"))),
    genus = vst_chr(vst_field(selected, c("genus"))),
    family = vst_chr(vst_field(selected, c("family"))),
    taxonRank = vst_chr(vst_field(selected, c("taxonRank"))),
    recordType = vst_chr(vst_field(selected, c("recordType"))),
    identificationQualifier = vst_chr(vst_field(selected, c("identificationQualifier"))),
    mappingDataQF = vst_chr(vst_field(selected, c("dataQF"))),
    stringsAsFactors = FALSE
  )
}

vst_assert_measurement_derivations <- function(table) {
  expected_year <- suppressWarnings(as.integer(format(vst_date(table$date), "%Y")))
  expected_live <- grepl(
    "^live", trimws(vst_chr(table$plantStatus)), ignore.case = TRUE
  )
  expected_permanent <- grepl("^NEON", vst_chr(table$individualID))
  expected_species <- vst_is_species_rank(
    table$taxonRank, table$scientificName
  )
  expected_label <- ifelse(
    vst_nonblank(table$scientificName), vst_chr(table$scientificName),
    ifelse(
      vst_nonblank(table$taxonID),
      paste0("Unresolved taxon (", vst_chr(table$taxonID), ")"),
      "Unresolved taxon"
    )
  )
  expected_resolution <- ifelse(
    expected_species & vst_nonblank(table$scientificName),
    "species-level", "coarse-or-unresolved"
  )
  expected_mapping_match <- vst_nonblank(table$mapping_source_uid)
  checks <- list(
    date_type = inherits(table$date, "Date"),
    year = is.integer(table$year) && identical(table$year, expected_year),
    live = is.logical(table$live) && identical(table$live, expected_live),
    permanent = is.logical(table$permanent) &&
      identical(table$permanent, expected_permanent),
    plant_key = is.character(table$plant_key) && identical(
      table$plant_key,
      paste(vst_chr(table$plotID), vst_chr(table$individualID), sep = "\r")
    ),
    event_key = is.character(table$event_key) && identical(
      table$event_key,
      paste(vst_chr(table$plotID), vst_chr(table$eventID), sep = "\r")
    ),
    mapping_source_uid_type = is.character(table$mapping_source_uid),
    mappingMatched = is.logical(table$mappingMatched) && identical(
      table$mappingMatched, expected_mapping_match
    ),
    is_species = is.logical(table$is_species) &&
      identical(table$is_species, expected_species),
    taxon_label = is.character(table$taxon_label) &&
      identical(table$taxon_label, expected_label),
    taxon_resolution = is.character(table$taxon_resolution) && identical(
      table$taxon_resolution, expected_resolution
    ),
    plant_id = is.character(table$plant_id) && !anyNA(table$plant_id),
    superseded_in_event = is.logical(table$superseded_in_event) &&
      !anyNA(table$superseded_in_event),
    duplicate_of_uid = is.character(table$duplicate_of_uid)
  )
  if (!all(unlist(checks, use.names = FALSE))) {
    stop(
      "measurement row derivation invariant failed: ",
      paste(names(checks)[!unlist(checks, use.names = FALSE)], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# C1 plant identity. NEON tags each additional bole of a multi-bole tree with
# the parent tag plus one letter (1234a, 1234b; NEON.DOC.000987 Table 8).
# plant_id is that parent tag for a lettered individual with at least one
# tree-form row whose parent tag exists in the same plot; otherwise it is the
# individualID. Growth stays per bole (plant_key); plant counts, status and
# mortality use plotID + plant_id.
vst_plant_id <- function(table) {
  id <- vst_chr(table$individualID)
  if (!length(id)) return(character())
  plot <- vst_chr(table$plotID)
  plant <- paste(plot, id, sep = "\r")
  tree_form <- tolower(trimws(vst_chr(table$growthForm))) %in% tolower(VST_TREE_FORMS)
  parent <- sub("[A-Za-z]$", "", id)
  promote <- !is.na(id) & grepl("[A-Za-z]$", id) &
    plant %in% unique(plant[tree_form]) &
    paste(plot, parent, sep = "\r") %in% unique(plant)
  ifelse(promote, parent, id)
}

# S4 (a). Within plotID x eventID x individualID, a row dated before that
# individual's latest date in the event is superseded (per individual, not per
# stem: tempStemIDs are reassigned each visit). Exception: a small-tree,
# sapling or shrub row whose tempStemID is absent from the latest date and is
# dated at most 3 days earlier, a measurement that spanned more than one day.
vst_superseded_in_event <- function(table) {
  if (!nrow(table)) return(logical())
  individual <- vst_key(table, c("plotID", "eventID", "individualID"))
  day <- as.numeric(vst_date(table$date))
  dated <- is.finite(day)
  latest <- rep(NA_real_, length(day))
  if (any(dated)) {
    by_individual <- tapply(day[dated], individual[dated], max)
    latest <- unname(as.numeric(by_individual[individual]))
  }
  earlier <- dated & is.finite(latest) & day < latest
  stem <- paste(individual, vst_chr(table$tempStemID), sep = "\r")
  latest_stems <- unique(stem[dated & is.finite(latest) & day == latest])
  split_visit <- tolower(trimws(vst_chr(table$growthForm))) %in% VST_SPLIT_VISIT_FORMS &
    !stem %in% latest_stems & (latest - day) <= VST_SPLIT_VISIT_DAYS
  earlier & !split_visit
}

# S4 (b). Tree-form rows of one individual, event and date that repeat the
# same growthForm, plantStatus, DBH and height (missing equals missing) count
# once. The copy first in source_uid byte order is kept; every other copy
# carries the kept uid in duplicate_of_uid (NA otherwise).
vst_duplicate_of_uid <- function(table) {
  out <- rep(NA_character_, nrow(table))
  tree_form <- tolower(trimws(vst_chr(table$growthForm))) %in% tolower(VST_TREE_FORMS)
  if (!any(tree_form)) return(out)
  rows <- which(tree_form)
  part <- table[rows, , drop = FALSE]
  key <- vst_key(part, c("plotID", "eventID", "individualID", "date", VST_DUPLICATE_FIELDS))
  uid <- vst_chr(part$source_uid)
  ordered <- order(vst_utf8_byte_key(key), vst_utf8_byte_key(uid), method = "radix")
  first <- !duplicated(key[ordered])
  kept <- stats::setNames(uid[ordered][first], key[ordered][first])[key]
  copy <- uid != kept
  out[rows[copy]] <- unname(kept[copy])
  out
}

vst_measurement_table <- function(apparent, identity) {
  apparent <- as.data.frame(apparent, stringsAsFactors = FALSE)
  required <- c("uid", "eventID", "plotID", "individualID", "tempStemID")
  missing <- setdiff(required, names(apparent))
  if (length(missing)) {
    stop("vst_apparentindividual lacks required fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(!vst_nonblank(apparent$uid)) || any(!vst_nonblank(apparent$eventID)) || any(!vst_nonblank(apparent$plotID)) ||
      any(!vst_nonblank(apparent$individualID))) {
    stop("vst_apparentindividual has blank uid, eventID, plotID, or individualID", call. = FALSE)
  }
  # uid is the published source-row identity and must remain unique. NEON says
  # eventID x individualID x tempStemID *should* be unique, but the official
  # release also warns that protocol/data-entry anomalies can violate that
  # locator. Preserve every uid, mark the conflicting locator, and let the
  # channel opportunity fail closed rather than deleting a possible legacy
  # bole or double-counting an ambiguous record.
  vst_assert_unique(apparent, "uid", "vst_apparentindividual")
  protocol_key <- vst_key(
    apparent, c("plotID", "eventID", "individualID", "tempStemID")
  )
  protocol_group_n <- as.integer(table(protocol_key)[protocol_key])

  measured_date <- vst_date(vst_field(apparent, c("date", "collectDate", "eventDate")))
  table <- data.frame(
    source_uid = vst_chr(apparent$uid),
    protocol_stem_key = protocol_key,
    protocol_key_group_n = protocol_group_n,
    protocol_key_conflict = protocol_group_n > 1L,
    eventID = vst_chr(apparent$eventID),
    plotID = vst_chr(apparent$plotID),
    individualID = vst_chr(apparent$individualID),
    tempStemID = vst_chr(apparent$tempStemID),
    subplotID = vst_chr(vst_field(apparent, c("subplotID", "nestedSubplotID"))),
    date = measured_date,
    year = suppressWarnings(as.integer(format(measured_date, "%Y"))),
    growthForm = vst_chr(vst_field(apparent, c("growthForm"))),
    plantStatus = vst_chr(vst_field(apparent, c("plantStatus"))),
    stemDiameter = vst_num(vst_field(apparent, c("stemDiameter"))),
    basalStemDiameter = vst_num(vst_field(apparent, c("basalStemDiameter"))),
    height = vst_num(vst_field(apparent, c("height"))),
    maxCrownDiameter = vst_num(vst_field(apparent, c("maxCrownDiameter"))),
    ninetyCrownDiameter = vst_num(vst_field(apparent, c("ninetyCrownDiameter"))),
    canopyPosition = vst_chr(vst_field(apparent, c("canopyPosition"))),
    measurementHeight = vst_num(vst_field(apparent, c("measurementHeight"))),
    basalMeasurementHeight = vst_num(vst_field(
      apparent,
      c("basalStemDiameterMsrmntHeight", "basalMeasurementHeight")
    )),
    changedMeasurementLocation = vst_chr(vst_field(
      apparent, c("changedMeasurementLocation")
    )),
    tagStatus = vst_chr(vst_field(apparent, c("tagStatus"))),
    dendrometerCondition = vst_chr(vst_field(apparent, c("dendrometerCondition"))),
    heightQualifier = vst_chr(vst_field(apparent, c("heightQualifier"))),
    dataQF = vst_chr(vst_field(apparent, c("dataQF"))),
    stringsAsFactors = FALSE
  )
  table$live <- grepl("^live", trimws(table$plantStatus), ignore.case = TRUE)
  table$permanent <- grepl("^NEON", table$individualID)
  table$plant_key <- paste(table$plotID, table$individualID, sep = "\r")
  table$event_key <- paste(table$plotID, table$eventID, sep = "\r")
  table$plant_id <- vst_plant_id(table)
  table$superseded_in_event <- vst_superseded_in_event(table)
  table$duplicate_of_uid <- vst_duplicate_of_uid(table)

  # Preserve any additional published QC/qualifier/status columns verbatim.
  qc_names <- grep("(QF$|Qualifier$|Status$|Condition$|quality|flag)",
                   names(apparent), value = TRUE, ignore.case = TRUE)
  qc_names <- setdiff(qc_names, names(table))
  if (length(qc_names)) table <- dplyr::bind_cols(table, apparent[qc_names])

  table$.source_row <- seq_len(nrow(table))
  table <- dplyr::left_join(table, identity, by = c("plotID", "individualID"))
  table <- table[order(table$.source_row), , drop = FALSE]
  table$.source_row <- NULL
  table$mappingMatched <- vst_nonblank(table$mapping_source_uid)
  table$is_species <- vst_is_species_rank(table$taxonRank, table$scientificName)
  table$taxon_label <- ifelse(
    vst_nonblank(table$scientificName), table$scientificName,
    ifelse(vst_nonblank(table$taxonID),
           paste0("Unresolved taxon (", table$taxonID, ")"), "Unresolved taxon")
  )
  table$taxon_resolution <- ifelse(
    table$is_species %in% TRUE & vst_nonblank(table$scientificName),
    "species-level", "coarse-or-unresolved"
  )
  vst_assert_measurement_derivations(table)
  tibble::as_tibble(table)
}

vst_prepare_opportunities <- function(perplot) {
  perplot <- as.data.frame(perplot, stringsAsFactors = FALSE)
  required <- c("uid", "eventID", "plotID")
  missing <- setdiff(required, names(perplot))
  if (length(missing)) {
    stop("vst_perplotperyear lacks required fields: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (any(!vst_nonblank(perplot$uid)) || any(!vst_nonblank(perplot$eventID)) ||
      any(!vst_nonblank(perplot$plotID))) {
    stop("vst_perplotperyear has blank uid, eventID, or plotID", call. = FALSE)
  }
  vst_assert_unique(perplot, "uid", "vst_perplotperyear")

  event_key <- vst_key(perplot, c("eventID", "plotID"))
  group_n <- as.integer(table(event_key)[event_key])
  source <- perplot
  source$source_record_key <- vst_chr(source$uid)
  source$protocol_key_group_n <- group_n
  source$protocol_key_conflict <- group_n > 1L

  date_raw <- vst_field(perplot, c("date", "collectDate", "eventDate"), required = TRUE)
  date_order <- vst_datetime_order(date_raw)
  date_order[!is.finite(date_order)] <- -Inf
  signature <- vst_row_signature(perplot)
  source_uid <- vst_chr(perplot$uid)
  ordered_rows <- order(
    vst_utf8_byte_key(event_key), -date_order,
    vst_utf8_byte_key(signature), vst_utf8_byte_key(source_uid),
    method = "radix", na.last = TRUE
  )
  ordered <- perplot[ordered_rows, , drop = FALSE]
  ordered_key <- event_key[ordered_rows]
  selected_rows <- ordered_rows[!duplicated(ordered_key)]
  selected <- perplot[selected_rows, , drop = FALSE]
  selected_key <- event_key[selected_rows]

  uid_sets <- tapply(source_uid, event_key, function(value) {
    value <- unique(value)
    paste(value[vst_byte_order(value)], collapse = ";")
  })
  selected$opportunity_source_uid <- source_uid[selected_rows]
  selected$opportunity_source_record_count <- as.integer(table(event_key)[selected_key])
  selected$opportunity_key_conflict <- selected$opportunity_source_record_count > 1L
  selected$opportunity_source_uids <- unname(uid_sets[selected_key])
  selected$opportunity_source_missing <- FALSE

  list(
    canonical = selected,
    source = tibble::as_tibble(source)
  )
}

vst_add_measurement_only_contexts <- function(perplot, trees) {
  perplot <- as.data.frame(perplot, stringsAsFactors = FALSE)
  trees <- as.data.frame(trees, stringsAsFactors = FALSE)
  required <- c(
    "eventID", "plotID", "opportunity_source_uid",
    "opportunity_source_record_count", "opportunity_key_conflict",
    "opportunity_source_uids", "opportunity_source_missing"
  )
  missing <- setdiff(required, names(perplot))
  if (length(missing)) {
    stop("canonical opportunity table lacks fields: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!all(c("eventID", "plotID") %in% names(trees))) {
    stop("measurement table lacks plotID or eventID", call. = FALSE)
  }

  source_keys <- unique(vst_key(perplot, c("plotID", "eventID")))
  measurement_keys <- unique(vst_key(trees, c("plotID", "eventID")))
  missing_keys <- setdiff(measurement_keys, source_keys)
  if (length(missing_keys)) {
    measurement_context <- unique(trees[, c("plotID", "eventID"), drop = FALSE])
    measurement_context$.context_key <- vst_key(
      measurement_context, c("plotID", "eventID")
    )
    measurement_context <- measurement_context[
      measurement_context$.context_key %in% missing_keys, , drop = FALSE
    ]
    measurement_context <- measurement_context[
      vst_byte_order(measurement_context$.context_key),
      , drop = FALSE
    ]

    contexts <- perplot[rep(NA_integer_, nrow(measurement_context)), , drop = FALSE]
    contexts$plotID <- vst_chr(measurement_context$plotID)
    contexts$eventID <- vst_chr(measurement_context$eventID)
    contexts$opportunity_source_uid <- NA_character_
    contexts$opportunity_source_record_count <- 0L
    contexts$opportunity_key_conflict <- FALSE
    contexts$opportunity_source_uids <- NA_character_
    contexts$opportunity_source_missing <- TRUE
    perplot <- rbind(perplot, contexts)
  }

  context_key <- vst_key(perplot, c("plotID", "eventID"))
  perplot <- perplot[
    vst_byte_order(context_key, na.last = TRUE),
    , drop = FALSE
  ]
  rownames(perplot) <- NULL
  tibble::as_tibble(perplot)
}

vst_presence_state <- function(x) {
  value <- tolower(trimws(vst_chr(x)))
  out <- rep("unknown", length(value))
  out[value %in% c("n", "no") | grepl("not present|absent", value)] <-
    "absent"
  out[value %in% c("y", "yes") | grepl("^present", value)] <- "present"
  out[is.na(value) | !nzchar(value)] <- "unknown"
  out
}

vst_shrub_presence <- function(trees_present, shrubs_present) {
  tree_state <- vst_presence_state(trees_present)
  shrub_state <- vst_presence_state(shrubs_present)
  # shrub/sapling sampled area also contains the protocol's small-tree class.
  # Only the joint absence of tree and shrub presence is an explicit zero for
  # the entire nested channel; a tree-only presence is otherwise ambiguous.
  ifelse(shrub_state == "present", "present",
         ifelse(shrub_state == "absent" & tree_state == "absent", "absent", "unknown"))
}

vst_count_records <- function(trees, plots, forms) {
  result <- integer(nrow(plots))
  if (!nrow(trees) || !length(forms)) return(result)
  keep <- tolower(trimws(trees$growthForm)) %in% tolower(forms)
  if (!any(keep)) return(result)
  counts <- as.data.frame(table(vst_key(trees[keep, , drop = FALSE], c("plotID", "eventID"))),
                          stringsAsFactors = FALSE)
  names(counts) <- c("key", "n")
  matched <- match(vst_key(plots, c("plotID", "eventID")), counts$key)
  result[!is.na(matched)] <- as.integer(counts$n[matched[!is.na(matched)]])
  result
}

# Contract v3 multi-bole rule (NEON.DOC.000987 Table 8): a multi-bole tree needs
# one bole >= 10 cm DBH; additional boles are tagged 1234a, 1234b ... and are
# measured down to 2.5 cm. A lettered tree-form row is therefore a valid
# secondary bole when its parent tag (letter stripped) has a tree-form row in
# the same plotID x eventID with a finite DBH >= 10 cm, whatever that bole's
# status. Such a row never holds its plot-event; under Option A it still stays
# out of basal area, density and QMD, which count boles 10 cm and over only.
# Unlettered sub-10 rows, lettered rows with no qualifying parent, and missing,
# zero or negative DBH keep the v2 rule and hold the plot-event.
vst_tree_secondary_bole_ok <- function(trees) {
  if (!nrow(trees)) return(logical())
  form <- tolower(trimws(vst_chr(trees$growthForm))) %in% tolower(VST_TREE_FORMS)
  id <- vst_chr(trees$individualID)
  lettered <- !is.na(id) & grepl("[A-Za-z]$", id)
  parent <- sub("[A-Za-z]$", "", id)
  dbh <- vst_num(trees$stemDiameter)
  plot_event <- paste(vst_chr(trees$plotID), vst_chr(trees$eventID), sep = "\r")
  qualifying <- form & is.finite(dbh) & dbh >= 10
  parents <- unique(paste(plot_event, id, sep = "\r")[qualifying])
  form & lettered & paste(plot_event, parent, sep = "\r") %in% parents
}

vst_count_invalid_metric_records <- function(trees, plots, forms, metric,
                                             minimum, secondary_ok = NULL) {
  result <- integer(nrow(plots))
  if (!nrow(trees) || !length(forms)) return(result)
  if (!metric %in% names(trees)) {
    stop("measurement table lacks required channel metric: ", metric,
         call. = FALSE)
  }
  channel_row <- tolower(trimws(trees$growthForm)) %in% tolower(forms)
  live_row <- trees$live %in% TRUE
  value <- vst_num(trees[[metric]])
  valid_metric <- is.finite(value) & value > 0 & value >= minimum
  if (!is.null(secondary_ok)) {
    valid_metric <- valid_metric | (secondary_ok %in% TRUE & is.finite(value) & value > 0)
  }
  keep <- channel_row & live_row & !valid_metric
  if (!any(keep)) return(result)
  counts <- as.data.frame(
    table(vst_key(trees[keep, , drop = FALSE], c("plotID", "eventID"))),
    stringsAsFactors = FALSE
  )
  names(counts) <- c("key", "n")
  matched <- match(vst_key(plots, c("plotID", "eventID")), counts$key)
  result[!is.na(matched)] <- as.integer(counts$n[matched[!is.na(matched)]])
  result
}

vst_count_identity_conflict_keys <- function(trees, plots, forms) {
  result <- integer(nrow(plots))
  required <- c("growthForm", "protocol_key_conflict", "protocol_stem_key")
  if (!nrow(trees) || !length(forms) || !all(required %in% names(trees))) return(result)
  keep <- tolower(trimws(trees$growthForm)) %in% tolower(forms) &
    trees$protocol_key_conflict %in% TRUE
  if (!any(keep)) return(result)
  conflicts <- unique(data.frame(
    event_key = vst_key(trees[keep, , drop = FALSE], c("plotID", "eventID")),
    protocol_stem_key = vst_chr(trees$protocol_stem_key[keep]),
    stringsAsFactors = FALSE
  ))
  counts <- as.data.frame(table(conflicts$event_key), stringsAsFactors = FALSE)
  names(counts) <- c("key", "n")
  matched <- match(vst_key(plots, c("plotID", "eventID")), counts$key)
  result[!is.na(matched)] <- as.integer(counts$n[matched[!is.na(matched)]])
  result
}

# Per-channel note for an event the partial-event curation reviewed (S1).
vst_curation_note <- function(rule_id, complete) {
  ifelse(is.na(rule_id), NA_character_, sprintf(
    "dataCollected=partial reviewed as %s for this channel (%s)",
    ifelse(complete %in% TRUE, "complete", "incomplete"), rule_id
  ))
}

vst_support_status <- function(area, sampling_impractical, data_collected,
                               presence, records, invalid_metric,
                               identity_conflicts = integer(length(area)),
                               opportunity_conflict = logical(length(area)),
                               opportunity_source_missing = logical(length(area)),
                               event_date = rep(as.Date(NA), length(area)),
                               curation_note = rep(NA_character_, length(area))) {
  sampling <- gsub("[^a-z]", "", tolower(trimws(vst_chr(sampling_impractical))))
  collected <- gsub("[^a-z]", "", tolower(trimws(vst_chr(data_collected))))
  sampling[is.na(sampling)] <- ""
  collected[is.na(collected)] <- ""
  day <- as.numeric(vst_date(event_date))
  # Legacy schema (contract v3, cedar A): on or before 2020-03-10, with
  # samplingImpractical blank or OK, dataCollected blank or allGrowthForms, and
  # at least one of the two blank.
  legacy <- is.finite(day) & day <= as.numeric(VST_LEGACY_SCHEMA_END) &
    sampling %in% c("", "ok") & collected %in% c("", "allgrowthforms") &
    (sampling == "" | collected == "") &
    # A reviewed partial event keeps today's path: its published value is
    # "partial", whatever the curation reads for this channel.
    is.na(curation_note)
  identity_reason <- function(n) sprintf(
    "%d channel stem locator%s violate%s plotID + eventID + individualID + tempStemID uniqueness",
    n, if (n == 1L) "" else "s", if (n == 1L) "s" else ""
  )
  metric_reason <- function(n) sprintf(
    "%d live channel record%s lack%s a finite positive threshold-compatible diameter",
    n, if (n == 1L) "" else "s", if (n == 1L) "s" else ""
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
      reason[[i]] <- "multiple published vst_perplotperyear rows share this plot-event key"
    } else if (legacy[[i]] && records[[i]] > 0L) {
      if (!is.finite(area[[i]]) || area[[i]] <= 0) {
        status[[i]] <- "held_missing_area"
        reason[[i]] <- "event-specific total sampled area is missing or non-positive"
      } else if (identical(presence[[i]], "absent")) {
        status[[i]] <- "held_presence_record_conflict"
        reason[[i]] <- "presence says absent but measurement records exist"
      } else if (is.finite(identity_conflicts[[i]]) && identity_conflicts[[i]] > 0L) {
        status[[i]] <- "held_identity_conflict"
        reason[[i]] <- identity_reason(identity_conflicts[[i]])
      } else if (invalid_metric[[i]] > 0L) {
        status[[i]] <- "held_metric_invalid"
        reason[[i]] <- metric_reason(invalid_metric[[i]])
      } else {
        status[[i]] <- VST_LEGACY_STATUS
        reason[[i]] <- VST_LEGACY_REASON
      }
    } else if (!nzchar(sampling[[i]])) {
      status[[i]] <- "held_opportunity_unknown"
      reason[[i]] <- "samplingImpractical is missing"
    } else if (!identical(sampling[[i]], "ok")) {
      status[[i]] <- "held_sampling_impractical"
      reason[[i]] <- paste0("samplingImpractical=", sampling_impractical[[i]])
    } else if (identical(collected[[i]], "dendrometeronly")) {
      status[[i]] <- "held_dendrometer_only"
      reason[[i]] <- "dataCollected=dendrometerOnly cannot scale a plot response"
    } else if (!identical(collected[[i]], "allgrowthforms")) {
      status[[i]] <- "held_opportunity_unknown"
      reason[[i]] <- if (is.na(data_collected[[i]]) || !nzchar(data_collected[[i]]))
        "dataCollected is missing" else paste0("dataCollected=", data_collected[[i]])
    } else if (!is.finite(area[[i]]) || area[[i]] <= 0) {
      status[[i]] <- "held_missing_area"
      reason[[i]] <- "event-specific total sampled area is missing or non-positive"
    } else if (records[[i]] > 0L && identical(presence[[i]], "absent")) {
      status[[i]] <- "held_presence_record_conflict"
      reason[[i]] <- "presence says absent but measurement records exist"
    } else if (is.finite(identity_conflicts[[i]]) && identity_conflicts[[i]] > 0L) {
      status[[i]] <- "held_identity_conflict"
      reason[[i]] <- identity_reason(identity_conflicts[[i]])
    } else if (invalid_metric[[i]] > 0L) {
      status[[i]] <- "held_metric_invalid"
      reason[[i]] <- metric_reason(invalid_metric[[i]])
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
    if (!is.na(curation_note[[i]])) reason[[i]] <- paste0(reason[[i]], "; ", curation_note[[i]])
  }
  data.frame(status = status, reason = reason, stringsAsFactors = FALSE)
}

# Rows per plot-event whose plantStatus is exactly `status` (case-insensitive).
vst_count_status <- function(trees, plots, status) {
  result <- integer(nrow(plots))
  if (!nrow(trees)) return(result)
  keep <- tolower(trimws(vst_chr(trees$plantStatus))) %in% status
  if (!any(keep)) return(result)
  counts <- table(vst_key(trees[keep, , drop = FALSE], c("plotID", "eventID")))
  matched <- match(vst_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}

vst_opportunity_table <- function(perplot, trees, site = NA_character_,
                                  curation = vst_empty_curation()) {
  perplot <- as.data.frame(perplot, stringsAsFactors = FALSE)
  required <- c(
    "eventID", "plotID", "opportunity_source_uid",
    "opportunity_source_record_count", "opportunity_key_conflict",
    "opportunity_source_uids", "opportunity_source_missing"
  )
  missing <- setdiff(required, names(perplot))
  if (length(missing)) {
    stop("vst_perplotperyear lacks required fields: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (any(!vst_nonblank(perplot$eventID)) || any(!vst_nonblank(perplot$plotID))) {
    stop("vst_perplotperyear has blank eventID or plotID", call. = FALSE)
  }
  vst_assert_unique(perplot, c("eventID", "plotID"), "vst_perplotperyear")

  event_date <- vst_date(vst_field(perplot, c("date", "collectDate", "eventDate")))
  plots <- data.frame(
    opportunity_source_uid = vst_chr(perplot$opportunity_source_uid),
    opportunity_source_record_count = as.integer(perplot$opportunity_source_record_count),
    opportunity_key_conflict = perplot$opportunity_key_conflict %in% TRUE,
    opportunity_source_uids = vst_chr(perplot$opportunity_source_uids),
    opportunity_source_missing = perplot$opportunity_source_missing %in% TRUE,
    eventID = vst_chr(perplot$eventID),
    plotID = vst_chr(perplot$plotID),
    date = event_date,
    year = suppressWarnings(as.integer(format(event_date, "%Y"))),
    eventType = vst_chr(vst_field(perplot, c("eventType"))),
    plotType = vst_chr(vst_field(perplot, c("plotType"))),
    nlcdClass = vst_chr(vst_field(perplot, c("nlcdClass"))),
    lat = vst_num(vst_field(perplot, c("decimalLatitude", "latitude", "lat"))),
    lng = vst_num(vst_field(perplot, c("decimalLongitude", "longitude", "lng"))),
    samplingImpractical = vst_chr(vst_field(perplot, c("samplingImpractical"))),
    dataCollected = vst_chr(vst_field(perplot, c("dataCollected"))),
    treesPresent = vst_chr(vst_field(perplot, c("treesPresent", "treePresent"))),
    shrubsPresent = vst_chr(vst_field(perplot, c("shrubsPresent", "shrubPresent"))),
    area_trees = vst_num(vst_field(perplot, c("totalSampledAreaTrees"))),
    area_shrub = vst_num(vst_field(perplot, c("totalSampledAreaShrubSapling"))),
    stringsAsFactors = FALSE
  )
  measurement_key <- vst_key(trees, c("plotID", "eventID"))
  context_key <- vst_key(plots, c("plotID", "eventID"))
  if (nrow(trees)) {
    measurement_count <- table(measurement_key)
    plots$measurement_record_count_all <- as.integer(
      measurement_count[context_key]
    )
    plots$measurement_record_count_all[
      is.na(plots$measurement_record_count_all)
    ] <- 0L
    measurement_date <- vst_date(trees$date)
    measurement_date_number <- as.numeric(measurement_date)
    date_min <- tapply(measurement_date_number, measurement_key, function(value) {
      value <- value[is.finite(value)]
      if (length(value)) min(value) else NA_real_
    })
    date_max <- tapply(measurement_date_number, measurement_key, function(value) {
      value <- value[is.finite(value)]
      if (length(value)) max(value) else NA_real_
    })
    date_n <- tapply(measurement_date_number, measurement_key, function(value) {
      as.integer(length(unique(value[is.finite(value)])))
    })
    plots$measurement_date_min <- as.Date(
      unname(date_min[context_key]), origin = "1970-01-01"
    )
    plots$measurement_date_max <- as.Date(
      unname(date_max[context_key]), origin = "1970-01-01"
    )
    plots$measurement_date_distinct_n <- as.integer(
      unname(date_n[context_key])
    )
    plots$measurement_date_distinct_n[
      is.na(plots$measurement_date_distinct_n)
    ] <- 0L
  } else {
    plots$measurement_record_count_all <- 0L
    plots$measurement_date_min <- as.Date(NA)
    plots$measurement_date_max <- as.Date(NA)
    plots$measurement_date_distinct_n <- 0L
  }
  plots$treePresence <- vst_presence_state(plots$treesPresent)
  plots$shrubSaplingPresence <- vst_shrub_presence(
    plots$treesPresent, plots$shrubsPresent
  )
  plots$tree_records <- vst_count_records(trees, plots, VST_TREE_FORMS)
  plots$shrub_records <- vst_count_records(trees, plots, VST_SHRUB_FORMS)
  plots$small_tree_records <- vst_count_records(trees, plots, VST_SMALL_TREE_FORMS)
  plots$tree_invalid_metric_records <- vst_count_invalid_metric_records(
    trees, plots, VST_TREE_FORMS, "stemDiameter", 10,
    secondary_ok = vst_tree_secondary_bole_ok(trees)
  )
  plots$shrub_invalid_metric_records <- vst_count_invalid_metric_records(
    trees, plots, VST_SHRUB_FORMS, "basalStemDiameter", 0
  )
  plots$tree_identity_conflict_keys <- vst_count_identity_conflict_keys(
    trees, plots, VST_TREE_FORMS
  )
  plots$shrub_identity_conflict_keys <- vst_count_identity_conflict_keys(
    trees, plots, VST_SHRUB_FORMS
  )

  # S1: a reviewed partial event reads as all-growth-forms for each woody
  # channel the remark shows was measured in full; the other channel stays held.
  partial <- vst_match_partial_curation(perplot, site, curation$partial)
  tree_collected <- ifelse(partial$tree_channel_complete, "allGrowthForms",
                           plots$dataCollected)
  shrub_collected <- ifelse(partial$shrub_channel_complete, "allGrowthForms",
                            plots$dataCollected)
  tree_support <- vst_support_status(
    plots$area_trees, plots$samplingImpractical, tree_collected,
    plots$treePresence, plots$tree_records, plots$tree_invalid_metric_records,
    plots$tree_identity_conflict_keys, plots$opportunity_key_conflict,
    plots$opportunity_source_missing, plots$date,
    vst_curation_note(partial$partial_event_rule_id, partial$tree_channel_complete)
  )
  shrub_support <- vst_support_status(
    plots$area_shrub, plots$samplingImpractical, shrub_collected,
    plots$shrubSaplingPresence, plots$shrub_records,
    plots$shrub_invalid_metric_records, plots$shrub_identity_conflict_keys,
    plots$opportunity_key_conflict, plots$opportunity_source_missing, plots$date,
    vst_curation_note(partial$partial_event_rule_id, partial$shrub_channel_complete)
  )
  plots$tree_support <- tree_support$status
  plots$tree_support_reason <- tree_support$reason
  plots$shrub_support <- shrub_support$status
  plots$shrub_support_reason <- shrub_support$reason
  plots$tree_supported <- plots$tree_support %in% VST_SUPPORTED_STATUS
  plots$shrub_supported <- plots$shrub_support %in% VST_SUPPORTED_STATUS
  plots$event_key <- paste(plots$plotID, plots$eventID, sep = "\r")
  plots$partial_event_rule_id <- partial$partial_event_rule_id
  plots$tree_channel_complete <- partial$tree_channel_complete
  plots$shrub_channel_complete <- partial$shrub_channel_complete
  # S2: a reviewed disturbance label; it never changes support.
  disturbance <- vst_match_disturbance_curation(perplot, site, curation$disturbance)
  plots$disturbance_classes <- disturbance$disturbance_classes
  plots$disturbance_curation_id <- disturbance$disturbance_curation_id
  plots$n_status_lost_burned <- vst_count_status(trees, plots, "lost, burned")
  plots$n_status_removed <- vst_count_status(trees, plots, "removed")

  # Keep every additional opportunity/status/area field with its published name.
  opportunity_names <- grep(
    "(Present$|samplingImpractical$|dataCollected$|eventType$|totalSampledArea|nestedSubplotArea|CollectDate$|QF$)",
    names(perplot), value = TRUE, ignore.case = TRUE
  )
  opportunity_names <- setdiff(opportunity_names, names(plots))
  if (length(opportunity_names)) {
    plots <- dplyr::bind_cols(plots, perplot[opportunity_names])
  }
  tibble::as_tibble(plots)
}

vst_latest_supported_events <- function(plots, channel) {
  support_column <- if (identical(channel, "tree_dbh")) "tree_support" else "shrub_support"
  selected <- plots[plots[[support_column]] %in% VST_SUPPORTED_STATUS, , drop = FALSE]
  if (!nrow(selected)) return(selected)
  date_order <- as.numeric(selected$date)
  date_order[!is.finite(date_order)] <- -Inf
  selected <- selected[
    order(selected$plotID, -date_order, -xtfrm(selected$eventID), na.last = TRUE),
    , drop = FALSE
  ]
  selected[!duplicated(selected$plotID), , drop = FALSE]
}

vst_channel_spec <- function(channel) {
  if (identical(channel, "tree_dbh")) {
    return(list(
      channel = channel, forms = VST_TREE_FORMS, diameter = "stemDiameter",
      area = "area_trees", support = "tree_support", minimum = 10,
      metric = "bole-DBH basal area, boles 10 cm and over (breast height)"
    ))
  }
  list(
    channel = "shrub_sapling_basal", forms = VST_SHRUB_FORMS,
    diameter = "basalStemDiameter", area = "area_shrub",
    support = "shrub_support", minimum = 0,
    metric = "basal-diameter cover (stem base)"
  )
}

vst_channel_snapshot <- function(trees, plots, channel) {
  spec <- vst_channel_spec(channel)
  # Shared size screen (R/size_plausibility.R) over the plant's full history,
  # per measure: a failing height leaves only the tallest extreme, a failing
  # diameter only the biggest. Every other summary keeps every row.
  if (nrow(trees)) {
    screen <- vst_size_screen(trees)
    trees$.size_height_ok <- screen$size_height_ok
    trees$.size_diameter_ok <- screen$size_diameter_ok
  }
  opportunities <- vst_latest_supported_events(plots, spec$channel)
  if (!nrow(opportunities)) {
    return(list(spec = spec, opportunities = opportunities,
                records = trees[0, , drop = FALSE]))
  }
  event_keys <- vst_key(opportunities, c("plotID", "eventID"))
  # S4: superseded and duplicate rows stay in the bundle but never count.
  records <- trees[
    vst_key(trees, c("plotID", "eventID")) %in% event_keys &
      tolower(trimws(trees$growthForm)) %in% tolower(spec$forms) &
      !(trees$superseded_in_event %in% TRUE) & is.na(trees$duplicate_of_uid), , drop = FALSE
  ]
  records$.diameter_cm <- vst_num(records[[spec$diameter]])
  records <- records[
    records$live %in% TRUE & is.finite(records$.diameter_cm) &
      records$.diameter_cm > 0 & records$.diameter_cm >= spec$minimum, , drop = FALSE
  ]
  records$.basal_cross_section_m2 <- pi * (records$.diameter_cm / 200)^2
  records <- dplyr::left_join(
    records,
    opportunities[, c("plotID", "eventID", spec$area), drop = FALSE],
    by = c("plotID", "eventID")
  )
  names(records)[names(records) == spec$area] <- ".sampled_area_m2"
  list(spec = spec, opportunities = opportunities, records = records)
}

vst_taxa_index <- function(snapshot, site) {
  records <- snapshot$records
  opportunities <- snapshot$opportunities
  spec <- snapshot$spec
  if (!nrow(records)) return(data.frame())
  records <- records[vst_nonblank(records$taxon_label), , drop = FALSE]
  if (!nrow(records)) return(data.frame())
  n_supported <- dplyr::n_distinct(opportunities$plotID)
  if (!n_supported) return(data.frame())

  by_plot <- records %>%
    dplyr::group_by(.data$taxon_label, .data$plotID) %>%
    dplyr::summarise(
      # Row-wise first: a later summarise expression would read the modal
      # scientificName instead of each row's (neon-suite lint L13).
      is_species = any(.data$is_species %in% TRUE & vst_nonblank(.data$scientificName)),
      scientificName = vst_mode_chr(.data$scientificName),
      taxonID = vst_mode_chr(.data$taxonID),
      taxonRank = vst_mode_chr(.data$taxonRank),
      family = vst_mode_chr(.data$family),
      stems = dplyr::n(),
      individuals = dplyr::n_distinct(paste(.data$plotID, .data$plant_id, sep = "\r")),
      basal_m2 = sum(.data$.basal_cross_section_m2, na.rm = TRUE),
      area_m2 = dplyr::first(.data$.sampled_area_m2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(plot_basal_m2_ha = .data$basal_m2 / .data$area_m2 * 10000)

  result <- by_plot %>%
    dplyr::group_by(.data$taxon_label) %>%
    dplyr::summarise(
      scientificName = vst_mode_chr(.data$scientificName),
      taxonID = vst_mode_chr(.data$taxonID),
      taxonRank = vst_mode_chr(.data$taxonRank),
      is_species = any(.data$is_species %in% TRUE),
      family = vst_mode_chr(.data$family),
      n_stems = sum(.data$stems),
      n_individuals = sum(.data$individuals),
      n_occurrence_plots = dplyr::n_distinct(.data$plotID),
      mean_plot_basal_m2_ha = sum(.data$plot_basal_m2_ha, na.rm = TRUE) / n_supported,
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      taxon_label = .data$taxon_label,
      scientificName = .data$scientificName,
      taxonID = .data$taxonID,
      taxonRank = .data$taxonRank,
      is_species = .data$is_species,
      family = .data$family,
      site = site,
      contract_id = VST_CONTRACT_ID,
      channel = spec$channel,
      metric_kind = spec$metric,
      n_stems = as.integer(.data$n_stems),
      n_individuals = as.integer(.data$n_individuals),
      n_occurrence_plots = as.integer(.data$n_occurrence_plots),
      n_supported_plots = as.integer(n_supported),
      mean_plot_basal_m2_ha = .data$mean_plot_basal_m2_ha,
      ba_m2_ha = .data$mean_plot_basal_m2_ha,
      inference_scope = "mean across latest supported sampled plot events; explicit absences are zero"
    )
  dated_records <- records[is.finite(records$year), , drop = FALSE]
  if (nrow(dated_records)) {
    ranges <- dated_records %>%
      dplyr::group_by(.data$taxon_label) %>%
      dplyr::summarise(
        year_min = min(.data$year), year_max = max(.data$year), .groups = "drop"
      )
    result <- dplyr::left_join(result, ranges, by = "taxon_label")
  } else {
    result$year_min <- NA_integer_
    result$year_max <- NA_integer_
  }
  result
}

vst_channel_summary <- function(snapshot) {
  opportunities <- snapshot$opportunities
  records <- snapshot$records
  spec <- snapshot$spec
  if (!nrow(opportunities)) {
    return(list(
      channel = spec$channel, n_supported_plots = 0L, n_record_plots = 0L,
      n_stems = 0L, n_individuals = 0L, n_species = 0L,
      n_taxa = 0L, n_sampled_absence = 0L,
      ba_ha = NA_real_, density_ha = NA_real_, qmd_cm = NA_real_,
      metric_kind = spec$metric,
      tallest_m = NA_real_, biggest_diam_cm = NA_real_
    ))
  }
  per_plot <- opportunities[, c("plotID", "eventID", spec$area, spec$support), drop = FALSE]
  names(per_plot)[names(per_plot) == spec$area] <- ".sampled_area_m2"
  names(per_plot)[names(per_plot) == spec$support] <- ".support_status"
  if (nrow(records)) {
    observed <- records %>%
      dplyr::group_by(.data$plotID, .data$eventID) %>%
      dplyr::summarise(
        stems = dplyr::n(),
        basal_m2 = sum(.data$.basal_cross_section_m2, na.rm = TRUE),
        sum_d2 = sum(.data$.diameter_cm^2, na.rm = TRUE),
        .groups = "drop"
      )
    per_plot <- dplyr::left_join(per_plot, observed, by = c("plotID", "eventID"))
  } else {
    per_plot$stems <- NA_integer_
    per_plot$basal_m2 <- NA_real_
    per_plot$sum_d2 <- NA_real_
  }
  for (field in c("stems", "basal_m2", "sum_d2"))
    per_plot[[field]][is.na(per_plot[[field]])] <- 0
  per_plot$ba_ha <- per_plot$basal_m2 / per_plot$.sampled_area_m2 * 10000
  per_plot$density_ha <- per_plot$stems / per_plot$.sampled_area_m2 * 10000
  total_stems <- sum(per_plot$stems)
  list(
    channel = spec$channel,
    n_supported_plots = dplyr::n_distinct(opportunities$plotID),
    n_record_plots = dplyr::n_distinct(records$plotID),
    n_stems = nrow(records),
    # C1: plants, not boles: a lettered secondary bole counts with its parent.
    n_individuals = dplyr::n_distinct(paste(records$plotID, records$plant_id, sep = "\r")),
    n_species = dplyr::n_distinct(records$scientificName[
      records$is_species %in% TRUE & vst_nonblank(records$scientificName)
    ]),
    n_taxa = dplyr::n_distinct(records$taxon_label[vst_nonblank(records$taxon_label)]),
    n_sampled_absence = sum(per_plot$.support_status == "sampled_absence"),
    ba_ha = mean(per_plot$ba_ha),
    density_ha = mean(per_plot$density_ha),
    # Quadratic mean diameter from the equal-plot means of basal area and
    # density (Curtis & Marshall 2000), the same rule the app uses.
    qmd_cm = if (total_stems > 0 && mean(per_plot$density_ha) > 0)
      sqrt(40000 * mean(per_plot$ba_ha) / (pi * mean(per_plot$density_ha))) else NA_real_,
    metric_kind = spec$metric,
    tallest_m = vst_safe_max(records$height[records$.size_height_ok %in% TRUE]),
    biggest_diam_cm = vst_safe_max(records$.diameter_cm[records$.size_diameter_ok %in% TRUE])
  )
}

vst_contract_payload <- function(site, trees, plots, latitude, longitude,
                                 curation = vst_empty_curation()) {
  snapshots <- list(
    tree_dbh = vst_channel_snapshot(trees, plots, "tree_dbh"),
    shrub_sapling_basal = vst_channel_snapshot(trees, plots, "shrub_sapling_basal")
  )
  summaries <- lapply(snapshots, vst_channel_summary)
  record_plots <- vapply(summaries, `[[`, integer(1), "n_record_plots")
  supported_plots <- vapply(summaries, `[[`, integer(1), "n_supported_plots")
  tree <- summaries$tree_dbh
  tree_default <- tree$n_record_plots >= VST_PRIMARY_TREE_MIN_PLOTS &&
    tree$n_record_plots >= VST_PRIMARY_TREE_MIN_SHARE * tree$n_supported_plots
  primary <- if (all(record_plots == 0L) && all(supported_plots == 0L)) {
    "unavailable"
  } else if (tree_default) {
    "tree_dbh"
  } else if (all(record_plots == 0L) &&
             supported_plots[["shrub_sapling_basal"]] > supported_plots[["tree_dbh"]]) {
    "shrub_sapling_basal"
  } else if (record_plots[["shrub_sapling_basal"]] > record_plots[["tree_dbh"]]) {
    "shrub_sapling_basal"
  } else {
    "tree_dbh"
  }
  chosen <- if (identical(primary, "unavailable")) summaries$tree_dbh else summaries[[primary]]
  measurement_only <- plots$opportunity_source_missing %in% TRUE
  measurement_only_keys <- vst_key(
    plots[measurement_only, , drop = FALSE], c("plotID", "eventID")
  )
  measurement_rows_without_source <- if (length(measurement_only_keys)) {
    sum(vst_key(trees, c("plotID", "eventID")) %in% measurement_only_keys)
  } else {
    0L
  }
  structure_type <- if (identical(primary, "shrub_sapling_basal")) {
    "shrubland"
  } else if (identical(primary, "tree_dbh")) {
    "forest"
  } else {
    "unknown"
  }
  unavailable <- identical(primary, "unavailable")
  site_index <- data.frame(
    site = site,
    contract_id = VST_CONTRACT_ID,
    primary_channel = primary,
    structure_type = structure_type,
    size_metric = if (identical(primary, "shrub_sapling_basal")) {
      "basal diameter"
    } else if (identical(primary, "tree_dbh")) {
      "DBH"
    } else {
      "unavailable"
    },
    n_supported_plots = if (unavailable) NA_integer_ else as.integer(chosen$n_supported_plots),
    n_record_plots = if (unavailable) NA_integer_ else as.integer(chosen$n_record_plots),
    n_stems = if (unavailable) NA_integer_ else as.integer(chosen$n_stems),
    n_individuals = if (unavailable) NA_integer_ else as.integer(chosen$n_individuals),
    n_species = if (unavailable) NA_integer_ else as.integer(chosen$n_species),
    n_taxa = if (unavailable) NA_integer_ else as.integer(chosen$n_taxa),
    n_sampled_absence = if (unavailable) NA_integer_ else as.integer(chosen$n_sampled_absence),
    ba_ha = if (unavailable) NA_real_ else chosen$ba_ha,
    density_ha = if (unavailable) NA_real_ else chosen$density_ha,
    qmd_cm = if (unavailable) NA_real_ else chosen$qmd_cm,
    metric_kind = if (unavailable) "unavailable" else chosen$metric_kind,
    support_status = if (chosen$n_supported_plots > 0L) "supported_sampled_context" else "held_no_supported_event",
    tallest_m = if (unavailable) NA_real_ else chosen$tallest_m,
    biggest_diam_cm = if (unavailable) NA_real_ else chosen$biggest_diam_cm,
    n_trees = if (unavailable) NA_integer_ else as.integer(chosen$n_individuals),
    n_plots = if (unavailable) NA_integer_ else as.integer(chosen$n_supported_plots),
    n_measurement_only_contexts = as.integer(sum(measurement_only)),
    n_measurement_records_without_opportunity_source = as.integer(
      measurement_rows_without_source
    ),
    lat = latitude,
    lng = longitude,
    inference_scope = "latest supported event per sampled plot; not a site-wide census",
    stringsAsFactors = FALSE
  )
  taxa <- dplyr::bind_rows(
    vst_taxa_index(snapshots$tree_dbh, site),
    vst_taxa_index(snapshots$shrub_sapling_basal, site)
  )
  list(
    id = VST_CONTRACT_ID,
    version = 3L,
    product = VST_PRODUCT,
    release = VST_RELEASE,
    plant_key = c("plotID", "individualID"),
    plant_id_key = c("plotID", "plant_id"),
    event_key = c("plotID", "eventID"),
    source_record_key = "source_uid",
    mapping_source_record_key = "mapping_source_uid",
    protocol_stem_locator = c("plotID", "eventID", "individualID", "tempStemID"),
    opportunity_source_record_key = "source_record_key",
    support_status = list(
      supported = VST_SUPPORTED_STATUS,
      zero = "sampled_absence",
      held = VST_HELD_STATUS,
      history_only = VST_LEGACY_STATUS
    ),
    legacy_schema = list(
      status = VST_LEGACY_STATUS, on_or_before = format(VST_LEGACY_SCHEMA_END),
      reason = VST_LEGACY_REASON,
      admitted_by = "change history only (growth, mortality, plant careers, QC); never the latest supported snapshot, indexes or default channel"
    ),
    row_exclusions = list(
      superseded_in_event = "an earlier-dated row of the same individual in the same event; small-tree, sapling and shrub rows of a stem absent from the latest date and at most 3 days earlier are kept",
      duplicate_of_uid = "a tree-form row repeating growthForm, plantStatus, stemDiameter and height for the same individual, event and date; the copy first in source_uid byte order is kept",
      scope = "rows stay in trees and exports; snapshot and history summaries skip them; support counts include them"
    ),
    curation = vst_curation_receipt(curation),
    channel_summary = summaries,
    primary_channel_reason = "presentation default is tree DBH when trees are recorded in at least 5 latest supported plots and in at least half of the supported tree plots; otherwise the channel represented in more latest supported record-bearing plots; if neither has records, supported opportunity count with tree DBH as the deterministic tie-break; channels remain separate",
    excluded_from_channel_summaries = list(
      small_tree = "preserved in trees; nested-area DBH needs its own physical channel and is not pooled with full-plot tree DBH or basal-diameter cover"
    ),
    inference_scope = "conditional summaries of latest supported sampled plot events",
    index = list(site = site_index, taxa = taxa)
  )
}

vst_build_site_from_tables <- function(site, raw, source_receipt = NULL,
                                       curation = vst_read_curation()) {
  required_tables <- c(
    "vst_mappingandtagging", "vst_apparentindividual", "vst_perplotperyear"
  )
  missing <- setdiff(required_tables, names(raw))
  if (length(missing)) {
    stop("raw bundle lacks required tables: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  identity <- vst_latest_mapping(raw$vst_mappingandtagging)
  trees <- vst_measurement_table(raw$vst_apparentindividual, identity)
  opportunity <- vst_prepare_opportunities(raw$vst_perplotperyear)
  contexts <- vst_add_measurement_only_contexts(opportunity$canonical, trees)
  measurement_only_keys <- vst_key(
    contexts[contexts$opportunity_source_missing %in% TRUE, , drop = FALSE],
    c("plotID", "eventID")
  )
  trees$opportunity_source_missing <- vst_key(
    trees, c("plotID", "eventID")
  ) %in% measurement_only_keys
  plots <- vst_opportunity_table(contexts, trees, site, curation)
  orphan_events <- setdiff(
    unique(vst_key(trees, c("plotID", "eventID"))),
    unique(vst_key(plots, c("plotID", "eventID")))
  )
  if (length(orphan_events)) {
    stop(sprintf(
      "internal error: %d measurement plot/event keys lack plot-event context rows",
      length(orphan_events)
    ), call. = FALSE)
  }
  latitude <- vst_safe_median(plots$lat)
  longitude <- vst_safe_median(plots$lng)
  contract <- vst_contract_payload(site, trees, plots, latitude, longitude, curation)
  meta <- list(
    site = site,
    product = VST_PRODUCT,
    release = VST_RELEASE,
    contract_id = VST_CONTRACT_ID,
    primary_channel = contract$index$site$primary_channel[[1L]],
    structure_type = contract$index$site$structure_type[[1L]],
    structure_type_scope = "presentation compatibility only; physical channels are never pooled or ranked",
    lat = latitude,
    lng = longitude,
    years = sort(unique(trees$year[is.finite(trees$year)]))
  )
  meta$n_measurement_only_contexts <- as.integer(
    sum(plots$opportunity_source_missing %in% TRUE)
  )
  missing_context_keys <- vst_key(
    plots[plots$opportunity_source_missing %in% TRUE, , drop = FALSE],
    c("plotID", "eventID")
  )
  meta$n_measurement_records_without_opportunity_source <- as.integer(
    sum(vst_key(trees, c("plotID", "eventID")) %in% missing_context_keys)
  )
  if (!is.null(source_receipt)) meta$source_receipt <- source_receipt
  list(
    trees = trees,
    plots = plots,
    # NEON publishes field-staff identities in measuredBy/recordedBy; the app
    # never uses them, so contract v3 does not republish them.
    opportunity_source = opportunity$source[
      , setdiff(names(opportunity$source), VST_PERSONNEL_FIELDS), drop = FALSE
    ],
    meta = meta,
    contract = contract
  )
}

vst_bundle_main <- function() {
  raw_dir <- Sys.getenv("VST_RAW_DIR", unset = "../veg-data-fetch")
  site_dir <- Sys.getenv("VST_SITE_OUT_DIR", unset = "data/sites")
  sample_dir <- Sys.getenv("VST_SAMPLE_OUT_DIR", unset = "data-sample")
  site_index_out <- Sys.getenv("VST_SITE_INDEX_OUT", unset = "data/site_index.rds")
  sites <- vst_assert_site_inventory(raw_dir, suffix = "_raw.rds", label = "raw source")
  source_receipt <- vst_source_receipt_from_env()
  curation <- vst_read_curation()
  stray <- setdiff(c(curation$partial$site, curation$disturbance$site), sites)
  if (length(stray)) {
    stop("curation rows name sites outside the build: ", paste(sort(unique(stray)), collapse = ", "),
         call. = FALSE)
  }
  demo <- "SRER"

  dir.create(site_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(sample_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(site_index_out), showWarnings = FALSE, recursive = TRUE)
  if (length(list.files(site_dir, pattern = "[.]rds$", full.names = TRUE))) {
    stop("VST_SITE_OUT_DIR must be empty; build candidates in isolated staging",
         call. = FALSE)
  }
  if (length(list.files(sample_dir, pattern = "[.]rds$", full.names = TRUE))) {
    stop("VST_SAMPLE_OUT_DIR must be empty; build candidates in isolated staging",
         call. = FALSE)
  }

  index_rows <- list()
  latest_dates <- character()
  for (site in sites) {
    cat("=== bundling", site, "===\n")
    raw <- readRDS(file.path(raw_dir, paste0(site, "_raw.rds")))
    bundle <- vst_build_site_from_tables(site, raw, source_receipt, curation)
    path <- file.path(site_dir, paste0(site, ".rds"))
    saveRDS(bundle, path, compress = "xz")
    if (identical(site, demo)) {
      saveRDS(bundle, file.path(sample_dir, "demo.rds"), compress = "xz")
    }
    index_rows[[site]] <- bundle$contract$index$site
    dates <- suppressWarnings(as.Date(bundle$trees$date))
    if (any(!is.na(dates))) latest_dates <- c(latest_dates, as.character(max(dates, na.rm = TRUE)))
    cat(sprintf(
      "  %s: %d measurement rows; %d plot-event contexts; primary=%s; size=%s\n",
      site, nrow(bundle$trees), nrow(bundle$plots), bundle$meta$primary_channel,
      format(file.size(path), big.mark = ",")
    ))
  }

  index <- dplyr::bind_rows(index_rows)
  if (nrow(index) != length(VST_EXPECTED_SITES) ||
      !identical(sort(index$site), sort(VST_EXPECTED_SITES))) {
    stop("candidate index does not cover the registered site family", call. = FALSE)
  }
  attr(index, "contract_id") <- VST_CONTRACT_ID
  if (length(latest_dates)) attr(index, "data_through") <- max(latest_dates)
  if (!is.null(source_receipt)) attr(index, "source_receipt") <- source_receipt
  saveRDS(index, site_index_out, compress = "xz")
  vst_assert_site_inventory(site_dir)
  cat(sprintf("COMPLETE: %d event-keyed bundles under contract %s\n",
              length(sites), VST_CONTRACT_ID))
}

if (!identical(tolower(Sys.getenv("VST_BUNDLE_FUNCTIONS_ONLY", unset = "false")),
               "true")) {
  vst_bundle_main()
}
