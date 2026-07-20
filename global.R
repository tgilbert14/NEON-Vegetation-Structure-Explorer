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

# ---- bundled per-site data: trees, plots, opportunity source, metadata ----
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
  "raw_source_digest", "neon_utilities_version", "source_normalization",
  "built_at", "builder_commit"
)
VEG_CONTEXT_DERIVED_FIELDS <- c(
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

.veg_scalar_chr <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  trimws(as.character(x))
}
.veg_exact_integer <- function(x) {
  value <- suppressWarnings(as.numeric(as.character(x)))
  ok <- !any(!is.finite(value)) && !any(value < 0) &&
    !any(value != floor(value)) && !any(value > .Machine$integer.max)
  list(ok = ok, value = if (ok) as.integer(value) else rep(NA_integer_, length(x)))
}
.veg_date_number <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x))
  suppressWarnings(as.numeric(as.Date(substr(as.character(x), 1L, 10L))))
}
.veg_key <- function(data, columns) {
  parts <- lapply(columns, function(column) {
    value <- as.character(data[[column]])
    value[is.na(value)] <- "<NA>"
    paste0(nchar(value), ":", value)
  })
  do.call(paste, c(parts, sep = "\u001f"))
}
.veg_nonblank <- function(value) {
  !is.na(value) & nzchar(trimws(as.character(value)))
}
.veg_expected_species <- function(rank, scientific_name) {
  rank <- tolower(trimws(as.character(rank)))
  scientific_name <- as.character(scientific_name)
  accepted <- !is.na(rank) & rank %in% c(
    "species", "subspecies", "variety", "form"
  )
  named <- .veg_nonblank(scientific_name)
  safe_name <- ifelse(is.na(scientific_name), "", scientific_name)
  ambiguous <- grepl("\\bsp\\.?$", safe_name, ignore.case = TRUE) |
    grepl("/", safe_name, fixed = TRUE)
  accepted & named & !ambiguous
}
.veg_expected_taxon_label <- function(scientific_name, taxon_id) {
  scientific_name <- as.character(scientific_name)
  taxon_id <- as.character(taxon_id)
  ifelse(
    .veg_nonblank(scientific_name), scientific_name,
    ifelse(
      .veg_nonblank(taxon_id),
      paste0("Unresolved taxon (", taxon_id, ")"),
      "Unresolved taxon"
    )
  )
}
.veg_row_derivation_problems <- function(trees) {
  required <- c(
    "date", "year", "plotID", "eventID", "individualID", "plantStatus",
    "live", "permanent", "plant_key", "event_key", "mapping_source_uid",
    "mappingMatched", "scientificName", "taxonID", "taxonRank",
    "is_species", "taxon_label", "taxon_resolution"
  )
  missing <- setdiff(required, names(trees))
  if (length(missing)) {
    return(paste0("missing row-derivation fields: ", paste(missing, collapse = ",")))
  }
  expected_date <- .veg_source_date(trees$date)
  expected_year <- suppressWarnings(as.integer(format(expected_date, "%Y")))
  expected_live <- grepl(
    "^live", trimws(as.character(trees$plantStatus)), ignore.case = TRUE
  )
  expected_permanent <- grepl("^NEON", as.character(trees$individualID))
  expected_species <- .veg_expected_species(
    trees$taxonRank, trees$scientificName
  )
  expected_label <- .veg_expected_taxon_label(
    trees$scientificName, trees$taxonID
  )
  expected_resolution <- ifelse(
    expected_species & .veg_nonblank(trees$scientificName),
    "species-level", "coarse-or-unresolved"
  )
  mapping_matched <- .veg_nonblank(trees$mapping_source_uid)
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
    mapping_plants <- paste(
      trees$plotID[mapping_matched], trees$individualID[mapping_matched],
      sep = "\r"
    )
    uid_to_plant <- tapply(mapping_plants, mapping_uid, function(value) {
      length(unique(value))
    })
    plant_to_uid <- tapply(mapping_uid, mapping_plants, function(value) {
      length(unique(value))
    })
    if (any(uid_to_plant != 1L) || any(plant_to_uid != 1L)) {
      problems <- c(problems, "mapping_source_uid traceability")
    }
  }
  unique(problems)
}
.veg_utf8_byte_key <- function(value) {
  value <- enc2utf8(as.character(value))
  vapply(value, function(item) {
    if (is.na(item)) return(NA_character_)
    paste(sprintf("%02x", as.integer(charToRaw(item))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}
.veg_byte_order <- function(value) {
  order(.veg_utf8_byte_key(value), method = "radix", na.last = TRUE)
}
.veg_source_field <- function(data, candidates, default = NA) {
  found <- intersect(candidates, names(data))
  if (length(found)) data[[found[[1L]]]] else rep(default, nrow(data))
}
.veg_source_date <- function(value) {
  if (inherits(value, "Date")) return(value)
  if (inherits(value, "POSIXt")) return(as.Date(value))
  suppressWarnings(as.Date(substr(as.character(value), 1L, 10L)))
}
.veg_selected_source_parity <- function(contexts, source) {
  contexts <- as.data.frame(contexts, stringsAsFactors = FALSE)
  source <- as.data.frame(source, stringsAsFactors = FALSE)
  required_context <- c(
    "plotID", "eventID", "opportunity_source_uid",
    "opportunity_source_missing"
  )
  required_source <- c("source_record_key", "plotID", "eventID")
  missing_context <- setdiff(required_context, names(contexts))
  missing_source <- setdiff(required_source, names(source))
  missing <- c(
    if (length(missing_context)) paste0("context:", missing_context),
    if (length(missing_source)) paste0("source:", missing_source)
  )
  if (length(missing)) return(list(ok = FALSE, fields = missing))

  source_missing <- as.logical(contexts$opportunity_source_missing)
  backed <- !is.na(source_missing) & !source_missing
  selected_index <- match(
    as.character(contexts$opportunity_source_uid),
    as.character(source$source_record_key)
  )
  bad <- character(0)
  if (any(is.na(source_missing))) bad <- c(bad, "opportunity_source_missing")
  if (any(backed & is.na(selected_index))) bad <- c(bad, "opportunity_source_uid")
  if (!any(backed) || any(backed & is.na(selected_index))) {
    return(list(ok = !length(bad), fields = unique(bad)))
  }

  selected <- source[selected_index[backed], , drop = FALSE]
  compare <- function(field, expected, kind = "character") {
    if (!field %in% names(contexts)) {
      bad <<- c(bad, field)
      return(invisible(NULL))
    }
    actual <- contexts[[field]][backed]
    equal <- switch(
      kind,
      date = inherits(actual, "Date") && inherits(expected, "Date") &&
        identical(as.numeric(actual), as.numeric(expected)),
      integer = is.integer(actual) &&
        identical(as.integer(actual), as.integer(expected)),
      numeric = is.numeric(actual) &&
        identical(as.numeric(actual), as.numeric(expected)),
      logical = is.logical(actual) &&
        identical(as.logical(actual), as.logical(expected)),
      identical(as.character(actual), as.character(expected))
    )
    if (!isTRUE(equal)) bad <<- c(bad, field)
    invisible(NULL)
  }

  selected_date <- .veg_source_date(.veg_source_field(
    selected, c("date", "collectDate", "eventDate")
  ))
  compare("plotID", selected$plotID)
  compare("eventID", selected$eventID)
  compare("date", selected_date, "date")
  compare(
    "year", suppressWarnings(as.integer(format(selected_date, "%Y"))),
    "integer"
  )
  compare("eventType", .veg_source_field(selected, "eventType"))
  compare("plotType", .veg_source_field(selected, "plotType"))
  compare("nlcdClass", .veg_source_field(selected, "nlcdClass"))
  compare(
    "lat", suppressWarnings(as.numeric(.veg_source_field(
      selected, c("decimalLatitude", "latitude", "lat")
    ))), "numeric"
  )
  compare(
    "lng", suppressWarnings(as.numeric(.veg_source_field(
      selected, c("decimalLongitude", "longitude", "lng")
    ))), "numeric"
  )
  compare(
    "samplingImpractical",
    .veg_source_field(selected, "samplingImpractical")
  )
  compare("dataCollected", .veg_source_field(selected, "dataCollected"))
  compare(
    "treesPresent",
    .veg_source_field(selected, c("treesPresent", "treePresent"))
  )
  compare(
    "shrubsPresent",
    .veg_source_field(selected, c("shrubsPresent", "shrubPresent"))
  )
  compare(
    "area_trees", suppressWarnings(as.numeric(.veg_source_field(
      selected, "totalSampledAreaTrees"
    ))), "numeric"
  )
  compare(
    "area_shrub", suppressWarnings(as.numeric(.veg_source_field(
      selected, "totalSampledAreaShrubSapling"
    ))), "numeric"
  )

  mapped_outputs <- c(
    "plotID", "eventID", "date", "year", "eventType", "plotType",
    "nlcdClass", "lat", "lng", "samplingImpractical", "dataCollected",
    "treesPresent", "shrubsPresent", "area_trees", "area_shrub"
  )
  direct_fields <- setdiff(intersect(names(selected), names(contexts)), c(
    mapped_outputs, "uid", "source_record_key", "protocol_key_group_n",
    "protocol_key_conflict"
  ))
  for (field in direct_fields) {
    expected <- selected[[field]]
    kind <- if (inherits(expected, "Date")) {
      "date"
    } else if (is.integer(expected)) {
      "integer"
    } else if (is.numeric(expected)) {
      "numeric"
    } else if (is.logical(expected)) {
      "logical"
    } else {
      "character"
    }
    compare(field, expected, kind)
  }
  list(ok = !length(bad), fields = unique(bad))
}

# Recompute the support algebra from the preserved measurement and opportunity
# rows inside the deployed runtime. These helpers intentionally do not source the
# candidate builder: a bundle is not trusted merely because its stored support
# fields were produced by the same code that wrote it.
.veg_event_counts <- function(trees, plots, keep) {
  result <- integer(nrow(plots))
  keep[is.na(keep)] <- FALSE
  if (!nrow(trees) || !any(keep)) return(result)
  counts <- table(.veg_key(
    trees[keep, , drop = FALSE], c("plotID", "eventID")
  ))
  matched <- match(.veg_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}

.veg_identity_conflict_counts <- function(trees, plots, keep) {
  result <- integer(nrow(plots))
  keep[is.na(keep)] <- FALSE
  if (!nrow(trees) || !any(keep) ||
      !all(c("protocol_key_conflict", "protocol_stem_key") %in% names(trees))) {
    return(result)
  }
  keep <- keep & trees$protocol_key_conflict %in% TRUE
  if (!any(keep)) return(result)
  rows <- unique(data.frame(
    event_key = .veg_key(
      trees[keep, , drop = FALSE], c("plotID", "eventID")
    ),
    protocol_stem_key = as.character(trees$protocol_stem_key[keep]),
    stringsAsFactors = FALSE
  ))
  counts <- table(rows$event_key)
  matched <- match(.veg_key(plots, c("plotID", "eventID")), names(counts))
  result[!is.na(matched)] <- as.integer(counts[matched[!is.na(matched)]])
  result
}

.veg_presence_state <- function(value) {
  value <- tolower(trimws(as.character(value)))
  result <- rep("unknown", length(value))
  result[value %in% c("n", "no") | grepl("not present|absent", value)] <-
    "absent"
  result[value %in% c("y", "yes") | grepl("^present", value)] <- "present"
  result[is.na(value) | !nzchar(value)] <- "unknown"
  result
}

.veg_shrub_presence_state <- function(trees_present, shrubs_present) {
  tree_state <- .veg_presence_state(trees_present)
  shrub_state <- .veg_presence_state(shrubs_present)
  ifelse(
    shrub_state == "present", "present",
    ifelse(shrub_state == "absent" & tree_state == "absent",
           "absent", "unknown")
  )
}

.veg_support_decision <- function(area, sampling_impractical, data_collected,
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
  }
  data.frame(status = status, reason = reason, stringsAsFactors = FALSE)
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
    release_doi = "https://doi.org/10.48443/pypa-qf12",
    source_normalization = "portable-vectors+published-uid-byte-order-v1"
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
  if (!isTRUE(all(query == "FULL_RELEASE"))) {
    reason <- c(
      reason,
      "release runtime requires a complete FULL_RELEASE source receipt"
    )
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
    "metric_kind", "support_status", "n_measurement_only_contexts",
    "n_measurement_records_without_opportunity_source", "inference_scope"
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
  diagnostic <- c(
    "n_measurement_only_contexts",
    "n_measurement_records_without_opportunity_source"
  )
  if (all(diagnostic %in% names(index))) {
    invalid_diagnostic <- vapply(diagnostic, function(field) {
      value <- suppressWarnings(as.numeric(index[[field]]))
      any(!is.finite(value) | value < 0 | value != floor(value))
    }, logical(1))
    if (any(invalid_diagnostic) ||
        sum(index$n_measurement_only_contexts) != 49L ||
        sum(index$n_measurement_records_without_opportunity_source) != 4365L ||
        sum(index$n_measurement_only_contexts > 0L) != 11L) {
      reason <- c(
        reason,
        "site index source-missing diagnostics differ from RELEASE-2026 49/4365/11"
      )
    }
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
      !is.data.frame(bundle$plots) || !is.data.frame(bundle$opportunity_source) ||
      !is.list(bundle$meta) ||
      !is.list(bundle$contract)) {
    return(.veg_gate_result(FALSE, c(reason, "bundle lacks trees, plots, opportunity_source, meta, or embedded contract")))
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
      !identical(.veg_scalar_chr(contract$source_record_key), VEG_CONTRACT$source_record_key) ||
      !identical(.veg_scalar_chr(contract$mapping_source_record_key),
                 VEG_CONTRACT$mapping_source_record_key) ||
      !identical(as.character(contract$protocol_stem_locator), VEG_CONTRACT$protocol_stem_locator) ||
      !identical(.veg_scalar_chr(contract$opportunity_source_record_key),
                 VEG_CONTRACT$opportunity_source_record_key) ||
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
  tree_identity <- c(
    "plotID", "eventID", "individualID", "tempStemID", "date",
    "source_uid", "protocol_stem_key",
    "protocol_key_group_n", "protocol_key_conflict",
    "opportunity_source_missing", "growthForm", "plantStatus", "live",
    "stemDiameter", "basalStemDiameter", "year", "permanent", "plant_key",
    "event_key", "mapping_source_uid", "mappingMatched", "scientificName",
    "taxonID", "taxonRank", "is_species", "taxon_label", "taxon_resolution"
  )
  tree_ready <- all(tree_identity %in% names(bundle$trees))
  if (!tree_ready ||
      any(is.na(bundle$trees$source_uid) |
          !nzchar(trimws(as.character(bundle$trees$source_uid)))) ||
      anyDuplicated(as.character(bundle$trees$source_uid))) {
    reason <- c(reason, "bundle measurement source-row identity is missing or non-unique")
  } else {
    protocol_key <- .veg_key(
      bundle$trees, c("plotID", "eventID", "individualID", "tempStemID")
    )
    expected_group_n <- as.integer(table(protocol_key)[protocol_key])
    stored_group_n <- .veg_exact_integer(bundle$trees$protocol_key_group_n)
    if (!stored_group_n$ok ||
        !identical(stored_group_n$value, expected_group_n) ||
        !identical(as.logical(bundle$trees$protocol_key_conflict),
                   expected_group_n > 1L) ||
        !identical(as.character(bundle$trees$protocol_stem_key), protocol_key)) {
      reason <- c(reason, "bundle measurement protocol locator audit is inconsistent")
    }
    row_derivation_problems <- .veg_row_derivation_problems(bundle$trees)
    if (length(row_derivation_problems)) {
      reason <- c(reason, paste0(
        "bundle row-derived invariants differ from preserved source fields: ",
        paste(row_derivation_problems, collapse = ", ")
      ))
    }
  }
  opportunity_identity <- c(
    "uid", "source_record_key", "plotID", "eventID",
    "protocol_key_group_n", "protocol_key_conflict"
  )
  opportunity_ready <- all(opportunity_identity %in% names(bundle$opportunity_source))
  if (!opportunity_ready ||
      any(is.na(bundle$opportunity_source$uid) |
          !nzchar(trimws(as.character(bundle$opportunity_source$uid)))) ||
      anyDuplicated(as.character(bundle$opportunity_source$uid)) ||
      any(is.na(bundle$opportunity_source$source_record_key) |
          !nzchar(trimws(as.character(bundle$opportunity_source$source_record_key)))) ||
      anyDuplicated(as.character(bundle$opportunity_source$source_record_key)) ||
      !identical(as.character(bundle$opportunity_source$source_record_key),
                 as.character(bundle$opportunity_source$uid))) {
    reason <- c(reason, "bundle opportunity source-row identity is missing, non-unique, or differs from published uid")
  }
  context_identity <- c(
    "plotID", "eventID", "opportunity_source_uid",
    "opportunity_source_record_count", "opportunity_source_uids",
    "opportunity_key_conflict", "opportunity_source_missing",
    "measurement_record_count_all", "measurement_date_min",
    "measurement_date_max", "measurement_date_distinct_n",
    "samplingImpractical", "dataCollected", "treesPresent", "shrubsPresent",
    "treePresence", "shrubSaplingPresence",
    "tree_records", "shrub_records", "tree_invalid_metric_records",
    "shrub_invalid_metric_records", "tree_identity_conflict_keys",
    "shrub_identity_conflict_keys", "tree_support", "tree_support_reason",
    "shrub_support", "shrub_support_reason", "tree_supported",
    "shrub_supported", "area_trees", "area_shrub", "event_key"
  )
  if (!all(context_identity %in% names(bundle$plots)) ||
      !tree_ready || !opportunity_ready) {
    reason <- c(reason, "bundle lacks exact plot-event context source fields")
  } else {
    context_key <- .veg_key(bundle$plots, c("plotID", "eventID"))
    source_key <- .veg_key(bundle$opportunity_source, c("plotID", "eventID"))
    measurement_key <- .veg_key(bundle$trees, c("plotID", "eventID"))
    source_missing <- as.logical(bundle$plots$opportunity_source_missing)
    source_missing_safe <- source_missing %in% TRUE
    source_count <- .veg_exact_integer(
      bundle$plots$opportunity_source_record_count
    )
    measurement_count_stored <- .veg_exact_integer(
      bundle$plots$measurement_record_count_all
    )
    measurement_date_n_stored <- .veg_exact_integer(
      bundle$plots$measurement_date_distinct_n
    )
    source_uid <- as.character(bundle$plots$opportunity_source_uid)
    source_uids <- as.character(bundle$plots$opportunity_source_uids)
    measurement_count <- as.integer(table(measurement_key)[context_key])
    measurement_count[is.na(measurement_count)] <- 0L
    canonical_source_n <- integer(nrow(bundle$plots))
    canonical_source_n[!source_missing_safe] <- as.integer(
      table(source_key)[context_key[!source_missing_safe]]
    )
    source_uid_sets <- tapply(
      as.character(bundle$opportunity_source$source_record_key), source_key,
      function(value) {
        value <- unique(value)
        paste(value[.veg_byte_order(value)], collapse = ";")
      }
    )
    canonical_source_uids <- rep(NA_character_, nrow(bundle$plots))
    canonical_source_uids[!source_missing_safe] <- unname(
      source_uid_sets[context_key[!source_missing_safe]]
    )
    selected_is_known <- mapply(function(uid, uids, missing) {
      if (missing) return(is.na(uid) || !nzchar(trimws(uid)))
      if (is.na(uid) || !nzchar(trimws(uid)) || is.na(uids)) return(FALSE)
      uid %in% strsplit(uids, ";", fixed = TRUE)[[1L]]
    }, source_uid, canonical_source_uids, source_missing_safe,
    USE.NAMES = FALSE)
    selected_source_parity <- .veg_selected_source_parity(
      bundle$plots, bundle$opportunity_source
    )
    if (!selected_source_parity$ok) {
      reason <- c(reason, paste0(
        "bundle canonical context differs from selected opportunity source row: ",
        paste(selected_source_parity$fields, collapse = ",")
      ))
    }
    expected_date_min <- expected_date_max <- rep(NA_real_, nrow(bundle$plots))
    expected_date_n <- integer(nrow(bundle$plots))
    if (nrow(bundle$trees)) {
      measurement_date <- .veg_date_number(bundle$trees$date)
      date_min <- tapply(measurement_date, measurement_key, function(value) {
        value <- value[is.finite(value)]
        if (length(value)) min(value) else NA_real_
      })
      date_max <- tapply(measurement_date, measurement_key, function(value) {
        value <- value[is.finite(value)]
        if (length(value)) max(value) else NA_real_
      })
      date_n <- tapply(measurement_date, measurement_key, function(value) {
        as.integer(length(unique(value[is.finite(value)])))
      })
      expected_date_min <- unname(date_min[context_key])
      expected_date_max <- unname(date_max[context_key])
      expected_date_n <- as.integer(unname(date_n[context_key]))
      expected_date_n[is.na(expected_date_n)] <- 0L
    }
    source_derived_fields <- setdiff(
      names(bundle$plots), VEG_CONTEXT_DERIVED_FIELDS
    )
    invented_source_value <- vapply(source_derived_fields, function(field) {
      value <- bundle$plots[[field]][source_missing_safe]
      if (is.character(value) || is.factor(value)) {
        any(!is.na(value) & nzchar(trimws(as.character(value))))
      } else {
        any(!is.na(value))
      }
    }, logical(1))
    expected_missing_records <- as.integer(sum(
      measurement_key %in% context_key[source_missing_safe]
    ))
    tree_support <- as.character(bundle$plots$tree_support)
    shrub_support <- as.character(bundle$plots$shrub_support)
    tree_form <- tolower(trimws(as.character(bundle$trees$growthForm))) %in%
      c("single bole tree", "multi-bole tree")
    shrub_form <- tolower(trimws(as.character(bundle$trees$growthForm))) %in%
      c("single shrub", "small shrub", "sapling")
    live <- grepl(
      "^live", trimws(as.character(bundle$trees$plantStatus)),
      ignore.case = TRUE
    )
    tree_diameter <- suppressWarnings(as.numeric(bundle$trees$stemDiameter))
    shrub_diameter <- suppressWarnings(as.numeric(bundle$trees$basalStemDiameter))
    expected_tree_presence <- .veg_presence_state(bundle$plots$treesPresent)
    expected_shrub_presence <- .veg_shrub_presence_state(
      bundle$plots$treesPresent, bundle$plots$shrubsPresent
    )
    expected_tree_records <- .veg_event_counts(
      bundle$trees, bundle$plots, tree_form
    )
    expected_shrub_records <- .veg_event_counts(
      bundle$trees, bundle$plots, shrub_form
    )
    expected_tree_invalid <- .veg_event_counts(
      bundle$trees, bundle$plots,
      tree_form & live &
        !(is.finite(tree_diameter) & tree_diameter > 0 & tree_diameter >= 10)
    )
    expected_shrub_invalid <- .veg_event_counts(
      bundle$trees, bundle$plots,
      shrub_form & live & !(is.finite(shrub_diameter) & shrub_diameter > 0)
    )
    expected_tree_conflicts <- .veg_identity_conflict_counts(
      bundle$trees, bundle$plots, tree_form
    )
    expected_shrub_conflicts <- .veg_identity_conflict_counts(
      bundle$trees, bundle$plots, shrub_form
    )
    expected_tree_support <- .veg_support_decision(
      suppressWarnings(as.numeric(bundle$plots$area_trees)),
      bundle$plots$samplingImpractical, bundle$plots$dataCollected,
      expected_tree_presence, expected_tree_records, expected_tree_invalid,
      expected_tree_conflicts, as.logical(bundle$plots$opportunity_key_conflict),
      source_missing
    )
    expected_shrub_support <- .veg_support_decision(
      suppressWarnings(as.numeric(bundle$plots$area_shrub)),
      bundle$plots$samplingImpractical, bundle$plots$dataCollected,
      expected_shrub_presence, expected_shrub_records, expected_shrub_invalid,
      expected_shrub_conflicts, as.logical(bundle$plots$opportunity_key_conflict),
      source_missing
    )
    stored_tree_records <- .veg_exact_integer(bundle$plots$tree_records)
    stored_shrub_records <- .veg_exact_integer(bundle$plots$shrub_records)
    stored_tree_invalid <- .veg_exact_integer(
      bundle$plots$tree_invalid_metric_records
    )
    stored_shrub_invalid <- .veg_exact_integer(
      bundle$plots$shrub_invalid_metric_records
    )
    stored_tree_conflicts <- .veg_exact_integer(
      bundle$plots$tree_identity_conflict_keys
    )
    stored_shrub_conflicts <- .veg_exact_integer(
      bundle$plots$shrub_identity_conflict_keys
    )
    support_algebra_invalid <-
      !stored_tree_records$ok || !stored_shrub_records$ok ||
      !stored_tree_invalid$ok || !stored_shrub_invalid$ok ||
      !stored_tree_conflicts$ok || !stored_shrub_conflicts$ok ||
      !identical(as.character(bundle$plots$treePresence),
                 expected_tree_presence) ||
      !identical(as.character(bundle$plots$shrubSaplingPresence),
                 expected_shrub_presence) ||
      !identical(stored_tree_records$value, expected_tree_records) ||
      !identical(stored_shrub_records$value, expected_shrub_records) ||
      !identical(stored_tree_invalid$value, expected_tree_invalid) ||
      !identical(stored_shrub_invalid$value, expected_shrub_invalid) ||
      !identical(stored_tree_conflicts$value, expected_tree_conflicts) ||
      !identical(stored_shrub_conflicts$value, expected_shrub_conflicts) ||
      !identical(tree_support, expected_tree_support$status) ||
      !identical(shrub_support, expected_shrub_support$status) ||
      !identical(as.character(bundle$plots$tree_support_reason),
                 expected_tree_support$reason) ||
      !identical(as.character(bundle$plots$shrub_support_reason),
                 expected_shrub_support$reason) ||
      !identical(as.logical(bundle$plots$tree_supported),
                 tree_support %in% VEG_CONTRACT$supported_status) ||
      !identical(as.logical(bundle$plots$shrub_supported),
                 shrub_support %in% VEG_CONTRACT$supported_status) ||
      !identical(as.character(bundle$plots$event_key),
                 paste(bundle$plots$plotID, bundle$plots$eventID, sep = "\r"))
    if (support_algebra_invalid) {
      reason <- c(
        reason,
        "bundle support states, reasons, counts, or presence differ from preserved rows"
      )
    }
    meta_missing_n <- .veg_exact_integer(meta$n_measurement_only_contexts)
    meta_missing_records <- .veg_exact_integer(
      meta$n_measurement_records_without_opportunity_source
    )
    invalid_context <- any(is.na(source_missing)) || anyDuplicated(context_key) ||
      !source_count$ok || !measurement_count_stored$ok ||
      !measurement_date_n_stored$ok ||
      !inherits(bundle$plots$measurement_date_min, "Date") ||
      !inherits(bundle$plots$measurement_date_max, "Date") ||
      !setequal(unique(source_key), context_key[!source_missing_safe]) ||
      !setequal(
        context_key[source_missing_safe],
        setdiff(unique(measurement_key), unique(source_key))
      ) ||
      length(setdiff(unique(measurement_key), context_key)) ||
      !identical(as.logical(bundle$trees$opportunity_source_missing),
                 measurement_key %in% context_key[source_missing_safe]) ||
      !identical(source_count$value, canonical_source_n) ||
      !identical(as.logical(bundle$plots$opportunity_key_conflict),
                 canonical_source_n > 1L) ||
      !identical(as.character(bundle$plots$opportunity_source_uids),
                 canonical_source_uids) ||
      !all(selected_is_known) ||
      !identical(measurement_count_stored$value, measurement_count) ||
      !identical(.veg_date_number(bundle$plots$measurement_date_min),
                 expected_date_min) ||
      !identical(.veg_date_number(bundle$plots$measurement_date_max),
                 expected_date_max) ||
      !identical(measurement_date_n_stored$value, expected_date_n) ||
      any(invented_source_value) ||
      !identical(tree_support == "held_opportunity_source_missing",
                 source_missing_safe) ||
      !identical(shrub_support == "held_opportunity_source_missing",
                 source_missing_safe) ||
      support_algebra_invalid ||
      !meta_missing_n$ok || !meta_missing_records$ok ||
      !identical(meta_missing_n$value,
                 as.integer(sum(source_missing_safe))) ||
      !identical(meta_missing_records$value, expected_missing_records)
    if (invalid_context) {
      reason <- c(reason, "bundle plot-event contexts violate source/missing algebra")
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
                          "density_ha", "qmd_cm", "tallest_m", "biggest_diam_cm",
                          "n_measurement_only_contexts",
                          "n_measurement_records_without_opportunity_source"), names(SITE_INDEX))
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
