# Shared release inventory for NEON Vegetation Structure (DP1.10098.001).
# A site-set change is a reviewed product-scope change, not an automatic monthly
# refresh side effect. Keep receipts, cover claims, tests, and Driver handoffs
# synchronized when this list changes.

VST_EXPECTED_SITES <- c(
  "ABBY", "BART", "BLAN", "BONA", "CLBJ", "CPER", "DCFS", "DEJU",
  "DELA", "DSNY", "GRSM", "GUAN", "HARV", "HEAL", "JERC", "JORN",
  "KONZ", "LAJA", "LENO", "MLBS", "MOAB", "NIWO", "NOGP", "ONAQ",
  "ORNL", "OSBS", "PUUM", "RMNP", "SCBI", "SERC", "SJER", "SOAP",
  "SRER", "STEI", "TALL", "TEAK", "TREE", "UKFS", "UNDE", "WOOD",
  "WREF", "YELL"
)

VST_SOURCE_NORMALIZATION <-
  "portable-vectors+published-uid-byte-order-v1"
VST_FETCH_RUNTIME_KEYS <- c(
  "product", "officialNeonRelease", "releaseDoi", "queryStart", "queryEnd",
  "neonUtilities", "sourceNormalization"
)

`%||%` <- function(left, right) {
  if (is.null(left) || !length(left)) right else left
}

# Materialize a fetched table as portable base-R vectors before serializing it.
# neonUtilities can return Arrow ALTREP character columns; saving those objects
# directly makes the raw RDS family depend on Arrow being loaded when it is read.
# Subsetting every column forces the same plain-vector conversion used by the
# repository's proven one-time ALTREP repair without changing values or classes
# that the builder relies on.
vst_plain_vector <- function(value) {
  if (inherits(value, "Date")) {
    return(structure(as.numeric(value[seq_along(value)]), class = "Date"))
  }
  if (inherits(value, "POSIXct")) {
    return(structure(
      as.numeric(value[seq_along(value)]),
      class = class(value), tzone = attr(value, "tzone")
    ))
  }
  if (is.factor(value)) {
    return(factor(
      as.character(value[seq_along(value)]),
      levels = levels(value), ordered = is.ordered(value)
    ))
  }
  value[seq_along(value)]
}

vst_portable_table <- function(table, label = "fetched table") {
  if (!is.data.frame(table)) {
    stop(label, " must be a data frame", call. = FALSE)
  }
  expected_rows <- nrow(table)
  expected_names <- names(table)
  portable <- as.data.frame(table, stringsAsFactors = FALSE, optional = TRUE)
  for (name in names(portable)) {
    portable[[name]] <- vst_plain_vector(portable[[name]])
  }
  lengths <- vapply(portable, length, integer(1))
  if (!identical(names(portable), expected_names) ||
      any(lengths != expected_rows)) {
    stop(label, " did not materialize to a rectangular portable table",
         call. = FALSE)
  }
  portable
}

vst_utf8_byte_key <- function(value) {
  value <- enc2utf8(as.character(value))
  vapply(value, function(item) {
    if (is.na(item)) return(NA_character_)
    paste(sprintf("%02x", as.integer(charToRaw(item))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

vst_byte_order <- function(value, na.last = TRUE) {
  order(vst_utf8_byte_key(value), method = "radix", na.last = na.last)
}

# NEON does not define response-row order as data. Normalize every fetched
# source table by its immutable published uid before hashing or serializing it,
# so an API ordering change cannot manufacture a new raw-family digest. The
# pinned runtime still defines the exact RDS encoding; this function defines the
# semantic row order within that encoding.
vst_portable_source_table <- function(table, label = "fetched source table") {
  portable <- vst_portable_table(table, label)
  if (!"uid" %in% names(portable)) {
    stop(label, " lacks published uid", call. = FALSE)
  }
  source_uid <- as.character(portable$uid)
  if (any(is.na(source_uid) | !nzchar(trimws(source_uid)))) {
    stop(label, " has blank published uid", call. = FALSE)
  }
  if (anyDuplicated(source_uid)) {
    stop(label, " has duplicate published uid", call. = FALSE)
  }
  ordered <- portable[
    vst_byte_order(source_uid, na.last = TRUE),
    , drop = FALSE
  ]
  rownames(ordered) <- NULL
  ordered
}

vst_site_codes <- function(directory, suffix = ".rds") {
  if (!dir.exists(directory)) return(character(0))
  escaped <- gsub("[.]", "\\\\.", suffix)
  sort(sub(paste0(escaped, "$"), "", basename(list.files(
    directory, pattern = paste0(escaped, "$"), full.names = TRUE
  ))))
}

vst_assert_site_inventory <- function(directory, suffix = ".rds", label = "site bundle") {
  actual <- vst_site_codes(directory, suffix)
  expected <- sort(VST_EXPECTED_SITES)
  if (!identical(actual, expected)) {
    stop(sprintf(
      "%s inventory mismatch: expected %d, found %d; missing=[%s]; extra=[%s]",
      label, length(expected), length(actual),
      paste(setdiff(expected, actual), collapse = ","),
      paste(setdiff(actual, expected), collapse = ",")
    ), call. = FALSE)
  }
  invisible(actual)
}

vst_read_fetch_runtime <- function(path) {
  if (!file.exists(path)) stop("fetch runtime receipt is missing: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE)
  keys <- sub("=.*$", "", lines)
  if (!identical(keys, VST_FETCH_RUNTIME_KEYS) ||
      any(!grepl("^[^=]+=[^[:space:]].*$", lines))) {
    stop(
      "fetch runtime receipt must contain the exact ordered seven-key contract",
      call. = FALSE
    )
  }
  values <- sub("^[^=]+=", "", lines)
  stats::setNames(as.list(values), keys)
}

vst_source_field <- function(data, candidates, default = NA) {
  found <- intersect(candidates, names(data))
  if (length(found)) data[[found[[1L]]]] else rep(default, nrow(data))
}

vst_source_date <- function(value) {
  if (inherits(value, "Date")) return(value)
  if (inherits(value, "POSIXt")) return(as.Date(value))
  suppressWarnings(as.Date(substr(as.character(value), 1L, 10L)))
}

# Prove that every source-backed plot-event context carries the fields from the
# exact published row named by opportunity_source_uid. This is deliberately
# independent of the support calculations: otherwise a corrupted area/status
# value could be used consistently by every downstream consumer and evade a
# parity check.
vst_selected_source_parity <- function(contexts, source) {
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

  selected_date <- vst_source_date(vst_source_field(
    selected, c("date", "collectDate", "eventDate")
  ))
  compare("plotID", selected$plotID)
  compare("eventID", selected$eventID)
  compare("date", selected_date, "date")
  compare(
    "year", suppressWarnings(as.integer(format(selected_date, "%Y"))),
    "integer"
  )
  compare("eventType", vst_source_field(selected, "eventType"))
  compare("plotType", vst_source_field(selected, "plotType"))
  compare("nlcdClass", vst_source_field(selected, "nlcdClass"))
  compare(
    "lat", suppressWarnings(as.numeric(vst_source_field(
      selected, c("decimalLatitude", "latitude", "lat")
    ))), "numeric"
  )
  compare(
    "lng", suppressWarnings(as.numeric(vst_source_field(
      selected, c("decimalLongitude", "longitude", "lng")
    ))), "numeric"
  )
  compare(
    "samplingImpractical",
    vst_source_field(selected, "samplingImpractical")
  )
  compare("dataCollected", vst_source_field(selected, "dataCollected"))
  compare(
    "treesPresent", vst_source_field(selected, c("treesPresent", "treePresent"))
  )
  compare(
    "shrubsPresent", vst_source_field(selected, c("shrubsPresent", "shrubPresent"))
  )
  compare(
    "area_trees", suppressWarnings(as.numeric(vst_source_field(
      selected, "totalSampledAreaTrees"
    ))), "numeric"
  )
  compare(
    "area_shrub", suppressWarnings(as.numeric(vst_source_field(
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

vst_source_receipt_from_env <- function() {
  fields <- c(
    schema_version = "VST_RECEIPT_SCHEMA_VERSION",
    provenance_class = "VST_PROVENANCE_CLASS",
    product = "VST_PRODUCT",
    neon_release = "VST_NEON_RELEASE",
    release_doi = "VST_RELEASE_DOI",
    query_start = "VST_QUERY_START",
    query_end = "VST_QUERY_END",
    source_receipt_id = "VST_SOURCE_RECEIPT_ID",
    raw_source_digest = "VST_RAW_SOURCE_DIGEST",
    neon_utilities_version = "VST_NEON_UTILITIES_VERSION",
    source_normalization = "VST_SOURCE_NORMALIZATION",
    built_at = "VST_BUILT_AT",
    builder_commit = "VST_BUILDER_COMMIT"
  )
  receipt <- as.list(stats::setNames(
    vapply(unname(fields), Sys.getenv, character(1), unset = ""),
    names(fields)
  ))
  if (!any(nzchar(unlist(receipt, use.names = FALSE)))) return(NULL)
  receipt$product <- if (nzchar(receipt$product)) receipt$product else "DP1.10098.001"
  receipt$schema_version <- if (nzchar(receipt$schema_version)) receipt$schema_version else "1"
  receipt
}

vst_receipts_identical <- function(receipts) {
  receipts <- Filter(Negate(is.null), receipts)
  if (!length(receipts)) return(NULL)
  first <- receipts[[1L]]
  if (!all(vapply(receipts, identical, logical(1), first))) {
    stop("source receipts differ across the candidate site family", call. = FALSE)
  }
  first
}
