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

`%||%` <- function(left, right) {
  if (is.null(left) || !length(left)) right else left
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
