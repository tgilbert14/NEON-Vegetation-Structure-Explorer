#!/usr/bin/env Rscript

# Write the human-readable source receipt and durable raw-file checksum ledger
# from a fully validated 42-site candidate. This is for candidate builds only;
# skip-download validation must preserve the existing receipt unchanged.

suppressPackageStartupMessages(library(digest))
source("scripts/vegetation_inventory.R")

SITE_DIR <- Sys.getenv("VST_SITE_DIR", unset = "data/sites")
RAW_DIR <- trimws(Sys.getenv("VST_RAW_DIR", unset = ""))
OUT <- Sys.getenv("VST_SOURCE_RECEIPT_OUT",
                  unset = "docs/VEGETATION-SOURCE-RECEIPT.md")
LEDGER_DIR <- Sys.getenv("VST_SOURCE_LEDGER_DIR", unset = "data/source")
AUDIT_PATH <- Sys.getenv(
  "VST_DATA_QUALITY_AUDIT_OUT",
  unset = file.path(LEDGER_DIR, "vegetation-data-quality-audit.csv")
)
RUN_URL <- trimws(Sys.getenv("VST_REFRESH_RUN_URL", unset = "NA"))

vst_assert_site_inventory(SITE_DIR)
if (!nzchar(RAW_DIR) || !dir.exists(RAW_DIR))
  stop("VST_RAW_DIR is required to write a refreshed source receipt", call. = FALSE)

bundles <- lapply(file.path(SITE_DIR, paste0(sort(VST_EXPECTED_SITES), ".rds")), readRDS)
receipts <- lapply(bundles, function(bundle) bundle$meta$source_receipt %||% NULL)
if (!all(vapply(receipts, Negate(is.null), logical(1))))
  stop("every refreshed site bundle must carry the same source receipt", call. = FALSE)
receipt <- vst_receipts_identical(receipts)

required <- c(
  "schema_version", "provenance_class", "product", "neon_release", "release_doi",
  "query_start", "query_end",
  "source_receipt_id", "raw_source_digest", "neon_utilities_version",
  "built_at", "builder_commit"
)
missing <- required[!vapply(required, function(field) {
  value <- receipt[[field]]
  length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value)))
}, logical(1))]
if (length(missing))
  stop("source receipt is incomplete: ", paste(missing, collapse = ", "), call. = FALSE)

raw_inventory_path <- file.path(RAW_DIR, "SOURCE-SHA256SUMS.txt")
raw_family_path <- file.path(RAW_DIR, "SOURCE-FAMILY-SHA256.txt")
raw_runtime_path <- file.path(RAW_DIR, "FETCH-RUNTIME.txt")
if (!all(file.exists(c(raw_inventory_path, raw_family_path, raw_runtime_path))))
  stop("raw checksum/runtime receipt files are incomplete", call. = FALSE)
raw_inventory <- readLines(raw_inventory_path, warn = FALSE)
if (length(raw_inventory) != length(VST_EXPECTED_SITES))
  stop("raw checksum inventory must contain exactly 42 entries", call. = FALSE)
raw_family <- trimws(readLines(raw_family_path, warn = FALSE)[1L])
computed_raw_family <- digest::digest(
  paste0(paste0(raw_inventory, collapse = "\n"), "\n"),
  algo = "sha256", serialize = FALSE
)
if (!identical(raw_family, computed_raw_family) ||
    !identical(raw_family, receipt$raw_source_digest))
  stop("raw family checksum does not match the embedded source receipt", call. = FALSE)

bundle_paths <- file.path(SITE_DIR, paste0(sort(VST_EXPECTED_SITES), ".rds"))
bundle_hashes <- vapply(bundle_paths, digest::digest, character(1),
                        algo = "sha256", file = TRUE)
bundle_lines <- sprintf("%s %s", unname(bundle_hashes), basename(bundle_paths))
bundle_family <- digest::digest(
  paste0(paste0(bundle_lines, collapse = "\n"), "\n"),
  algo = "sha256", serialize = FALSE
)
if (!file.exists(AUDIT_PATH))
  stop("deterministic data-quality audit is missing: ", AUDIT_PATH, call. = FALSE)
audit <- utils::read.csv(
  AUDIT_PATH, stringsAsFactors = FALSE, check.names = FALSE,
  colClasses = "character"
)
expected_audit_rows <- length(VST_EXPECTED_SITES) * 2L
expected_audit_keys <- sort(
  as.vector(outer(
    sort(VST_EXPECTED_SITES), c("tree_dbh", "shrub_sapling_basal"),
    function(site, channel) paste(site, channel, sep = "\r")
  )), method = "radix"
)
actual_audit_keys <- sort(paste(audit$site, audit$channel, sep = "\r"),
                          method = "radix")
if (nrow(audit) != expected_audit_rows ||
    !all(c("site", "channel", "audit_schema", "contract_id",
           "raw_source_digest") %in% names(audit)) ||
    anyDuplicated(actual_audit_keys) ||
    !identical(actual_audit_keys, expected_audit_keys) ||
    any(audit$audit_schema != "NEON-VST-data-quality-audit-v1") ||
    any(audit$contract_id != "NEON-VST-DP1.10098.001-v2") ||
    any(audit$raw_source_digest != receipt$raw_source_digest)) {
  stop("deterministic data-quality audit is incomplete or has mixed provenance",
       call. = FALSE)
}
audit_hash <- digest::digest(AUDIT_PATH, algo = "sha256", file = TRUE)

dir.create(LEDGER_DIR, recursive = TRUE, showWarnings = FALSE)
copied_inventory <- file.copy(
  raw_inventory_path,
  file.path(LEDGER_DIR, "vegetation-raw-SHA256SUMS.txt"), overwrite = TRUE
)
writeLines(raw_family,
           file.path(LEDGER_DIR, "vegetation-raw-family-SHA256.txt"), useBytes = TRUE)
copied_runtime <- file.copy(
  raw_runtime_path,
  file.path(LEDGER_DIR, "vegetation-fetch-runtime.txt"), overwrite = TRUE
)
if (!isTRUE(copied_inventory) || !isTRUE(copied_runtime))
  stop("failed to preserve durable raw source ledgers", call. = FALSE)
writeLines(bundle_lines,
           file.path(LEDGER_DIR, "vegetation-bundle-SHA256SUMS.txt"), useBytes = TRUE)
writeLines(bundle_family,
           file.path(LEDGER_DIR, "vegetation-bundle-family-SHA256.txt"), useBytes = TRUE)
writeLines(
  sprintf("%s %s", audit_hash, basename(AUDIT_PATH)),
  file.path(LEDGER_DIR, "vegetation-data-quality-audit-SHA256.txt"),
  useBytes = TRUE
)

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
lines <- c(
  "# Vegetation source receipt",
  "",
  "Status: **official-release candidate; scientific and human review required before promotion**.",
  "",
  sprintf("This receipt describes one complete %d-site candidate for NEON Vegetation structure `%s`, explicitly selected from the immutable `%s` data release. Release identity and DOI describe upstream provenance; candidate build date and repository commit remain separate fields.",
          length(VST_EXPECTED_SITES), receipt$product, receipt$neon_release),
  "",
  "## Candidate identity",
  "",
  sprintf("- Receipt schema: `%s`.", receipt$schema_version),
  sprintf("- Provenance class: `%s`.", receipt$provenance_class),
  sprintf("- Product: `%s`.", receipt$product),
  sprintf("- Official NEON release: `%s`.", receipt$neon_release),
  sprintf("- Release DOI: `%s`.", receipt$release_doi),
  sprintf("- Query window: `%s` through `%s` (`FULL_RELEASE` means no month subset was applied).", receipt$query_start, receipt$query_end),
  sprintf("- Actual candidate bundle build date: `%s`.", receipt$built_at),
  sprintf("- Builder commit: `%s`.", receipt$builder_commit),
  sprintf("- `neonUtilities` fetch version: `%s`.", receipt$neon_utilities_version),
  sprintf("- Immutable release-snapshot label: `%s`.", receipt$source_receipt_id),
  sprintf("- Raw source family SHA-256: `%s`.", receipt$raw_source_digest),
  sprintf("- Bundled 42-site family SHA-256: `%s`.", bundle_family),
  sprintf("- Deterministic 42-site × two-channel data-quality audit SHA-256: `%s`.", audit_hash),
  sprintf("- Refresh workflow evidence: `%s`.", RUN_URL),
  "",
  "Both family hashes use basename-ordered inventory lines in the exact form `<sha256> <basename>\\n`. The raw and bundled per-file ledgers, aggregate hashes, fetch runtime, deterministic site × channel data-quality audit, and its checksum are preserved under `data/source/`. The raw response artifact is retained with the workflow run; the ledgers remain durable in the repository.",
  "",
  "## Promotion contract",
  "",
  "Promotion requires all 42 bundles, `data/site_index.rds`, `data/search_index.rds`, the source ledgers, science contract, user-facing claims, Driver package, suite handoff, and manifest to agree on this candidate. Bundles must preserve the official event × individual × temporary-stem identity and the per-plot/per-year sampling-opportunity fields needed to join measurements to the correct denominator. Any missing site, mixed receipt, unmatched source digest, unreviewed denominator condition, dropped support field, or stale empirical claim blocks promotion.",
  "",
  "`skip_download=true` accepts only an already-promoted v2 family and revalidates its committed inputs. It must not change this receipt, stamp a new build date, invent a NEON release, or treat a repository/manifest time as upstream vintage."
)
writeLines(lines, OUT, useBytes = TRUE)
cat(sprintf("wrote %s; bundle family SHA-256 %s\n", OUT, bundle_family))
