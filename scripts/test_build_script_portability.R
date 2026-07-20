#!/usr/bin/env Rscript

# Dependency-free static regression checks for refresh/build isolation.

targets <- c(
  "scripts/fetch_veg_data.R", "scripts/bundle_veg_data.R",
  "scripts/build_site_index.R", "scripts/build_search_index.R",
  "scripts/refresh_data.R", "scripts/write_data_quality_audit.R",
  "scripts/write_source_receipt.R", "scripts/derived_parity.R",
  "scripts/verify_derived_parity.R"
)
invisible(lapply(targets, parse))
texts <- stats::setNames(
  lapply(targets, function(path) paste(readLines(path, warn = FALSE), collapse = "\n")),
  targets
)

forbidden <- c(
  "setwd\\s*\\(", "[A-Za-z]:[/\\\\](Users|home)[/\\\\]",
  paste0("/", "Users/"), paste0("/", "home/[^/]+/")
)
for (path in names(texts)) {
  hits <- vapply(forbidden, grepl, logical(1), x = texts[[path]], perl = TRUE,
                 ignore.case = TRUE)
  if (any(hits))
    stop(sprintf("%s contains non-portable path pattern(s): %s",
                 path, paste(forbidden[hits], collapse = ", ")), call. = FALSE)
}

required <- list(
  "scripts/fetch_veg_data.R" = c(
    "VST_RAW_OUT_DIR", "VST_QUERY_START", "VST_QUERY_END",
    "vst_assert_site_inventory", "vst_portable_source_table",
    "SOURCE-SHA256SUMS.txt", "sourceNormalization"
  ),
  "scripts/bundle_veg_data.R" = c(
    "VST_RAW_DIR", "VST_SITE_OUT_DIR", "VST_SAMPLE_OUT_DIR",
    "VST_SITE_INDEX_OUT", "must be empty"
  ),
  "scripts/build_site_index.R" = c("VST_SITE_DIR", "VST_SITE_INDEX_OUT"),
  "scripts/build_search_index.R" = c(
    "VST_SITE_DIR", "VST_SITE_INDEX", "VST_SEARCH_INDEX_OUT", "as.Date(NA)"
  ),
  "scripts/refresh_data.R" = c(
    "VST_CANDIDATE_ROOT", "outside the repository", "must be empty",
    "VST_SOURCE_NORMALIZATION", "sourceNormalization", "--show-toplevel",
    "--porcelain=v1", "local candidate source is dirty", "head_commit"
  ),
  "scripts/write_data_quality_audit.R" = c(
    "VST_SITE_DIR", "VST_DATA_QUALITY_AUDIT_OUT",
    "vegetation-data-quality-audit.csv", "NEON-VST-data-quality-audit-v2",
    "held_reason_counts", "dataQF",
    "held_identity_conflict", "protocol_key_conflict",
    "held_opportunity_source_missing", "opportunity_source_missing",
    "n_measurement_only_contexts",
    "source_normalization", "measurement_date_distinct_n",
    "tagStatus", "changedMeasurementLocation",
    "preserved_and_counted_not_excluded", "^--file=",
    "sys.nframe() == 0L"
  ),
  "scripts/write_source_receipt.R" = c(
    "VST_SOURCE_RECEIPT_OUT", "vegetation-raw-SHA256SUMS.txt",
    "vegetation-bundle-family-SHA256.txt",
    "vegetation-data-quality-audit-SHA256.txt", "vst_read_fetch_runtime",
    "49 contexts / 4365 records / 11 sites"
  ),
  "scripts/derived_parity.R" = c(
    "R/veg_helpers.R", "vst_parity_bundle_projection",
    "vst_parity_family_problems", "search channel rows"
  ),
  "scripts/verify_derived_parity.R" = c(
    "VST_SITE_DIR", "VST_SITE_INDEX", "VST_SEARCH_INDEX",
    "vst_assert_derived_parity"
  )
)
for (path in names(required)) {
  missing <- required[[path]][!vapply(required[[path]], grepl, logical(1),
                                     x = texts[[path]], fixed = TRUE)]
  if (length(missing))
    stop(sprintf("%s lacks portability contract(s): %s",
                 path, paste(missing, collapse = ", ")), call. = FALSE)
}

audit_text <- texts[["scripts/write_data_quality_audit.R"]]
wall_clock <- c("Sys[.]time\\s*\\(", "Sys[.]Date\\s*\\(", "built_at")
if (any(vapply(wall_clock, grepl, logical(1), x = audit_text,
               perl = TRUE, ignore.case = TRUE))) {
  stop("write_data_quality_audit.R must not contain wall-clock inputs",
       call. = FALSE)
}

workflow <- paste(readLines(".github/workflows/refresh-data.yml", warn = FALSE),
                  collapse = "\n")
ci_workflow <- paste(readLines(".github/workflows/ci.yml", warn = FALSE),
                     collapse = "\n")
for (token in c(
  "VST_SOURCE_SHA: ${{ github.event.pull_request.head.sha || github.sha }}",
  "ref: ${{ env.VST_SOURCE_SHA }}",
  "actual_sha=$(git rev-parse HEAD)",
  '"$actual_sha" != "$VST_SOURCE_SHA"'
)) {
  if (!grepl(token, ci_workflow, fixed = TRUE)) {
    stop(sprintf("ordinary CI lacks exact reviewed-head contract: %s", token),
         call. = FALSE)
  }
}
for (token in c(
  "github.actor == github.repository_owner",
  "github.ref_name == github.event.repository.default_branch"
)) {
  if (!grepl(token, workflow, fixed = TRUE)) {
    stop(sprintf("manual refresh lacks owner/default-branch guard: %s", token),
         call. = FALSE)
  }
}
audit_call <- "Rscript --vanilla scripts/write_data_quality_audit.R"
receipt_call <- "Rscript --vanilla scripts/write_source_receipt.R"
audit_position <- regexpr(audit_call, workflow, fixed = TRUE)[[1L]]
receipt_position <- regexpr(receipt_call, workflow, fixed = TRUE)[[1L]]
if (audit_position < 1L || receipt_position < 1L ||
    audit_position > receipt_position ||
    !grepl('build_one "$RUNNER_TEMP/candidate-a"', workflow, fixed = TRUE) ||
    !grepl('build_one "$RUNNER_TEMP/candidate-b"', workflow, fixed = TRUE)) {
  stop("refresh workflow must audit each isolated candidate before source ledgers",
       call. = FALSE)
}
if (!grepl("SOURCE_NORMALIZATION", workflow, fixed = TRUE) ||
    !grepl("portable-vectors+published-uid-byte-order-v1", workflow,
           fixed = TRUE)) {
  stop("refresh workflow must bind the reviewed source-normalization contract",
       call. = FALSE)
}
if (!grepl("Rscript --vanilla scripts/verify_derived_parity.R", workflow,
           fixed = TRUE) ||
    !grepl("Rscript --vanilla scripts/validation/test_runtime_bundle_gate.R",
           workflow, fixed = TRUE) ||
    grepl("inputs.query_start", workflow, fixed = TRUE) ||
    grepl("inputs.query_end", workflow, fixed = TRUE)) {
  stop(
    "refresh workflow must run consumer/runtime parity and expose no bounded-query candidate input",
    call. = FALSE
  )
}
candidate_allowlist_tokens <- c(
  "verify_candidate_inventory", "vegetation-promotion",
  "data/site_index.rds", "data/search_index.rds", "data-sample/demo.rds",
  "vegetation-raw-SHA256SUMS.txt", "vegetation-raw-family-SHA256.txt",
  "vegetation-fetch-runtime.txt", "vegetation-bundle-SHA256SUMS.txt",
  "vegetation-bundle-family-SHA256.txt",
  "vegetation-data-quality-audit.csv",
  "vegetation-data-quality-audit-SHA256.txt",
  "docs/VEGETATION-SOURCE-RECEIPT.md"
)
missing_allowlist <- candidate_allowlist_tokens[
  !vapply(candidate_allowlist_tokens, grepl, logical(1), x = workflow, fixed = TRUE)
]
if (length(missing_allowlist) ||
    grepl("runner.temp }}/candidate-a/data", workflow, fixed = TRUE)) {
  stop(sprintf(
    "refresh workflow lacks exact candidate promotion closure: %s",
    paste(missing_allowlist, collapse = ", ")
  ), call. = FALSE)
}

source("scripts/vegetation_inventory.R", local = TRUE)
portable_fixture <- data.frame(
  text = c("oak", NA_character_),
  count = c(1L, 2L),
  measured = as.Date(c("2026-01-02", "2026-02-03")),
  status = factor(c("live", "dead"), levels = c("live", "dead")),
  stringsAsFactors = FALSE
)
portable_result <- vst_portable_table(portable_fixture, "portability fixture")
stopifnot(
  identical(names(portable_result), names(portable_fixture)),
  identical(dim(portable_result), dim(portable_fixture)),
  identical(portable_result$text, portable_fixture$text),
  identical(portable_result$count, portable_fixture$count),
  identical(portable_result$measured, portable_fixture$measured),
  identical(portable_result$status, portable_fixture$status),
  identical(unname(vapply(portable_result, length, integer(1))), rep(2L, 4L))
)

source_fixture <- data.frame(
  uid = c("source-é", "source-z", "source-a"),
  value = c(3L, 2L, 1L),
  measured = as.Date(c("2026-03-04", "2026-02-03", "2026-01-02")),
  stringsAsFactors = FALSE
)
source_normalized <- vst_portable_source_table(
  source_fixture, "source-order fixture"
)
source_reversed <- vst_portable_source_table(
  source_fixture[3:1, , drop = FALSE], "reversed source-order fixture"
)
stopifnot(
  identical(source_normalized, source_reversed),
  identical(source_normalized$uid, c("source-a", "source-z", "source-é")),
  identical(rownames(source_normalized), c("1", "2", "3"))
)

cat("Vegetation refresh/build portability contracts passed.\n")
