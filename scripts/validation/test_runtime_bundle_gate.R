#!/usr/bin/env Rscript

# Positive and mutation fixtures for the deployed bundle gate. This test runs
# only after a complete v2 candidate family has been built: it sources the same
# global.R shipped to Connect, accepts an untouched actual bundle, and proves
# valid-vocabulary support/status corruption cannot pass the runtime boundary.

Sys.setenv(VST_LIVE = "0")
source("global.R")

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_true(isTRUE(VEG_FAMILY_READY) && length(BUNDLED) == 42L,
            "runtime mutation fixtures require the exact ready 42-site v2 family")
assert_true(identical(
  .veg_presence_state(c("Y", "Yes", "N", "No")),
  c("present", "present", "absent", "absent")
), "runtime gate does not recognize reviewed RELEASE-2026 Y/N presence values")
actual_bundles <- stats::setNames(lapply(BUNDLED, load_site_bundle), BUNDLED)
positive_results <- lapply(BUNDLED, function(candidate_site) {
  bundle_contract_check(
    actual_bundles[[candidate_site]], expected_site = candidate_site
  )
})
positive_failures <- BUNDLED[!vapply(
  positive_results, function(result) isTRUE(result$ok), logical(1)
)]
assert_true(!length(positive_failures), paste(
  "runtime gate rejected untouched actual bundle(s):",
  paste(vapply(positive_failures, function(candidate_site) {
    paste0(candidate_site, "=[", paste(
      positive_results[[match(candidate_site, BUNDLED)]]$reason, collapse = "; "
    ), "]")
  }, character(1)), collapse = ", ")
))
eligible_sites <- BUNDLED[vapply(actual_bundles, function(candidate) {
  is.data.frame(candidate$trees) && nrow(candidate$trees) > 0L &&
    any(!(candidate$plots$opportunity_source_missing %in% TRUE))
}, logical(1))]
assert_true(length(eligible_sites) > 0L,
            "runtime mutation fixtures found no nonempty source-backed bundle")
site <- eligible_sites[[1L]]
bundle <- actual_bundles[[site]]

source_backed <- which(!(bundle$plots$opportunity_source_missing %in% TRUE))
assert_true(length(source_backed) > 0L,
            "runtime mutation fixture found no source-backed context")
row <- source_backed[[1L]]
expected_status <- as.character(bundle$plots$tree_support[[row]])

status_probe <- bundle
status_probe$plots$tree_support[[row]] <- if (
  identical(expected_status, "sampled_absence")
) "sampled_with_records" else "sampled_absence"
status_result <- bundle_contract_check(status_probe, expected_site = site)
assert_true(!isTRUE(status_result$ok) && any(grepl(
  "support states, reasons, counts, or presence differ",
  status_result$reason, fixed = TRUE
)), "runtime gate accepted a corrupted source-backed support status")

reason_probe <- bundle
reason_probe$plots$tree_support_reason[[row]] <- paste0(
  reason_probe$plots$tree_support_reason[[row]], " [corrupted]"
)
reason_result <- bundle_contract_check(reason_probe, expected_site = site)
assert_true(!isTRUE(reason_result$ok) && any(grepl(
  "support states, reasons, counts, or presence differ",
  reason_result$reason, fixed = TRUE
)), "runtime gate accepted a corrupted source-backed support reason")

count_probe <- bundle
count_probe$plots$tree_records[[row]] <-
  as.integer(count_probe$plots$tree_records[[row]]) + 1L
count_result <- bundle_contract_check(count_probe, expected_site = site)
assert_true(!isTRUE(count_result$ok) && any(grepl(
  "support states, reasons, counts, or presence differ",
  count_result$reason, fixed = TRUE
)), "runtime gate accepted a corrupted channel record count")

assert_row_rejected <- function(probe, message) {
  result <- bundle_contract_check(probe, expected_site = site)
  assert_true(!isTRUE(result$ok) && any(grepl(
    "row-derived invariants differ", result$reason, fixed = TRUE
  )), message)
}
live_probe <- bundle
live_probe$trees$live[[1L]] <- !live_probe$trees$live[[1L]]
assert_row_rejected(
  live_probe, "runtime gate accepted live inconsistent with plantStatus"
)
year_probe <- bundle
dated_rows <- which(!is.na(year_probe$trees$year))
assert_true(length(dated_rows) > 0L,
            "runtime mutation fixture found no dated measurement row")
dated_row <- dated_rows[[1L]]
year_probe$trees$year[[dated_row]] <- year_probe$trees$year[[dated_row]] + 1L
assert_row_rejected(
  year_probe, "runtime gate accepted year inconsistent with measurement date"
)
taxonomy_probe <- bundle
taxonomy_probe$trees$taxon_label[[1L]] <- "Corrupted taxonomy"
taxonomy_probe$trees$is_species[[1L]] <- !taxonomy_probe$trees$is_species[[1L]]
assert_row_rejected(
  taxonomy_probe,
  "runtime gate accepted taxonomy derivations inconsistent with raw taxonomy"
)
identity_probe <- bundle
identity_probe$trees$permanent[[1L]] <- !identity_probe$trees$permanent[[1L]]
identity_probe$trees$plant_key[[1L]] <- "corrupted-plant-key"
identity_probe$trees$event_key[[1L]] <- "corrupted-event-key"
assert_row_rejected(
  identity_probe, "runtime gate accepted corrupted permanence or identity keys"
)

cat(sprintf(
  paste0(
    "Runtime bundle gate fixtures OK: %d untouched bundles accepted; ",
    "%s support and row-derivation mutations rejected.\n"
  ),
  length(BUNDLED), site
))
