#!/usr/bin/env Rscript

# Propose rows for the two reviewed curation files from official raw
# DP1.10098.001 tables. This script only proposes: a person reviews every row
# before it is copied into scripts/curation/*.csv, and the bundle builder
# (scripts/bundle_veg_data.R) fails when a reviewed row goes stale.
#
#   vst_partial_event_completeness.csv  one row per dataCollected = "partial"
#     event; says per woody channel whether the remark shows that channel was
#     measured in full.
#   vst_event_disturbance.csv  one row per event whose remark matches the
#     disturbance regex below; classes "none" records a reviewed exclusion
#     (snow, access, "not yet burned").
#
# Usage: VST_RAW_DIR=<dir of SITE_raw.rds> Rscript scripts/curation/propose_vst_curation.R <out_dir>

source("scripts/curation/vst_curation.R")

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1L]] else stop("usage: propose_vst_curation.R <out_dir>")
raw_dir <- Sys.getenv("VST_RAW_DIR", unset = "../veg-data-fetch")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files <- sort(list.files(raw_dir, pattern = "_raw[.]rds$", full.names = TRUE))
if (!length(files)) stop("no *_raw.rds files under ", raw_dir)

partial <- list()
disturbance <- list()
for (path in files) {
  site <- sub("_raw[.]rds$", "", basename(path))
  perplot <- as.data.frame(readRDS(path)$vst_perplotperyear, stringsAsFactors = FALSE)
  remarks <- if ("remarks" %in% names(perplot)) as.character(perplot$remarks) else
    rep(NA_character_, nrow(perplot))
  collected <- as.character(perplot$dataCollected)
  is_partial <- !is.na(collected) & tolower(trimws(collected)) == "partial"
  if (any(is_partial)) {
    partial[[site]] <- data.frame(
      site = site, plotID = perplot$plotID[is_partial],
      eventID = perplot$eventID[is_partial],
      opportunity_source_uid = perplot$uid[is_partial],
      dataCollected = collected[is_partial],
      remarks_sha256 = vst_remarks_sha256(remarks[is_partial]),
      remarks = remarks[is_partial], stringsAsFactors = FALSE
    )
  }
  hit <- !is.na(remarks) & grepl(VST_DISTURBANCE_REGEX, remarks, ignore.case = TRUE)
  if (any(hit)) {
    disturbance[[site]] <- data.frame(
      site = site, plotID = perplot$plotID[hit], eventID = perplot$eventID[hit],
      source_uid = perplot$uid[hit],
      remarks_sha256 = vst_remarks_sha256(remarks[hit]),
      remarks = remarks[hit], stringsAsFactors = FALSE
    )
  }
}
partial <- do.call(rbind, partial)
disturbance <- do.call(rbind, disturbance)
utils::write.csv(partial, file.path(out_dir, "proposed_partial_events.csv"), row.names = FALSE)
utils::write.csv(disturbance, file.path(out_dir, "proposed_disturbance_events.csv"), row.names = FALSE)
cat(sprintf("proposed %d partial events and %d disturbance candidates under %s\n",
            nrow(partial), nrow(disturbance), out_dir))
