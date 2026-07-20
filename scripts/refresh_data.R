#!/usr/bin/env Rscript

# Safe local orchestrator for a full Vegetation Structure candidate. This never
# deletes or overwrites the committed data tree. Point VST_CANDIDATE_ROOT at a
# new/empty directory outside the repository and leave VST_QUERY_START/END blank
# for the required full-release candidate. Closed month ranges are fetch-only
# diagnostics and are deliberately rejected before candidate construction.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
git_root <- trimws(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
if (length(git_root) != 1L ||
    !identical(normalizePath(git_root, winslash = "/", mustWork = TRUE), repo_root)) {
  stop("run the local candidate orchestrator from the exact repository root",
       call. = FALSE)
}
dirty <- system2(
  "git", c("status", "--porcelain=v1", "--untracked-files=all"),
  stdout = TRUE, stderr = TRUE
)
if (length(dirty)) {
  stop(
    "local candidate source is dirty; commit/stash every change so builder_commit names the exact executed source",
    call. = FALSE
  )
}
head_commit <- trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE))
if (length(head_commit) != 1L ||
    !grepl("^[0-9a-f]{40}$", head_commit, ignore.case = TRUE)) {
  stop("could not resolve the exact local builder commit", call. = FALSE)
}
candidate_input <- trimws(Sys.getenv("VST_CANDIDATE_ROOT", unset = ""))
if (!nzchar(candidate_input))
  stop("VST_CANDIDATE_ROOT is required; refreshes must build outside the repository",
       call. = FALSE)
dir.create(candidate_input, recursive = TRUE, showWarnings = FALSE)
candidate_root <- normalizePath(candidate_input, winslash = "/", mustWork = TRUE)
if (identical(candidate_root, repo_root) || startsWith(candidate_root, paste0(repo_root, "/")))
  stop("VST_CANDIDATE_ROOT must be outside the repository", call. = FALSE)
if (length(list.files(candidate_root, all.files = TRUE, no.. = TRUE)))
  stop("VST_CANDIDATE_ROOT must be empty", call. = FALSE)

raw_dir <- file.path(candidate_root, "raw")
site_dir <- file.path(candidate_root, "data", "sites")
sample_dir <- file.path(candidate_root, "data-sample")
site_index <- file.path(candidate_root, "data", "site_index.rds")
search_index <- file.path(candidate_root, "data", "search_index.rds")

if (!nzchar(Sys.getenv("VST_NEON_RELEASE"))) Sys.setenv(VST_NEON_RELEASE = "RELEASE-2026")
Sys.setenv(VST_RAW_OUT_DIR = raw_dir)
source("scripts/fetch_veg_data.R")

raw_digest <- trimws(readLines(file.path(raw_dir, "SOURCE-FAMILY-SHA256.txt"),
                               warn = FALSE)[1L])
fetch_runtime <- readLines(file.path(raw_dir, "FETCH-RUNTIME.txt"), warn = FALSE)
receipt_value <- function(key) {
  line <- fetch_runtime[startsWith(fetch_runtime, paste0(key, "="))]
  if (length(line) != 1L) stop("fetch runtime lacks ", key, call. = FALSE)
  sub(paste0("^", key, "="), "", line)
}
neon_version <- receipt_value("neonUtilities")
source_normalization <- receipt_value("sourceNormalization")
if (!identical(source_normalization, VST_SOURCE_NORMALIZATION)) {
  stop("fetch runtime uses an unreviewed source normalization", call. = FALSE)
}
neon_release <- receipt_value("officialNeonRelease")
release_doi <- receipt_value("releaseDoi")
query_start <- receipt_value("queryStart")
query_end <- receipt_value("queryEnd")
if (!identical(query_start, "FULL_RELEASE") ||
    !identical(query_end, "FULL_RELEASE")) {
  stop(
    "bounded query fetch completed as a local diagnostic, but cannot build a promotable candidate; rerun with VST_QUERY_START/END unset",
    call. = FALSE
  )
}
receipt_id <- sprintf("VST-DP1.10098.001-%s-sha256-%s", neon_release, raw_digest)
builder_commit <- trimws(Sys.getenv("VST_BUILDER_COMMIT", unset = ""))
if (nzchar(builder_commit) && !identical(tolower(builder_commit), tolower(head_commit))) {
  stop("VST_BUILDER_COMMIT differs from the clean working tree HEAD",
       call. = FALSE)
}
builder_commit <- head_commit

Sys.setenv(
  VST_RAW_DIR = raw_dir,
  VST_SITE_OUT_DIR = site_dir,
  VST_SAMPLE_OUT_DIR = sample_dir,
  VST_SITE_INDEX_OUT = site_index,
  VST_SEARCH_INDEX_OUT = search_index,
  VST_SITE_DIR = site_dir,
  VST_SITE_INDEX = site_index,
  VST_RECEIPT_SCHEMA_VERSION = "1",
  VST_PROVENANCE_CLASS = "official-release",
  VST_PRODUCT = "DP1.10098.001",
  VST_NEON_RELEASE = neon_release,
  VST_RELEASE_DOI = release_doi,
  VST_QUERY_START = query_start,
  VST_QUERY_END = query_end,
  VST_SOURCE_RECEIPT_ID = receipt_id,
  VST_RAW_SOURCE_DIGEST = raw_digest,
  VST_NEON_UTILITIES_VERSION = neon_version,
  VST_SOURCE_NORMALIZATION = source_normalization,
  VST_BUILT_AT = format(Sys.Date(), "%Y-%m-%d"),
  VST_BUILDER_COMMIT = builder_commit
)
source("scripts/bundle_veg_data.R")
source("scripts/build_site_index.R")
source("scripts/build_search_index.R")

cat("Candidate built without touching committed data:\n", candidate_root, "\n", sep = "")
cat("Next: validate exact bytes, generate the source receipt and manifest in a staged app tree, then open a review PR.\n")
