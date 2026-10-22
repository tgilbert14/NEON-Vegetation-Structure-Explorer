#!/usr/bin/env Rscript

# Fetch the complete registered 42-site DP1.10098.001 source family into an
# isolated directory. A partial fetch never becomes a build input. The query
# selects an immutable official NEON release; provisional data are excluded.

suppressPackageStartupMessages({
  library(neonUtilities)
  library(jsonlite)
  library(digest)
})
source("scripts/vegetation_inventory.R")

DPID <- "DP1.10098.001"
OUT_DIR <- Sys.getenv("VST_RAW_OUT_DIR", unset = "../veg-data-fetch")
NEON_RELEASE <- Sys.getenv("VST_NEON_RELEASE", unset = "RELEASE-2026")
QUERY_START <- Sys.getenv("VST_QUERY_START", unset = "")
QUERY_END <- Sys.getenv("VST_QUERY_END", unset = "")
if (!grepl("^RELEASE-[0-9]{4}$", NEON_RELEASE))
  stop("VST_NEON_RELEASE must be an explicit official release tag", call. = FALSE)
if (xor(nzchar(QUERY_START), nzchar(QUERY_END)))
  stop("set both VST_QUERY_START and VST_QUERY_END, or leave both blank for the full release",
       call. = FALSE)
if (nzchar(QUERY_START) && !grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", QUERY_START))
  stop("VST_QUERY_START must be YYYY-MM", call. = FALSE)
if (nzchar(QUERY_END) && !grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", QUERY_END))
  stop("VST_QUERY_END must be YYYY-MM", call. = FALSE)
if (nzchar(QUERY_END) && QUERY_END < QUERY_START)
  stop("VST_QUERY_END must not precede VST_QUERY_START", call. = FALSE)
if (utils::packageVersion("neonUtilities") < package_version("4.0.1"))
  stop("NEON data downloads need neonUtilities >= 4.0.1 (token auth)", call. = FALSE)

token <- trimws(Sys.getenv("NEON_TOKEN", unset = ""))
if (!nzchar(token)) {
  token <- tryCatch(trimws(readLines(".neon_token", warn = FALSE))[1L],
                    error = function(error) "")
}
if (!nzchar(token))
  stop("NEON_TOKEN is required for data downloads; the token is never printed", call. = FALSE)

safe_error <- function(error) {
  message <- conditionMessage(error)
  secrets <- unique(c(token, utils::URLencode(token, reserved = TRUE)))
  for (secret in secrets[nzchar(secrets)]) {
    message <- gsub(secret, "<redacted>", message, fixed = TRUE)
  }
  message <- gsub(
    "([?&](token|api[_-]?token|key)=)[^&[:space:]]+",
    "\\1<redacted>", message, ignore.case = TRUE, perl = TRUE
  )
  message <- gsub("(Authorization:[[:space:]]*)[^[:space:]]+",
                  "\\1<redacted>", message, ignore.case = TRUE, perl = TRUE)
  sprintf("%s: %s", class(error)[[1L]], message)
}

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
existing <- list.files(OUT_DIR, all.files = TRUE, no.. = TRUE)
if (length(existing))
  stop("VST_RAW_OUT_DIR must be empty; use a new staging directory", call. = FALSE)

product_metadata <- tryCatch({
  response <- jsonlite::fromJSON(
    sprintf("https://data.neonscience.org/api/v0/products/%s", DPID),
    simplifyVector = FALSE
  )
  response$data
}, error = function(error) {
  stop("NEON product availability request failed: ", conditionMessage(error), call. = FALSE)
})

# ---- upstream watches (report only; never fail the run) -------------------
# Both read the anonymous product metadata above. They print GitHub warning
# annotations and, inside Actions, append to the step summary. They never
# touch OUT_DIR, so the raw staging inventory and checksums are unchanged.
watch_summary <- function(lines) {
  path <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
  if (nzchar(path)) cat(lines, file = path, sep = "\n", append = TRUE)
}
watch_clean <- function(text, limit = 200L) {
  text <- gsub("[[:space:]]+", " ", trimws(as.character(text %||% "")))
  if (nchar(text) > limit) text <- paste0(substr(text, 1L, limit - 3L), "...")
  gsub("%", "%25", text, fixed = TRUE)
}
watch_release <- function() {
  tags <- vapply(product_metadata$releases %||% list(),
                 function(item) as.character(item$release %||% ""), "")
  tags <- tags[grepl("^RELEASE-[0-9]{4}$", tags)]
  if (!length(tags)) return(invisible(NULL))
  newest <- tags[which.max(as.integer(sub("^RELEASE-", "", tags)))]
  if (as.integer(sub("^RELEASE-", "", newest)) >
      as.integer(sub("^RELEASE-", "", NEON_RELEASE))) {
    line <- sprintf("::warning title=Newer NEON release::%s is published; this run fetched %s",
                    newest, NEON_RELEASE)
    cat(line, "\n", sep = "")
    watch_summary(line)
  }
  invisible(NULL)
}
watch_cutoff <- function() {
  date_pattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  runtime <- tryCatch(readLines("data/source/vegetation-fetch-runtime.txt", warn = FALSE),
                      error = function(error) character(0))
  hit <- grep(sprintf("^(builtAt|buildDate|queryDate|fetchedAt)=%s", date_pattern),
              runtime, value = TRUE)
  if (length(hit)) {
    return(list(date = as.Date(regmatches(hit[1L], regexpr(date_pattern, hit[1L]))),
                source = "data/source/vegetation-fetch-runtime.txt"))
  }
  receipt <- tryCatch(readLines("docs/VEGETATION-SOURCE-RECEIPT.md", warn = FALSE),
                      error = function(error) character(0))
  hit <- grep(sprintf("bundle build date: `%s`", date_pattern), receipt, value = TRUE)
  if (length(hit)) {
    return(list(date = as.Date(regmatches(hit[1L], regexpr(date_pattern, hit[1L]))),
                source = "docs/VEGETATION-SOURCE-RECEIPT.md build date"))
  }
  list(date = Sys.Date() - 120L, source = "last 120 days (no committed receipt date parsed)")
}
watch_issue_log <- function() {
  cutoff <- watch_cutoff()
  entries <- product_metadata$changeLogs %||% list()
  entry_date <- function(entry) {
    raw <- as.character(entry$issueDate %||% entry$dateRangeStart %||% "")
    if (!length(raw) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", raw)) return(as.Date(NA))
    as.Date(substr(raw, 1L, 10L))
  }
  dates <- do.call(c, c(list(as.Date(character(0))), lapply(entries, entry_date)))
  recent <- which(!is.na(dates) & dates > cutoff$date)
  if (!length(recent)) return(invisible(NULL))
  rows <- character(0)
  for (index in recent[order(dates[recent])]) {
    entry <- entries[[index]]
    id <- watch_clean(entry$id, 20L)
    when <- format(dates[[index]])
    where <- watch_clean(entry$locationAffected, 80L)
    text <- watch_clean(entry$issue)
    cat(sprintf("::warning title=NEON issue log::%s (%s; %s): %s\n", id, when, where, text))
    rows <- c(rows, sprintf("| %s | %s | %s | %s |", id, when,
                            gsub("|", "/", where, fixed = TRUE),
                            gsub("|", "/", watch_clean(entry$issue, 120L), fixed = TRUE)))
  }
  watch_summary(c(
    "", sprintf("### NEON issue log entries after %s (%s)", format(cutoff$date), cutoff$source),
    "", "| id | date | locations | issue |", "| --- | --- | --- | --- |", rows, ""
  ))
  invisible(NULL)
}
for (watch in list(watch_release, watch_issue_log)) {
  tryCatch(watch(), error = function(error) {
    cat(sprintf("::warning title=NEON metadata watch::skipped: %s\n",
                watch_clean(conditionMessage(error))))
  })
}
release_sites <- sort(unique(vapply(Filter(function(site) {
  any(vapply(site$availableReleases %||% list(), function(item) {
    identical(item$release, NEON_RELEASE)
  }, logical(1)))
}, product_metadata$siteCodes), function(site) site$siteCode, "")))
release_rows <- Filter(function(item) identical(item$release, NEON_RELEASE),
                       product_metadata$releases %||% list())
if (length(release_rows) != 1L || is.null(release_rows[[1L]]$productDoi$url))
  stop("the requested official release or its product DOI is absent from product metadata",
       call. = FALSE)
RELEASE_DOI <- release_rows[[1L]]$productDoi$url
if (!identical(release_sites, sort(VST_EXPECTED_SITES))) {
  stop(sprintf(
    "%s availability changed; review product scope: missing=[%s] new=[%s]",
    NEON_RELEASE,
    paste(setdiff(VST_EXPECTED_SITES, release_sites), collapse = ","),
    paste(setdiff(release_sites, VST_EXPECTED_SITES), collapse = ",")
  ), call. = FALSE)
}

required_tables <- c(
  "vst_mappingandtagging", "vst_apparentindividual", "vst_perplotperyear"
)
failures <- character(0)

fetch_site <- function(site) {
  args <- list(
    dpID = DPID, site = site, release = NEON_RELEASE,
    package = "basic", check.size = FALSE, progress = FALSE,
    include.provisional = FALSE,
    token = token
  )
  if (nzchar(QUERY_START)) {
    args$startdate <- QUERY_START
    args$enddate <- QUERY_END
  }
  result <- tryCatch(
    do.call(neonUtilities::loadByProduct, args),
    error = function(error) error
  )
  if (inherits(result, "error")) {
    failures <<- c(failures, sprintf("%s: %s", site, safe_error(result)))
    return(invisible(FALSE))
  }
  missing <- setdiff(required_tables, names(result))
  empty <- required_tables[vapply(required_tables, function(table) {
    !is.data.frame(result[[table]]) || nrow(result[[table]]) == 0L
  }, logical(1))]
  if (length(missing) || length(empty)) {
    failures <<- c(failures, sprintf(
      "%s: missing tables=[%s], empty tables=[%s]", site,
      paste(missing, collapse = ","), paste(empty, collapse = ",")
    ))
    return(invisible(FALSE))
  }
  portable <- stats::setNames(lapply(required_tables, function(table) {
    vst_portable_source_table(result[[table]], paste(site, table))
  }), required_tables)
  saveRDS(
    portable, file.path(OUT_DIR, paste0(site, "_raw.rds")),
    compress = "xz", version = 3
  )
  cat(sprintf("%s: mapping=%d apparent=%d plots=%d\n", site,
              nrow(result$vst_mappingandtagging),
              nrow(result$vst_apparentindividual),
              nrow(result$vst_perplotperyear)))
  invisible(TRUE)
}

for (site in VST_EXPECTED_SITES) fetch_site(site)
if (length(failures)) {
  cat("Fetch failures:\n", paste0("- ", failures, collapse = "\n"), "\n")
  stop(sprintf("the source family is incomplete (%d failed sites)", length(failures)),
       call. = FALSE)
}
vst_assert_site_inventory(OUT_DIR, suffix = "_raw.rds", label = "raw source")

raw_files <- file.path(OUT_DIR, paste0(sort(VST_EXPECTED_SITES), "_raw.rds"))
inventory <- vapply(raw_files, digest::digest, character(1), algo = "sha256", file = TRUE)
lines <- sprintf("%s %s", unname(inventory), basename(raw_files))
writeLines(lines, file.path(OUT_DIR, "SOURCE-SHA256SUMS.txt"), useBytes = TRUE)
family_digest <- digest::digest(
  paste0(paste0(lines, collapse = "\n"), "\n"),
  algo = "sha256", serialize = FALSE
)
writeLines(family_digest, file.path(OUT_DIR, "SOURCE-FAMILY-SHA256.txt"), useBytes = TRUE)
writeLines(c(
  sprintf("product=%s", DPID),
  sprintf("officialNeonRelease=%s", NEON_RELEASE),
  sprintf("releaseDoi=%s", RELEASE_DOI),
  sprintf("queryStart=%s", if (nzchar(QUERY_START)) QUERY_START else "FULL_RELEASE"),
  sprintf("queryEnd=%s", if (nzchar(QUERY_END)) QUERY_END else "FULL_RELEASE"),
  sprintf("neonUtilities=%s", as.character(utils::packageVersion("neonUtilities"))),
  sprintf("sourceNormalization=%s", VST_SOURCE_NORMALIZATION)
), file.path(OUT_DIR, "FETCH-RUNTIME.txt"), useBytes = TRUE)

cat(sprintf("COMPLETE: %d raw sites; raw family SHA-256 %s\n",
            length(raw_files), family_digest))
