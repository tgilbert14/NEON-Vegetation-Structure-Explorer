#!/usr/bin/env Rscript

# Verify that manifest.json is the exact bundle-only runtime closure generated
# from the working tree in the pinned validator. Never repair a manifest here.

suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(left, right) if (is.null(left) || !length(left)) right else left

manifest <- jsonlite::read_json("manifest.json", simplifyVector = FALSE)
problems <- character(0)
note <- function(message) problems <<- c(problems, message)

if (!identical(manifest$platform, "4.5.2"))
  note(sprintf("manifest platform is %s, expected 4.5.2", manifest$platform %||% "missing"))
if (!identical(manifest$metadata$appmode, "shiny")) note("manifest appmode is not shiny")

expected_files <- unique(c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  Sys.glob("data/*.rds"),
  list.files("data/sites", pattern = "[.]rds$", full.names = TRUE),
  list.files("data/env", pattern = "[.]rds$", full.names = TRUE),
  list.files("data-sample", pattern = "[.]rds$", full.names = TRUE)
))
expected_files <- sort(expected_files[file.exists(expected_files)])
actual_files <- sort(names(manifest$files))
if (!identical(actual_files, expected_files)) {
  note(sprintf("manifest file closure differs: missing=[%s] extra=[%s]",
               paste(setdiff(expected_files, actual_files), collapse = ","),
               paste(setdiff(actual_files, expected_files), collapse = ",")))
}
for (path in intersect(expected_files, actual_files)) {
  expected_md5 <- unname(tools::md5sum(path))
  actual_md5 <- manifest$files[[path]]$checksum
  if (!identical(actual_md5, expected_md5))
    note(sprintf("manifest checksum drift: %s", path))
}

packages <- manifest$packages
package_names <- names(packages)
required <- c(
  "shiny", "bslib", "bsicons", "dplyr", "tidyr", "stringr", "tibble",
  "plotly", "leaflet", "DT", "shinyjs", "shinycssloaders", "RColorBrewer",
  "htmltools", "digest", "jsonlite", "data.table"
)
forbidden <- c("neonUtilities", "arrow", "rsconnect")
missing_packages <- setdiff(required, package_names)
if (length(missing_packages))
  note(sprintf("manifest lacks runtime packages: %s", paste(missing_packages, collapse = ",")))
hit <- intersect(forbidden, package_names)
if (length(hit)) note(sprintf("manifest contains build-only packages: %s", paste(hit, collapse = ",")))

geo_versions <- c(
  terra = "1.8-50", sf = "1.1-1", s2 = "1.1.11", units = "1.0-1",
  wk = "0.9.5", classInt = "0.4-11", raster = "3.6-32", sp = "2.2-1"
)
for (package in names(geo_versions)) {
  if (is.null(packages[[package]])) {
    note(sprintf("manifest lacks pinned geographic package %s", package))
    next
  }
  version <- packages[[package]]$description$Version
  if (!identical(version, unname(geo_versions[[package]])))
    note(sprintf("%s version is %s, expected %s", package, version, geo_versions[[package]]))
}

repository <- "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
for (package in package_names) {
  value <- packages[[package]]$Repository
  if (is.null(value) || length(value) != 1L || !nzchar(value) ||
      !grepl("^https://", value)) {
    note(sprintf("%s has a blank or non-HTTPS Repository field", package))
  } else if (!identical(value, repository)) {
    note(sprintf("%s repository is not the pinned Jammy snapshot: %s", package, value))
  }
}

if (length(problems)) {
  cat("Manifest verification failed:\n", paste0("- ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat(sprintf("Manifest OK: %d files, %d packages, R %s.\n",
            length(actual_files), length(package_names), manifest$platform))
