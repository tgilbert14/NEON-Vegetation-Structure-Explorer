#!/usr/bin/env Rscript

# Verify that manifest.json is the exact bundle-only runtime closure generated
# from the working tree in the pinned validator. Never repair a manifest here.

suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(left, right) if (is.null(left) || !length(left)) right else left

EXPECTED_R_PLATFORM <- "4.5.2"
EXPECTED_LOCALE <- "C"
EXPECTED_REPOSITORY <-
  "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
EXPECTED_CRAN_REPOSITORY <- "https://cran.r-project.org"
EXPECTED_GEO_PINS <- c(
  terra = "1.8-50", sf = "1.1-1", s2 = "1.1.11", units = "1.0-1",
  wk = "0.9.5", classInt = "0.4-11", raster = "3.6-32", sp = "2.2-1"
)
EXPECTED_GEO_URLS <- c(
  terra = "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-50.tar.gz",
  sf = "https://cran.r-project.org/src/contrib/sf_1.1-1.tar.gz",
  s2 = "https://cran.r-project.org/src/contrib/s2_1.1.11.tar.gz",
  units = "https://cran.r-project.org/src/contrib/units_1.0-1.tar.gz",
  wk = "https://cran.r-project.org/src/contrib/wk_0.9.5.tar.gz",
  classInt = "https://cran.r-project.org/src/contrib/classInt_0.4-11.tar.gz",
  raster = "https://cran.r-project.org/src/contrib/raster_3.6-32.tar.gz",
  sp = "https://cran.r-project.org/src/contrib/sp_2.2-1.tar.gz"
)

manifest <- jsonlite::read_json("manifest.json", simplifyVector = FALSE)
problems <- character(0)
note <- function(message) problems <<- c(problems, message)

if (!identical(manifest$platform, EXPECTED_R_PLATFORM))
  note(sprintf("manifest platform is %s, expected %s",
               manifest$platform %||% "missing", EXPECTED_R_PLATFORM))
if (!identical(manifest$locale, EXPECTED_LOCALE))
  note(sprintf("manifest locale is %s, expected %s",
               manifest$locale %||% "missing", EXPECTED_LOCALE))
if (!identical(manifest$metadata$appmode, "shiny")) note("manifest appmode is not shiny")

expected_files <- unique(c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  Sys.glob("data/*.rds"),
  list.files("data/sites", pattern = "[.]rds$", full.names = TRUE),
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

for (package in package_names) {
  info <- packages[[package]]
  version <- as.character(info$description$Version %||% "")
  declared <- as.character(info$description$Package %||% "")
  source <- as.character(info$Source %||% "")
  repository <- as.character(info$Repository %||% "")
  if (length(version) != 1L || is.na(version) || !nzchar(version) ||
      !identical(declared, package)) {
    note(sprintf("%s has invalid package identity/version metadata", package))
  }
  if (package %in% names(EXPECTED_GEO_PINS)) {
    remote_type <- as.character(info$description$RemoteType %||% "")
    remote_ref <- as.character(info$description$RemotePkgRef %||% "")
    built <- as.character(info$description$Built %||% "")
    expected_ref <- paste0("url::", unname(EXPECTED_GEO_URLS[[package]]))
    if (!identical(version, unname(EXPECTED_GEO_PINS[[package]]))) {
      note(sprintf("%s version is %s, expected %s", package, version,
                   unname(EXPECTED_GEO_PINS[[package]])))
    }
    if (!identical(source, "CRAN") ||
        !identical(repository, EXPECTED_CRAN_REPOSITORY) ||
        !identical(remote_type, "url") ||
        !identical(remote_ref, expected_ref) || nzchar(built)) {
      note(sprintf(
        paste0(
          "%s geo provenance Source=%s Repository=%s RemoteType=%s ",
          "RemotePkgRef=%s Built=%s; expected exact %s and no geo build clock"
        ),
        package, source, repository, remote_type, remote_ref, built, expected_ref
      ))
    }
  } else if (!identical(source, "CRAN") ||
             !identical(repository, EXPECTED_REPOSITORY)) {
    note(sprintf(
      "%s ordinary provenance is Source=%s Repository=%s; expected CRAN + %s",
      package, source, repository, EXPECTED_REPOSITORY
    ))
  }
}
missing_geo <- setdiff(names(EXPECTED_GEO_PINS), package_names)
if (length(missing_geo))
  note(sprintf("manifest lacks pinned geographic packages: %s",
               paste(missing_geo, collapse = ",")))

if (length(problems)) {
  cat("Manifest verification failed:\n", paste0("- ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat(sprintf("Manifest OK: %d files, %d packages, R %s.\n",
            length(actual_files), length(package_names), manifest$platform))
