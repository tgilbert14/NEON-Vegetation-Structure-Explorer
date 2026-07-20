# ===========================================================================
# write_manifest.R — (re)generate manifest.json for a lean, bundle-only
# Posit Connect Cloud deploy (git-backed).
#
# Bundles ONLY what the running app needs: global/ui/server + R/ + www/ + the
# precomputed indexes (data/*.rds) + the per-site bundles (data/sites/*.rds) +
# the demo sample. It does NOT bundle scripts/, docs/, rsconnect/, or the README.
#
# neonUtilities is intentionally EXCLUDED — it's referenced dynamically in
# global.R (.NEON_PKG) so the dependency scanner never pins it, keeping the
# deploy lean (no wasm build; live-pull-on-cold-worker is a hang risk). The
# deployed app is bundle-only; the optional live-fetch still works in local dev.
#
# Run only in the pinned validator (R 4.5.2 + the dated Jammy repository):
#   Rscript --vanilla scripts/write_manifest.R
# Re-run whenever runtime dependencies change, then commit manifest.json.
# ===========================================================================
suppressMessages({
  library(rsconnect)
  library(jsonlite)
})

`%||%` <- function(left, right) {
  if (is.null(left) || !length(left)) right else left
}

RSPM_SNAPSHOT <-
  "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
CRAN_REPOSITORY <- "https://cran.r-project.org"
R_PLATFORM_PIN <- "4.5.2"

# leaflet pulls this native closure into Connect even though the app uses only
# markers and tiles. CI installs these exact CRAN tarballs before writeManifest
# runs; this script verifies the installed truth and never fabricates Version or
# RemoteSha metadata after the fact.
GEO_PINS <- c(
  terra = "1.8-50", sf = "1.1-1", s2 = "1.1.11", units = "1.0-1",
  wk = "0.9.5", classInt = "0.4-11", raster = "3.6-32", sp = "2.2-1"
)
GEO_URLS <- c(
  terra = "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-50.tar.gz",
  sf = "https://cran.r-project.org/src/contrib/sf_1.1-1.tar.gz",
  s2 = "https://cran.r-project.org/src/contrib/s2_1.1.11.tar.gz",
  units = "https://cran.r-project.org/src/contrib/units_1.0-1.tar.gz",
  wk = "https://cran.r-project.org/src/contrib/wk_0.9.5.tar.gz",
  classInt = "https://cran.r-project.org/src/contrib/classInt_0.4-11.tar.gz",
  raster = "https://cran.r-project.org/src/contrib/raster_3.6-32.tar.gz",
  sp = "https://cran.r-project.org/src/contrib/sp_2.2-1.tar.gz"
)

appFiles <- c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  Sys.glob("data/*.rds"),                                       # precomputed indexes
  list.files("data/sites", pattern = "\\.rds$", full.names = TRUE),
  list.files("data-sample", pattern = "\\.rds$", full.names = TRUE)
)
appFiles <- unique(appFiles[file.exists(appFiles)])

cat(sprintf("Writing manifest for %d files (%d site bundles)...\n",
            length(appFiles), length(list.files("data/sites", pattern = "\\.rds$"))))
rsconnect::writeManifest(appDir = ".", appFiles = appFiles)

# ---- freeze ordinary packages to the exact dated Jammy snapshot -----------
# Direct URL installs retain their exact tarball in RemotePkgRef. Do not rewrite
# CRAN URLs globally: doing so would destroy that immutable provenance. Only the
# ordinary repository aliases emitted by writeManifest are frozen here.
mtxt <- readLines("manifest.json", warn = FALSE)
for (repository in c(
  "https://cloud.r-project.org",
  "https://cran.rstudio.com",
  "https://packagemanager.posit.co/cran/latest",
  "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
)) {
  mtxt <- gsub(repository, RSPM_SNAPSHOT, mtxt, fixed = TRUE)
}
writeLines(mtxt, "manifest.json")
cat(sprintf("Ordinary package repository frozen to %s.\n", RSPM_SNAPSHOT))

# ---- canonicalize only non-semantic geo build clocks and deploy lane -------
# Source-built DESCRIPTION records contain a wall-clock Built field, so the same
# exact package compiles to different manifest bytes on different validator runs.
# Remove that field only for the named URL closure. Connect also needs a complete
# network URL in top-level Repository (a symbolic `CRAN` value produced the prior
# wk archive outage), while RemoteType/RemotePkgRef retain the exact install input.
canonical <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
for (package in names(GEO_PINS)) {
  if (!is.null(canonical$packages[[package]]$description)) {
    canonical$packages[[package]]$description$Built <- NULL
    canonical$packages[[package]]$Source <- "CRAN"
    canonical$packages[[package]]$Repository <- CRAN_REPOSITORY
  }
}
jsonlite::write_json(
  canonical, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null"
)
cat(paste0(
  "Canonicalized only the geo Built clocks and their absolute CRAN deployment lane; ",
  "installed Version/RemoteSha metadata was not rewritten.\n"
))

# ---- HARD GATE --------------------------------------------------------------
# neonUtilities is kept out by the dynamic .NEON_PKG reference (the scanner
# never sees it); arrow is a heavy over-capture nothing here hard-depends on.
# writeManifest() does not capture either, so there is nothing to prune — we
# only VERIFY and fail loud if either slipped in.
# data.table is a legitimate plotly hard dependency (Connect's base image lacks
# it) and MUST stay.
banned <- c("neonUtilities", "arrow")
m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
pkgs <- names(m$packages)
cat(sprintf("manifest.json written: %d packages, %d appFiles.\n", length(pkgs), length(m$files)))
hit <- intersect(banned, pkgs)
if (length(hit))
  stop(sprintf("BANNED package(s) in manifest.json: %s. Investigate the .NEON_PKG dodge / appFiles scope and regenerate — do NOT hand-edit the manifest.",
               paste(hit, collapse = ", ")))
if ("plotly" %in% pkgs && !"data.table" %in% pkgs)
  stop("data.table is MISSING while plotly is present. Connect Cloud's base image lacks data.table, so the plotly install (and the deploy) will fail. Regenerate with writeManifest; never prune data.table.")

bad <- character(0)
if (!identical(m$platform, R_PLATFORM_PIN)) {
  bad <- c(bad, sprintf(
    "platform=%s (want actual %s)", m$platform %||% "<missing>", R_PLATFORM_PIN
  ))
}
for (package in pkgs) {
  info <- m$packages[[package]]
  source <- as.character(info$Source %||% "")
  repository <- as.character(info$Repository %||% "")
  version <- as.character(info$description$Version %||% "")
  declared <- as.character(info$description$Package %||% "")
  if (!identical(declared, package) || length(version) != 1L ||
      is.na(version) || !nzchar(version)) {
    bad <- c(bad, sprintf("%s has invalid package identity/version metadata", package))
  }
  if (package %in% names(GEO_PINS)) {
    remote_type <- as.character(info$description$RemoteType %||% "")
    remote_ref <- as.character(info$description$RemotePkgRef %||% "")
    built <- as.character(info$description$Built %||% "")
    expected_ref <- paste0("url::", unname(GEO_URLS[[package]]))
    if (!identical(version, unname(GEO_PINS[[package]])) ||
        !identical(source, "CRAN") ||
        !identical(repository, CRAN_REPOSITORY) ||
        !identical(remote_type, "url") ||
        !identical(remote_ref, expected_ref) || nzchar(built)) {
      bad <- c(bad, sprintf(
        paste0(
          "%s origin/version Source=%s Repository=%s Version=%s ",
          "RemoteType=%s RemotePkgRef=%s Built=%s (want exact %s and no geo build clock)"
        ),
        package, source, repository, version, remote_type, remote_ref, built,
        expected_ref
      ))
    }
  } else if (!identical(source, "CRAN") ||
             !identical(repository, RSPM_SNAPSHOT)) {
    bad <- c(bad, sprintf(
      "%s ordinary provenance Source=%s Repository=%s (want CRAN + %s)",
      package, source, repository, RSPM_SNAPSHOT
    ))
  }
}
missing_geo <- setdiff(names(GEO_PINS), pkgs)
if (length(missing_geo)) {
  bad <- c(bad, sprintf(
    "missing pinned geographic packages: %s", paste(missing_geo, collapse = ",")
  ))
}
if (length(bad)) {
  stop(sprintf(
    paste0(
      "MANIFEST PROVENANCE GATE FAILED: %s. The validator must install the exact ",
      "declared closure; never repair Version or RemoteSha in manifest.json."
    ),
    paste(bad, collapse = "; ")
  ), call. = FALSE)
}
cat(paste0(
  "OK: lean manifest; exact installed geo URL closure; ordinary packages on the ",
  "dated Jammy snapshot.\n"
))
