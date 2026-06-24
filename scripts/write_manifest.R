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
# Run with an R that has the app's runtime packages (R 4.3.1 here has them all):
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/write_manifest.R
# Re-run whenever runtime dependencies change, then commit manifest.json.
# ===========================================================================
suppressMessages(library(rsconnect))

appFiles <- c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  Sys.glob("data/*.rds"),                                       # precomputed indexes
  list.files("data/sites", pattern = "\\.rds$", full.names = TRUE),
  list.files("data/env",   pattern = "\\.rds$", full.names = TRUE),   # env overlays
  list.files("data-sample", pattern = "\\.rds$", full.names = TRUE)
)
appFiles <- unique(appFiles[file.exists(appFiles)])

cat(sprintf("Writing manifest for %d files (%d site bundles)...\n",
            length(appFiles), length(list.files("data/sites", pattern = "\\.rds$"))))
rsconnect::writeManifest(appDir = ".", appFiles = appFiles)

# ---- pin terra to the last release before the GDAL-3.8 multidim code (1.8-54) ----
# terra >= 1.8-54 ships gdal_multidimensional.cpp using a GDAL 3.8 call unguarded in
# releases, so it FAILS to compile against Connect Cloud's GDAL 3.4.1. Connect compiles
# from source regardless of repo. 1.8-50 is the last release before 1.8-54: it compiles
# on 3.4.1 and still satisfies raster's terra (>= 1.8-5). terra/raster are install-only
# (leaflet -> raster -> terra; app never calls terra) -> zero runtime impact. Also pin
# the repo to the RSPM jammy binary mirror for suite consistency.
# NOTE: deliberately a TEXT-level edit (readLines/gsub/writeLines), NOT a jsonlite
# re-serialization — the HARD GATE below documents that re-serializing manifest.json
# mangles writeManifest()'s canonical format and Connect rejects it. A line-oriented
# substitution touches only terra's Version/RemoteSha and repo URLs, leaving the
# canonical structure and app-file checksums intact.
local({
  mtxt <- readLines("manifest.json", warn = FALSE)
  in_terra <- FALSE
  for (i in seq_along(mtxt)) {
    if (grepl('^\\s*"terra"\\s*:\\s*\\{', mtxt[i])) in_terra <- TRUE
    if (in_terra) {
      mtxt[i] <- sub('("Version"\\s*:\\s*")[^"]+(")',  '\\11.8-50\\2', mtxt[i])
      mtxt[i] <- sub('("RemoteSha"\\s*:\\s*")[^"]+(")', '\\11.8-50\\2', mtxt[i])
      if (grepl('^\\s*\\},?\\s*$', mtxt[i])) in_terra <- FALSE
    }
  }
  mtxt <- gsub("https://cloud.r-project.org", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  mtxt <- gsub("https://packagemanager.posit.co/cran/latest", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  writeLines(mtxt, "manifest.json")
  cat("Pinned terra to 1.8-50 + RSPM jammy repo (text-level; canonical format preserved).\n")
})

# ---- HARD GATE (CHECK-ONLY — never re-serialize the manifest) --------------
# neonUtilities is kept out by the dynamic .NEON_PKG reference (the scanner
# never sees it); arrow is a heavy over-capture nothing here hard-depends on.
# writeManifest() does not capture either, so there is nothing to prune — we
# only VERIFY and fail loud if either slipped in.
# CRITICAL: do NOT rewrite manifest.json here. rsconnect::writeManifest() emits
# a canonical format (file checksums, metadata) that Connect Cloud validates;
# re-serializing it with jsonlite mangles that format and Connect rejects the
# deploy as "invalid manifest." data.table is a legitimate plotly hard
# dependency (Connect's base image lacks it) and MUST stay.
banned <- c("neonUtilities", "arrow")
m <- jsonlite::fromJSON("manifest.json")
pkgs <- names(m$packages)
cat(sprintf("manifest.json written: %d packages, %d appFiles.\n", length(pkgs), length(m$files)))
hit <- intersect(banned, pkgs)
if (length(hit))
  stop(sprintf("BANNED package(s) in manifest.json: %s. Investigate the .NEON_PKG dodge / appFiles scope and regenerate — do NOT hand-edit the manifest.",
               paste(hit, collapse = ", ")))
if ("plotly" %in% pkgs && !"data.table" %in% pkgs)
  stop("data.table is MISSING while plotly is present. Connect Cloud's base image lacks data.table, so the plotly install (and the deploy) will fail. Regenerate with writeManifest; never prune data.table.")
cat("OK: lean manifest (no neonUtilities/arrow); data.table present for plotly.\n")
