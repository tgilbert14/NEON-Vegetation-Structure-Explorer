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
