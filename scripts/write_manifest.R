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

# ---- LEAN PRUNE + HARD GATE -----------------------------------------------
# Banned packages must never ship in this bundle-only deploy. neonUtilities is
# kept out by the dynamic .NEON_PKG reference (the scanner never sees it); arrow
# is a heavy, wasm-hostile over-capture nothing here hard-depends on. We prune
# those two, then gate to fail loud if either reappears.
# IMPORTANT: data.table is NOT banned. plotly *Imports* data.table as a hard
# dependency, and Connect Cloud's base image does NOT provide it, so pruning it
# breaks the plotly install and the whole deploy. It must stay in the manifest.
banned <- c("neonUtilities", "arrow")

prune_banned <- function() {
  raw <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  if (is.null(raw$packages)) return(invisible())
  keep <- setdiff(names(raw$packages), banned)
  dropped <- setdiff(names(raw$packages), keep)
  raw$packages <- raw$packages[keep]
  jsonlite::write_json(raw, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
  dropped
}

# Sanity: refuse to prune anything a kept package HARD-depends on (that would
# break the deploy). If a banned pkg is a true hard dependency, stop() instead.
m0 <- jsonlite::fromJSON("manifest.json")
for (b in intersect(banned, names(m0$packages))) {
  dependents <- Filter(function(p) {
    dd <- tryCatch(tools::package_dependencies(p, which = c("Depends","Imports","LinkingTo"))[[1]],
                   error = function(e) NULL)
    !is.null(dd) && b %in% dd
  }, setdiff(names(m0$packages), b))
  if (length(dependents))
    stop(sprintf("Banned package '%s' is a HARD dependency of: %s — cannot prune safely. Investigate before deploying.",
                 b, paste(dependents, collapse = ", ")))
}

dropped <- prune_banned()
if (length(dropped)) cat(sprintf("Pruned spurious banned package(s): %s\n", paste(dropped, collapse = ", ")))

# Final gate on the on-disk manifest.
m <- jsonlite::fromJSON("manifest.json")
pkgs <- names(m$packages)
cat(sprintf("manifest.json written: %d packages, %d appFiles.\n", length(pkgs), length(m$files)))
hit <- banned[vapply(banned, function(b) any(grepl(b, pkgs, ignore.case = TRUE)), logical(1))]
if (length(hit))
  stop(sprintf("BANNED package(s) still in manifest.json: %s. The deploy must stay lean (bundle-only).",
               paste(hit, collapse = ", ")))
cat("OK: no banned packages (neonUtilities/arrow) in the manifest; data.table KEPT (plotly hard-dep).\n")
