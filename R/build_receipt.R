# ===========================================================================
# build_receipt.R: one definition of the deployed file set, and a build digest
# the live page carries so a post-deploy check can prove which revision runs.
#
# veg_app_files() is the exact appFiles list scripts/write_manifest.R passes to
# rsconnect::writeManifest(); scripts/verify_manifest.R checks the committed
# manifest against the same list. The digest is sha256 over the sorted lines
# "<path>:<md5>" (one per file, each ending in "\n"), which the post-deploy
# workflow recomputes from manifest.json's "files" map (md5 checksums).
# Base R + tools + digest only (digest is already in the runtime closure).
# ===========================================================================

veg_app_files <- function(root = ".") {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  files <- c(
    "global.R", "ui.R", "server.R",
    list.files("R", pattern = "\\.R$", full.names = TRUE),
    list.files("www", recursive = TRUE, full.names = TRUE),
    Sys.glob("data/*.rds"),                                       # precomputed indexes
    list.files("data/sites", pattern = "\\.rds$", full.names = TRUE),
    list.files("data-sample", pattern = "\\.rds$", full.names = TRUE)
  )
  unique(files[file.exists(files)])
}

veg_build_digest_text <- function(root = ".") {
  files <- veg_app_files(root)
  md5 <- unname(tools::md5sum(file.path(root, files)))
  lines <- sort(sprintf("%s:%s", files, md5), method = "radix")
  paste0(lines, "\n", collapse = "")
}

veg_build_digest <- function(root = ".") {
  digest::digest(veg_build_digest_text(root), algo = "sha256", serialize = FALSE)
}

# Computed once when the app starts; a failure never blocks the app.
VEG_BUILD_DIGEST <- tryCatch(veg_build_digest("."), error = function(error) "unavailable")

veg_build_meta_tag <- function() {
  htmltools::tags$meta(name = "ddl-build-digest", content = VEG_BUILD_DIGEST)
}
