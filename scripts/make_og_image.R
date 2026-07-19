# Render the canonical Living Poster social composition at exactly 1200 x 630.
#
# The words and layout live in docs/social-card.html; this script deliberately
# contains no second copy of the marketing or science claims. That keeps future
# regeneration from reviving stale cover language.

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source_html <- file.path(root, "docs", "social-card.html")
output_png <- file.path(root, "docs", "og-image.png")

if (!file.exists(source_html)) {
  stop("Missing canonical social-card source: ", source_html, call. = FALSE)
}

chrome_candidates <- unique(c(
  Sys.getenv("CHROME_BIN", unset = ""),
  unname(Sys.which(c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser", "chrome"))),
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Chromium.app/Contents/MacOS/Chromium",
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
))
chrome_candidates <- chrome_candidates[nzchar(chrome_candidates) & file.exists(chrome_candidates)]

if (!length(chrome_candidates)) {
  stop(
    "Chrome/Chromium is required to render docs/social-card.html. ",
    "Set CHROME_BIN to the browser executable and rerun.",
    call. = FALSE
  )
}

chrome <- chrome_candidates[[1]]
source_path <- normalizePath(source_html, winslash = "/", mustWork = TRUE)
output_path <- file.path(
  normalizePath(dirname(output_png), winslash = "/", mustWork = TRUE),
  basename(output_png)
)
page_url <- if (.Platform$OS.type == "windows") {
  paste0("file:///", source_path)
} else {
  paste0("file://", source_path)
}
page_url <- gsub(" ", "%20", page_url, fixed = TRUE)

render_log <- system2(
  chrome,
  args = c(
    "--headless=new",
    "--hide-scrollbars",
    "--disable-gpu",
    "--allow-file-access-from-files",
    "--force-device-scale-factor=1",
    "--window-size=1200,630",
    paste0("--screenshot=", shQuote(output_path)),
    shQuote(page_url)
  ),
  stdout = TRUE,
  stderr = TRUE
)

exit_status <- attr(render_log, "status")
if (!is.null(exit_status) && exit_status != 0L) {
  stop("Chrome failed to render the social card:\n", paste(render_log, collapse = "\n"), call. = FALSE)
}
if (!file.exists(output_png)) {
  stop("Chrome exited without writing ", output_png, call. = FALSE)
}

header <- readBin(output_png, what = "raw", n = 24L)
png_signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
if (length(header) < 24L || !identical(header[1:8], png_signature)) {
  stop("Rendered social image is not a valid PNG.", call. = FALSE)
}
be32 <- function(bytes) sum(as.numeric(bytes) * 256^(3:0))
dimensions <- c(be32(header[17:20]), be32(header[21:24]))
if (!identical(dimensions, c(1200, 630))) {
  stop("Rendered social image is ", paste(dimensions, collapse = "x"), "; expected 1200x630.", call. = FALSE)
}

cat("wrote", output_png, "from", source_html, "\n")
