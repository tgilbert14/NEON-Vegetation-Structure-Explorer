#----------------------------------------------------------------------
# make_og_image.R — draws docs/og-image.png (1200x630), the social card for
# the landing page. Self-contained base-R graphics in the CROSS-BIOME house
# palette (sand paper + teal-pine + desert ochre + amber), with a horizon band
# and both a forest stand AND desert shrubs — signalling the all-biome coverage.
# Regenerate:
#   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/make_og_image.R
#----------------------------------------------------------------------
ROOT <- getwd()
out  <- file.path(ROOT, "docs", "og-image.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

parch <- "#f1efe6"; paper <- "#fdfcf7"; ink <- "#1d2a24"; ink2 <- "#5c6b62"
pine  <- "#1f6a63"; bark <- "#8a5a2b"; gold <- "#E0A500"
canopy <- grDevices::colorRampPalette(c("#cfe6e2", "#7fc0b6", "#3f9a90", "#1f6a63", "#164d48"))(1200)

png(out, width = 1200, height = 630, res = 144)
op <- par(mar = c(0, 0, 0, 0), bg = parch); on.exit({ par(op); dev.off() })
plot.new(); plot.window(xlim = c(0, 1200), ylim = c(0, 630), xaxs = "i", yaxs = "i")

# warm forest-floor base + faint ring/contour texture
rect(0, 0, 1200, 630, col = parch, border = NA)
for (yy in seq(40, 600, by = 26)) segments(0, yy, 1200, yy, col = grDevices::adjustcolor(bark, .03), lwd = 1)

# canopy band across the top (the hero motif), 1px vertical gradient rects
for (i in 1:1200) rect(i - 1, 486, i, 630, col = canopy[i], border = NA)
# soft feather of the band into the page
for (k in 0:30) rect(0, 486 - k, 1200, 487 - k, col = grDevices::adjustcolor(parch, k / 30 * 0.9), border = NA)

# a faint stand of trees (trunk + canopy) for texture
tree <- function(x, y, s, col) {
  segments(x, y, x, y + s * 0.5, col = col, lwd = max(1.5, s / 6))           # trunk
  polygon(x + c(-s * 0.5, 0, s * 0.5), y + c(s * 0.5, s * 1.5, s * 0.5),     # canopy
          col = col, border = NA)
}
# a faint low desert shrub (rounded mound) for texture
shrub <- function(x, y, s, col) {
  for (a in seq(0, pi, length.out = 7)) segments(x, y, x + cos(a) * s, y + sin(a) * s * 0.7, col = col, lwd = max(1.2, s / 7))
}
set.seed(7)
for (k in 1:9)  tree(runif(1, 90, 760),  runif(1, 110, 455), runif(1, 14, 30),
                     grDevices::adjustcolor(pine, runif(1, .05, .11)))
for (k in 1:7)  shrub(runif(1, 770, 1130), runif(1, 100, 430), runif(1, 12, 22),
                      grDevices::adjustcolor(bark, runif(1, .06, .12)))

# badge
text(70, 556, "NEON · VEGETATION STRUCTURE · DP1.10098.001",
     col = "#1f4a45", cex = .9, font = 2, adj = 0)

# title
text(68, 470, "NEON Vegetation",   col = ink, cex = 3.5, font = 2, adj = 0)
text(68, 394, "Structure Explorer", col = ink, cex = 3.5, font = 2, adj = 0)
# a small amber-lit tree by the wordmark
tree(640, 404, 30, gold)

# subtitle
text(70, 322, "Every tagged tree and desert shrub's diameter and height, the stand's size",
     col = ink2, cex = 1.12, adj = 0)
text(70, 292, "structure, and how plants grow over the years. 42 NEON sites, every biome.",
     col = ink2, cex = 1.12, adj = 0)

# stat chips
chips <- list(c("42 sites", "every biome"), c("DBH + ø", "trees & shrubs"),
              c("growth", "careers"), c("real", "public data"))
x0 <- 70; gap <- 14; w <- 250; h <- 96; y1 <- 64
for (i in seq_along(chips)) {
  xl <- x0 + (i - 1) * (w + gap)
  rect(xl, y1, xl + w, y1 + h, col = paper, border = grDevices::adjustcolor(ink, .12))
  rect(xl, y1, xl + 6, y1 + h, col = pine, border = NA)                  # canopy spine
  text(xl + 22, y1 + 62, chips[[i]][1], col = ink, cex = 1.85, font = 2, adj = 0)
  text(xl + 22, y1 + 28, chips[[i]][2], col = ink2, cex = .92, adj = 0)
}
cat("wrote", out, "\n")
