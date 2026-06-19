#----------------------------------------------------------------------
# make_og_image.R — draws docs/og-image.png (1200x630), the social card for
# the landing page. Self-contained base-R graphics in the "Old-Growth Canopy"
# house palette (warm forest-floor paper + canopy green + bark + sunlit amber),
# with a canopy band and a faint stand of trees — deliberately distinct from the
# navy mammal card and the parchment bird card. Regenerate:
#   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/make_og_image.R
#----------------------------------------------------------------------
ROOT <- getwd()
out  <- file.path(ROOT, "docs", "og-image.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

parch <- "#f3f1e9"; paper <- "#fffdf8"; ink <- "#20281f"; ink2 <- "#5f6f63"
pine  <- "#1f6b3a"; bark <- "#7a5230"; gold <- "#E6A700"
canopy <- grDevices::colorRampPalette(c("#bfe0c2", "#7fc090", "#3f9f5a", "#1f6b3a", "#14532a"))(1200)

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
set.seed(7)
for (k in 1:13) tree(runif(1, 90, 1120), runif(1, 90, 455), runif(1, 14, 30),
                     grDevices::adjustcolor(pine, runif(1, .05, .11)))

# badge
text(70, 556, "NEON · VEGETATION STRUCTURE · DP1.10098.001",
     col = "#2c5a3a", cex = .9, font = 2, adj = 0)

# title
text(68, 470, "NEON Vegetation",   col = ink, cex = 3.5, font = 2, adj = 0)
text(68, 394, "Structure Explorer", col = ink, cex = 3.5, font = 2, adj = 0)
# a small amber-lit tree by the wordmark
tree(640, 404, 30, gold)

# subtitle
text(70, 322, "Every tagged tree's diameter and height, the stand's size structure, and",
     col = ink2, cex = 1.12, adj = 0)
text(70, 292, "how individual trees grow over the years — on real NEON woody-survey data.",
     col = ink2, cex = 1.12, adj = 0)

# stat chips
chips <- list(c("trees", "tagged & mapped"), c("DBH × ht", "every tree"),
              c("growth", "careers"), c("instant", "no API waits"))
x0 <- 70; gap <- 14; w <- 250; h <- 96; y1 <- 64
for (i in seq_along(chips)) {
  xl <- x0 + (i - 1) * (w + gap)
  rect(xl, y1, xl + w, y1 + h, col = paper, border = grDevices::adjustcolor(ink, .12))
  rect(xl, y1, xl + 6, y1 + h, col = pine, border = NA)                  # canopy spine
  text(xl + 22, y1 + 62, chips[[i]][1], col = ink, cex = 1.85, font = 2, adj = 0)
  text(xl + 22, y1 + 28, chips[[i]][2], col = ink2, cex = .92, adj = 0)
}
cat("wrote", out, "\n")
