# ===========================================================================
# NEON Vegetation Structure Explorer — global.R
# A NEONize sibling (Desert Data Labs) for the Vegetation structure product
# (DP1.10098.001): individual tagged stems remeasured over years. Chrome +
# bundling spine + pin-card interaction ported from the Small Mammal Tracker /
# Plant Diversity siblings; the analysis layer is woody-structure-native.
# ===========================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons)
  library(dplyr); library(tidyr); library(stringr); library(tibble)
  library(plotly); library(leaflet); library(DT)
  library(shinyjs); library(shinycssloaders); library(RColorBrewer); library(htmltools)
})

source("R/site_metadata.R", local = FALSE)
source("R/veg_helpers.R", local = FALSE)
source("R/report_pdf.R", local = FALSE)
source("R/map_picker.R", local = FALSE)   # reusable national site-picker map (flagship front door)

NEON_DPID <- "DP1.10098.001"   # Vegetation structure
.NEON_PKG <- paste0("neon", "Utilities")
LIVE_FETCH <- (Sys.getenv("VST_LIVE", "0") != "0") && requireNamespace(.NEON_PKG, quietly = TRUE)

# ---- bundled per-site data: list(trees, plots, meta) ----------------------
SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/demo.rds"
DEMO_META <- list(site = "HARV", label = "HARV · Harvard Forest — demo")

read_bundle <- function(f) {
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) { warning(sprintf("read_bundle('%s'): %s", f, conditionMessage(e))); NULL })
  if (is.null(out)) return(NULL)
  if (is.data.frame(out)) return(out)                  # site_index
  if (is.null(out$trees) || !nrow(out$trees)) NULL else out
}
load_site_bundle <- function(site) read_bundle(file.path(SITE_DIR, paste0(site, ".rds")))
load_demo <- function() { b <- load_site_bundle(DEMO_META$site); if (!is.null(b)) b else read_bundle(DEMO_PATH) }

SITE_INDEX <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)
BUNDLED <- if (!is.null(SITE_INDEX)) SITE_INDEX$site else character(0)
site_table <- if (length(BUNDLED)) {
  m <- neon_sites[match(BUNDLED, neon_sites$site), ]
  idx_cols <- intersect(c("structure_type", "size_metric", "n_trees", "n_species",
                          "tallest_m", "biggest_dbh_cm"), names(SITE_INDEX))
  out <- cbind(m, SITE_INDEX[match(m$site, SITE_INDEX$site), idx_cols])
  if (!"structure_type" %in% names(out)) out$structure_type <- "forest"
  if (!"size_metric" %in% names(out)) out$size_metric <- "DBH"
  out
} else neon_sites[0, ]

veg_state_choices <- function() {
  st <- sort(unique(site_table$state)); if (!length(st)) return(NULL)
  setNames(st, sprintf("%s (%d)", state_names[st] %||% st, as.integer(table(site_table$state)[st])))
}
veg_sites_in_state <- function(stt) {
  rows <- site_table[site_table$state == stt, ]; rows <- rows[order(rows$name), ]
  if (!nrow(rows)) return(character(0))
  setNames(rows$site, sprintf("%s — %s", rows$site, rows$name))
}

# ---- theme: cross-biome identity ------------------------------------------
# "Sun-warmed earth meets high-country water" — a biome-neutral palette so the
# app reads as forest AND desert AND tundra AND tropical, not forest-only:
# a deep teal-pine primary (forest + alpine water), desert-ochre accent, amber
# highlight, on a sun-bleached sand canvas. No single biome owns the identity.
# Key names 'navy'/'cardinal' are KEPT for low churn but hold cross-biome values.
DDL <- list(
  navy = "#1f6a63", navy2 = "#164d48", cardinal = "#8a5a2b", gold = "#E0A500",
  gold2 = "#8a6310", sky = "#356f80", green = "#2f7d46", green2 = "#1c4d2c",
  bark = "#8a5a2b", ink = "#1d2a24", muted = "#5c6b62", bg = "#f1efe6",
  paper = "#fdfcf7", line = "#e1ddcf",
  live = "#2f7d46", dead = "#9a5a3a", rust = "#b5471f")   # rust = reserved true-error red

app_theme <- bs_theme(
  version = 5, bg = "#fdfcf7", fg = DDL$ink,
  primary = DDL$navy, secondary = DDL$bark,
  success = DDL$green, info = DDL$sky, warning = DDL$gold, danger = DDL$rust,
  base_font = font_google("Rubik"), heading_font = font_google("Rubik"), "border-radius" = "10px")

asset_url <- function(path) {
  f <- file.path("www", path)
  v <- if (file.exists(f)) as.integer(as.numeric(file.mtime(f))) else 0L
  sprintf("%s?v=%s", path, v)
}

spin <- function(x, img = NULL) shinycssloaders::withSpinner(x, color = DDL$green, type = 6)
info_pop <- function(title, ..., placement = "auto")
  bslib::popover(tags$span(class = "info-dot", bsicons::bs_icon("info-circle")), ..., title = title, placement = placement)
insight_banner <- function(icon, ..., tone = "navy")
  div(class = paste("chart-insight", paste0("ci-", tone)), bsicons::bs_icon(icon), div(class = "ci-text", ...))
glow_badge <- function(label, color = "#0C234B", glow = color)
  span(class = "glow-badge", style = sprintf("color:#fff; background:%s; border-color:%s;", color, color), label)
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon), tags$span(class = "ch-title", " ", title), ...)
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
