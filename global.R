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
DEMO_META <- list(site = "HARV", label = "HARV · Harvard Forest · demo")

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
  idx_cols <- intersect(c("structure_type", "size_metric", "n_trees", "n_plots", "n_species",
                          "tallest_m", "biggest_diam_cm"), names(SITE_INDEX))
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
  setNames(rows$site, sprintf("%s · %s", rows$site, rows$name))
}

# ---- theme: DDL desert-night creative system ------------------------------
# Matches the DDL suite cover + the Small Mammal Tracker sibling: teal primary,
# coral accent, gold highlight on a dark sky — carried by the chart layer. The
# app DEFAULTS to LIGHT (ui.R input_dark_mode mode="light"); these DDL values
# drive the plotly markers/lines, which read crisp in both modes. Key NAMES are
# KEPT (server.R references DDL$navy/$gold/$bark/etc.), VALUES remapped to the
# desert palette so every chart re-themes from this one edit.
DDL <- list(
  navy = "#102018", navy2 = "#16412a", cardinal = "#c98a4c", gold = "#ffd24a",
  gold2 = "#e0b43a", sky = "#2f8fc4", green = "#4eb86a", green2 = "#2f8a52",
  bark = "#c98a4c", ink = "#eaf4ec", muted = "#a4c0aa", bg = "#0a140e",
  paper = "#102018", line = "rgba(255,255,255,0.12)",
  live = "#4eb86a", dead = "#c98a4c", rust = "#c98a4c")   # rust = reserved bark true-error tone

# Light "desert-day" base (DEFAULT). styles.css [data-bs-theme="dark"] carries
# the full desert-night system; both modes show the dark command-band hero +
# dark stat info-boxes (the "light page, dark hero" look).
app_theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = "#16261c",
  primary = "#2f8a52", secondary = "#b07a3c",
  success = "#3f9a52", info = "#2f8fc4", warning = "#c79a1c", danger = "#b07a3c",
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
# Auto-picks DARK text (#16261c) on a bright fill (gold/canopy/bark) and white on
# a dark fill via a luminance check, so the badge reads in both themes.
glow_badge <- function(label, color = "#2f8a52", glow = color) {
  txt <- tryCatch({
    rc <- grDevices::col2rgb(color)
    if ((0.299 * rc[1] + 0.587 * rc[2] + 0.114 * rc[3]) / 255 > 0.6) "#16261c" else "#ffffff"
  }, error = function(e) "#ffffff")
  span(class = "glow-badge", style = sprintf("color:%s; background:%s; border-color:%s;", txt, color, color), label)
}
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon), tags$span(class = "ch-title", " ", title), ...)
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

# The app mascot — a flat (no-gradient, no-id so it's safely reusable) cute shrub
# in the canopy-green & gold accent. Used as the loading spinner, the splash guide,
# and the celebration hop. Parts are classed so the CSS can wiggle leaves / blink eyes.
MASCOT_CRITTER <- htmltools::HTML(paste0(
  '<svg class="mascot" viewBox="0 0 120 120" aria-hidden="true">',
  '<rect x="54" y="80" width="12" height="26" rx="3" fill="#8a5a2b"/>',
  '<g class="mascot-ear-l"><circle cx="40" cy="44" r="15" fill="#4eb86a"/></g>',
  '<g class="mascot-ear-r"><circle cx="80" cy="44" r="15" fill="#4eb86a"/></g>',
  '<ellipse cx="60" cy="58" rx="34" ry="30" fill="#4eb86a"/>',
  '<circle cx="42" cy="66" r="3" fill="#ffd24a"/>',
  '<circle cx="80" cy="50" r="2.6" fill="#ffd24a"/>',
  '<g class="mascot-eyes"><circle cx="51" cy="56" r="6.5" fill="#11331f"/>',
  '<circle cx="69" cy="56" r="6.5" fill="#11331f"/>',
  '<circle cx="49" cy="53.5" r="2.4" fill="#ffffff"/>',
  '<circle cx="69" cy="53.5" r="2.4" fill="#ffffff"/></g>',
  '</svg>'))
