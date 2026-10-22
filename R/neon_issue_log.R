# ===========================================================================
# NEON Vegetation Structure Explorer: curated NEON issue-log notes.
#
# NEON publishes an issue log (changeLogs) for DP1.10098.001. This table keeps
# the entries that change how a number in this app should be read, in plain
# words, with the NEON issue id so a reader can check the source. It was
# curated from https://data.neonscience.org/api/v0/products/DP1.10098.001
# (retrieved 2026-10-22, 38 entries). The monthly refresh warns when NEON adds
# entries newer than the bundle (scripts/fetch_veg_data.R), so this table is
# reviewed whenever that warning fires.
#
# topic: "structure" (the latest-census snapshot: basal area, density),
# "change" (growth and mortality between censuses), "coverage" (which events
# exist). sites: NEON site codes, or "ALL". Dates are the issue's affected
# date range; an entry applies to a site when any plot-event in use falls
# inside it ("coverage" entries apply to the site regardless of dates).
# ===========================================================================

VEG_ISSUE_LOG_RETRIEVED <- "2026-10-22"
VEG_ISSUE_LOG_URL <- "https://data.neonscience.org/data-products/DP1.10098.001"

VEG_ISSUE_LOG <- data.frame(
  id = c("83404", "52905", "86204", "16302", "16303", "68808", "110707", "38604"),
  topic = c("structure", "change", "change", "change", "change", "coverage", "coverage", "coverage"),
  start = as.Date(c("2023-09-19", "2020-01-01", "2020-03-01", "2014-01-01", "2014-01-01",
                    "2022-06-12", "2025-12-31", "2014-01-01")),
  end = as.Date(c("2023-11-03", "2021-12-31", "2023-08-01", "2017-03-09", "2017-03-09",
                  "2022-10-31", "2025-12-31", "2020-01-01")),
  sites = c(
    "MOAB",
    "STEI TREE UNDE",
    "STEI TREE UNDE KONZ GRSM MLBS ORNL DELA LENO CLBJ BONA DEJU PUUM",
    "MOAB JORN SRER ONAQ",
    "RMNP YELL NIWO MOAB SRER JORN ONAQ WREF SJER SOAP TEAK BONA DEJU HEAL",
    "YELL",
    "DELA",
    "HARV WOOD CLBJ GRSM ORNL OSBS HEAL GUAN ABBY DEJU KONZ SJER TREE"
  ),
  note = c(
    "From 19 Sep to 3 Nov 2023, MOAB crews measured a small multi-stem shrub only when at least half its stems reached 1 cm basal diameter, while the protocol needs one stem, so small shrubs are undercounted in plots sampled then.",
    "The annual tower plots sampled at STEI and TREE in 2020 and at UNDE in 2020 and 2021 were not NEON's intended annual subset, so change between censuses that starts in those years follows different plots.",
    "Between March 2020 and August 2023 the tower plots sampled each year here sometimes differed from NEON's priority list, so year-to-year change may follow different plots.",
    "Annual tower-plot sampling at desert sites stopped in 2017 because it damaged soils and brittle shrubs; tower plots are now remeasured every 5 years, so change rests on long intervals.",
    "Annual diameter remeasurement stopped at slow-growing sites in 2017; tower plots are now remeasured every 3 to 5 years, so change rests on long intervals.",
    "Flooding closed the Yellowstone roads from June to October 2022, so field events planned then were not sampled.",
    "NEON field work at DELA ended on 31 December 2025, so no newer censuses will come.",
    "Some plot visits before 2020 have measurements but no plot-visit record; this app keeps those rows and holds them, not zero."
  ),
  stringsAsFactors = FALSE
)

# Notes that apply to `site` for the given topics, given the dates of the
# plot-events in use. Returns the matching rows of VEG_ISSUE_LOG.
veg_issue_notes <- function(site, dates = NULL, topics = c("structure", "change", "coverage")) {
  if (is.null(site) || !length(site) || is.na(site[[1]])) return(VEG_ISSUE_LOG[0, , drop = FALSE])
  site <- toupper(as.character(site[[1]]))
  log <- VEG_ISSUE_LOG[VEG_ISSUE_LOG$topic %in% topics, , drop = FALSE]
  if (!nrow(log)) return(log)
  at_site <- vapply(strsplit(log$sites, " ", fixed = TRUE),
                    function(s) "ALL" %in% s || site %in% s, logical(1))
  log <- log[at_site, , drop = FALSE]
  if (!nrow(log)) return(log)
  d <- suppressWarnings(as.Date(dates))
  d <- d[!is.na(d)]
  in_range <- vapply(seq_len(nrow(log)), function(i) {
    if (identical(log$topic[[i]], "coverage")) return(TRUE)
    length(d) > 0L && any(d >= log$start[[i]] & d <= log$end[[i]])
  }, logical(1))
  log[in_range, , drop = FALSE]
}

# Semicolon-joined issue ids per plot-event row (site, date), for exports.
veg_issue_ids <- function(site, dates, topics = c("structure", "change", "coverage")) {
  d <- suppressWarnings(as.Date(dates))
  vapply(seq_along(d), function(i) {
    hits <- veg_issue_notes(site, d[i], setdiff(topics, "coverage"))
    if (!nrow(hits)) "" else paste(hits$id, collapse = ";")
  }, character(1))
}

# Small inline chips for a banner; NULL when nothing applies.
veg_issue_chips <- function(notes) {
  if (is.null(notes) || !nrow(notes)) return(NULL)
  htmltools::tags$div(class = "issue-chips",
    lapply(seq_len(nrow(notes)), function(i)
      htmltools::tags$span(class = "issue-chip",
        title = notes$note[[i]],
        htmltools::tags$b(paste0("NEON issue ", notes$id[[i]], ": ")),
        notes$note[[i]])))
}
