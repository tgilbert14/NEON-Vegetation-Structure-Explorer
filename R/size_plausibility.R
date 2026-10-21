# ===========================================================================
# NEON Vegetation Structure Explorer: shared size-plausibility screen.
#
# One definition, used by the bundle builder (scripts/bundle_veg_data.R), the
# independent consumer-parity gate (scripts/derived_parity.R) and the app
# (R/veg_helpers.R, server.R, R/report_pdf.R). Base R only, so the builder can
# source it without the app helpers.
#
# The screen never deletes or edits a row. A row that fails is kept in every
# table, count, basal-area and growth summary exactly as before; it is only
# left out of the size EXTREMES (tallest / biggest headlines, the champions
# podium, the network ranking and the compare table) and it raises a
# warn-level "size_spike" QC flag that says "verify, not wrong". The screen is
# per measure: a failing height leaves the tallest extremes, a failing
# diameter leaves the biggest extremes, and a rule that cannot tell which
# number is wrong (the height-to-DBH ratio) leaves both.
#
# Rules (cedar, 2026-10-21; NEON DP1.10098.001 has no published height screen):
#   a) same-plant spike: a height or diameter more than 3x the median of the
#      same plant's OTHER events AND more than 5 m / 5 cm above it. Each event
#      contributes its largest stem, so a multi-stem plant is compared with
#      its own biggest stems, not with its small ones. Heights compare intact
#      LIVE records only: a snapped, broken or physically damaged stem
#      (including "Live, broken bole") is legitimately far shorter than its
#      intact self, as are standing-dead stems. Diameters compare every event.
#   b) tree forms: height (m) / DBH (cm) > 2.5 on stems with DBH >= 10 cm.
#      Lead decision over cedar's 2.5 cm floor: on RELEASE-2026, 71 of the 78
#      live ratio flags below 10 cm were lettered secondary boles that carry
#      the whole tree's height (protocol-normal, not an entry error).
#   c) shrub and sapling forms: height > 10 m with basal diameter < 5 cm or
#      missing (no shrub ratio rule: ocotillo-like canes are thin and tall).
#   d) any form: height > 100 m.
# Single-event plants are screened by (b) to (d) only. Published qualifier
# fields (heightQualifier, dataQF) never rescue a row. With exactly two events
# a spike can only be read as the larger value; the screen flags it, which
# errs toward leaving a possibly true record out of the extremes.
# ===========================================================================

VST_SIZE_SCREEN_VERSION <- "size-screen-v1"
VST_SIZE_SCREEN_TREE_FORMS <- c("single bole tree", "multi-bole tree")
VST_SIZE_SCREEN_SHRUB_FORMS <- c("single shrub", "small shrub", "sapling")
VST_SIZE_SCREEN_RULES <- list(
  spike_ratio = 3, spike_gap_height_m = 5, spike_gap_diameter_cm = 5,
  tree_height_dbh_ratio = 2.5, tree_ratio_min_dbh_cm = 10,
  shrub_height_m = 10, shrub_basal_floor_cm = 5,
  height_cap_m = 100
)
# Status text that marks a stem whose height is not its intact height.
VST_SIZE_SCREEN_NOT_INTACT <- "broken|physically damaged"

.vst_screen_num <- function(x) suppressWarnings(as.numeric(x))
.vst_screen_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "<NA>"
  x
}

# Same-plant spike. Returns list(flag, reference): flag is TRUE where value is
# more than `ratio` x and more than `gap` above the median of the same plant's
# other events (each event reduced to its maximum); reference is that median
# (NA where no other event exists or the row is not compared).
# Vectorized leave-one-out median: within a plant the event maxima are sorted,
# and the median of the other k-1 maxima is read from the sorted vector with
# the event's own rank skipped.
.vst_screen_spike <- function(value, plant, event, keep, ratio, gap) {
  n <- length(value)
  flag <- rep(FALSE, n)
  reference <- rep(NA_real_, n)
  ok <- keep & is.finite(value) & value > 0
  if (!any(ok)) return(list(flag = flag, reference = reference))
  pe_key <- paste(plant[ok], event[ok], sep = "\037")
  ev_max <- tapply(value[ok], pe_key, max)
  first <- match(names(ev_max), pe_key)
  tab <- data.frame(key = names(ev_max), plant = plant[ok][first],
                    v = as.numeric(ev_max), stringsAsFactors = FALSE)
  tab <- tab[order(tab$plant, tab$v), , drop = FALSE]
  k <- as.integer(ave(tab$v, tab$plant, FUN = length))
  r <- as.integer(ave(tab$v, tab$plant, FUN = seq_along))
  start <- seq_len(nrow(tab)) - r + 1L
  m <- k - 1L
  pick <- function(j) {
    j <- pmax(j, 1L)
    at <- start + ifelse(j < r, j, j + 1L) - 1L
    tab$v[pmin(at, nrow(tab))]
  }
  loo <- ifelse(m >= 1L, (pick((m + 1L) %/% 2L) + pick((m + 2L) %/% 2L)) / 2, NA_real_)
  ref_row <- loo[match(pe_key, tab$key)]
  v <- value[ok]
  flag[ok] <- is.finite(ref_row) & v > ratio * ref_row & (v - ref_row) > gap
  reference[ok] <- ref_row
  list(flag = flag, reference = reference)
}

# One row per input row:
#   size_plausible            both measures pass
#   size_height_ok            the height may enter the tallest extremes
#   size_diameter_ok          the channel diameter may enter the biggest extremes
#   size_screen_reason        NA when plausible; "; "-joined rule labels otherwise
#   size_height_reference_m   median of the plant's other intact live heights
#   size_diameter_reference_cm median of the plant's other diameters
vst_size_screen <- function(trees, rules = VST_SIZE_SCREEN_RULES) {
  n <- if (is.null(trees)) 0L else nrow(trees)
  if (!n) {
    return(data.frame(size_plausible = logical(), size_height_ok = logical(),
                      size_diameter_ok = logical(), size_screen_reason = character(),
                      size_height_reference_m = numeric(),
                      size_diameter_reference_cm = numeric(), stringsAsFactors = FALSE))
  }
  col <- function(name) if (name %in% names(trees)) trees[[name]] else rep(NA, n)
  form <- tolower(trimws(as.character(col("growthForm"))))
  tree <- form %in% VST_SIZE_SCREEN_TREE_FORMS
  shrub <- form %in% VST_SIZE_SCREEN_SHRUB_FORMS
  height <- .vst_screen_num(col("height"))
  dbh <- .vst_screen_num(col("stemDiameter"))
  basal <- .vst_screen_num(col("basalStemDiameter"))
  # The channel's own diameter: DBH for tree forms, basal for shrub forms.
  diameter <- ifelse(tree, dbh, ifelse(shrub, basal, NA_real_))
  plant <- paste(.vst_screen_chr(col("plotID")), .vst_screen_chr(col("individualID")),
                 sep = "\r")
  # A plant is compared only within its own growth-form family, so a sapling
  # that becomes a tree is not read as a spike against its sapling events.
  family <- ifelse(tree, "tree", ifelse(shrub, "shrub", "other"))
  plant_family <- paste(plant, family, sep = "\r")
  event <- .vst_screen_chr(col("eventID"))
  status <- trimws(as.character(col("plantStatus")))
  live <- if ("live" %in% names(trees)) trees$live %in% TRUE else
    grepl("^live", status, ignore.case = TRUE)
  intact <- live & !grepl(VST_SIZE_SCREEN_NOT_INTACT, status, ignore.case = TRUE)

  spike_h <- .vst_screen_spike(height, plant_family, event, (tree | shrub) & intact,
                               rules$spike_ratio, rules$spike_gap_height_m)
  spike_d <- .vst_screen_spike(diameter, plant_family, event, tree | shrub,
                               rules$spike_ratio, rules$spike_gap_diameter_cm)
  ratio_bad <- tree & is.finite(height) & is.finite(dbh) &
    dbh >= rules$tree_ratio_min_dbh_cm & height / dbh > rules$tree_height_dbh_ratio
  shrub_bad <- shrub & is.finite(height) & height > rules$shrub_height_m &
    (!is.finite(basal) | basal < rules$shrub_basal_floor_cm)
  cap_bad <- is.finite(height) & height > rules$height_cap_m

  labels <- cbind(
    ifelse(spike_h$flag, "height spike against this plant's other intact live records", NA_character_),
    ifelse(spike_d$flag, "diameter spike against this plant's other records", NA_character_),
    ifelse(ratio_bad, "height above 2.5 m per cm of DBH on DBH 10 cm or more", NA_character_),
    ifelse(shrub_bad, "shrub or sapling taller than 10 m on a basal diameter under 5 cm or none", NA_character_),
    ifelse(cap_bad, "height above 100 m", NA_character_)
  )
  reason <- apply(labels, 1L, function(x) {
    x <- x[!is.na(x)]
    if (length(x)) paste(x, collapse = "; ") else NA_character_
  })
  height_ok <- !(spike_h$flag | ratio_bad | shrub_bad | cap_bad)
  diameter_ok <- !(spike_d$flag | ratio_bad)
  data.frame(size_plausible = height_ok & diameter_ok,
             size_height_ok = height_ok,
             size_diameter_ok = diameter_ok,
             size_screen_reason = as.character(reason),
             size_height_reference_m = round(spike_h$reference, 2),
             size_diameter_reference_cm = round(spike_d$reference, 2),
             stringsAsFactors = FALSE)
}

vst_size_plausible <- function(trees, rules = VST_SIZE_SCREEN_RULES) {
  vst_size_screen(trees, rules)$size_plausible
}
