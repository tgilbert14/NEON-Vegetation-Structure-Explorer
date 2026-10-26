# Reviewed curation inputs for the Vegetation Structure bundle builder
# (scripts/bundle_veg_data.R). Base R plus digest only.
#
# Two files, both reviewed row by row (cedar's rulings, 2026-10-26):
#   vst_partial_event_completeness.csv  every vst_perplotperyear event with
#     dataCollected = "partial", with a per-channel verdict read from the
#     event remark: tree_complete / shrub_complete. TRUE lets that woody
#     channel read the event as all-growth-forms; FALSE keeps it held.
#   vst_event_disturbance.csv  events whose remark names a disturbance. A
#     label only: it never changes support. classes "none" records a reviewed
#     exclusion (snow, access, "not yet burned").
# Every row pins the source uid and a SHA-256 of the remark text, so a NEON
# re-publication that edits the remark makes the row stale and the build
# stops until a person reviews it again.

VST_CURATION_DIR <- Sys.getenv("VST_CURATION_DIR", unset = "scripts/curation")
VST_PARTIAL_CURATION_FILE <- "vst_partial_event_completeness.csv"
VST_DISTURBANCE_CURATION_FILE <- "vst_event_disturbance.csv"
VST_PARTIAL_CURATION_FIELDS <- c(
  "site", "plotID", "eventID", "opportunity_source_uid", "dataCollected",
  "remarks_sha256", "rule_id", "tree_complete", "shrub_complete",
  "reviewed_by", "reviewed_on"
)
VST_DISTURBANCE_CURATION_FIELDS <- c(
  "site", "plotID", "eventID", "source_uid", "remarks_sha256", "classes",
  "timing", "reviewed_by", "reviewed_on"
)
VST_PARTIAL_RULES <- c(
  "VST-PARTIAL-01" = "remark says only ferns, palms, cacti or yucca went unmeasured; both woody channels complete",
  "VST-PARTIAL-02" = "remark says shrubs and saplings went unmeasured; tree channel complete only",
  "VST-PARTIAL-03" = "remark says single- and multi-bole trees went unmeasured; shrub channel complete only",
  "VST-PARTIAL-04" = "remark reports reduced or unstated woody sampling; neither channel complete"
)
# fire_unspecified: the remark records a fire but not whether it was
# prescribed or wild (lead addition to cedar's six classes).
VST_DISTURBANCE_CLASSES <- c(
  "fire_wild", "burn_prescribed", "fire_unspecified", "wind_hurricane",
  "flood", "harvest_logging", "brush_mowing"
)
VST_DISTURBANCE_TIMING <- c("before", "after", "unstated")
VST_DISTURBANCE_CURATION_ID <- "vst-event-disturbance-v1"
# Proposes candidates only; every hit is reviewed before it is labelled.
VST_DISTURBANCE_REGEX <- paste0(
  "fire|burn|hurricane|storm|wind ?throw|blowdown|flood|harvest|logg|",
  "clear[- ]?cut|brush|mow|bush ?hog|thinn|timber"
)

vst_remarks_sha256 <- function(remarks) {
  remarks <- as.character(remarks)
  remarks[is.na(remarks)] <- ""
  vapply(enc2utf8(remarks), digest::digest, character(1), algo = "sha256",
         serialize = FALSE, USE.NAMES = FALSE)
}

.vst_curation_read <- function(path, fields, label) {
  if (!file.exists(path)) stop(label, " curation file is missing: ", path, call. = FALSE)
  table <- utils::read.csv(path, colClasses = "character", na.strings = character(),
                           check.names = FALSE, stringsAsFactors = FALSE,
                           encoding = "UTF-8")
  if (!identical(names(table), fields)) {
    stop(label, " curation columns must be exactly: ", paste(fields, collapse = ", "),
         call. = FALSE)
  }
  if (any(!nzchar(trimws(unlist(table[c("site", "plotID", "eventID")], use.names = FALSE))))) {
    stop(label, " curation has a blank site, plotID or eventID", call. = FALSE)
  }
  key <- paste(table$site, table$plotID, table$eventID, sep = "\r")
  if (anyDuplicated(key)) stop(label, " curation repeats a site + plotID + eventID", call. = FALSE)
  if (any(!grepl("^[0-9a-f]{64}$", table$remarks_sha256))) {
    stop(label, " curation remarks_sha256 must be lowercase SHA-256", call. = FALSE)
  }
  if (any(!nzchar(trimws(table$reviewed_by))) ||
      any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", table$reviewed_on))) {
    stop(label, " curation rows need reviewed_by and an ISO reviewed_on date", call. = FALSE)
  }
  table
}

vst_read_partial_curation <- function(dir = VST_CURATION_DIR) {
  table <- .vst_curation_read(file.path(dir, VST_PARTIAL_CURATION_FILE),
                              VST_PARTIAL_CURATION_FIELDS, "partial-event")
  if (any(!table$rule_id %in% names(VST_PARTIAL_RULES))) {
    stop("partial-event curation has an unknown rule_id", call. = FALSE)
  }
  for (field in c("tree_complete", "shrub_complete")) {
    if (any(!table[[field]] %in% c("TRUE", "FALSE"))) {
      stop("partial-event curation ", field, " must be TRUE or FALSE", call. = FALSE)
    }
    table[[field]] <- table[[field]] == "TRUE"
  }
  table
}

vst_read_disturbance_curation <- function(dir = VST_CURATION_DIR) {
  table <- .vst_curation_read(file.path(dir, VST_DISTURBANCE_CURATION_FILE),
                              VST_DISTURBANCE_CURATION_FIELDS, "disturbance")
  classes_ok <- vapply(strsplit(table$classes, ";", fixed = TRUE), function(value) {
    identical(value, "none") ||
      (length(value) > 0L && all(value %in% VST_DISTURBANCE_CLASSES) && !anyDuplicated(value))
  }, logical(1))
  if (!all(classes_ok)) {
    stop("disturbance curation classes must be 'none' or ';'-joined from: ",
         paste(VST_DISTURBANCE_CLASSES, collapse = ", "), call. = FALSE)
  }
  if (any(!table$timing %in% VST_DISTURBANCE_TIMING)) {
    stop("disturbance curation timing must be one of: ",
         paste(VST_DISTURBANCE_TIMING, collapse = ", "), call. = FALSE)
  }
  table
}

vst_read_curation <- function(dir = VST_CURATION_DIR) {
  curation <- list(partial = vst_read_partial_curation(dir),
                   disturbance = vst_read_disturbance_curation(dir))
  files <- file.path(dir, c(VST_PARTIAL_CURATION_FILE, VST_DISTURBANCE_CURATION_FILE))
  attr(curation, "sha256") <- vapply(files, digest::digest, character(1),
                                     algo = "sha256", file = TRUE, USE.NAMES = FALSE)
  curation
}

# The provenance each bundle embeds: which reviewed files, at which bytes.
vst_curation_receipt <- function(curation) {
  sha <- attr(curation, "sha256")
  if (is.null(sha)) sha <- c(NA_character_, NA_character_)
  list(
    partial_event_completeness = list(
      file = paste0("scripts/curation/", VST_PARTIAL_CURATION_FILE),
      sha256 = sha[[1L]], rows = nrow(curation$partial)
    ),
    event_disturbance = list(
      file = paste0("scripts/curation/", VST_DISTURBANCE_CURATION_FILE),
      sha256 = sha[[2L]], rows = nrow(curation$disturbance),
      curation_id = VST_DISTURBANCE_CURATION_ID
    )
  )
}

# An empty curation, for synthetic fixtures that carry no partial or
# disturbance events.
vst_empty_curation <- function() {
  partial <- as.data.frame(stats::setNames(
    replicate(length(VST_PARTIAL_CURATION_FIELDS), character(), simplify = FALSE),
    VST_PARTIAL_CURATION_FIELDS
  ), stringsAsFactors = FALSE)
  partial$tree_complete <- logical()
  partial$shrub_complete <- logical()
  disturbance <- as.data.frame(stats::setNames(
    replicate(length(VST_DISTURBANCE_CURATION_FIELDS), character(), simplify = FALSE),
    VST_DISTURBANCE_CURATION_FIELDS
  ), stringsAsFactors = FALSE)
  list(partial = partial, disturbance = disturbance)
}

# Per perplot row: the S1 verdict. Fails on a partial event with no reviewed
# row, a reviewed row whose uid, dataCollected or remark no longer matches,
# and a reviewed row for this site that matches no partial event.
vst_match_partial_curation <- function(perplot, site, curation) {
  n <- nrow(perplot)
  out <- data.frame(partial_event_rule_id = rep(NA_character_, n),
                    tree_channel_complete = rep(FALSE, n),
                    shrub_channel_complete = rep(FALSE, n),
                    stringsAsFactors = FALSE)
  rows <- curation[curation$site == site, , drop = FALSE]
  collected <- tolower(trimws(as.character(perplot$dataCollected)))
  partial <- !(perplot$opportunity_source_missing %in% TRUE) &
    !is.na(collected) & collected == "partial"
  event <- paste(perplot$plotID, perplot$eventID, sep = "\r")
  reviewed <- paste(rows$plotID, rows$eventID, sep = "\r")
  orphan <- setdiff(reviewed, event[partial])
  if (length(orphan)) {
    stop(sprintf("%s: %d reviewed partial-event row(s) match no dataCollected=partial event: %s",
                 site, length(orphan), paste(sub("\r", " ", orphan), collapse = ", ")),
         call. = FALSE)
  }
  remarks <- if ("remarks" %in% names(perplot)) perplot$remarks else rep(NA_character_, n)
  for (i in which(partial)) {
    j <- match(event[[i]], reviewed)
    if (is.na(j)) {
      stop(sprintf("%s: partial event %s %s has no reviewed row in %s", site,
                   perplot$plotID[[i]], perplot$eventID[[i]], VST_PARTIAL_CURATION_FILE),
           call. = FALSE)
    }
    if (!identical(rows$opportunity_source_uid[[j]], as.character(perplot$opportunity_source_uid[[i]])) ||
        !identical(rows$dataCollected[[j]], as.character(perplot$dataCollected[[i]])) ||
        !identical(rows$remarks_sha256[[j]], vst_remarks_sha256(remarks[[i]]))) {
      stop(sprintf("%s: reviewed partial-event row %s %s is stale (uid, dataCollected or remark changed)",
                   site, perplot$plotID[[i]], perplot$eventID[[i]]), call. = FALSE)
    }
    out$partial_event_rule_id[[i]] <- rows$rule_id[[j]]
    out$tree_channel_complete[[i]] <- rows$tree_complete[[j]]
    out$shrub_channel_complete[[i]] <- rows$shrub_complete[[j]]
  }
  out
}

# Per perplot row: the S2 label. Stale and orphan rows fail; an unreviewed
# regex hit only warns, because the label never changes support.
vst_match_disturbance_curation <- function(perplot, site, curation) {
  n <- nrow(perplot)
  out <- data.frame(disturbance_classes = rep(NA_character_, n),
                    disturbance_curation_id = rep(NA_character_, n),
                    stringsAsFactors = FALSE)
  rows <- curation[curation$site == site, , drop = FALSE]
  remarks <- if ("remarks" %in% names(perplot)) as.character(perplot$remarks) else
    rep(NA_character_, n)
  source_row <- !(perplot$opportunity_source_missing %in% TRUE)
  event <- paste(perplot$plotID, perplot$eventID, sep = "\r")
  reviewed <- paste(rows$plotID, rows$eventID, sep = "\r")
  orphan <- setdiff(reviewed, event[source_row])
  if (length(orphan)) {
    stop(sprintf("%s: %d reviewed disturbance row(s) match no published plot-event: %s",
                 site, length(orphan), paste(sub("\r", " ", orphan), collapse = ", ")),
         call. = FALSE)
  }
  for (j in seq_len(nrow(rows))) {
    i <- match(reviewed[[j]], event)
    if (!identical(rows$source_uid[[j]], as.character(perplot$opportunity_source_uid[[i]])) ||
        !identical(rows$remarks_sha256[[j]], vst_remarks_sha256(remarks[[i]]))) {
      stop(sprintf("%s: reviewed disturbance row %s %s is stale (uid or remark changed)",
                   site, rows$plotID[[j]], rows$eventID[[j]]), call. = FALSE)
    }
    out$disturbance_curation_id[[i]] <- VST_DISTURBANCE_CURATION_ID
    if (!identical(rows$classes[[j]], "none")) out$disturbance_classes[[i]] <- rows$classes[[j]]
  }
  hits <- which(source_row & !is.na(remarks) &
                  grepl(VST_DISTURBANCE_REGEX, remarks, ignore.case = TRUE) &
                  !event %in% reviewed)
  if (length(hits)) {
    warning(sprintf("%s: %d disturbance-remark candidate(s) have no reviewed row (label left blank): %s",
                    site, length(hits),
                    paste(perplot$plotID[hits], perplot$eventID[hits], collapse = ", ")),
            call. = FALSE)
  }
  out
}
