# Independent consumer-side parity for Vegetation Structure v2 bundles.
#
# The bundle builder owns serialization, but it does not get to certify its own
# derived site/taxon/channel summaries. This module uses the deployed analysis
# helpers in R/veg_helpers.R to select supported snapshots and recompute the
# release-facing summaries from preserved rows. `scripts/verify_bundle.R`
# separately proves the stored support decisions from source fields; the two
# gates compose into a fail-closed row -> support -> summary trust boundary.

suppressPackageStartupMessages(library(dplyr))
source("R/veg_helpers.R")

VST_PARITY_CONTRACT_ID <- "NEON-VST-DP1.10098.001-v2"
VST_PARITY_CHANNELS <- c("tree_dbh", "shrub_sapling_basal")
VST_PARITY_TOLERANCE <- 1e-10

vst_parity_nonblank <- function(value) {
  !is.na(value) & nzchar(trimws(as.character(value)))
}

vst_parity_key <- function(data, columns) {
  parts <- lapply(columns, function(column) {
    value <- as.character(data[[column]])
    value[is.na(value)] <- "<NA>"
    paste0(nchar(value), ":", value)
  })
  do.call(paste, c(parts, sep = "\u001f"))
}

vst_parity_column <- function(data, name, default = NA) {
  if (name %in% names(data)) data[[name]] else rep(default, nrow(data))
}

vst_parity_mode <- function(value) {
  value <- as.character(value)
  value <- value[vst_parity_nonblank(value)]
  if (!length(value)) return(NA_character_)
  counts <- table(value)
  sort(names(counts)[counts == max(counts)])[[1L]]
}

vst_parity_safe_max <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value)]
  if (length(value)) max(value) else NA_real_
}

vst_parity_safe_median <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value)]
  if (length(value)) stats::median(value) else NA_real_
}

vst_parity_expected_species <- function(rank, scientific_name) {
  rank <- tolower(trimws(as.character(rank)))
  scientific_name <- as.character(scientific_name)
  accepted <- !is.na(rank) & rank %in% c(
    "species", "subspecies", "variety", "form"
  )
  safe_name <- ifelse(is.na(scientific_name), "", scientific_name)
  accepted & vst_parity_nonblank(scientific_name) &
    !grepl("\\bsp\\.?$", safe_name, ignore.case = TRUE) &
    !grepl("/", safe_name, fixed = TRUE)
}

vst_parity_expected_taxon_label <- function(scientific_name, taxon_id) {
  scientific_name <- as.character(scientific_name)
  taxon_id <- as.character(taxon_id)
  ifelse(
    vst_parity_nonblank(scientific_name), scientific_name,
    ifelse(
      vst_parity_nonblank(taxon_id),
      paste0("Unresolved taxon (", taxon_id, ")"),
      "Unresolved taxon"
    )
  )
}

vst_parity_expected_year <- function(date) {
  parsed <- suppressWarnings(as.Date(substr(as.character(date), 1L, 10L)))
  suppressWarnings(as.integer(format(parsed, "%Y")))
}

vst_parity_row_derivation_problems <- function(trees) {
  required <- c(
    "date", "year", "plotID", "eventID", "individualID", "plantStatus",
    "live", "permanent", "plant_key", "event_key", "mapping_source_uid",
    "mappingMatched", "scientificName", "taxonID", "taxonRank",
    "is_species", "taxon_label", "taxon_resolution"
  )
  missing <- setdiff(required, names(trees))
  if (length(missing)) {
    return(paste0("missing row-derivation fields: ", paste(missing, collapse = ",")))
  }
  species <- vst_parity_expected_species(
    trees$taxonRank, trees$scientificName
  )
  label <- vst_parity_expected_taxon_label(
    trees$scientificName, trees$taxonID
  )
  resolution <- ifelse(
    species & vst_parity_nonblank(trees$scientificName),
    "species-level", "coarse-or-unresolved"
  )
  mapping_matched <- vst_parity_nonblank(trees$mapping_source_uid)
  checks <- c(
    date_type = inherits(trees$date, "Date"),
    year = is.integer(trees$year) && identical(
      trees$year, vst_parity_expected_year(trees$date)
    ),
    live = is.logical(trees$live) && identical(
      trees$live,
      grepl("^live", trimws(as.character(trees$plantStatus)), ignore.case = TRUE)
    ),
    permanent = is.logical(trees$permanent) && identical(
      trees$permanent,
      grepl("^NEON", as.character(trees$individualID))
    ),
    plant_key = is.character(trees$plant_key) && identical(
      trees$plant_key,
      paste(trees$plotID, trees$individualID, sep = "\r")
    ),
    event_key = is.character(trees$event_key) && identical(
      trees$event_key,
      paste(trees$plotID, trees$eventID, sep = "\r")
    ),
    mapping_source_uid_type = is.character(trees$mapping_source_uid),
    mappingMatched = is.logical(trees$mappingMatched) && identical(
      trees$mappingMatched, mapping_matched
    ),
    is_species = is.logical(trees$is_species) &&
      identical(trees$is_species, species),
    taxon_label = is.character(trees$taxon_label) &&
      identical(trees$taxon_label, label),
    taxon_resolution = is.character(trees$taxon_resolution) && identical(
      trees$taxon_resolution, resolution
    )
  )
  problems <- names(checks)[!checks]
  mapping_uid <- as.character(trees$mapping_source_uid[mapping_matched])
  if (length(mapping_uid)) {
    mapping_plant <- paste(
      trees$plotID[mapping_matched], trees$individualID[mapping_matched],
      sep = "\r"
    )
    if (any(tapply(mapping_plant, mapping_uid, function(value) {
      length(unique(value))
    }) != 1L) || any(tapply(mapping_uid, mapping_plant, function(value) {
      length(unique(value))
    }) != 1L)) {
      problems <- c(problems, "mapping_source_uid traceability")
    }
  }
  unique(problems)
}

vst_parity_taxa_projection <- function(records, per_plot, spec, site) {
  if (is.null(records) || !nrow(records)) return(data.frame())
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  records$.taxon_label <- vst_parity_expected_taxon_label(
    records$scientificName, records$taxonID
  )
  records$.plant_key <- paste(
    as.character(records$plotID), as.character(records$individualID), sep = "\r"
  )
  records$.diameter_cm <- suppressWarnings(as.numeric(records[[spec$col]]))
  records$.basal_m2 <- pi * (records$.diameter_cm / 200)^2

  supported <- per_plot[per_plot$supported %in% TRUE, , drop = FALSE]
  n_supported <- length(unique(as.character(supported$plotID)))
  if (!n_supported) return(data.frame())
  supported_key <- vst_parity_key(supported, c("plotID", "eventID"))
  record_key <- vst_parity_key(records, c("plotID", "eventID"))
  records$.area_ha <- supported$area_ha[match(record_key, supported_key)]
  if (any(!is.finite(records$.area_ha) | records$.area_ha <= 0)) {
    stop(site, " ", spec$channel,
         " consumer projection found a record without positive supported area",
         call. = FALSE)
  }

  labels <- sort(unique(records$.taxon_label[vst_parity_nonblank(
    records$.taxon_label
  )]))
  rows <- lapply(labels, function(label) {
    taxon <- records[records$.taxon_label == label, , drop = FALSE]
    plot_ids <- sort(unique(as.character(taxon$plotID)))
    by_plot <- lapply(plot_ids, function(plot_id) {
      part <- taxon[as.character(taxon$plotID) == plot_id, , drop = FALSE]
      scientific_name <- vst_parity_column(part, "scientificName", NA_character_)
      is_species <- vst_parity_expected_species(
        part$taxonRank, part$scientificName
      )
      data.frame(
        scientificName = vst_parity_mode(scientific_name),
        taxonID = vst_parity_mode(vst_parity_column(
          part, "taxonID", NA_character_
        )),
        taxonRank = vst_parity_mode(vst_parity_column(
          part, "taxonRank", NA_character_
        )),
        is_species = any(is_species & vst_parity_nonblank(scientific_name)),
        family = vst_parity_mode(vst_parity_column(
          part, "family", NA_character_
        )),
        stems = as.integer(nrow(part)),
        individuals = as.integer(length(unique(part$.plant_key))),
        basal_m2 = sum(part$.basal_m2),
        area_ha = part$.area_ha[[1L]],
        stringsAsFactors = FALSE
      )
    })
    by_plot <- dplyr::bind_rows(by_plot)
    years <- vst_parity_expected_year(taxon$date)
    years <- years[is.finite(years)]
    mean_basal <- sum(by_plot$basal_m2 / by_plot$area_ha) / n_supported
    data.frame(
      taxon_label = label,
      scientificName = vst_parity_mode(by_plot$scientificName),
      taxonID = vst_parity_mode(by_plot$taxonID),
      taxonRank = vst_parity_mode(by_plot$taxonRank),
      is_species = any(by_plot$is_species %in% TRUE),
      family = vst_parity_mode(by_plot$family),
      site = site,
      contract_id = VST_PARITY_CONTRACT_ID,
      channel = spec$channel,
      metric_kind = spec$metric_kind,
      n_stems = as.integer(sum(by_plot$stems)),
      n_individuals = as.integer(sum(by_plot$individuals)),
      n_occurrence_plots = as.integer(nrow(by_plot)),
      n_supported_plots = as.integer(n_supported),
      mean_plot_basal_m2_ha = mean_basal,
      ba_m2_ha = mean_basal,
      inference_scope = paste(
        "mean across latest supported sampled plot events;",
        "explicit absences are zero"
      ),
      year_min = if (length(years)) as.integer(min(years)) else NA_integer_,
      year_max = if (length(years)) as.integer(max(years)) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

vst_parity_channel_projection <- function(bundle, spec, site) {
  snapshot <- tree_snapshot(bundle$trees, bundle$plots, spec)
  per_plot <- stand_by_plot(snapshot, bundle$plots, spec)
  if (is.null(per_plot)) {
    per_plot <- data.frame(
      plotID = character(), eventID = character(), supported = logical(),
      sampled_absence = logical(), area_ha = numeric(), stringsAsFactors = FALSE
    )
  }
  records <- .selected_stems(snapshot, per_plot, spec, live = TRUE)
  if (is.null(records)) records <- bundle$trees[0, , drop = FALSE]
  stand <- if (nrow(per_plot)) stand_site(snapshot, bundle$plots, spec) else NULL
  n_supported <- length(unique(as.character(
    per_plot$plotID[per_plot$supported %in% TRUE]
  )))
  record_plots <- if (nrow(records)) {
    length(unique(as.character(records$plotID)))
  } else {
    0L
  }
  plant_keys <- if (nrow(records)) paste(
    as.character(records$plotID), as.character(records$individualID), sep = "\r"
  ) else character()
  scientific_name <- vst_parity_column(records, "scientificName", NA_character_)
  is_species <- if (nrow(records)) {
    vst_parity_expected_species(records$taxonRank, records$scientificName)
  } else {
    logical()
  }
  taxon_label <- if (nrow(records)) {
    vst_parity_expected_taxon_label(records$scientificName, records$taxonID)
  } else {
    character()
  }
  diameter <- if (nrow(records)) {
    suppressWarnings(as.numeric(records[[spec$col]]))
  } else {
    numeric()
  }
  height <- suppressWarnings(as.numeric(vst_parity_column(
    records, "height", NA_real_
  )))

  summary <- list(
    channel = spec$channel,
    n_supported_plots = as.integer(n_supported),
    n_record_plots = as.integer(record_plots),
    n_stems = as.integer(nrow(records)),
    n_individuals = as.integer(length(unique(plant_keys))),
    n_species = as.integer(length(unique(scientific_name[
      is_species & vst_parity_nonblank(scientific_name)
    ]))),
    n_taxa = as.integer(length(unique(taxon_label[
      vst_parity_nonblank(taxon_label)
    ]))),
    n_sampled_absence = as.integer(sum(
      per_plot$supported %in% TRUE & per_plot$sampled_absence %in% TRUE
    )),
    ba_ha = if (is.null(stand)) NA_real_ else as.numeric(stand$ba_ha),
    density_ha = if (is.null(stand)) NA_real_ else as.numeric(stand$density_ha),
    qmd_cm = if (is.null(stand)) NA_real_ else as.numeric(stand$qmd),
    metric_kind = spec$metric_kind,
    tallest_m = vst_parity_safe_max(height),
    biggest_diam_cm = vst_parity_safe_max(diameter)
  )
  list(
    summary = summary,
    taxa = vst_parity_taxa_projection(records, per_plot, spec, site)
  )
}

vst_parity_bundle_projection <- function(bundle, expected_site = NULL) {
  if (!is.list(bundle) || !is.data.frame(bundle$trees) ||
      !is.data.frame(bundle$plots)) {
    stop("consumer projection requires bundle trees and plots", call. = FALSE)
  }
  site <- as.character(bundle$meta$site %||% NA_character_)
  if (length(site) != 1L || is.na(site) || !nzchar(site)) {
    stop("consumer projection requires one nonblank bundle site", call. = FALSE)
  }
  if (!is.null(expected_site) && !identical(site, as.character(expected_site))) {
    stop("consumer projection received a different requested site", call. = FALSE)
  }
  derivation_problems <- vst_parity_row_derivation_problems(bundle$trees)
  if (length(derivation_problems)) {
    stop(
      site, " row-derived invariants differ from preserved source fields: ",
      paste(derivation_problems, collapse = ", "), call. = FALSE
    )
  }

  channels <- list(
    tree_dbh = vst_parity_channel_projection(bundle, SIZE_FOREST, site),
    shrub_sapling_basal = vst_parity_channel_projection(bundle, SIZE_SHRUB, site)
  )
  summaries <- lapply(channels, `[[`, "summary")
  record_plots <- vapply(summaries, `[[`, integer(1), "n_record_plots")
  supported_plots <- vapply(summaries, `[[`, integer(1), "n_supported_plots")
  primary <- if (all(record_plots == 0L) && all(supported_plots == 0L)) {
    "unavailable"
  } else if (all(record_plots == 0L) &&
             supported_plots[["shrub_sapling_basal"]] >
               supported_plots[["tree_dbh"]]) {
    "shrub_sapling_basal"
  } else if (record_plots[["shrub_sapling_basal"]] >
             record_plots[["tree_dbh"]]) {
    "shrub_sapling_basal"
  } else {
    "tree_dbh"
  }
  chosen <- if (identical(primary, "unavailable")) {
    summaries$tree_dbh
  } else {
    summaries[[primary]]
  }
  unavailable <- identical(primary, "unavailable")
  measurement_only <- bundle$plots$opportunity_source_missing %in% TRUE
  measurement_only_keys <- vst_parity_key(
    bundle$plots[measurement_only, , drop = FALSE], c("plotID", "eventID")
  )
  measurement_records <- if (length(measurement_only_keys)) {
    sum(vst_parity_key(bundle$trees, c("plotID", "eventID")) %in%
          measurement_only_keys)
  } else {
    0L
  }
  latitude <- vst_parity_safe_median(bundle$plots$lat)
  longitude <- vst_parity_safe_median(bundle$plots$lng)
  site_row <- data.frame(
    site = site,
    contract_id = VST_PARITY_CONTRACT_ID,
    primary_channel = primary,
    structure_type = if (identical(primary, "shrub_sapling_basal")) {
      "shrubland"
    } else if (identical(primary, "tree_dbh")) {
      "forest"
    } else {
      "unknown"
    },
    size_metric = if (identical(primary, "shrub_sapling_basal")) {
      "basal diameter"
    } else if (identical(primary, "tree_dbh")) {
      "DBH"
    } else {
      "unavailable"
    },
    n_supported_plots = if (unavailable) NA_integer_ else chosen$n_supported_plots,
    n_record_plots = if (unavailable) NA_integer_ else chosen$n_record_plots,
    n_stems = if (unavailable) NA_integer_ else chosen$n_stems,
    n_individuals = if (unavailable) NA_integer_ else chosen$n_individuals,
    n_species = if (unavailable) NA_integer_ else chosen$n_species,
    n_taxa = if (unavailable) NA_integer_ else chosen$n_taxa,
    n_sampled_absence = if (unavailable) NA_integer_ else chosen$n_sampled_absence,
    ba_ha = if (unavailable) NA_real_ else chosen$ba_ha,
    density_ha = if (unavailable) NA_real_ else chosen$density_ha,
    qmd_cm = if (unavailable) NA_real_ else chosen$qmd_cm,
    metric_kind = if (unavailable) "unavailable" else chosen$metric_kind,
    support_status = if (chosen$n_supported_plots > 0L) {
      "supported_sampled_context"
    } else {
      "held_no_supported_event"
    },
    tallest_m = if (unavailable) NA_real_ else chosen$tallest_m,
    biggest_diam_cm = if (unavailable) NA_real_ else chosen$biggest_diam_cm,
    n_trees = if (unavailable) NA_integer_ else chosen$n_individuals,
    n_plots = if (unavailable) NA_integer_ else chosen$n_supported_plots,
    n_measurement_only_contexts = as.integer(sum(measurement_only)),
    n_measurement_records_without_opportunity_source = as.integer(
      measurement_records
    ),
    lat = latitude,
    lng = longitude,
    inference_scope =
      "latest supported event per sampled plot; not a site-wide census",
    stringsAsFactors = FALSE
  )
  taxa <- dplyr::bind_rows(lapply(channels, `[[`, "taxa"))
  list(site = site_row, taxa = taxa, summaries = summaries)
}

vst_parity_frame_problems <- function(actual, expected, keys, label,
                                      tolerance = VST_PARITY_TOLERANCE) {
  problems <- character()
  if (!is.data.frame(actual) || !is.data.frame(expected)) {
    return(paste0(label, " is not a data-frame pair"))
  }
  if (!identical(names(actual), names(expected))) {
    problems <- c(problems, sprintf(
      "%s columns differ: missing=[%s] extra=[%s]", label,
      paste(setdiff(names(expected), names(actual)), collapse = ","),
      paste(setdiff(names(actual), names(expected)), collapse = ",")
    ))
  }
  if (nrow(actual) != nrow(expected)) {
    problems <- c(problems, sprintf(
      "%s row count differs: expected %d, found %d",
      label, nrow(expected), nrow(actual)
    ))
  }
  if (!nrow(expected) && !nrow(actual)) return(unique(problems))
  if (!all(keys %in% names(actual)) || !all(keys %in% names(expected))) {
    return(unique(c(problems, paste0(label, " lacks comparison keys"))))
  }
  actual_key <- vst_parity_key(actual, keys)
  expected_key <- vst_parity_key(expected, keys)
  if (anyDuplicated(actual_key) || anyDuplicated(expected_key) ||
      !setequal(actual_key, expected_key)) {
    return(unique(c(problems, paste0(label, " key inventory differs"))))
  }
  actual <- actual[match(expected_key, actual_key), , drop = FALSE]
  fields <- intersect(names(expected), names(actual))
  for (field in fields) {
    left <- actual[[field]]
    right <- expected[[field]]
    equal <- if (is.numeric(right)) {
      if (!is.numeric(left) || length(left) != length(right)) {
        FALSE
      } else {
        same_na <- is.na(left) == is.na(right)
        finite <- !is.na(left) & !is.na(right)
        close <- rep(TRUE, length(right))
        close[finite] <- abs(left[finite] - right[finite]) <=
          tolerance * pmax(1, abs(right[finite]))
        all(same_na & close)
      }
    } else {
      identical(as.vector(left), as.vector(right))
    }
    if (!isTRUE(equal)) {
      problems <- c(problems, sprintf("%s field %s differs", label, field))
    }
  }
  unique(problems)
}

vst_parity_summary_problems <- function(actual, expected, label,
                                        tolerance = VST_PARITY_TOLERANCE) {
  if (!is.list(actual) || !is.list(expected)) {
    return(paste0(label, " is not a summary-list pair"))
  }
  vst_parity_frame_problems(
    as.data.frame(actual, stringsAsFactors = FALSE, optional = TRUE),
    as.data.frame(expected, stringsAsFactors = FALSE, optional = TRUE),
    "channel", label, tolerance
  )
}

vst_parity_bundle_problems <- function(bundle, expected_site = NULL,
                                       tolerance = VST_PARITY_TOLERANCE,
                                       projected = NULL) {
  if (is.null(projected)) {
    projected <- tryCatch(
      vst_parity_bundle_projection(bundle, expected_site),
      error = function(error) error
    )
  }
  if (inherits(projected, "error")) {
    return(paste0(
      expected_site %||% "bundle", " consumer projection failed: ",
      conditionMessage(projected)
    ))
  }
  site <- as.character(projected$site$site[[1L]])
  contract <- bundle$contract %||% list()
  problems <- character()
  if (!identical(as.character(contract$id %||% ""),
                 VST_PARITY_CONTRACT_ID)) {
    problems <- c(problems, paste0(site, " embedded contract ID differs"))
  }
  for (channel in VST_PARITY_CHANNELS) {
    problems <- c(problems, vst_parity_summary_problems(
      (contract$channel_summary %||% list())[[channel]],
      projected$summaries[[channel]],
      paste(site, channel, "embedded channel summary"), tolerance
    ))
  }
  problems <- c(problems, vst_parity_frame_problems(
    (contract$index %||% list())$site %||% data.frame(),
    projected$site, "site", paste(site, "embedded site index"), tolerance
  ))
  expected_taxa <- projected$taxa
  actual_taxa <- (contract$index %||% list())$taxa %||% data.frame()
  if (!nrow(expected_taxa) && !nrow(actual_taxa)) {
    # Empty per-site taxon indexes are intentionally schema-free in the v2
    # serializer; family indexes acquire the schema from non-empty sites.
  } else {
    problems <- c(problems, vst_parity_frame_problems(
      actual_taxa, expected_taxa, c("site", "channel", "taxon_label"),
      paste(site, "embedded taxon index"), tolerance
    ))
  }
  meta <- bundle$meta %||% list()
  if (!identical(as.character(meta$primary_channel %||% ""),
                 as.character(projected$site$primary_channel[[1L]])) ||
      !identical(as.character(meta$structure_type %||% ""),
                 as.character(projected$site$structure_type[[1L]]))) {
    problems <- c(problems, paste0(
      site, " metadata primary channel/structure differs from consumer projection"
    ))
  }
  unique(problems)
}

vst_parity_channel_rows <- function(projections, expected_sites) {
  rows <- lapply(expected_sites, function(site) {
    projected <- projections[[site]]
    dplyr::bind_rows(lapply(VST_PARITY_CHANNELS, function(channel) {
      summary <- projected$summaries[[channel]]
      supported <- summary$n_supported_plots > 0L
      data.frame(
        site = site,
        contract_id = VST_PARITY_CONTRACT_ID,
        channel = channel,
        channel_label = if (identical(channel, "tree_dbh")) {
          "Tree DBH"
        } else {
          "Shrub & sapling basal"
        },
        is_default_channel = identical(
          projected$site$primary_channel[[1L]], channel
        ),
        support_status = if (supported) {
          "supported_sampled_context"
        } else {
          "held_no_supported_event"
        },
        n_supported_plots = summary$n_supported_plots,
        n_record_plots = summary$n_record_plots,
        n_stems = summary$n_stems,
        n_individuals = summary$n_individuals,
        n_species = summary$n_species,
        n_taxa = summary$n_taxa,
        n_sampled_absence = summary$n_sampled_absence,
        ba_ha = summary$ba_ha,
        density_ha = summary$density_ha,
        qmd_cm = summary$qmd_cm,
        metric_kind = summary$metric_kind,
        tallest_m = summary$tallest_m,
        biggest_diam_cm = summary$biggest_diam_cm,
        inference_scope = paste(
          "latest supported event per sampled plot within this physical channel"
        ),
        stringsAsFactors = FALSE
      )
    }))
  })
  rows <- dplyr::bind_rows(rows)
  rows[order(
    rows$site, match(rows$channel, VST_PARITY_CHANNELS)
  ), , drop = FALSE]
}

vst_parity_family_projection <- function(bundles, expected_sites) {
  projections <- stats::setNames(lapply(expected_sites, function(site) {
    vst_parity_bundle_projection(bundles[[site]], site)
  }), expected_sites)
  sites <- dplyr::bind_rows(lapply(projections, `[[`, "site"))
  taxa <- dplyr::bind_rows(lapply(projections, `[[`, "taxa"))
  if (nrow(taxa)) {
    taxa <- taxa[order(
      taxa$taxon_label, taxa$site, taxa$channel,
      -taxa$mean_plot_basal_m2_ha, na.last = TRUE
    ), , drop = FALSE]
  }
  list(
    projections = projections,
    sites = sites,
    taxa = taxa,
    channel_sites = vst_parity_channel_rows(projections, expected_sites)
  )
}

vst_parity_family_problems <- function(bundles, site_index, search,
                                       expected_sites,
                                       tolerance = VST_PARITY_TOLERANCE) {
  problems <- character()
  if (!is.list(bundles) ||
      !identical(sort(names(bundles)), sort(expected_sites))) {
    return("consumer parity did not receive the exact expected bundle family")
  }
  expected <- tryCatch(
    vst_parity_family_projection(bundles, expected_sites),
    error = function(error) error
  )
  if (inherits(expected, "error")) {
    return(unique(c(problems, paste0(
      "consumer family projection failed: ", conditionMessage(expected)
    ))))
  }
  for (site in expected_sites) {
    problems <- c(problems, vst_parity_bundle_problems(
      bundles[[site]], site, tolerance,
      projected = expected$projections[[site]]
    ))
  }
  problems <- c(problems, vst_parity_frame_problems(
    site_index, expected$sites, "site", "canonical site index", tolerance
  ))
  if (!is.list(search)) {
    return(unique(c(problems, "search index is not a list")))
  }
  problems <- c(problems, vst_parity_frame_problems(
    search$sites %||% data.frame(), expected$sites, "site",
    "search site rows", tolerance
  ))
  problems <- c(problems, vst_parity_frame_problems(
    search$channel_sites %||% data.frame(), expected$channel_sites,
    c("site", "channel"), "search channel rows", tolerance
  ))
  if (!nrow(expected$taxa) &&
      !nrow(search$taxa %||% data.frame())) {
    # Same schema-free empty-family exception as the per-site contract.
  } else {
    problems <- c(problems, vst_parity_frame_problems(
      search$taxa %||% data.frame(), expected$taxa,
      c("site", "channel", "taxon_label"), "search taxon rows", tolerance
    ))
  }
  unique(problems)
}

vst_assert_derived_parity <- function(bundles, site_index, search,
                                      expected_sites,
                                      tolerance = VST_PARITY_TOLERANCE) {
  problems <- vst_parity_family_problems(
    bundles, site_index, search, expected_sites, tolerance
  )
  if (length(problems)) {
    stop(
      "derived consumer parity failed:\n- ",
      paste(problems, collapse = "\n- "), call. = FALSE
    )
  }
  invisible(TRUE)
}
