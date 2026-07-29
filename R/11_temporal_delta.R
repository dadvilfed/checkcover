#### MODULE 11: TEMPORAL DELTA ####
# Author: 
# Spec: temporal_delta_implementation_guide_for_David (Lucian Pârvulescu, 2026-04)
# Phase 1: Core temporal_delta functions + version management primitives.
# Phase 2/3 (output rendering, full pipeline integration) intentionally deferred —
#   they require decisions on per-scenario versioning that have not been finalised.
#
# DESIGN DECISIONS (defaults; flip in CONFIG if professor wants otherwise):
#   1. Baseline storage:         RDS for occurrences, JSON for metrics
#   2. Comparison logic:         Incremental (v1.X compares to v1.X-1, NOT v1.0)
#   3. Spatial buffer:           Fixed 500m geodesic radius
#   4. Unlinked extinctions:     Included in summary; logged as warning, not error
#   5. Previous-version metrics: Cached in JSON; recomputed only if JSON missing
#
# ADAPTATION FROM SPEC TO ACTUAL CODEBASE:
#   Spec column     →  cheCkOVER column
#   Locality_ID     →  record_id
#   Latitude        →  latitude
#   Longitude       →  longitude
#   Year            →  year
#   Species         →  species
#   Comments        →  comments
#   Claim_extinction == "Extinct"  →  is_extinct == TRUE  (already computed in 01_ingest.R)
#   origin == "native"  →  status == "native"
#   Country         →  country
#   Basin           →  hydrobasin           (single merged column)
#   FEOW            →  freshwater_ecoregion
#   WDPA_name       →  protected_area
#
# DIRECTORY LAYOUT (decoupled from per-run folders for cross-run version continuity):
#   {root_output_dir}/temporal/{species_clean}/
#       {species_clean}_v1.0.md            ← canonical narrative
#       {species_clean}_v1.0.json          ← metrics + delta summary (Section 6)
#       {species_clean}_occurrences_v1.0.rds  ← snapshot for next-version comparison
#       {species_clean}_manifest.json      ← full version history
#       archive/
#           {species_clean}_v1.0_archived_20260415.{md,json}
#           {species_clean}_occurrences_v1.0_archived_20260415.rds

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sf)
  library(jsonlite)
  library(lubridate)
})


# ──────────────────────────────────────────────────────────────────────────────
# PATH HELPERS
# ──────────────────────────────────────────────────────────────────────────────

#' Get the temporal artifacts root directory
#'
#' @param root_output_dir Top-level cheCkOVER output directory; defaults to
#'   CONFIG$root_output_dir if available.
#' @return Absolute path to the temporal artifacts root.
#' @export
temporal_root_dir <- function(root_output_dir = NULL) {
  if (is.null(root_output_dir)) {
    if (exists("CONFIG", envir = .GlobalEnv) &&
        !is.null(.GlobalEnv$CONFIG$root_output_dir)) {
      root_output_dir <- .GlobalEnv$CONFIG$root_output_dir
    } else {
      root_output_dir <- "checkover_output"
    }
  }
  file.path(root_output_dir, "temporal")
}

#' Build the per-species temporal directory path
#'
#' @param species_clean Path-safe species identifier (e.g. "Astacus_astacus").
#' @param root_output_dir Optional override for top-level output dir.
#' @return Absolute path; created on disk if not already present.
#' @export
species_temporal_dir <- function(species_clean, root_output_dir = NULL) {
  d <- file.path(temporal_root_dir(root_output_dir), species_clean)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Convert a binomial scientific name to a path-safe slug
#'
#' Replaces any non-alphanumeric character (including dots, since dots in
#' filenames confuse extension parsers like `tools::file_ext()`) with
#' underscores, collapses runs of underscores, and trims edges.
#'
#' @param species Scientific name (may contain spaces, dots, etc).
#' @return A safe filename component.
#' @export
clean_species_name <- function(species) {
  s <- gsub("[^A-Za-z0-9_-]+", "_", as.character(species))
  s <- gsub("_+", "_", s)
  gsub("^_|_$", "", s)
}

# Internal: sortable key for "v1.10" > "v1.2" comparison (numeric, not lexical)
.version_sort_key <- function(v) {
  parts <- as.numeric(strsplit(sub("^v", "", v), "\\.", fixed = FALSE)[[1]])
  if (length(parts) == 1L) parts <- c(parts, 0)
  parts[1] * 1e6 + parts[2]
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.1: ARTIFACT DETECTION
# ──────────────────────────────────────────────────────────────────────────────

#' Detect prior temporal artifacts for a species
#'
#' Scans the per-species temporal directory for any v*.md or v*.json files and
#' returns metadata about the MOST RECENT version (numerically, not lexically).
#'
#' @param species_clean Path-safe species identifier.
#' @param root_output_dir Optional override for output root.
#' @return A list with `artifacts_exist`, `latest_version`, `latest_date`,
#'   `latest_files`, `all_versions`.
#' @export
detect_prior_artifacts <- function(species_clean, root_output_dir = NULL) {
  
  out_dir <- file.path(temporal_root_dir(root_output_dir), species_clean)
  
  empty <- list(
    artifacts_exist = FALSE,
    latest_version  = NULL,
    latest_date     = NULL,
    latest_files    = NULL,
    all_versions    = character(0)
  )
  
  if (!dir.exists(out_dir)) return(empty)
  
  pattern <- sprintf("^%s_v[0-9]+\\.[0-9]+\\.(md|json)$", species_clean)
  files   <- list.files(out_dir, pattern = pattern)
  
  if (length(files) == 0L) return(empty)
  
  versions <- unique(stringr::str_extract(files, "v[0-9]+\\.[0-9]+"))
  
  latest_version <- versions[which.max(vapply(versions, .version_sort_key, numeric(1)))]
  
  json_file <- file.path(out_dir, sprintf("%s_%s.json", species_clean, latest_version))
  latest_date <- if (file.exists(json_file)) {
    md <- tryCatch(jsonlite::read_json(json_file, simplifyVector = TRUE),
                   error = function(e) NULL)
    if (!is.null(md$processing_date)) md$processing_date
    else as.character(file.info(json_file)$mtime)
  } else NA_character_
  
  latest_files <- list.files(
    out_dir,
    pattern = sprintf("^%s_(occurrences_)?%s\\.", species_clean, latest_version)
  )
  
  list(
    artifacts_exist = TRUE,
    latest_version  = latest_version,
    latest_date     = latest_date,
    latest_files    = latest_files,
    all_versions    = versions[order(vapply(versions, .version_sort_key, numeric(1)))]
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.2: LOAD PREVIOUS VERSION
# ──────────────────────────────────────────────────────────────────────────────

#' Load occurrences and metrics for the most recent prior version
#'
#' @param species_clean Path-safe species identifier.
#' @param version Specific version string (e.g. "v1.2"); auto-detected if NULL.
#' @param root_output_dir Optional override for output root.
#' @return List with `occurrences_previous`, `metrics_previous`, `version`,
#'   `timestamp`, `manifest`.
#' @export
load_previous_version <- function(species_clean,
                                  version = NULL,
                                  root_output_dir = NULL) {
  
  if (is.null(version)) {
    art <- detect_prior_artifacts(species_clean, root_output_dir)
    if (!art$artifacts_exist) {
      stop("No prior temporal artifacts found for species: ", species_clean)
    }
    version <- art$latest_version
  }
  
  out_dir <- file.path(temporal_root_dir(root_output_dir), species_clean)
  
  occ_file <- file.path(out_dir, sprintf("%s_occurrences_%s.rds", species_clean, version))
  if (!file.exists(occ_file)) {
    stop("Occurrences snapshot not found: ", occ_file,
         "\n  (expected from previous version save_baseline_artifacts call)")
  }
  occurrences_previous <- readRDS(occ_file)
  
  json_file <- file.path(out_dir, sprintf("%s_%s.json", species_clean, version))
  if (!file.exists(json_file)) {
    stop("Metrics JSON not found: ", json_file)
  }
  metrics_json <- jsonlite::read_json(json_file, simplifyVector = TRUE)
  
  # Pull metrics with graceful fallbacks. We read from a `metrics` block that
  # save_baseline_artifacts() writes; if it's missing (older file), recompute
  # from the occurrence snapshot using cheCkOVER's own EOO/AOO helpers.
  metrics_previous <- if (!is.null(metrics_json$metrics)) {
    metrics_json$metrics
  } else {
    list(
      EOO_km2     = .calc_eoo_default(occurrences_previous$longitude,
                                      occurrences_previous$latitude),
      AOO_km2     = .calc_aoo_default(occurrences_previous$longitude,
                                      occurrences_previous$latitude),
      countries   = unique(stats::na.omit(occurrences_previous$country)),
      basins      = unique(stats::na.omit(occurrences_previous$hydrobasin)),
      record_count_total  = nrow(occurrences_previous),
      record_count_native = sum(occurrences_previous$status == "native", na.rm = TRUE),
      record_count_alien  = sum(occurrences_previous$status == "alien",  na.rm = TRUE)
    )
  }
  
  manifest_file <- file.path(out_dir, sprintf("%s_manifest.json", species_clean))
  manifest <- if (file.exists(manifest_file)) {
    # simplifyVector = FALSE keeps `versions` as a list-of-lists rather than
    # collapsing it into a data frame, so [[i]]$field indexing works downstream.
    jsonlite::read_json(manifest_file, simplifyVector = FALSE)
  } else NULL
  
  list(
    occurrences_previous = occurrences_previous,
    metrics_previous     = metrics_previous,
    version              = version,
    timestamp            = metrics_json$processing_date %||% NA_character_,
    manifest             = manifest
  )
}

# Null-coalescing operator (cheCkOVER convention)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a


# ──────────────────────────────────────────────────────────────────────────────
# DEFAULT EOO/AOO HELPERS (mirror those in 03a_metrics_indigenous.R)
# Kept here as standalone copies so this module is self-contained for testing.
# ──────────────────────────────────────────────────────────────────────────────

.calc_eoo_default <- function(lon, lat) {
  coords <- unique(data.frame(lon = lon, lat = lat))
  coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
  if (nrow(coords) >= 3L) {
    pts  <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
    hull <- sf::st_convex_hull(sf::st_union(pts))
    return(as.numeric(sf::st_area(hull)) / 1e6)  # km²
  }
  NA_real_
}

.calc_aoo_default <- function(lon, lat) {
  coords <- data.frame(lon = lon, lat = lat)
  coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
  if (nrow(coords) == 0L) return(NA_real_)
  grid_size <- 0.018  # ~2 km
  n_cells <- length(unique(paste(floor(coords$lon / grid_size),
                                 floor(coords$lat / grid_size))))
  n_cells * 4  # km² per 2x2 km cell
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.3: PARSE EXTINCTION CAUSES
# ──────────────────────────────────────────────────────────────────────────────

#' Extract extinction events with causes and coordinates for spatial masking
#'
#' Filters occurrences flagged `is_extinct == TRUE`, parses `comments` for
#' cause keywords (case-insensitive), and returns the event tibble used by
#' apply_spatial_temporal_mask() and summarize_extinctions().
#'
#' @param occurrences A cheCkOVER occurrence dataframe with at minimum
#'   `record_id`, `latitude`, `longitude`, `year`, `is_extinct`, `comments`.
#' @return Tibble with `record_id`, `latitude`, `longitude`, `extinction_year`,
#'   `cause_category`, `cause_detail`, `comments_raw`, `buffer_radius_m`.
#' @export
parse_extinction_causes <- function(occurrences) {
  
  empty <- tibble::tibble(
    record_id        = character(),
    species          = character(),
    latitude         = numeric(),
    longitude        = numeric(),
    extinction_year  = numeric(),
    cause_category   = character(),
    cause_detail     = character(),
    comments_raw     = character(),
    buffer_radius_m  = numeric()
  )
  
  if (!"is_extinct" %in% names(occurrences)) {
    warning("`is_extinct` column missing — returning empty extinctions tibble.")
    return(empty)
  }
  
  ext_raw <- occurrences %>% dplyr::filter(.data$is_extinct == TRUE)
  if (nrow(ext_raw) == 0L) return(empty)
  
  # Comments column may be missing on some inputs — handle gracefully
  if (!"comments" %in% names(ext_raw)) ext_raw$comments <- NA_character_
  if (!"species"  %in% names(ext_raw)) ext_raw$species  <- NA_character_
  
  classify_cause <- function(cmt) {
    if (is.na(cmt) || nchar(stringr::str_trim(cmt)) == 0L) return("unknown")
    c <- stringr::str_to_lower(cmt)
    if (stringr::str_detect(c, "invasive"))    return("invasive")
    if (stringr::str_detect(c, "disease"))     return("disease")
    if (stringr::str_detect(c, "habitat"))     return("habitat_loss")
    if (stringr::str_detect(c, "climate"))     return("climate")
    if (stringr::str_detect(c, "overharvest")) return("overharvest")
    if (stringr::str_detect(c, "multiple"))    return("multiple")
    "unknown"
  }
  
  ext_raw %>%
    dplyr::mutate(
      cause_category  = vapply(.data$comments, classify_cause, character(1)),
      cause_detail    = .data$comments,
      comments_raw    = .data$comments,
      extinction_year = .data$year,
      buffer_radius_m = 500
    ) %>%
    dplyr::transmute(
      record_id       = .data$record_id,
      species         = .data$species,
      latitude        = .data$latitude,
      longitude       = .data$longitude,
      extinction_year = .data$extinction_year,
      cause_category  = .data$cause_category,
      cause_detail    = .data$cause_detail,
      comments_raw    = .data$comments_raw,
      buffer_radius_m = .data$buffer_radius_m
    )
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.3B: SPATIAL-TEMPORAL MASKING (500 m geodesic radius)
# ──────────────────────────────────────────────────────────────────────────────

#' Suppress occurrences within 500 m geodesic radius of an extinction event
#'
#' Each extinction creates a circular buffer using `sf::st_buffer(dist = 500)`
#' on EPSG:3857 (transformed back to EPSG:4326 for storage). Occurrences of the
#' same species inside the circle with `year < extinction_year` are marked
#' `temporal_status = "suppressed"`. Occurrences with `year >= extinction_year`
#' remain `"active"` (recovery — no special tracking, per Lucian's simplified spec).
#'
#' NB: cheCkOVER already has a `status` column (native/alien/unknown) from ingest,
#' so we use a separate `temporal_status` column to avoid clobbering it.
#'
#' Important: this is intended to operate per-species. If `occurrences` contains
#' multiple species, masking is restricted to records sharing the species value
#' of each extinction record.
#'
#' NB (per Lucian, 2026-04): Extinction-claim records (is_extinct == TRUE)
#' themselves are tagged `temporal_status = "extinct"`. This is a distinct
#' state from "active" and "suppressed". Extinction claims contribute to
#' NOTHING in downstream metrics: not EOO/AOO, not country/basin/PA counts,
#' not occurrence counts, not active-presence maps. They are pure
#' spatial+temporal markers. All downstream filters use `temporal_status ==
#' "active"`, which excludes "extinct" for free.
#'
#' @param occurrences cheCkOVER occurrence dataframe.
#' @param extinctions Output of parse_extinction_causes().
#' @param log_unlinked Logical; warn on extinction events that suppress nothing.
#' @return `occurrences` with two added columns: `temporal_status`
#'   ("active" | "suppressed" | "extinct") and `suppressed_by_extinction`
#'   (NA or extinction record_id).
#' @export
apply_spatial_temporal_mask <- function(occurrences,
                                        extinctions,
                                        log_unlinked = TRUE) {
  
  # Tag extinction-claim records as "extinct" up front; they are neither
  # active nor suppressed — they're markers that do not contribute to metrics.
  occurrences <- occurrences %>%
    dplyr::mutate(
      temporal_status = dplyr::if_else(
        !is.na(.data$is_extinct) & .data$is_extinct == TRUE,
        "extinct",
        "active"
      ),
      suppressed_by_extinction = NA_character_
    )
  
  if (nrow(extinctions) == 0L) return(occurrences)
  
  # We need species on extinction events to scope masking. If parse_extinction_causes
  # didn't carry it through, attach via record_id lookup.
  if (!"species" %in% names(extinctions)) {
    extinctions <- extinctions %>%
      dplyr::left_join(
        occurrences %>% dplyr::select(.data$record_id, .data$species),
        by = "record_id"
      )
  }
  
  occ_sf_wgs <- sf::st_as_sf(
    occurrences,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  occ_sf_proj <- sf::st_transform(occ_sf_wgs, 3857)
  
  unlinked_count <- 0L
  
  for (i in seq_len(nrow(extinctions))) {
    ext <- extinctions[i, ]
    
    if (is.na(ext$latitude) || is.na(ext$longitude)) {
      warning(sprintf("Extinction %s skipped: missing coordinates.", ext$record_id))
      next
    }
    
    # Build 500 m circle in projected CRS, keep projected for st_intersects
    ext_pt_proj <- sf::st_sfc(
      sf::st_point(c(ext$longitude, ext$latitude)),
      crs = 4326
    ) %>% sf::st_transform(3857)
    
    ext_circle_proj <- sf::st_buffer(ext_pt_proj, dist = ext$buffer_radius_m)
    
    inside <- sf::st_intersects(occ_sf_proj, ext_circle_proj, sparse = FALSE)[, 1]
    
    same_sp <- if (!is.null(ext$species) && !is.na(ext$species)) {
      occurrences$species == ext$species
    } else {
      rep(TRUE, nrow(occurrences))  # fall back to all-species masking
    }
    
    older <- occurrences$year < ext$extinction_year
    
    # Never suppress extinction-claim records — they stay "extinct".
    not_extinct <- occurrences$temporal_status != "extinct"
    
    suppress <- inside & same_sp & older & !is.na(occurrences$year) & not_extinct
    # Defensive: NA in any operand becomes NA in `suppress`. Coerce to FALSE so
    # logical indexing into occurrences$temporal_status is well-defined.
    suppress[is.na(suppress)] <- FALSE
    
    if (!any(suppress) && log_unlinked) {
      unlinked_count <- unlinked_count + 1L
      warning(sprintf(
        "Unlinked extinction: record_id=%s at (%.4f, %.4f), year=%s — no occurrences in 500 m radius with year < %s.",
        ext$record_id, ext$latitude, ext$longitude,
        ext$extinction_year, ext$extinction_year
      ))
    }
    
    occurrences$temporal_status[suppress] <- "suppressed"
    occurrences$suppressed_by_extinction[suppress] <- ext$record_id
  }
  
  if (unlinked_count > 0L) {
    message(sprintf("[apply_spatial_temporal_mask] %d unlinked extinction(s) — see warnings.",
                    unlinked_count))
  }
  
  occurrences
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.4: TEMPORAL CHANGE DETECTION (vs PREVIOUS version, no recolonization)
# ──────────────────────────────────────────────────────────────────────────────

#' Detect new presences and extinctions vs the previous version
#'
#' Anti-joins on `record_id` to identify records present in current but not in
#' previous. Excludes records flagged `is_extinct == TRUE` from new_presences
#' (those are summarised separately as extinction events). Computes
#' net_change = n_new - n_extinct (no recolonization term, per Lucian's
#' simplified spec).
#'
#' Note: we use anti-join on record_id rather than a date-based filter for
#' extinctions. Extinction events documented in past years can be added to the
#' database later, so "new since previous version" must be defined by record-set
#' membership, not by the year on the record. The `previous_date` arg is kept
#' for backward compatibility but no longer used for filtering.
#'
#' @param current Current cheCkOVER occurrences (post-masking, with `temporal_status`).
#' @param previous Previous version occurrences snapshot.
#' @param extinctions Output of parse_extinction_causes() on `current`.
#' @param previous_date ISO date string of the previous version's processing date
#'   (kept for backward compatibility; no longer used).
#' @return List with `new_presences`, `extinctions`, `net_change`, `n_new`,
#'   `n_extinct`.
#' @export
detect_temporal_changes <- function(current,
                                    previous,
                                    extinctions,
                                    previous_date = NULL) {
  
  current_active <- current %>% dplyr::filter(.data$temporal_status == "active")
  
  if (!"record_id" %in% names(previous)) {
    stop("`previous` must contain `record_id` for anti-join.")
  }
  
  # New presences: in current_active, not in previous, and not themselves extinct
  # (extinction events are tracked in the extinctions tibble, not as new presences).
  new_presences <- current_active %>%
    dplyr::anti_join(previous %>% dplyr::select(.data$record_id), by = "record_id")
  
  if ("is_extinct" %in% names(new_presences)) {
    # Keep records where is_extinct is FALSE or NA; drop records explicitly extinct.
    new_presences <- new_presences %>%
      dplyr::filter(is.na(.data$is_extinct) | .data$is_extinct == FALSE)
  }
  
  new_presences <- new_presences %>% dplyr::mutate(change_type = "new_presence")
  
  # New extinctions: same anti-join logic, applied to the extinctions tibble.
  extinctions_new <- if (nrow(extinctions) == 0L) {
    extinctions
  } else {
    extinctions %>%
      dplyr::anti_join(previous %>% dplyr::select(.data$record_id), by = "record_id")
  }
  
  n_new     <- nrow(new_presences)
  n_extinct <- nrow(extinctions_new)
  
  list(
    new_presences = new_presences,
    extinctions   = extinctions_new,
    net_change    = n_new - n_extinct,
    n_new         = n_new,
    n_extinct     = n_extinct
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.5: GEOGRAPHIC CHANGE ANALYSIS
# ──────────────────────────────────────────────────────────────────────────────

#' Detect unit-level extirpations and additions vs previous version
#'
#' Compares country / hydrobasin / freshwater_ecoregion / protected_area sets
#' between active current records and previous records. Also computes per-unit
#' partial losses (number of extinctions vs previous presence count).
#'
#' @param current Current occurrences (with `temporal_status`).
#' @param previous Previous version occurrences snapshot.
#' @param extinctions Output of parse_extinction_causes() on current.
#' @return List of vectors and tibbles describing extirpations, additions, and
#'   partial losses.
#' @export
analyze_geographic_changes <- function(current, previous, extinctions) {
  
  current_active <- current %>% dplyr::filter(.data$temporal_status == "active")
  
  diff_unit <- function(col) {
    if (!col %in% names(previous) || !col %in% names(current_active)) {
      return(list(extirpated = character(0), added = character(0)))
    }
    p <- unique(stats::na.omit(previous[[col]]))
    c <- unique(stats::na.omit(current_active[[col]]))
    list(extirpated = setdiff(p, c), added = setdiff(c, p))
  }
  
  countries   <- diff_unit("country")
  basins      <- diff_unit("hydrobasin")
  ecoregions  <- diff_unit("freshwater_ecoregion")
  pas         <- diff_unit("protected_area")
  
  partial_loss <- function(unit_col) {
    if (!unit_col %in% names(previous) || nrow(extinctions) == 0L) {
      return(tibble::tibble())
    }
    if (!"record_id" %in% names(previous)) return(tibble::tibble())
    
    ext_with_unit <- extinctions %>%
      dplyr::left_join(
        previous %>% dplyr::select(.data$record_id, !!rlang::sym(unit_col)),
        by = "record_id"
      ) %>%
      dplyr::filter(!is.na(!!rlang::sym(unit_col)))
    
    if (nrow(ext_with_unit) == 0L) return(tibble::tibble())
    
    ext_count <- ext_with_unit %>%
      dplyr::count(!!rlang::sym(unit_col), name = "extinctions")
    
    base_count <- previous %>%
      dplyr::filter(!is.na(!!rlang::sym(unit_col))) %>%
      dplyr::count(!!rlang::sym(unit_col), name = "baseline_presences")
    
    ext_count %>%
      dplyr::left_join(base_count, by = unit_col) %>%
      dplyr::mutate(percent_loss = round((.data$extinctions / .data$baseline_presences) * 100, 1)) %>%
      dplyr::arrange(dplyr::desc(.data$percent_loss))
  }
  
  list(
    countries_extirpated   = countries$extirpated,
    countries_added        = countries$added,
    basins_extirpated      = basins$extirpated,
    basins_added           = basins$added,
    ecoregions_extirpated  = ecoregions$extirpated,
    ecoregions_added       = ecoregions$added,
    PAs_extirpated         = pas$extirpated,
    PAs_added              = pas$added,
    partial_losses_country = partial_loss("country"),
    partial_losses_basin   = partial_loss("hydrobasin")
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.6: RANGE METRICS DELTA (full recalculation on ACTIVE occurrences)
# ──────────────────────────────────────────────────────────────────────────────

#' Recalculate EOO/AOO on active occurrences and compare to previous metrics
#'
#' Uses cheCkOVER's existing `.calc_eoo` / `.calc_aoo` formulas for consistency
#' with module 3A. Only `temporal_status == "active"` records contribute.
#'
#' Range signal classification (per spec):
#'   contraction: ≤-10% in EOO or AOO
#'   expansion:   ≥+10% in EOO or AOO
#'   stable:      |Δ| < 5% in both
#'   mixed:       between thresholds or contradictory
#'
#' @param current Current occurrences (with `temporal_status`).
#' @param previous_metrics Named list with `EOO_km2` and `AOO_km2`.
#' @param eoo_fn Optional EOO function (defaults to .calc_eoo_default).
#' @param aoo_fn Optional AOO function (defaults to .calc_aoo_default).
#' @return List with previous/current EOO and AOO values, deltas, and signal.
#' @export
calculate_range_delta <- function(current,
                                  previous_metrics,
                                  eoo_fn = .calc_eoo_default,
                                  aoo_fn = .calc_aoo_default) {
  
  active <- current %>% dplyr::filter(.data$temporal_status == "active")
  
  EOO_curr <- eoo_fn(active$longitude, active$latitude)
  AOO_curr <- aoo_fn(active$longitude, active$latitude)
  
  EOO_prev <- previous_metrics$EOO_km2
  AOO_prev <- previous_metrics$AOO_km2
  
  pct <- function(curr, prev) {
    if (is.null(prev) || is.na(prev) || prev == 0) return(NA_real_)
    round(((curr - prev) / prev) * 100, 1)
  }
  
  EOO_pct <- pct(EOO_curr, EOO_prev)
  AOO_pct <- pct(AOO_curr, AOO_prev)
  
  signal <- if (is.na(EOO_pct) && is.na(AOO_pct)) {
    "unknown"
  } else {
    pcts <- c(EOO_pct, AOO_pct)
    pcts <- pcts[!is.na(pcts)]
    if (any(pcts <= -10)) "contraction"
    else if (any(pcts >=  10)) "expansion"
    else if (all(abs(pcts) < 5)) "stable"
    else "mixed"
  }
  
  list(
    EOO_previous_km2    = EOO_prev,
    EOO_current_km2     = EOO_curr,
    EOO_change_absolute = if (is.na(EOO_prev) || is.na(EOO_curr)) NA_real_ else round(EOO_curr - EOO_prev, 1),
    EOO_change_percent  = EOO_pct,
    AOO_previous_km2    = AOO_prev,
    AOO_current_km2     = AOO_curr,
    AOO_change_absolute = if (is.na(AOO_prev) || is.na(AOO_curr)) NA_real_ else round(AOO_curr - AOO_prev, 1),
    AOO_change_percent  = AOO_pct,
    range_signal        = signal
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# TASK 1.7: EXTINCTION SUMMARIZATION
# ──────────────────────────────────────────────────────────────────────────────

#' Summarise extinctions by cause, geography, temporal pattern, and rate
#'
#' @param extinctions Output of parse_extinction_causes() (full set, not filtered).
#' @param current Current occurrences (with `temporal_status`).
#' @param previous Previous version occurrences snapshot.
#' @return List with cause breakdown, primary invaders, geographic breakdown,
#'   temporal trend, hotspot, and percent_range_lost.
#' @export
summarize_extinctions <- function(extinctions, current, previous) {
  
  empty <- list(
    total_extinctions    = 0L,
    extinction_timeframe = NA_character_,
    percent_range_lost   = 0,
    by_cause             = tibble::tibble(),
    primary_invaders     = tibble::tibble(),
    by_country           = tibble::tibble(),
    by_basin             = tibble::tibble(),
    extinctions_by_decade= tibble::tibble(),
    temporal_trend       = "none",
    hotspot              = NULL
  )
  
  if (nrow(extinctions) == 0L) return(empty)
  
  # A. CAUSE BREAKDOWN
  by_cause <- extinctions %>%
    dplyr::count(.data$cause_category, name = "count") %>%
    dplyr::mutate(percent = round((.data$count / sum(.data$count)) * 100, 1)) %>%
    dplyr::arrange(dplyr::desc(.data$count))
  
  primary_invaders <- extinctions %>%
    dplyr::filter(.data$cause_category == "invasive") %>%
    dplyr::count(.data$cause_detail, name = "localities") %>%
    dplyr::arrange(dplyr::desc(.data$localities)) %>%
    dplyr::rename(invader_info = .data$cause_detail) %>%
    utils::head(5)
  
  # B. GEOGRAPHIC DISTRIBUTION (link extinctions to country/basin via previous)
  geo_join <- function(unit_col) {
    if (!unit_col %in% names(previous) || !"record_id" %in% names(previous)) {
      return(tibble::tibble())
    }
    ext_geo <- extinctions %>%
      dplyr::left_join(
        previous %>% dplyr::select(.data$record_id, !!rlang::sym(unit_col)),
        by = "record_id"
      ) %>%
      dplyr::filter(!is.na(!!rlang::sym(unit_col)))
    
    if (nrow(ext_geo) == 0L) return(tibble::tibble())
    
    by_unit <- ext_geo %>%
      dplyr::count(!!rlang::sym(unit_col), name = "extinctions")
    
    totals <- previous %>%
      dplyr::filter(!is.na(!!rlang::sym(unit_col))) %>%
      dplyr::count(!!rlang::sym(unit_col), name = "total_localities")
    
    by_unit %>%
      dplyr::left_join(totals, by = unit_col) %>%
      dplyr::mutate(percent = round((.data$extinctions / .data$total_localities) * 100, 1)) %>%
      dplyr::arrange(dplyr::desc(.data$extinctions))
  }
  
  by_country <- geo_join("country")
  by_basin   <- geo_join("hydrobasin")
  
  # Hotspot: narrative-only marker (per Lucian, 2026-04). Minimum-N threshold
  # prevents noisy "hotspot" classifications from low-baseline units (e.g.
  # 3/4 lost = 75% but with only 4 baseline localities is not meaningful).
  HOTSPOT_MIN_BASELINE <- 5L
  HOTSPOT_MIN_PERCENT  <- 50
  
  hotspot <- NULL
  # Filter candidate units by minimum baseline size before picking.
  # Guard against empty tibbles (geo_join returns tibble::tibble() when the
  # extinction has no matching country/basin in previous) — those have zero
  # columns, so referencing total_localities raises a pronoun error.
  country_cand <- if (nrow(by_country) > 0L && "total_localities" %in% names(by_country)) {
    by_country %>%
      dplyr::filter(!is.na(.data$total_localities) &
                      .data$total_localities >= HOTSPOT_MIN_BASELINE)
  } else tibble::tibble()
  basin_cand <- if (nrow(by_basin) > 0L && "total_localities" %in% names(by_basin)) {
    by_basin %>%
      dplyr::filter(!is.na(.data$total_localities) &
                      .data$total_localities >= HOTSPOT_MIN_BASELINE)
  } else tibble::tibble()
  
  if (nrow(country_cand) > 0L && country_cand$percent[1] > HOTSPOT_MIN_PERCENT) {
    hotspot <- list(
      unit_type     = "country",
      unit_name     = country_cand$country[1],
      extinctions   = country_cand$extinctions[1],
      baseline_localities = country_cand$total_localities[1],
      percent_of_unit = country_cand$percent[1],
      primary_cause = by_cause$cause_category[1]
    )
  } else if (nrow(basin_cand) > 0L) {
    hotspot <- list(
      unit_type     = "basin",
      unit_name     = basin_cand$hydrobasin[1],
      extinctions   = basin_cand$extinctions[1],
      baseline_localities = basin_cand$total_localities[1],
      percent_of_unit = basin_cand$percent[1],
      primary_cause = by_cause$cause_category[1]
    )
  }
  
  # C. TEMPORAL PATTERN
  by_decade <- extinctions %>%
    dplyr::mutate(
      decade = dplyr::case_when(
        .data$extinction_year < 1980 ~ "pre-1980",
        .data$extinction_year < 2000 ~ "1980-2000",
        TRUE ~ "post-2000"
      )
    ) %>%
    dplyr::count(.data$decade, name = "n")
  
  n_post  <- by_decade %>% dplyr::filter(.data$decade == "post-2000") %>% dplyr::pull(.data$n)
  n_post  <- if (length(n_post) == 0L) 0 else n_post
  n_early <- sum((by_decade %>% dplyr::filter(.data$decade != "post-2000"))$n)
  
  trend <- dplyr::case_when(
    n_early == 0 & n_post > 0      ~ "accelerating",
    n_post  > n_early * 1.5        ~ "accelerating",
    n_post  < n_early * 0.67       ~ "decelerating",
    TRUE                           ~ "stable"
  )
  
  # D. RATE
  # Per Lucian (2026-04): single-year timeframe is NOT a date range, it's a
  # backward-mask marker. Display as "2008" not "2008–2008".
  min_y <- min(extinctions$extinction_year, na.rm = TRUE)
  max_y <- max(extinctions$extinction_year, na.rm = TRUE)
  timeframe <- if (min_y == max_y) as.character(min_y) else sprintf("%d–%d", min_y, max_y)
  
  pct_lost <- if (nrow(previous) > 0L) {
    round((nrow(extinctions) / nrow(previous)) * 100, 1)
  } else NA_real_
  
  list(
    total_extinctions     = nrow(extinctions),
    extinction_timeframe  = timeframe,
    percent_range_lost    = pct_lost,
    by_cause              = by_cause,
    primary_invaders      = primary_invaders,
    by_country            = by_country,
    by_basin              = by_basin,
    extinctions_by_decade = by_decade,
    temporal_trend        = trend,
    hotspot               = hotspot
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# VERSION MANAGEMENT (Tasks 2.1, 2.3, 2.4 — kept here for cohesion with detect_*)
# ──────────────────────────────────────────────────────────────────────────────

#' Determine the next version number for a species
#'
#' @param species_clean Path-safe species identifier.
#' @param major_bump If TRUE, increments major (v1.X → v2.0); else minor (v1.X → v1.X+1).
#' @param root_output_dir Optional override.
#' @return Version string ("v1.0" if no priors).
#' @export
generate_version_number <- function(species_clean,
                                    major_bump = FALSE,
                                    root_output_dir = NULL) {
  
  art <- detect_prior_artifacts(species_clean, root_output_dir)
  if (!art$artifacts_exist) return("v1.0")
  
  parts <- as.numeric(strsplit(sub("^v", "", art$latest_version), "\\.", fixed = FALSE)[[1]])
  if (length(parts) < 2L) parts <- c(parts, 0)
  
  if (major_bump) sprintf("v%d.0", parts[1] + 1)
  else            sprintf("v%d.%d", parts[1], parts[2] + 1)
}


#' Archive a previous version's files to the archive subdirectory
#'
#' Copies (does not move) all files matching the version pattern into
#' `archive/`, with a `_archived_YYYYMMDD` suffix. Originals are deleted
#' AFTER the archive copy succeeds.
#'
#' @param species_clean Path-safe species identifier.
#' @param version_to_archive Version string (e.g. "v1.0").
#' @param root_output_dir Optional override.
#' @return Path to archive directory (invisibly).
#' @export
archive_previous_version <- function(species_clean,
                                     version_to_archive,
                                     root_output_dir = NULL) {
  
  out_dir <- file.path(temporal_root_dir(root_output_dir), species_clean)
  archive_dir <- file.path(out_dir, "archive")
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  
  pattern <- sprintf("^%s_(occurrences_)?%s\\.", species_clean, version_to_archive)
  files <- list.files(out_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0L) {
    warning(sprintf("No files found to archive for %s %s", species_clean, version_to_archive))
    return(invisible(archive_dir))
  }
  
  ts <- format(Sys.Date(), "%Y%m%d")
  
  for (f in files) {
    base <- basename(f)
    ext  <- tools::file_ext(base)
    stem <- tools::file_path_sans_ext(base)
    arch <- file.path(archive_dir, sprintf("%s_archived_%s.%s", stem, ts, ext))
    ok <- file.copy(f, arch, overwrite = FALSE)
    if (ok) {
      file.remove(f)
      message("Archived: ", base, " → ", basename(arch))
    } else {
      warning("Failed to archive ", base, " (target may already exist).")
    }
  }
  
  invisible(archive_dir)
}


#' Update or create the species version manifest
#'
#' @param species_clean Path-safe species identifier.
#' @param version Version string for the new entry.
#' @param delta_summary Output of temporal_delta() or NULL for baseline.
#' @param metadata Named list with `record_count_total`, `record_count_active`,
#'   `record_count_suppressed`.
#' @param root_output_dir Optional override.
#' @return The full manifest list (invisibly).
#' @export
update_version_manifest <- function(species_clean,
                                    version,
                                    delta_summary = NULL,
                                    metadata,
                                    root_output_dir = NULL) {
  
  out_dir <- file.path(temporal_root_dir(root_output_dir), species_clean)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  manifest_file <- file.path(out_dir, sprintf("%s_manifest.json", species_clean))
  
  manifest <- if (file.exists(manifest_file)) {
    jsonlite::read_json(manifest_file, simplifyVector = FALSE)
  } else {
    list(species = species_clean, versions = list())
  }
  
  entry <- list(
    version              = version,
    date                 = as.character(Sys.Date()),
    records_total        = metadata$record_count_total,
    records_active       = metadata$record_count_active,
    records_suppressed   = metadata$record_count_suppressed,
    processing_framework = "cheCkOVER (temporal_delta v1)"
  )
  
  if (!is.null(delta_summary)) {
    entry$comparison_base       <- delta_summary$previous_version
    entry$baseline_version      <- delta_summary$baseline_version
    entry$records_added         <- delta_summary$changes$n_new
    entry$extinctions_documented<- delta_summary$extinction_summary$total_extinctions
    entry$net_change            <- delta_summary$changes$net_change
    entry$range_signal          <- delta_summary$range_delta$range_signal
    entry$EOO_change_percent    <- delta_summary$range_delta$EOO_change_percent
    entry$AOO_change_percent    <- delta_summary$range_delta$AOO_change_percent
  } else {
    entry$comparison_base <- NULL
  }
  
  manifest$versions <- c(manifest$versions, list(entry))
  
  jsonlite::write_json(manifest, manifest_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  message("Manifest updated: ", manifest_file)
  
  invisible(manifest)
}


#' Save baseline artifacts for use as the previous-version cache on next run
#'
#' Writes the occurrence snapshot (.rds) and a metrics JSON; both are read by
#' load_previous_version() during the next pipeline invocation. This is the
#' minimum set of files needed for the temporal logic to chain across runs;
#' full canonical narrative + maps come from Phase 2/3.
#'
#' @param species_clean Path-safe species identifier.
#' @param occurrences Current occurrences (with `temporal_status` and `status`).
#' @param metrics Named list with at least `EOO_km2`, `AOO_km2`.
#' @param version Version string.
#' @param delta_data Optional delta_data (NULL for baseline).
#' @param root_output_dir Optional override.
#' @return List of files written (invisibly).
#' @export
save_versioned_snapshot <- function(species_clean,
                                    occurrences,
                                    metrics,
                                    version,
                                    delta_data = NULL,
                                    root_output_dir = NULL) {
  
  out_dir <- species_temporal_dir(species_clean, root_output_dir)
  
  rds_file  <- file.path(out_dir, sprintf("%s_occurrences_%s.rds", species_clean, version))
  json_file <- file.path(out_dir, sprintf("%s_%s.json", species_clean, version))
  
  saveRDS(occurrences, rds_file)
  
  # Defensive: temporal_status may be absent if caller forgot to apply mask first.
  if (!"temporal_status" %in% names(occurrences)) {
    occurrences$temporal_status <- "active"
  }
  
  payload <- list(
    species          = species_clean,
    version          = version,
    processing_date  = as.character(Sys.Date()),
    metrics          = list(
      EOO_km2     = metrics$EOO_km2,
      AOO_km2     = metrics$AOO_km2,
      countries   = metrics$countries  %||% unique(stats::na.omit(occurrences$country)),
      basins      = metrics$basins     %||% unique(stats::na.omit(occurrences$hydrobasin)),
      record_count_total      = nrow(occurrences),
      record_count_native     = sum(occurrences$temporal_status == "active" &
                                      occurrences$status %in% "native", na.rm = TRUE),
      record_count_alien      = sum(occurrences$temporal_status == "active" &
                                      occurrences$status %in% "alien",  na.rm = TRUE),
      record_count_active     = sum(occurrences$temporal_status == "active",     na.rm = TRUE),
      record_count_suppressed = sum(occurrences$temporal_status == "suppressed", na.rm = TRUE),
      record_count_extinct    = sum(occurrences$temporal_status == "extinct",    na.rm = TRUE)
    )
  )
  
  if (!is.null(delta_data)) payload$delta_data <- delta_data
  
  jsonlite::write_json(payload, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  
  invisible(list(rds = rds_file, json = json_file))
}


# ──────────────────────────────────────────────────────────────────────────────
# TOP-LEVEL ORCHESTRATOR
# ──────────────────────────────────────────────────────────────────────────────

#' Run the full temporal_delta pipeline for a single species + scope
#'
#' Wraps Tasks 1.1 through 1.7 plus version generation. Does NOT save snapshots
#' or render outputs — those are the caller's responsibility (Phase 2/3 work).
#'
#' If no prior artifacts exist, returns a baseline result (temporal_status == "active"
#' for all occurrences, delta_data == NULL).
#'
#' @param occurrences_current Cumulative occurrences for the species in this scope.
#' @param species_clean Path-safe species identifier.
#' @param scope Optional label ("indigenous", "non_indigenous", "merged") — does
#'   not affect logic but is carried through delta_data for downstream rendering.
#' @param root_output_dir Optional override.
#' @param major_bump If TRUE, force major version increment.
#' @return List with `version`, `occurrences` (with temporal_status), `delta_data`,
#'   `metrics_current`, `is_baseline`.
#' @export
temporal_delta <- function(occurrences_current,
                           species_clean,
                           scope = NA_character_,
                           root_output_dir = NULL,
                           major_bump = FALSE) {
  
  message("[temporal_delta] Species: ", species_clean,
          " | Scope: ", scope,
          " | Records in: ", nrow(occurrences_current))
  
  # 1. Detect priors
  art <- detect_prior_artifacts(species_clean, root_output_dir)
  
  # Compute current metrics (always — needed for snapshot whether baseline or not)
  metrics_current_fn <- function(occ_active) {
    list(
      EOO_km2   = .calc_eoo_default(occ_active$longitude, occ_active$latitude),
      AOO_km2   = .calc_aoo_default(occ_active$longitude, occ_active$latitude),
      countries = unique(stats::na.omit(occ_active$country)),
      basins    = unique(stats::na.omit(occ_active$hydrobasin))
    )
  }
  
  # Baseline path
  if (!art$artifacts_exist) {
    message("[temporal_delta] No prior artifacts — generating baseline v1.0.")
    occurrences_current <- occurrences_current %>%
      dplyr::mutate(temporal_status = "active", suppressed_by_extinction = NA_character_)
    
    return(list(
      version         = "v1.0",
      previous_version= NULL,
      scope           = scope,
      occurrences     = occurrences_current,
      delta_data      = NULL,
      metrics_current = metrics_current_fn(occurrences_current),
      is_baseline     = TRUE
    ))
  }
  
  # Temporal path
  message("[temporal_delta] Prior artifact: ", art$latest_version,
          " (", art$latest_date, ") — entering temporal mode.")
  
  prev <- load_previous_version(species_clean, art$latest_version, root_output_dir)
  
  message("[temporal_delta] Parsing extinctions…")
  ext <- parse_extinction_causes(occurrences_current)
  message("[temporal_delta]   extinction events: ", nrow(ext))
  
  message("[temporal_delta] Applying spatial-temporal mask (500 m geodesic radius)…")
  occurrences_current <- apply_spatial_temporal_mask(occurrences_current, ext)
  n_supp <- sum(occurrences_current$temporal_status == "suppressed", na.rm = TRUE)
  n_act  <- sum(occurrences_current$temporal_status == "active",     na.rm = TRUE)
  n_ext  <- sum(occurrences_current$temporal_status == "extinct",    na.rm = TRUE)
  message("[temporal_delta]   suppressed: ", n_supp, " | active: ", n_act)
  
  message("[temporal_delta] Detecting changes vs ", art$latest_version, "…")
  changes <- detect_temporal_changes(
    current       = occurrences_current,
    previous      = prev$occurrences_previous,
    extinctions   = ext,
    previous_date = prev$timestamp
  )
  message("[temporal_delta]   new presences: ", changes$n_new,
          " | extinctions: ", changes$n_extinct,
          " | net: ", changes$net_change)
  
  message("[temporal_delta] Geographic change analysis…")
  geo <- analyze_geographic_changes(
    current     = occurrences_current,
    previous    = prev$occurrences_previous,
    extinctions = ext
  )
  
  message("[temporal_delta] Range delta (full recalculation)…")
  rng <- calculate_range_delta(occurrences_current, prev$metrics_previous)
  message("[temporal_delta]   ΔEOO: ", rng$EOO_change_percent, "%",
          " | ΔAOO: ", rng$AOO_change_percent, "%",
          " | signal: ", rng$range_signal)
  
  message("[temporal_delta] Extinction summary…")
  ext_sum <- summarize_extinctions(ext, occurrences_current, prev$occurrences_previous)
  
  version_new <- generate_version_number(species_clean, major_bump, root_output_dir)
  message("[temporal_delta] New version: ", version_new)
  
  # Resolve baseline_version: first entry of manifest if present, else assume v1.0.
  # Defensive: handle both list-of-lists and data.frame manifest representations.
  baseline_version <- "v1.0"
  if (!is.null(prev$manifest) && !is.null(prev$manifest$versions)) {
    v <- prev$manifest$versions
    extracted <- tryCatch({
      if (is.data.frame(v) && nrow(v) > 0L) {
        as.character(v$version[1])
      } else if (is.list(v) && length(v) > 0L) {
        as.character(v[[1]]$version)
      } else NA_character_
    }, error = function(e) NA_character_)
    if (!is.null(extracted) && !is.na(extracted) && nzchar(extracted)) {
      baseline_version <- extracted
    }
  }
  
  delta_data <- list(
    version          = version_new,
    previous_version = art$latest_version,
    baseline_version = baseline_version,
    scope            = scope,
    previous_date    = prev$timestamp,
    current_date     = as.character(Sys.Date()),
    changes          = changes,
    geo_changes      = geo,
    range_delta      = rng,
    extinction_summary = ext_sum,
    masking_summary  = list(
      occurrences_suppressed = n_supp,
      occurrences_active     = n_act,
      occurrences_extinct    = n_ext,
      extinction_zones       = nrow(ext)
    )
  )
  
  list(
    version          = version_new,
    previous_version = art$latest_version,
    scope            = scope,
    occurrences      = occurrences_current,
    delta_data       = delta_data,
    metrics_current  = metrics_current_fn(occurrences_current %>% dplyr::filter(.data$temporal_status == "active")),
    is_baseline      = FALSE
  )
}
# ──────────────────────────────────────────────────────────────────────────────
# PER-SPECIES CHANGE DETECTION (added per Lucian, 2026-04)
#
# Compares current species records against the saved temporal baseline by
# WoC ID (record_id) and is_extinct flag. Used by Phase 5C in
# checkcover_main.R to skip species with no data changes — those stay
# frozen at their current version and receive no empty v1.1.
# ──────────────────────────────────────────────────────────────────────────────

#' Compare current species records against the previous temporal snapshot
#'
#' Lightweight per-species check that avoids creating empty temporal versions
#' for species whose data hasn't changed between pipeline runs.
#'
#' Checks two things:
#'   1. Record ID sets: any new or removed WoC IDs?
#'   2. Extinction flags: did any record's is_extinct status change?
#'
#' @param current_records Data.frame of current records for one species
#'   (must have `record_id`; optionally `is_extinct`).
#' @param previous_records Data.frame from the temporal RDS snapshot
#'   (must have `record_id`; optionally `is_extinct`).
#' @return List with `$status` ("unchanged", "changed", "new_species",
#'   "species_removed") and `$details` (human-readable explanation).
#' @export
compare_species_to_baseline <- function(current_records, previous_records) {
  
  # ── Edge cases ──
  has_curr <- !is.null(current_records) && nrow(current_records) > 0L
  has_prev <- !is.null(previous_records) && nrow(previous_records) > 0L
  
  if (!has_prev && !has_curr) {
    return(list(status = "unchanged", details = "No data in either version"))
  }
  if (!has_prev) {
    return(list(status = "new_species",
                details = sprintf("New species with %d records", nrow(current_records))))
  }
  if (!has_curr) {
    return(list(status = "species_removed",
                details = sprintf("Species removed (%d previous records)", nrow(previous_records))))
  }
  
  # ── Ensure record_id exists ──
  if (!"record_id" %in% names(current_records)) {
    return(list(status = "changed", details = "No record_id in current data (cannot compare)"))
  }
  if (!"record_id" %in% names(previous_records)) {
    return(list(status = "changed", details = "No record_id in previous snapshot (legacy format)"))
  }
  
  curr_ids <- as.character(current_records$record_id)
  prev_ids <- as.character(previous_records$record_id)
  
  # ── 1. Compare record ID sets ──
  new_ids     <- setdiff(curr_ids, prev_ids)
  removed_ids <- setdiff(prev_ids, curr_ids)
  
  if (length(new_ids) > 0L || length(removed_ids) > 0L) {
    return(list(
      status   = "changed",
      n_new    = length(new_ids),
      n_removed = length(removed_ids),
      details  = sprintf("+%d records, -%d records", length(new_ids), length(removed_ids))
    ))
  }
  
  # ── 2. Same IDs — check extinction flags ──
  # Build a named vector of is_extinct per record_id for both datasets
  .get_extinct_flags <- function(df) {
    flags <- if ("is_extinct" %in% names(df)) {
      as.logical(df$is_extinct)
    } else {
      rep(FALSE, nrow(df))
    }
    flags[is.na(flags)] <- FALSE
    stats::setNames(flags, as.character(df$record_id))
  }
  
  curr_flags <- .get_extinct_flags(current_records)
  prev_flags <- .get_extinct_flags(previous_records)
  
  # Align by record_id (both have the same IDs at this point)
  shared_ids <- curr_ids  # guaranteed equal to prev_ids after setdiff check
  n_new_extinct   <- sum(curr_flags[shared_ids] & !prev_flags[shared_ids])
  n_revived       <- sum(!curr_flags[shared_ids] & prev_flags[shared_ids])
  
  if (n_new_extinct > 0L || n_revived > 0L) {
    parts <- character(0)
    if (n_new_extinct > 0L) parts <- c(parts, sprintf("%d new extinction(s)", n_new_extinct))
    if (n_revived > 0L)     parts <- c(parts, sprintf("%d revived record(s)", n_revived))
    return(list(
      status          = "changed",
      n_new_extinct   = n_new_extinct,
      n_revived       = n_revived,
      details         = paste(parts, collapse = ", ")
    ))
  }
  
  # ── Everything identical ──
  list(status = "unchanged",
       details = sprintf("Identical (%d records, same extinction flags)", length(curr_ids)))
}
