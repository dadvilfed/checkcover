#### MODULE 13: TEMPORAL PIPELINE (per-species, scenario-aware) ####
# Author: 
# Phase 3 wrapper that orchestrates the temporal_delta workflow for a single
# species, handling Scenarios 1, 2, and 3 with the agreed
# **single-version-per-species, nested-scope-blocks** storage architecture.
#
# Storage layout (per species):
#   {root_output_dir}/temporal/{species_clean}/
#       {species_clean}_v1.X.md            full canonical narrative + Section 6
#       {species_clean}_v1.X.json          metrics + delta_data (nested scopes)
#       {species_clean}_occurrences_v1.X.rds  full per-species snapshot
#       {species_clean}_temporal_map_v1.X.{geojson,kml}
#       {species_clean}_manifest.json       full version history
#       archive/                            previous versions, timestamped
#
# JSON `delta_data` shape (for Scenario 3):
#   {
#     "version": "v1.1",
#     "previous_version": "v1.0",
#     "baseline_version": "v1.0",
#     "previous_date": "...", "current_date": "...",
#     "indigenous":     { ... per-scope delta from Phase 1 temporal_delta() ... },
#     "non_indigenous": { ... per-scope delta from Phase 1 temporal_delta() ... }
#   }
# For Scenario 1: only `indigenous` is present; Scenario 2: only `non_indigenous`.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(sf)
})

# Source guards: assume modules 11 and 12 are already loaded by the orchestrator
# (checkcover_main.R sources them in order).


# ──────────────────────────────────────────────────────────────────────────────
# Helper: per-scope computation reusing Phase 1 functions
# ──────────────────────────────────────────────────────────────────────────────

# Compute a per-scope delta given current + previous occurrences (already
# filtered to a single scope) and previous metrics (or NULL for baseline).
# Returns a list with `occurrences` (with temporal_status), `delta_data`
# (NULL for baseline), `metrics_current`.
.compute_scope_delta <- function(occurrences_current,
                                 occurrences_previous,   # NULL or tibble
                                 metrics_previous,       # NULL or list with EOO_km2/AOO_km2
                                 scope) {

  if (is.null(occurrences_current) || nrow(occurrences_current) == 0L) {
    return(list(
      occurrences     = NULL,
      delta_data      = NULL,
      metrics_current = NULL,
      is_baseline     = is.null(occurrences_previous)
    ))
  }

  # Baseline path — no previous data for this scope
  if (is.null(occurrences_previous) || nrow(occurrences_previous) == 0L) {
    occ_b <- occurrences_current %>%
      dplyr::mutate(temporal_status = "active",
                    suppressed_by_extinction = NA_character_)
    return(list(
      occurrences     = occ_b,
      delta_data      = NULL,
      metrics_current = list(
        EOO_km2   = .calc_eoo_default(occ_b$longitude, occ_b$latitude),
        AOO_km2   = .calc_aoo_default(occ_b$longitude, occ_b$latitude),
        countries = unique(stats::na.omit(occ_b$country)),
        basins    = unique(stats::na.omit(occ_b$hydrobasin))
      ),
      is_baseline = TRUE
    ))
  }

  # Temporal path — full Phase 1 sequence on this scope
  ext  <- parse_extinction_causes(occurrences_current)
  occ  <- apply_spatial_temporal_mask(occurrences_current, ext)

  changes <- detect_temporal_changes(
    current       = occ,
    previous      = occurrences_previous,
    extinctions   = ext,
    previous_date = NULL  # arg retained for API compatibility; logic uses anti-join
  )
  geo <- analyze_geographic_changes(occ, occurrences_previous, ext)
  rng <- calculate_range_delta(occ, metrics_previous)
  ext_sum <- summarize_extinctions(ext, occ, occurrences_previous)

  n_supp <- sum(occ$temporal_status == "suppressed", na.rm = TRUE)
  n_act  <- sum(occ$temporal_status == "active",     na.rm = TRUE)
  n_ext  <- sum(occ$temporal_status == "extinct",    na.rm = TRUE)

  delta_data <- list(
    scope             = scope,
    changes           = changes,
    geo_changes       = geo,
    range_delta       = rng,
    extinction_summary= ext_sum,
    masking_summary   = list(
      occurrences_suppressed = n_supp,
      occurrences_active     = n_act,
      occurrences_extinct    = n_ext,
      extinction_zones       = nrow(ext)
    )
  )

  active <- occ %>% dplyr::filter(.data$temporal_status == "active")
  metrics_current <- list(
    EOO_km2   = .calc_eoo_default(active$longitude, active$latitude),
    AOO_km2   = .calc_aoo_default(active$longitude, active$latitude),
    countries = unique(stats::na.omit(active$country)),
    basins    = unique(stats::na.omit(active$hydrobasin))
  )

  list(
    occurrences     = occ,
    delta_data      = delta_data,
    metrics_current = metrics_current,
    is_baseline     = FALSE
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# Top-level per-species wrapper
# ──────────────────────────────────────────────────────────────────────────────

#' Process a single species through the full temporal pipeline
#'
#' Detects priors once for the species, computes per-scope deltas (Scenarios 1,
#' 2, 3), assembles the nested delta_data, splices Section 6 into the canonical
#' markdown, generates 4-layer temporal maps, saves the snapshot + JSON, updates
#' the manifest, and archives the previous version.
#'
#' Inputs are filtered occurrence dataframes. The caller (Module 14 or
#' integration in checkcover_main.R) is responsible for filtering the WoC dataset
#' to a single species before calling this.
#'
#' @param species_clean Path-safe species identifier.
#' @param scenario Integer 1, 2, or 3 (from scenario_table).
#' @param occurrences_indigenous Tibble of indigenous occurrences for this species
#'   (NULL or empty if scenario = 2).
#' @param occurrences_non_indigenous Tibble of non-indigenous occurrences (NULL or
#'   empty if scenario = 1).
#' @param baseline_canonical_md Character — the v1.0 canonical markdown produced
#'   by Module 10 for this species. Used as the splice target.
#' @param root_output_dir Optional override for output root.
#' @param major_bump If TRUE, force major version increment.
#' @param map_formats Character vector of map formats to write.
#' @return List with `version`, `is_baseline`, `delta_data` (combined nested),
#'   `files_written` (named list of paths), `manifest` (updated).
#' @export
process_species_temporal <- function(species_clean,
                                     scenario,
                                     occurrences_indigenous     = NULL,
                                     occurrences_non_indigenous = NULL,
                                     baseline_canonical_md,
                                     root_output_dir = NULL,
                                     major_bump = FALSE,
                                     map_formats = c("geojson", "kml")) {

  message(sprintf("[temporal_pipeline] === %s (Scenario %d) ===", species_clean, scenario))

  # 1. Detect prior versions for this species (single-version-per-species)
  art <- detect_prior_artifacts(species_clean, root_output_dir)
  is_first_run <- !art$artifacts_exist

  # 2. Load the previous version (full per-species snapshot) if any
  prev <- if (!is_first_run) {
    load_previous_version(species_clean, art$latest_version, root_output_dir)
  } else NULL

  prev_full_occ <- if (!is.null(prev)) prev$occurrences_previous else NULL

  # 3. Pull per-scope previous data from the nested metrics structure
  prev_metrics_ind <- if (!is.null(prev) && !is.null(prev$metrics_previous$indigenous)) {
    prev$metrics_previous$indigenous
  } else NULL
  prev_metrics_ni  <- if (!is.null(prev) && !is.null(prev$metrics_previous$non_indigenous)) {
    prev$metrics_previous$non_indigenous
  } else NULL

  # If older snapshots stored a flat metrics block (single-scope), interpret
  # by scenario: that flat block belongs to whichever scope existed.
  if (!is.null(prev) && is.null(prev_metrics_ind) && is.null(prev_metrics_ni) &&
      !is.null(prev$metrics_previous$EOO_km2)) {
    if (scenario == 2L) prev_metrics_ni  <- prev$metrics_previous
    else                prev_metrics_ind <- prev$metrics_previous
  }

  prev_occ_ind <- if (!is.null(prev_full_occ) && "population_status" %in% names(prev_full_occ)) {
    prev_full_occ %>% dplyr::filter(.data$population_status == "indigenous")
  } else prev_full_occ  # fallback: all of it
  prev_occ_ni <- if (!is.null(prev_full_occ) && "population_status" %in% names(prev_full_occ)) {
    prev_full_occ %>% dplyr::filter(.data$population_status == "non-indigenous")
  } else NULL

  # 4. Compute per-scope deltas for the applicable scopes
  ind_result <- if (scenario %in% c(1L, 3L) &&
                    !is.null(occurrences_indigenous) &&
                    nrow(occurrences_indigenous) > 0L) {
    .compute_scope_delta(occurrences_indigenous, prev_occ_ind, prev_metrics_ind, "indigenous")
  } else NULL

  ni_result  <- if (scenario %in% c(2L, 3L) &&
                    !is.null(occurrences_non_indigenous) &&
                    nrow(occurrences_non_indigenous) > 0L) {
    .compute_scope_delta(occurrences_non_indigenous, prev_occ_ni, prev_metrics_ni, "non_indigenous")
  } else NULL

  # 5. Decide if this is overall baseline (no priors) or temporal update
  is_baseline_overall <- is_first_run

  version_new <- if (is_baseline_overall) {
    "v1.0"
  } else {
    generate_version_number(species_clean, major_bump, root_output_dir)
  }

  # 6. Combine per-scope occurrences into a single per-species snapshot
  combined_occ <- dplyr::bind_rows(
    if (!is.null(ind_result)) ind_result$occurrences else NULL,
    if (!is.null(ni_result))  ni_result$occurrences  else NULL
  )

  # 7. Assemble nested delta_data (NULL for baseline, populated otherwise)
  baseline_version <- "v1.0"
  if (!is.null(prev) && !is.null(prev$manifest)) {
    v <- prev$manifest$versions
    extracted <- tryCatch({
      if (is.data.frame(v) && nrow(v) > 0L) as.character(v$version[1])
      else if (is.list(v) && length(v) > 0L) as.character(v[[1]]$version)
      else NA_character_
    }, error = function(e) NA_character_)
    if (!is.null(extracted) && !is.na(extracted) && nzchar(extracted)) {
      baseline_version <- extracted
    }
  }

  delta_combined <- if (is_baseline_overall) NULL else list(
    version          = version_new,
    previous_version = art$latest_version,
    baseline_version = baseline_version,
    previous_date    = prev$timestamp %||% NA_character_,
    current_date     = as.character(Sys.Date()),
    scenario         = scenario,
    indigenous       = if (!is.null(ind_result)) ind_result$delta_data else NULL,
    non_indigenous   = if (!is.null(ni_result))  ni_result$delta_data  else NULL
  )

  # 8. Compose nested metrics for the snapshot
  metrics_combined <- list()
  if (!is.null(ind_result)) metrics_combined$indigenous     <- ind_result$metrics_current
  if (!is.null(ni_result))  metrics_combined$non_indigenous <- ni_result$metrics_current
  metrics_combined$record_count_total      <- nrow(combined_occ)
  metrics_combined$record_count_active     <- sum(combined_occ$temporal_status == "active",     na.rm = TRUE)
  metrics_combined$record_count_suppressed <- sum(combined_occ$temporal_status == "suppressed", na.rm = TRUE)
  metrics_combined$record_count_extinct    <- sum(combined_occ$temporal_status == "extinct",    na.rm = TRUE)

  # 9. Render full canonical markdown (baseline + Section 6 if temporal)
  full_md <- append_temporal_to_canonical(
    canonical_md = baseline_canonical_md,
    delta_data   = delta_combined,
    version      = version_new
  )

  # 10. Write canonical markdown
  out_dir <- species_temporal_dir(species_clean, root_output_dir)
  md_file <- file.path(out_dir, sprintf("%s_%s.md", species_clean, version_new))
  writeLines(full_md, md_file)

  # 11. Save snapshot (RDS + JSON) — extends save_versioned_snapshot to handle
  #     nested metrics by writing the JSON ourselves rather than using the helper.
  rds_file  <- file.path(out_dir, sprintf("%s_occurrences_%s.rds", species_clean, version_new))
  json_file <- file.path(out_dir, sprintf("%s_%s.json", species_clean, version_new))

  saveRDS(combined_occ, rds_file)

  json_payload <- list(
    species         = species_clean,
    version         = version_new,
    processing_date = as.character(Sys.Date()),
    scenario        = scenario,
    metrics         = metrics_combined,
    delta_data      = delta_combined  # NULL for baseline, omitted from JSON
  )
  jsonlite::write_json(json_payload, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")

  # 12. Generate temporal maps
  map_files <- tryCatch(
    generate_temporal_maps(
      occurrences   = combined_occ,
      delta_data    = delta_combined,
      output_dir    = out_dir,
      species_clean = species_clean,
      version       = version_new,
      formats       = map_formats
    ),
    error = function(e) {
      warning("generate_temporal_maps failed for ", species_clean, ": ",
              conditionMessage(e))
      list()
    }
  )

  # 13. Update manifest
  manifest <- update_version_manifest(
    species_clean = species_clean,
    version       = version_new,
    delta_summary = if (is_baseline_overall) NULL else list(
      previous_version = delta_combined$previous_version,
      baseline_version = delta_combined$baseline_version,
      # For manifest top-line summary, pool per-scope counts into species totals
      changes = list(
        n_new      = (delta_combined$indigenous$changes$n_new     %||% 0L) +
                     (delta_combined$non_indigenous$changes$n_new %||% 0L),
        n_extinct  = (delta_combined$indigenous$changes$n_extinct     %||% 0L) +
                     (delta_combined$non_indigenous$changes$n_extinct %||% 0L),
        net_change = (delta_combined$indigenous$changes$net_change     %||% 0L) +
                     (delta_combined$non_indigenous$changes$net_change %||% 0L)
      ),
      extinction_summary = list(
        total_extinctions = (delta_combined$indigenous$extinction_summary$total_extinctions     %||% 0L) +
                            (delta_combined$non_indigenous$extinction_summary$total_extinctions %||% 0L)
      ),
      # Range_delta + signal: report indigenous if available, else non-indigenous
      range_delta = if (!is.null(delta_combined$indigenous)) {
        delta_combined$indigenous$range_delta
      } else delta_combined$non_indigenous$range_delta
    ),
    metadata = list(
      record_count_total      = metrics_combined$record_count_total,
      record_count_active     = metrics_combined$record_count_active,
      record_count_suppressed = metrics_combined$record_count_suppressed
    ),
    root_output_dir = root_output_dir
  )

  # 14. Archive previous version (only if temporal update)
  if (!is_baseline_overall) {
    suppressMessages(archive_previous_version(
      species_clean       = species_clean,
      version_to_archive  = art$latest_version,
      root_output_dir     = root_output_dir
    ))
  }

  files_written <- list(
    markdown = md_file,
    rds      = rds_file,
    json     = json_file,
    maps     = map_files
  )

  message(sprintf("[temporal_pipeline] === %s done: %s%s ===",
                  species_clean, version_new,
                  if (is_baseline_overall) " (baseline)" else
                    sprintf(" (vs %s)", art$latest_version)))

  invisible(list(
    version       = version_new,
    is_baseline   = is_baseline_overall,
    delta_data    = delta_combined,
    files_written = files_written,
    manifest      = manifest
  ))
}
