#### MODULE 4A: METRICS & CATEGORIZATION (NON-INDIGENOUS) ####

#' Calculate EOO/AOO and categorize non-indigenous species
#' @param result_non_indigenous List with clean_data and clean_sf (non-indigenous records only)
#' @param output_dir Output directory
#' @return result_non_indigenous with added metrics and categories
calculate_non_indigenous_metrics <- function(result_non_indigenous, output_dir = "checkover_output") {
  module <- "MODULE4A_METRICS_NON_IND"
  
  with_log_section(module, {
    log_info("=== MODULE 4A: NON-INDIGENOUS METRICS & CATEGORIZATION ===", module = module)
    
    cd <- result_non_indigenous$clean_data
    
    if (nrow(cd) == 0) {
      log_warn("No non-indigenous data to process.", module = module)
      return(result_non_indigenous)
    }
    
    log_info("Processing %d species with non-indigenous records...",
             length(unique(cd$species)),
             module = module)
    
    # Filter to active records for metric computation.
    cd_active <- if ("temporal_status" %in% names(cd)) {
      n_excluded <- sum(cd$temporal_status != "active", na.rm = TRUE)
      if (n_excluded > 0L) {
        log_info("Excluding %d non-active records (extinct + suppressed) from metrics.",
                 n_excluded, module = module)
      }
      cd[cd$temporal_status == "active", , drop = FALSE]
    } else cd
    
    # Helper: Calculate EOO
    .calc_eoo <- function(lon, lat) {
      coords <- unique(data.frame(lon, lat))
      coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
      if (nrow(coords) >= 3) {
        pts <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
        hull <- sf::st_convex_hull(sf::st_union(pts))
        return(as.numeric(sf::st_area(hull)) / 1e6)  # km²
      }
      return(NA_real_)
    }
    
    # Helper: Calculate AOO
    .calc_aoo <- function(lon, lat) {
      coords <- data.frame(lon, lat)
      coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
      if (nrow(coords) == 0) return(NA_real_)
      grid_size <- 0.018  # ~2km
      n_cells <- length(unique(paste(floor(coords$lon/grid_size), floor(coords$lat/grid_size))))
      return(n_cells * 4)  # 4 km² per cell
    }
    
    # Calculate metrics per species
    log_info("Calculating EOO and AOO...", module = module)
    
    metrics <- cd_active %>%
      group_by(species) %>%
      summarise(
        n_records = n(),
        n_countries = n_distinct_geo(country),   # excludes NA/blank/unresolved
        # Same thresholded rule as the indigenous branch (informational here —
        # the local/widespread split keys off EOO, not continents).
        n_continents = count_continents(continents),
        eoo_km2 = .calc_eoo(longitude, latitude),
        aoo_km2 = .calc_aoo(longitude, latitude),
        .groups = "drop"
      )
    
    # Non-Indigenous Categorization
    # Local: EOO ≤ 5,000 km² (same threshold as endemic)
    # Widespread: EOO > 5,000 km² (equivalent to regional/cosmopolitan)
    
    log_info("Applying local/widespread categorization...", module = module)
    
    metrics <- metrics %>%
      mutate(
        category = case_when(
          # <3 records: a convex-hull EOO needs >=3 non-collinear points, so
          # eoo_km2 is NA here. Without this branch such species fell through the
          # `TRUE ~ "widespread"` fallback and were labelled widespread purely
          # because their EOO was undefined (10 of 29 in v1.0) — the same
          # NA-falls-through pattern fixed on the indigenous side. Too few
          # records is "local" (short-range), never "widespread".
          n_records < 3 ~ "local",
          !is.na(eoo_km2) & eoo_km2 <= 5000 ~ "local",
          TRUE ~ "widespread"  # Everything else
        ),
        hydrobasins_level = case_when(
          category == "local" ~ 10L,
          category == "widespread" ~ 6L,
          TRUE ~ 6L
        )
      )
    
    # Log categorization summary
    cat_summary <- table(metrics$category)
    log_info("=== CATEGORIZATION SUMMARY ===", module = module)
    log_info("Local species (EOO ≤ 5,000 km²): %d", cat_summary["local"] %||% 0, module = module)
    log_info("Widespread species (EOO > 5,000 km²): %d", cat_summary["widespread"] %||% 0, module = module)
    
    # Detailed breakdown
    for (i in seq_len(nrow(metrics))) {
      log_info("  %s: %s (EOO=%.0f km², countries=%d, continents=%d) → Level %d",
               metrics$species[i],
               metrics$category[i],
               metrics$eoo_km2[i],
               metrics$n_countries[i],
               metrics$n_continents[i],
               metrics$hydrobasins_level[i],
               module = module)
    }
    
    # Join back to clean_data
    cd <- cd %>%
      left_join(metrics %>% select(species, eoo_km2, aoo_km2, category, hydrobasins_level),
                by = "species")
    
    result_non_indigenous$clean_data <- cd
    
    # Also update clean_sf
    if (!is.null(result_non_indigenous$clean_sf)) {
      result_non_indigenous$clean_sf <- result_non_indigenous$clean_sf %>%
        left_join(metrics %>% select(species, eoo_km2, aoo_km2, category, hydrobasins_level),
                  by = "species")
    }
    
    # Store metrics table
    result_non_indigenous$metrics <- metrics
    
    # Save metrics
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    metrics_file <- file.path(output_dir, "non_indigenous_metrics.tsv")
    write_tsv(metrics, metrics_file)
    log_info("Saved metrics to: %s", metrics_file, module = module)
    
    log_info("Non-indigenous metrics calculation complete.", module = module)
    
    return(result_non_indigenous)
  })
}