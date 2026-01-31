#### MODULE 3A: METRICS & CATEGORIZATION (INDIGENOUS) ####

#' Calculate EOO/AOO and categorize species using IUCN standards
#' @param result_indigenous List with clean_data and clean_sf (indigenous records only)
#' @param output_dir Output directory
#' @return result_indigenous with added metrics and categories
calculate_indigenous_metrics <- function(result_indigenous, output_dir = "checkover_output") {
  module <- "MODULE3A_METRICS_IND"
  
  with_log_section(module, {
    log_info("=== MODULE 3A: INDIGENOUS METRICS & IUCN CATEGORIZATION ===", module = module)
    
    cd <- result_indigenous$clean_data
    
    if (nrow(cd) == 0) {
      log_warn("No indigenous data to process.", module = module)
      return(result_indigenous)
    }
    
    log_info("Processing %d species with indigenous records...",
             length(unique(cd$species)),
             module = module)
    
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
    
    metrics <- cd %>%
      group_by(species) %>%
      summarise(
        n_records = n(),
        n_countries = n_distinct(country, na.rm = TRUE),
        n_continents = n_distinct(continents, na.rm = TRUE),
        eoo_km2 = .calc_eoo(longitude, latitude),
        aoo_km2 = .calc_aoo(longitude, latitude),
        .groups = "drop"
      )
    
    # IUCN Categorization
    # Endemic: EOO ≤ 5,000 km² AND ≤ 2 countries AND single continent
    # Regional: EOO > 5,000 km² AND single continent
    # Cosmopolitan: Multiple continents
    
    log_info("Applying IUCN categorization thresholds...", module = module)
    
    metrics <- metrics %>%
      mutate(
        iucn_category = case_when(
          n_continents > 1 ~ "cosmopolitan",
          !is.na(eoo_km2) & eoo_km2 <= 5000 & n_countries <= 2 & n_continents == 1 ~ "endemic",
          n_continents == 1 ~ "regional",
          TRUE ~ "regional"  # fallback
        ),
        hydrobasins_level = case_when(
          iucn_category == "endemic" ~ 10L,
          iucn_category == "regional" ~ 8L,
          iucn_category == "cosmopolitan" ~ 6L,
          TRUE ~ 8L
        )
      )
    
    # Log categorization summary
    cat_summary <- table(metrics$iucn_category)
    log_info("=== CATEGORIZATION SUMMARY ===", module = module)
    log_info("Endemic species: %d", cat_summary["endemic"] %||% 0, module = module)
    log_info("Regional species: %d", cat_summary["regional"] %||% 0, module = module)
    log_info("Cosmopolitan species: %d", cat_summary["cosmopolitan"] %||% 0, module = module)
    
    # Detailed breakdown
    for (i in seq_len(nrow(metrics))) {
      log_info("  %s: %s (EOO=%.0f km², n_countries=%d, n_continents=%d) → Level %d",
               metrics$species[i],
               metrics$iucn_category[i],
               metrics$eoo_km2[i],
               metrics$n_countries[i],
               metrics$n_continents[i],
               metrics$hydrobasins_level[i],
               module = module)
    }
    
    # Join back to clean_data
    cd <- cd %>%
      left_join(metrics %>% select(species, eoo_km2, aoo_km2, iucn_category, hydrobasins_level),
                by = "species")
    
    result_indigenous$clean_data <- cd
    
    # Also update clean_sf
    if (!is.null(result_indigenous$clean_sf)) {
      result_indigenous$clean_sf <- result_indigenous$clean_sf %>%
        left_join(metrics %>% select(species, eoo_km2, aoo_km2, iucn_category, hydrobasins_level),
                  by = "species")
    }
    
    # Store metrics table
    result_indigenous$metrics <- metrics
    
    # Save metrics
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    metrics_file <- file.path(output_dir, "indigenous_metrics.tsv")
    write_tsv(metrics, metrics_file)
    log_info("Saved metrics to: %s", metrics_file, module = module)
    
    log_info("Metrics calculation complete.", module = module)
    
    return(result_indigenous)
  })
}