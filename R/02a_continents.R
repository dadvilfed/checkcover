#### MODULE 2A: CONTINENTS ENRICHMENT ####

load_ne_continents_medium <- function(cache_dir, module = "MODULE2A_CONTINENTS") {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_path <- file.path(cache_dir, "ne_continents_50m.rds")
  
  if (file.exists(cache_path)) {
    log_info("Loading continents from cache: %s", cache_path, module = module)
    continents <- readRDS(cache_path)
    log_debug("Loaded %d continent polygons from cache.", nrow(continents), module = module)
    return(continents)
  }
  
  log_info("Loading Natural Earth admin-0 countries (scale = 'medium')...", module = module)
  
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::select(continent, geom = geometry)
  
  log_info("Cleaning and dissolving countries into continents...", module = module)
  
  countries$geom <- sf::st_make_valid(countries$geom)
  countries <- sf::st_zm(countries, drop = TRUE, what = "ZM")
  
  continents <- countries %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(geom = sf::st_union(geom), .groups = "drop") %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(4326)
  
  saveRDS(continents, cache_path)
  log_info("Cached continents to: %s", cache_path, module = module)
  
  continents
}

enrich_with_continents <- function(result, output_dir = "checkover_output",
                                   use_nearest_for_na = TRUE) {
  module <- "MODULE2A_CONTINENTS"
  
  with_log_section(module, {
    log_info("=== MODULE 2A: CONTINENTS ENRICHMENT ===", module = module)
    
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Expected list from ingest_clean(): missing 'clean_data' or 'clean_sf'.")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    
    continents_sf <- load_ne_continents_medium(
      cache_dir = file.path(output_dir, "cache"),
      module = module
    )
    
    continents_min <- continents_sf %>% dplyr::select(continents = continent)
    
    pts_sf <- result$clean_sf
    log_info("Intersecting %d points with continent polygons...", 
             nrow(pts_sf), module = module)
    
    joined <- suppressWarnings(
      sf::st_join(pts_sf, continents_min, join = sf::st_within, left = TRUE)
    )
    
    na_mask <- is.na(joined$continents)
    na_n <- sum(na_mask, na.rm = TRUE)
    
    if (na_n > 0 && use_nearest_for_na) {
      log_info("Assigning nearest continent for %d coastal/offshore points...", 
               na_n, module = module)
      idx <- sf::st_nearest_feature(joined[na_mask, ], continents_min)
      joined$continents[na_mask] <- continents_min$continents[idx]
      na_mask <- is.na(joined$continents)
      na_n <- sum(na_mask, na.rm = TRUE)
    }
    
    if (na_n > 0) {
      log_warn("After nearest-continent fallback, %d points still lack continent.", 
               na_n, module = module)
    } else {
      log_info("All points assigned to a continent.", module = module)
    }
    
    result$clean_sf <- joined
    result$clean_data <- result$clean_data %>%
      dplyr::mutate(continents = joined$continents)
    
    # Debug summary
    dbg <- result$clean_data %>%
      dplyr::count(continents, name = "n") %>%
      dplyr::arrange(dplyr::desc(n))
    
    if (nrow(dbg) > 0L) {
      dbg_str <- paste(capture.output(print(dbg)), collapse = "\n")
      log_info("Per-continent record counts:\n%s", dbg_str, module = module)
    }
    
    out_tsv <- file.path(output_dir, "clean_occurrences_with_continents.tsv")
    out_rds <- file.path(output_dir, "clean_occurrences_sf_with_continents.rds")
    
    write_tsv(result$clean_data, out_tsv)
    saveRDS(result$clean_sf, out_rds)
    
    log_info("Saved enriched occurrences with continents.", module = module)
    
    result$files_created <- c(result$files_created, out_tsv, out_rds)
    
    result
  })
}