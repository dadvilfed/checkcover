#### MODULE 2C: TEOW ENRICHMENT ####

load_teow <- function(cache_dir, module = "MODULE2C_TEOW") {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_path <- file.path(cache_dir, "teow_worldecoregions_min.rds")
  
  if (file.exists(cache_path)) {
    log_info("Loading TEOW from cache: %s", cache_path, module = module)
    teow_min <- readRDS(cache_path)
    teow_min <- sanitize_spatial_layer(teow_min, layer_name = "TEOW_cached")
    log_debug("Loaded TEOW cache with %d polygons.", nrow(teow_min), module = module)
    return(teow_min)
  }
  
  log_info("Loading TEOW (world terrestrial ecoregions)...", module = module)
  
  teow_env <- new.env(parent = emptyenv())
  utils::data("worldecoregions", package = "ecoregions", envir = teow_env)
  teow <- get("worldecoregions", envir = teow_env)
  
  log_debug("Raw TEOW polygons loaded: %d features, %d columns.",
            nrow(teow), ncol(teow), module = module)
  
  # teow <- sf::st_make_valid(teow)
  # teow <- sf::st_zm(teow, drop = TRUE, what = "ZM")
  # 
  # if (is.na(sf::st_crs(teow))) {
  #   log_warn("TEOW CRS missing. Setting to EPSG:4326.", module = module)
  #   teow <- sf::st_set_crs(teow, 4326)
  # }
  # if (!is.na(sf::st_crs(teow)$epsg) && sf::st_crs(teow)$epsg != 4326) {
  #   log_info("Transforming TEOW CRS to EPSG:4326.", module = module)
  #   teow <- sf::st_transform(teow, 4326)
  # }
  teow <- sanitize_spatial_layer(teow, layer_name = "TEOW_source")
  
  name_candidates <- c("ECO_NAME", "eco_name", "ECONAME", "ECOREGION", 
                       "ECOREGION_NAME", "ECO_NAME_E", "ECO_NAMEEN")
  name_col <- name_candidates[name_candidates %in% names(teow)]
  
  if (length(name_col) == 0) {
    log_error("Could not find an ecoregion name field in TEOW polygons.", module = module)
    stop("Could not find an ecoregion name field in TEOW polygons.")
  }
  
  name_col <- name_col[1]
  log_info("Using TEOW name column: %s", name_col, module = module)
  
  teow_min <- teow[, c(name_col, "geometry")]
  names(teow_min)[1] <- "ecoregion"
  
  saveRDS(teow_min, cache_path)
  log_info("Cached TEOW to: %s", cache_path, module = module)
  
  teow_min
}

enrich_with_teow <- function(result, output_dir = "checkover_output",
                             use_nearest_for_na = TRUE) {
  module <- "MODULE2C_TEOW"
  
  with_log_section(module, {
    log_info("=== MODULE 2C: TEOW ENRICHMENT (ecoregion) ===", module = module)
    
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Missing clean_data/clean_sf in result.")
    }
    if (!inherits(result$clean_sf, "sf")) {
      stop("result$clean_sf must be an sf object.")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    
    teow_sf <- load_teow(cache_dir = file.path(output_dir, "cache"), module = module)
    
    log_info("Intersecting %d points with TEOW polygons...",
             nrow(result$clean_sf), module = module)
    
    joined <- suppressWarnings(sf::st_join(
      result$clean_sf, teow_sf, join = sf::st_within, left = TRUE
    ))
    
    na_mask <- is.na(joined$ecoregion)
    na_n <- sum(na_mask, na.rm = TRUE)
    
    if (na_n > 0 && use_nearest_for_na) {
      log_info("Assigning nearest TEOW polygon for %d points...", na_n, module = module)
      nearest_idx <- sf::st_nearest_feature(joined[na_mask, ], teow_sf)
      joined$ecoregion[na_mask] <- teow_sf$ecoregion[nearest_idx]
      na_mask <- is.na(joined$ecoregion)
      na_n <- sum(na_mask, na.rm = TRUE)
    }
    
    if (na_n > 0) {
      log_warn("After nearest-polygon fallback, %d points still lack ecoregion.",
               na_n, module = module)
    } else {
      log_info("All points assigned to an ecoregion.", module = module)
    }
    
    result$clean_sf$ecoregion <- joined$ecoregion
    result$clean_data$ecoregion <- joined$ecoregion
    
    # Debug summary
    dbg <- result$clean_data %>%
      dplyr::count(ecoregion, name = "n") %>%
      dplyr::arrange(dplyr::desc(n))
    
    if (nrow(dbg) > 0L) {
      dbg_str <- paste(capture.output(print(utils::head(dbg, 15))), collapse = "\n")
      log_info("Top ecoregions:\n%s", dbg_str, module = module)
    }
    
    out_tsv <- file.path(output_dir, "clean_occurrences_with_teow.tsv")
    out_rds <- file.path(output_dir, "clean_occurrences_sf_with_teow.rds")
    
    write_tsv(result$clean_data, out_tsv)
    saveRDS(result$clean_sf, out_rds)
    
    log_info("Added 'ecoregion' to %d records.", nrow(result$clean_data), module = module)
    
    result$files_created <- unique(c(result$files_created, out_tsv, out_rds))
    
    result
  })
}