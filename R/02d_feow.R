#### MODULE 2D: FEOW ENRICHMENT ####

.feow_has_reader <- function() {
  if (!requireNamespace("feowR", quietly = TRUE)) return(FALSE)
  "read_feow" %in% getNamespaceExports("feowR")
}

load_feow_min <- function(cache_dir, feow_source = "auto", feow_shp_path = NULL,
                          verbose = TRUE, module = "MODULE2D_FEOW") {
  feow_source <- match.arg(feow_source, c("auto", "feowR", "local"))
  
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_path <- file.path(cache_dir, "feow_min.rds")
  
  if (file.exists(cache_path)) {
    if (verbose) log_info("Loading FEOW from cache: %s", cache_path, module = module)
    feow_min <- readRDS(cache_path)
    if (verbose) log_debug("Loaded FEOW cache with %d polygons.", nrow(feow_min), module = module)
    return(feow_min)
  }
  
  feow_sf <- NULL
  
  if (feow_source == "feowR") {
    if (verbose) log_info("FEOW source: feowR package (forced)", module = module)
    if (.feow_has_reader()) {
      if (verbose) log_info("Loading FEOW via feowR::read_feow()...", module = module)
      feow_sf <- try(feowR::read_feow(), silent = TRUE)
      if (inherits(feow_sf, "try-error")) {
        log_error("feowR::read_feow() failed: %s",
                  conditionMessage(attr(feow_sf, "condition")), module = module)
        stop("Failed to load FEOW via feowR package.")
      }
    } else {
      log_error("feowR package not available.", module = module)
      stop("feowR package not available. Install with: remotes::install_github('brunomioto/feowR')")
    }
    
  } else if (feow_source == "local") {
    if (verbose) log_info("FEOW source: local shapefile (forced)", module = module)
    if (is.null(feow_shp_path) || !file.exists(feow_shp_path)) {
      log_error("Local FEOW shapefile not found: %s", feow_shp_path, module = module)
      stop("Local FEOW shapefile not found: ", feow_shp_path)
    }
    if (verbose) log_info("Reading FEOW from: %s", feow_shp_path, module = module)
    feow_sf <- try(sf::st_read(feow_shp_path, quiet = TRUE), silent = TRUE)
    if (inherits(feow_sf, "try-error")) {
      log_error("Failed to read local FEOW: %s",
                conditionMessage(attr(feow_sf, "condition")), module = module)
      stop("Failed to read local FEOW shapefile.")
    }
    
  } else {
    # AUTO mode
    if (verbose) log_info("FEOW source: auto (trying feowR, then local)", module = module)
    if (.feow_has_reader()) {
      if (verbose) log_info("Attempting feowR::read_feow()...", module = module)
      feow_sf <- try(feowR::read_feow(), silent = TRUE)
      if (inherits(feow_sf, "try-error")) {
        if (verbose) log_warn("feowR failed: %s",
                              conditionMessage(attr(feow_sf, "condition")), module = module)
        feow_sf <- NULL
      } else {
        if (verbose) log_info("Successfully loaded FEOW via feowR.", module = module)
      }
    } else {
      if (verbose) log_warn("feowR not available. Will try local.", module = module)
    }
    
    if (is.null(feow_sf) && !is.null(feow_shp_path)) {
      if (verbose) log_info("Falling back to local: %s", feow_shp_path, module = module)
      feow_sf <- try(sf::st_read(feow_shp_path, quiet = TRUE), silent = TRUE)
      if (inherits(feow_sf, "try-error")) {
        if (verbose) log_error("Failed to read local FEOW: %s",
                               conditionMessage(attr(feow_sf, "condition")), module = module)
        feow_sf <- NULL
      }
    }
    
    if (is.null(feow_sf)) {
      log_error("Could not load FEOW from any source.", module = module)
      stop("Could not load FEOW. Install feowR or provide valid feow_shp_path.")
    }
  }
  
  # Clean and standardize
  feow_sf <- sf::st_as_sf(feow_sf)
  feow_sf <- sf::st_make_valid(feow_sf)
  feow_sf <- sf::st_zm(feow_sf, drop = TRUE, what = "ZM")
  
  if (is.na(sf::st_crs(feow_sf))) {
    if (verbose) log_warn("FEOW CRS missing. Setting to EPSG:4326.", module = module)
    feow_sf <- sf::st_set_crs(feow_sf, 4326)
  } else if (!identical(sf::st_crs(feow_sf)$epsg, 4326L)) {
    if (verbose) log_info("Transforming FEOW CRS to EPSG:4326.", module = module)
    feow_sf <- sf::st_transform(feow_sf, 4326)
  }
  
  # Find name column
  name_candidates <- c("ECO_NAME", "FEOW_NAME", "ECONAME", "ECOREGION",
                       "ECOREGION_NAME", "ECO_NAME_E", "ECO_NAMEEN")
  name_col <- name_candidates[name_candidates %in% names(feow_sf)]
  
  if (length(name_col) == 0) {
    id_candidates <- c("ECO_ID", "FEOW_ID", "ECO_CODE", "ECO_ID_UC")
    id_col <- id_candidates[id_candidates %in% names(feow_sf)]
    if (length(id_col) == 0) {
      log_error("FEOW loaded but no name or ID field found.", module = module)
      stop("FEOW loaded but no name or ID field found.")
    }
    if (verbose) log_warn("Using ID field '%s' as freshwater_ecoregion label.", 
                          id_col[1], module = module)
    feow_min <- feow_sf[, c(id_col[1], "geometry")]
    names(feow_min)[1] <- "freshwater_ecoregion"
  } else {
    if (verbose) log_info("Using FEOW name column: %s", name_col[1], module = module)
    feow_min <- feow_sf[, c(name_col[1], "geometry")]
    names(feow_min)[1] <- "freshwater_ecoregion"
  }
  
  saveRDS(feow_min, cache_path)
  if (verbose) {
    log_info("Cached FEOW to: %s", cache_path, module = module)
    log_debug("FEOW minimal cache: %d polygons", nrow(feow_min), module = module)
  }
  
  feow_min
}

enrich_with_feow <- function(result, output_dir = "checkover_output",
                             feow_source = "auto", feow_shp_path = NULL,
                             use_nearest_for_na = TRUE, crop_to_points_bbox = TRUE) {
  module <- "MODULE2D_FEOW"
  feow_source <- match.arg(feow_source, c("auto", "feowR", "local"))
  
  with_log_section(module, {
    log_info("=== MODULE 2D: FEOW ENRICHMENT (freshwater_ecoregion) ===", module = module)
    log_info("FEOW source mode: %s", feow_source, module = module)
    
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Missing 'clean_data' or 'clean_sf' in result.")
    }
    if (!inherits(result$clean_sf, "sf")) {
      stop("'result$clean_sf' must be an sf POINT object.")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    
    cache_dir <- file.path(output_dir, "cache")
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created cache directory: %s", cache_dir, module = module)
    }
    
    feow_min <- load_feow_min(
      cache_dir = cache_dir,
      feow_source = feow_source,
      feow_shp_path = feow_shp_path,
      verbose = TRUE,
      module = module
    )
    
    # Crop to bbox
    if (crop_to_points_bbox) {
      lon <- result$clean_data$longitude
      lat <- result$clean_data$latitude
      mask <- is.finite(lon) & is.finite(lat)
      
      if (any(mask)) {
        xmin <- min(lon[mask]); xmax <- max(lon[mask])
        ymin <- min(lat[mask]); ymax <- max(lat[mask])
        pad <- 0.25
        xmin <- xmin - pad; xmax <- xmax + pad
        ymin <- ymin - pad; ymax <- ymax + pad
        
        if (is.finite(xmin) && is.finite(xmax) && is.finite(ymin) && is.finite(ymax) &&
            xmin < xmax && ymin < ymax) {
          bb <- sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                            crs = sf::st_crs(result$clean_sf))
          log_info("Cropping FEOW to bbox [%.3f, %.3f, %.3f, %.3f].",
                   xmin, ymin, xmax, ymax, module = module)
          feow_min <- tryCatch(suppressWarnings(sf::st_crop(feow_min, bb)),
                               error = function(e) { log_warn("FEOW crop failed.", module = module); feow_min })
        }
      }
    }
    
    log_info("Intersecting %d points with FEOW polygons...", nrow(result$clean_sf), module = module)
    
    joined <- suppressWarnings(sf::st_join(
      result$clean_sf, feow_min, join = sf::st_within, left = TRUE))
    na_mask <- is.na(joined$freshwater_ecoregion)
    na_n <- sum(na_mask, na.rm = TRUE)
    
    if (na_n > 0 && use_nearest_for_na) {
      log_info("Assigning nearest FEOW polygon for %d points...", na_n, module = module)
      nearest_idx <- sf::st_nearest_feature(joined[na_mask, ], feow_min)
      joined$freshwater_ecoregion[na_mask] <- feow_min$freshwater_ecoregion[nearest_idx]
      na_mask <- is.na(joined$freshwater_ecoregion)
      na_n <- sum(na_mask, na.rm = TRUE)
    }
    
    if (na_n > 0) {
      log_warn("After nearest-polygon fallback, %d points still lack freshwater_ecoregion.",
               na_n, module = module)
    } else {
      log_info("All points assigned to a freshwater ecoregion.", module = module)
    }
    
    result$clean_sf$freshwater_ecoregion <- joined$freshwater_ecoregion
    result$clean_data$freshwater_ecoregion <- joined$freshwater_ecoregion
    
    # Debug summary
    dbg <- result$clean_data %>%
      dplyr::count(freshwater_ecoregion, name = "n") %>%
      dplyr::arrange(dplyr::desc(n))
    
    if (nrow(dbg) > 0L) {
      dbg_str <- paste(capture.output(print(utils::head(dbg, 15))), collapse = "\n")
      log_info("Top freshwater ecoregions:\n%s", dbg_str, module = module)
    }
    
    out_tsv <- file.path(output_dir, "clean_occurrences_with_feow.tsv")
    out_rds <- file.path(output_dir, "clean_occurrences_sf_with_feow.rds")
    
    write_tsv(result$clean_data, out_tsv)
    saveRDS(result$clean_sf, out_rds)
    
    log_info("Added 'freshwater_ecoregion' to %d records.", nrow(result$clean_data), module = module)
    
    result$files_created <- unique(c(result$files_created, out_tsv, out_rds))
    
    result
  })
}