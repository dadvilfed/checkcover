#### MODULE 8: SCENARIO-AWARE MAP GENERATION ####
## FIXED VERSION — Addresses:
##   Bug #1: rbind() → bind_rows() for Scenario 3 column mismatches
##   Bug #2: O(N×M) per-basin st_filter loop → vectorized st_intersects
##   Bug #3: do.call(rbind, basin_geometries) → bind_rows()
##   Bug #4: NA status basins now get fallback coloring
##   Bug #5: (02f_hydrobasins.R defaults — separate fix)
##   Bug #6: tryCatch around each species in main loop
##   Bug #7: AOO grid → floor-based cell computation to avoid memory explosion

#' Generate maps for all scenarios with proper styling
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @param formats Vector of output formats (geojson, kml)
#' @param bbox_expand_km Bounding box expansion in km
#' @return List of generated maps
generate_all_maps <- function(scenario_table,
                              result_indigenous,
                              result_non_indigenous,
                              output_dir = "checkover_output",
                              cache_dir = file.path(output_dir, "cache"),
                              formats = c("geojson", "kml"),
                              bbox_expand_km = 200) {
  module <- "MODULE8_MAPS"
  
  with_log_section(module, {
    log_info("=== MODULE 8: SCENARIO-AWARE MAP GENERATION ===", module = module)
    
    # Create maps directories
    maps_dir <- file.path(output_dir, "maps")
    if (!dir.exists(maps_dir)) dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
    
    eoo_dir <- file.path(maps_dir, "EOO")
    aoo_dir <- file.path(maps_dir, "AOO")
    basins_dir <- file.path(maps_dir, "basins")
    
    for (d in c(eoo_dir, aoo_dir, basins_dir)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Load all species
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]
    
    log_info("Generating maps for %d species across all scenarios...", length(all_species), module = module)
    
    generated_maps <- list()
    
    for (sp in all_species) {
      log_info("Processing maps for: %s", sp, module = module)
      
      # ═══ BUG #6 FIX: Wrap entire per-species block in tryCatch ═══
      sp_maps <- tryCatch({
        
        sp_clean <- make_package_id(sp)
        scenario <- scenario_table$scenario[scenario_table$species == sp][1]
        
        # Get species data from appropriate branch(es)
        sp_data <- NULL
        sp_sf <- NULL
        
        if (scenario == 1) {
          # Indigenous only
          sp_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
          sp_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
          population_types <- "indigenous"
          
        } else if (scenario == 2) {
          # Non-indigenous only
          sp_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
          sp_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
          population_types <- "non-indigenous"
          
        } else if (scenario == 3) {
          # ═══ BUG #1 FIX: bind_rows() instead of rbind() ═══
          # Indigenous branch has iucn_category; non-indigenous has category.
          # rbind() can crash or silently corrupt on column mismatches.
          ind_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
          non_ind_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
          sp_data <- dplyr::bind_rows(ind_data, non_ind_data)
          
          ind_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
          non_ind_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
          sp_sf <- dplyr::bind_rows(ind_sf, non_ind_sf)
          
          population_types <- "both"
        }
        
        if (is.null(sp_data) || nrow(sp_data) == 0) {
          log_warn("  No data for %s. Skipping.", sp, module = module)
          return(NULL)  # next in tryCatch context
        }
        
        maps_generated <- list()
        
        # --- 1. EOO (Extent of Occurrence) — INDIGENOUS RANGE ONLY ---
        # EOO reflects the native distribution. Scenario 2 species have no
        # native range so EOO is skipped entirely. Scenario 3 species use
        # only their indigenous points (the combined sp_sf would produce a
        # meaningless global hull spanning native + introduced ranges).
        
        eoo_sf_source <- NULL
        eoo_population_label <- NULL
        
        if (scenario == 1) {
          eoo_sf_source <- sp_sf
          eoo_population_label <- "indigenous"
        } else if (scenario == 3) {
          if ("population_type" %in% names(sp_sf)) {
            ind_mask <- !is.na(sp_sf$population_type) & sp_sf$population_type == "indigenous"
            if (sum(ind_mask) >= 3) {
              eoo_sf_source <- sp_sf[ind_mask, ]
              eoo_population_label <- "indigenous"
            } else {
              log_info("  EOO: skipping (Scenario 3 but <3 indigenous points)", module = module)
            }
          }
        } else {
          # Scenario 2: non-indigenous only — no native EOO
          log_info("  EOO: skipping (Scenario 2 — no native range)", module = module)
        }
        
        eoo_poly <- NULL
        if (!is.null(eoo_sf_source) && nrow(eoo_sf_source) >= 3) {
          eoo_poly <- tryCatch({
            hull <- sf::st_convex_hull(sf::st_union(eoo_sf_source))
            if (!all(sf::st_is_valid(hull))) hull <- sf::st_make_valid(hull)
            hull
          }, error = function(e) {
            log_warn("  EOO calculation failed: %s", conditionMessage(e), module = module)
            NULL
          })
        }
        
        if (!is.null(eoo_poly)) {
          eoo_sf <- sf::st_sf(geometry = sf::st_sfc(eoo_poly), crs = sf::st_crs(sp_sf))
          eoo_sf$Name <- paste(sp, "EOO")
          eoo_sf$population_type <- eoo_population_label
          
          # GeoJSON
          eoo_geo <- eoo_sf
          eoo_geo$fill <- "#FFFF00"
          eoo_geo$`fill-opacity` <- 0.80
          eoo_geo$stroke <- "#FFFF00"
          eoo_geo$`stroke-width` <- 2
          
          eoo_file <- file.path(eoo_dir, paste0(sp_clean, "_EOO.geojson"))
          try(sf::st_write(eoo_geo, eoo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
          maps_generated$eoo_geojson <- eoo_file
          
          # KML
          if ("kml" %in% formats) {
            eoo_kml <- file.path(eoo_dir, paste0(sp_clean, "_EOO.kml"))
            .write_styled_kml(eoo_sf, eoo_kml, "EOO", "#FFFF00", 0.80)
            maps_generated$eoo_kml <- eoo_kml
          }
        }
        
        # --- 2. AOO (Area of Occupancy) ---
        # ═══ BUG #7 FIX: Floor-based cell computation instead of st_make_grid ═══
        # st_make_grid on a global bounding box (e.g. P. clarkii: North America + Europe)
        # with 0.018° cells creates ~500 million grid cells → memory explosion.
        # Floor-based approach only creates cells where points actually exist.
        aoo_poly <- NULL
        tryCatch({
          coords <- sf::st_coordinates(sp_sf)
          grid_size <- 0.018  # ~2km
          
          # Compute unique grid cell origins using floor
          cell_x <- floor(coords[, 1] / grid_size) * grid_size
          cell_y <- floor(coords[, 2] / grid_size) * grid_size
          unique_cells <- unique(data.frame(x = cell_x, y = cell_y))
          
          log_info("  AOO: %d unique cells from %d points", nrow(unique_cells), nrow(coords), module = module)
          
          if (nrow(unique_cells) > 0) {
            # Build cell polygons directly from unique cell origins
            cell_polys <- lapply(seq_len(nrow(unique_cells)), function(i) {
              x0 <- unique_cells$x[i]
              y0 <- unique_cells$y[i]
              sf::st_polygon(list(matrix(c(
                x0, y0,
                x0 + grid_size, y0,
                x0 + grid_size, y0 + grid_size,
                x0, y0 + grid_size,
                x0, y0
              ), ncol = 2, byrow = TRUE)))
            })
            
            aoo_sfc <- sf::st_sfc(cell_polys, crs = sf::st_crs(sp_sf))
            u <- sf::st_union(aoo_sfc)
            if (!all(sf::st_is_valid(u))) u <- sf::st_make_valid(u)
            aoo_poly <- u
          }
        }, error = function(e) {
          log_warn("  AOO calculation failed: %s", conditionMessage(e), module = module)
        })
        
        if (!is.null(aoo_poly)) {
          aoo_sf <- sf::st_sf(geometry = sf::st_sfc(aoo_poly), crs = sf::st_crs(sp_sf))
          aoo_sf$Name <- paste(sp, "AOO")
          aoo_sf$population_type <- population_types
          
          aoo_geo <- aoo_sf
          aoo_geo$fill <- "#FFFF00"
          aoo_geo$`fill-opacity` <- 0.80
          aoo_geo$stroke <- "#FFFF00"
          aoo_geo$`stroke-width` <- 2
          
          aoo_file <- file.path(aoo_dir, paste0(sp_clean, "_AOO.geojson"))
          try(sf::st_write(aoo_geo, aoo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
          maps_generated$aoo_geojson <- aoo_file
          
          if ("kml" %in% formats) {
            aoo_kml <- file.path(aoo_dir, paste0(sp_clean, "_AOO.kml"))
            .write_styled_kml(aoo_sf, aoo_kml, "AOO", "#FFFF00", 0.80)
            maps_generated$aoo_kml <- aoo_kml
          }
        }
        
        # --- 3. HydroBASINS (with population-based styling) ---
        # For Scenario 3, pass both branch data objects so basin status can be
        # determined from the data origin rather than expensive spatial joins
        basins_map <- .generate_hydrobasins_map(
          sp, sp_data, sp_sf, scenario, cache_dir, bbox_expand_km, module
        )
        
        if (!is.null(basins_map)) {
          # GeoJSON
          if ("geojson" %in% formats) {
            basins_geo_file <- file.path(basins_dir, paste0(sp_clean, "_basins.geojson"))
            try(sf::st_write(basins_map$styled, basins_geo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
            maps_generated$basins_geojson <- basins_geo_file
          }
          
          # KML
          if ("kml" %in% formats) {
            basins_kml_file <- file.path(basins_dir, paste0(sp_clean, "_basins.kml"))
            .write_basins_kml(basins_map$raw, basins_kml_file)
            maps_generated$basins_kml <- basins_kml_file
          }
        }
        
        maps_generated
        
      }, error = function(e) {
        log_error("  FAILED to generate maps for %s: %s", sp, conditionMessage(e), module = module)
        list()  # Return empty list, don't stop the pipeline
      })
      # ═══ END BUG #6 FIX ═══
      
      if (!is.null(sp_maps)) {
        generated_maps[[sp]] <- sp_maps
        log_info("  Generated %d map files for %s", length(sp_maps), sp, module = module)
      }
    }
    
    # Summary
    total_files <- sum(sapply(generated_maps, length))
    log_info("Map generation complete: %d species, %d total files", 
             length(generated_maps), total_files, module = module)
    
    return(list(
      maps = generated_maps,
      summary = list(
        species_count = length(generated_maps),
        total_files = total_files
      )
    ))
  })
}


# --- HELPER: GENERATE HYDROBASINS MAP (FULLY FIXED) ---
.generate_hydrobasins_map <- function(sp, sp_data, sp_sf, scenario, cache_dir,
                                      bbox_expand_km, module) {
  
  log_info("  [BASINS] Cache directory: %s (exists: %s)", 
           cache_dir, dir.exists(cache_dir), module = module)
  
  # ── Step 1: Collect basin codes PER BRANCH for Scenario 3 ──
  # The combined sp_data already has population_type from the bind_rows step,
  # so we can derive per-branch basin codes without needing extra parameters.
  
  ind_codes <- character(0)
  non_codes <- character(0)
  
  if (scenario == 3 && "population_type" %in% names(sp_data)) {
    ind_rows <- sp_data[!is.na(sp_data$population_type) & sp_data$population_type == "indigenous", ]
    non_rows <- sp_data[!is.na(sp_data$population_type) & sp_data$population_type == "non-indigenous", ]
    
    ind_basins_raw <- unique(ind_rows$hydrobasin)
    ind_basins_raw <- ind_basins_raw[!is.na(ind_basins_raw) & nzchar(ind_basins_raw)]
    if (length(ind_basins_raw) > 0) {
      ind_codes <- unique(unlist(strsplit(ind_basins_raw, " \\| ")))
    }
    
    non_basins_raw <- unique(non_rows$hydrobasin)
    non_basins_raw <- non_basins_raw[!is.na(non_basins_raw) & nzchar(non_basins_raw)]
    if (length(non_basins_raw) > 0) {
      non_codes <- unique(unlist(strsplit(non_basins_raw, " \\| ")))
    }
    
    log_info("  [BASINS] Scenario 3 branch codes: %d indigenous, %d non-indigenous", 
             length(ind_codes), length(non_codes), module = module)
  }
  
  # ── Step 2: Parse ALL basin codes from combined data ──
  basins_raw <- unique(sp_data$hydrobasin)
  basins_raw <- basins_raw[!is.na(basins_raw) & nzchar(basins_raw)]
  
  if (length(basins_raw) == 0) {
    log_warn("  No HydroBASINS data for %s", sp, module = module)
    return(NULL)
  }
  
  all_basin_codes <- unique(unlist(strsplit(basins_raw, " \\| ")))
  
  log_info("  [BASINS] %d unique basin codes total", length(all_basin_codes), module = module)
  log_info("  [BASINS] Sample: %s", paste(head(all_basin_codes, 5), collapse = ", "), module = module)
  
  # Extract level and IDs
  basin_info <- data.frame(
    code = all_basin_codes,
    level = as.integer(sub("^L(\\d+):.*", "\\1", all_basin_codes)),
    id = sub("^L\\d+:", "", all_basin_codes),
    stringsAsFactors = FALSE
  )
  
  levels_needed <- unique(basin_info$level)
  log_info("  [BASINS] Levels needed: %s", paste(levels_needed, collapse = ", "), module = module)
  
  # ── Step 3: Load basin geometries from cache ──
  basin_geometries <- list()
  
  for (lvl in levels_needed) {
    cache_file <- file.path(cache_dir, sprintf("hydro_lev%02d_merged.rds", lvl))
    
    if (!file.exists(cache_file)) {
      log_warn("  HydroBASINS L%d cache not found: %s", lvl, cache_file, module = module)
      next
    }
    
    log_info("  [BASINS] Loading L%d from cache...", lvl, module = module)
    lyr <- readRDS(cache_file)
    log_info("  [BASINS] Loaded %d features for L%d", nrow(lyr), lvl, module = module)
    
    ids_needed <- basin_info$id[basin_info$level == lvl]
    log_info("  [BASINS] Need %d IDs for L%d", length(ids_needed), lvl, module = module)
    
    # ── Flexible column matching ──
    id_col <- NULL
    if ("HB_LABEL" %in% names(lyr)) {
      id_col <- "HB_LABEL"
    } else if ("HYBAS_ID" %in% names(lyr)) {
      id_col <- "HYBAS_ID"
      lyr$HB_LABEL <- as.character(lyr$HYBAS_ID)
    } else if ("PFAF_ID" %in% names(lyr)) {
      id_col <- "PFAF_ID"
      lyr$HB_LABEL <- as.character(lyr$PFAF_ID)
    } else {
      log_warn("  [BASINS] No recognized ID column in L%d. Columns: %s", 
               lvl, paste(names(lyr), collapse = ", "), module = module)
      next
    }
    
    basins_subset <- lyr[lyr$HB_LABEL %in% ids_needed, ]
    log_info("  [BASINS] Matched %d basins for L%d", nrow(basins_subset), lvl, module = module)
    
    if (nrow(basins_subset) > 0) {
      # Normalize to just HB_LABEL + geometry before combining
      basins_subset <- basins_subset[, c("HB_LABEL", "geometry"), drop = FALSE]
      basin_geometries[[as.character(lvl)]] <- basins_subset
    }
  }
  
  if (length(basin_geometries) == 0) {
    log_warn("  No HydroBASINS geometries found for %s", sp, module = module)
    return(NULL)
  }
  
  # ═══ BUG #3 FIX: bind_rows instead of do.call(rbind) ═══
  all_basins <- dplyr::bind_rows(basin_geometries)
  # Ensure it's still an sf object after bind_rows
  if (!inherits(all_basins, "sf")) {
    all_basins <- sf::st_as_sf(all_basins)
  }
  
  log_info("  [BASINS] Combined %d total basin features", nrow(all_basins), module = module)
  
  # ── Step 4: Assign population status to each basin ──
  # ═══ BUG #2 + #4 FIX: Data-driven status (fast) instead of per-basin spatial loop ═══
  
  if (scenario == 3) {
    # PRIMARY METHOD: Determine status from branch-level basin code tracking
    # This is O(1) per basin — no spatial operations needed
    
    all_basins$status <- NA_character_
    
    for (i in seq_len(nrow(all_basins))) {
      hb_id <- all_basins$HB_LABEL[i]
      
      # Check which branches contain this basin ID (match against full "L8:12345" codes)
      in_indigenous <- FALSE
      in_non_indigenous <- FALSE
      
      if (length(ind_codes) > 0) {
        in_indigenous <- any(grepl(paste0(":", hb_id, "$"), ind_codes))
      }
      if (length(non_codes) > 0) {
        in_non_indigenous <- any(grepl(paste0(":", hb_id, "$"), non_codes))
      }
      
      if (in_indigenous && in_non_indigenous) {
        all_basins$status[i] <- "Native"  # Native-priority: if both, classify as Native
      } else if (in_indigenous) {
        all_basins$status[i] <- "Native"
      } else if (in_non_indigenous) {
        all_basins$status[i] <- "Introduced"
      }
    }
    
    # FALLBACK: If any basins still have NA status (shouldn't happen but safety net),
    # use a single vectorized spatial join instead of the per-basin loop
    na_count <- sum(is.na(all_basins$status))
    if (na_count > 0) {
      log_warn("  [BASINS] %d basins with NA status after code matching — running spatial fallback", 
               na_count, module = module)
      
      na_idx <- which(is.na(all_basins$status))
      na_basins <- all_basins[na_idx, ]
      
      # Single vectorized spatial operation
      tryCatch({
        ix <- suppressWarnings(sf::st_intersects(na_basins, sp_sf))
        
        for (j in seq_along(na_idx)) {
          pts_idx <- ix[[j]]
          if (length(pts_idx) > 0) {
            pop_types <- unique(sp_sf$population_type[pts_idx])
            if ("indigenous" %in% pop_types) {
              all_basins$status[na_idx[j]] <- "Native"
            } else if ("non-indigenous" %in% pop_types) {
              all_basins$status[na_idx[j]] <- "Introduced"
            } else {
              all_basins$status[na_idx[j]] <- "Native"  # Default fallback
            }
          } else {
            all_basins$status[na_idx[j]] <- "Native"  # No points found, default
          }
        }
      }, error = function(e) {
        log_warn("  [BASINS] Spatial fallback failed: %s. Defaulting NA basins to Native.", 
                 conditionMessage(e), module = module)
        all_basins$status[na_idx] <<- "Native"
      })
    }
    
    # Log status breakdown
    status_tbl <- table(all_basins$status, useNA = "ifany")
    log_info("  [BASINS] Status breakdown: %s", 
             paste(names(status_tbl), status_tbl, sep = "=", collapse = ", "), module = module)
    
  } else if (scenario == 1) {
    all_basins$status <- "Native"
  } else {
    all_basins$status <- "Introduced"
  }
  
  # ═══ BUG #4 FIX: Catch any remaining NA status before styling ═══
  remaining_na <- sum(is.na(all_basins$status))
  if (remaining_na > 0) {
    log_warn("  [BASINS] %d basins still have NA status — defaulting to Native", remaining_na, module = module)
    all_basins$status[is.na(all_basins$status)] <- "Native"
  }
  
  # ── Step 5: Apply styling ──
  styled_basins <- all_basins
  styled_basins$fill <- dplyr::case_when(
    all_basins$status == "Native"     ~ "#D48D00",
    all_basins$status == "Introduced" ~ "#4D0073",
    TRUE                              ~ "#D48D00"  # Fallback: orange
  )
  styled_basins$`fill-opacity` <- 0.35
  styled_basins$stroke <- styled_basins$fill
  styled_basins$`stroke-width` <- 1.5
  
  log_info("  [BASINS] Successfully styled %d basins (%d Native, %d Introduced)", 
           nrow(styled_basins),
           sum(all_basins$status == "Native", na.rm = TRUE),
           sum(all_basins$status == "Introduced", na.rm = TRUE),
           module = module)
  
  return(list(
    raw = all_basins,
    styled = styled_basins
  ))
}


# --- HELPER: WRITE STYLED KML (EOO/AOO) ---
.write_styled_kml <- function(sf_obj, file_path, layer_name, color, opacity) {
  tmp <- tempfile(fileext = ".kml")
  sf_obj$kml_id <- seq_len(nrow(sf_obj))
  sf::st_write(sf_obj, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
  kml_txt <- paste(readLines(tmp), collapse = "\n")
  
  # Convert color to KML format (AABBGGRR)
  hex_color <- sub("^#", "", color)
  r <- substr(hex_color, 1, 2)
  g <- substr(hex_color, 3, 4)
  b <- substr(hex_color, 5, 6)
  opacity_hex <- sprintf("%02x", as.integer(opacity * 255))
  kml_color <- paste0(opacity_hex, b, g, r)
  
  style_def <- sprintf('
  <Style id="%sStyle">
    <LineStyle>
      <color>ff%s%s%s</color>
      <width>2</width>
    </LineStyle>
    <PolyStyle>
      <color>%s</color>
      <fill>1</fill>
      <outline>1</outline>
    </PolyStyle>
  </Style>', layer_name, b, g, r, kml_color)
  
  kml_txt <- sub("<Document>", paste0("<Document>", style_def), kml_txt)
  kml_txt <- gsub("(<Placemark[^>]*>)", paste0("\\1<styleUrl>#", layer_name, "Style</styleUrl>"), kml_txt)
  
  writeLines(kml_txt, file_path)
  unlink(tmp)
}


# --- HELPER: WRITE BASINS KML (with population-based styling) ---
.write_basins_kml <- function(basins_sf, file_path) {
  tmp <- tempfile(fileext = ".kml")
  sf::st_write(basins_sf, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
  kml_txt <- paste(readLines(tmp), collapse = "\n")
  
  # Define styles for each status
  # Native: #D48D00 (orange)
  # Introduced: #4D0073 (purple)
  # Mixed: #FF6600 (red-orange)
  
  style_defs <- '
  <Style id="nativeStyle">
    <LineStyle><color>ff008dd4</color><width>1.5</width></LineStyle>
    <PolyStyle><color>59008dd4</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>
  <Style id="introducedStyle">
    <LineStyle><color>ff73004d</color><width>1.5</width></LineStyle>
    <PolyStyle><color>5973004d</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>
  <Style id="mixedStyle">
    <LineStyle><color>ff0066ff</color><width>1.5</width></LineStyle>
    <PolyStyle><color>800066ff</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>'
  
  kml_txt <- sub("<Document>", paste0("<Document>", style_defs), kml_txt)
  
  # Apply styles based on status
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Native</n>)",
    "\\1<styleUrl>#nativeStyle</styleUrl>\\2",
    kml_txt
  )
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Introduced</n>)",
    "\\1<styleUrl>#introducedStyle</styleUrl>\\2",
    kml_txt
  )
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Mixed</n>)",
    "\\1<styleUrl>#mixedStyle</styleUrl>\\2",
    kml_txt
  )
  
  writeLines(kml_txt, file_path)
  unlink(tmp)
}

# Aliases so both call styles work
generate_all_maps_seq <- generate_all_maps
#generate_all_maps_par <- generate_all_maps