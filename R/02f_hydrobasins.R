#### MODULE 2F: ENRICH_WITH_HYDROBASINS_MERGED() — WITH BATCHING & BRANCH DETECTION ####

enrich_with_hydrobasins_merged <- function(result,
                                           hydro_dir           = "spatial_data/hydrobasins",
                                           global_files        = list(
                                             "6" = "hybas_lev10.shp",
                                             "8" = "hybas_lev10.shp",
                                             "10" = "hybas_lev10.shp"
                                           ),
                                           output_dir          = "checkover_output",
                                           cache_dir           = file.path(output_dir, "cache"),
                                           crop_to_points_bbox = TRUE,
                                           bbox_expand_km      = 50,
                                           nearest_fallback    = TRUE,
                                           NEAREST_MAX_KM      = 10,
                                           reuse_cached        = TRUE,
                                           parallel            = FALSE,
                                           batch_size          = 10) {  # ✅ RESTORED
  module <- "MODULE2F_HYDROBASINS"
  
  with_log_section(module, {
    log_info("=== MODULE 2F: HYDROBASINS ENRICHMENT (MEMORY-OPTIMIZED) ===", module = module)
    log_info("Parallel processing: %s", as.character(parallel), module = module)
    log_info("Batch size: %d", batch_size, module = module)
    
    # --- DETECT BRANCH TYPE ---
    branch_id <- "combined"
    
    if ("population_type" %in% names(result$clean_data)) {
      pop_types <- unique(result$clean_data$population_type)
      pop_types <- pop_types[!is.na(pop_types)]
      
      if (length(pop_types) == 1) {
        if (pop_types[1] == "indigenous") {
          branch_id <- "indigenous"
          log_info("Detected branch: INDIGENOUS", module = module)
        } else if (pop_types[1] == "non-indigenous") {
          branch_id <- "non_indigenous"
          log_info("Detected branch: NON-INDIGENOUS", module = module)
        }
      } else if (length(pop_types) > 1) {
        branch_id <- "combined"
        log_info("Detected branch: COMBINED (both population types)", module = module)
      }
    } else {
      log_info("Detected branch: COMBINED (no population_type column)", module = module)
    }
    
    # --- BRANCH-SPECIFIC OUTPUT FILES ---
    out_tsv <- file.path(output_dir, paste0("clean_occurrences_with_hydrobasin_", branch_id, ".tsv"))
    out_rds <- file.path(output_dir, paste0("clean_occurrences_sf_with_hydrobasin_", branch_id, ".rds"))
    
    log_info("Branch-specific cache files:", module = module)
    log_info("  TSV: %s", basename(out_tsv), module = module)
    log_info("  RDS: %s", basename(out_rds), module = module)
    
    # --- PRECONDITIONS ---
    if (!all(c("clean_data", "clean_sf") %in% names(result))) stop("Missing data")
    if (!dir.exists(hydro_dir)) stop("Hydro dir not found")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive=TRUE)
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive=TRUE)
    
    # --- CHECK FOR EXISTING ENRICHED DATA (RESUME) ---
    if (reuse_cached && file.exists(out_tsv) && file.exists(out_rds)) {
      log_info("Found existing HydroBASINS enriched data for branch '%s'. Loading from cache...", 
               branch_id, module = module)
      
      tryCatch({
        cached_df <- read_tsv(out_tsv, stringsAsFactors = FALSE)
        cached_sf <- readRDS(out_rds)
        # Note: this is the OUTPUT cache (already-enriched points), not a
        # reference layer. Sanitize would still be safe but is unnecessary.
        # Leaving as a comment — no change here.
        
        if (nrow(cached_df) == nrow(result$clean_data) &&
            all(sort(unique(cached_df$species)) == sort(unique(result$clean_data$species)))) {
          
          if ("hydrobasin" %in% names(cached_df) && "hydrobasin" %in% names(cached_sf)) {
            result$clean_data <- cached_df
            result$clean_sf <- cached_sf
            result$files_created <- unique(c(result$files_created, out_tsv, out_rds))
            
            log_info("Successfully loaded cached HydroBASINS enrichment for branch '%s'.", 
                     branch_id, module = module)
            assigned_count <- sum(!is.na(result$clean_data$hydrobasin))
            log_info("Cache validation: %d/%d records have hydrobasin assignments.", 
                     assigned_count, nrow(result$clean_data), module = module)
            return(result)
          }
        } else {
          log_warn("Cache validation failed: record/species count mismatch. Re-running enrichment.", 
                   module = module)
        }
      }, error = function(e) {
        log_warn("Failed to load cache: %s. Re-running enrichment.", 
                 conditionMessage(e), module = module)
      })
    }
    
    # --- HELPER FUNCTIONS ---
    .std_geom <- function(x) {
      g <- attr(x, "sf_column")
      if(!identical(g,"geometry")) {
        names(x)[names(x)==g] <- "geometry"
        attr(x,"sf_column") <- "geometry"
      }
      x
    }
    
    .expand_bbox_km <- function(bbox_sfc, target_crs, km) {
      if (km <= 0) return(bbox_sfc)
      ea <- 6933
      bb_ea <- try(sf::st_transform(bbox_sfc, ea), silent = TRUE)
      if (inherits(bb_ea, "try-error")) return(bbox_sfc)
      bb_poly <- sf::st_as_sfc(sf::st_bbox(bb_ea))
      buf <- sf::st_buffer(bb_poly, dist = km * 1000)
      tryCatch(sf::st_transform(buf, target_crs), error = function(e) bbox_sfc)
    }
    
    # --- LOAD GLOBAL LAYERS (CACHED) ---
    .process_global_level <- function(lvl, fname, target_crs) {
      cpath <- file.path(cache_dir, sprintf("hydro_lev%02d_merged.rds", as.integer(lvl)))
      
      expected_counts <- list("6" = 15000, "8" = 150000, "10" = 800000)
      min_threshold <- expected_counts[[as.character(lvl)]]
      if (is.null(min_threshold)) min_threshold <- 10000
      
      if (reuse_cached && file.exists(cpath)) {
        log_info("  Loading cached L%d...", lvl, module=module)
        cached <- readRDS(cpath)
        cached <- sanitize_spatial_layer(cached,
                                         layer_name = sprintf("HydroBASINS_L%s_cached", lvl))
        
        if (nrow(cached) < (min_threshold * 0.5)) {
          log_error("  CORRUPTED CACHE: L%d has only %d features (expected >%d)", 
                    lvl, nrow(cached), min_threshold, module=module)
          log_error("  Deleting corrupted cache and regenerating...", module=module)
          unlink(cpath)
        } else {
          log_info("  Cache validated: %d features", nrow(cached), module=module)
          return(cached)
        }
      }
      
      log_info("  Reading global L%d shapefile: %s", lvl, fname, module=module)
      shp <- file.path(hydro_dir, fname)
      if(!file.exists(shp)) stop(paste("Missing:", shp))
      
      lyr <- sf::st_read(shp, quiet=TRUE, stringsAsFactors=FALSE)
      lyr <- sanitize_spatial_layer(lyr,
                                    target_crs = target_crs,
                                    layer_name = sprintf("HydroBASINS_L%s_source", lvl))
      log_info("  Loaded %d features from source", nrow(lyr), module=module)
      
      lyr <- .std_geom(lyr)
      
      if ("HYBAS_ID" %in% names(lyr)) {
        lyr$HB_LABEL <- as.character(lyr$HYBAS_ID)
        log_debug("  Using HYBAS_ID column", module=module)
      } else if ("PFAF_ID" %in% names(lyr)) {
        lyr$HB_LABEL <- as.character(lyr$PFAF_ID)
        log_debug("  Using PFAF_ID column", module=module)
      } else {
        log_error("  No HYBAS_ID or PFAF_ID column found!", module=module)
        stop("HydroBASINS file missing required ID column")
      }
      
      lyr <- lyr[, c("HB_LABEL", "geometry"), drop=FALSE]
      
      dup_count <- sum(duplicated(lyr$HB_LABEL))
      if (dup_count > 0) {
        log_warn("  Found %d duplicate HB_LABELs - removing duplicates", dup_count, module=module)
        lyr <- lyr[!duplicated(lyr$HB_LABEL), ]
      }
      
      log_info("  Features after column selection: %d", nrow(lyr), module=module)
      
      final_count <- nrow(lyr)
      log_info("  Loaded %d features for L%d (validation complete)", final_count, lvl, module=module)
      
      log_info("  Saving cache: %s", basename(cpath), module="CACHE")
      saveRDS(lyr, cpath)      
      return(lyr)
    }
    
    cd <- result$clean_data
    target_crs <- sf::st_crs(result$clean_sf)
    
    # Load all levels once
    log_info("Loading HydroBASINS layers...", module = module)
    lyr_L10 <- .process_global_level(10, global_files[["10"]], target_crs); gc()
    lyr_L08 <- .process_global_level(8, global_files[["8"]], target_crs); gc()
    lyr_L06 <- .process_global_level(6, global_files[["6"]], target_crs); gc()
    
    # --- LEVEL MAPPING ---
    level_for_cat <- c("micro-endemic"=10, "endemic"=10, "regional"=8, "cosmopolitan"=6, "local"=10, "widespread"=6)
    default_level <- 8
    
    # Initialize output column
    result$clean_data$hydrobasin <- NA_character_
    result$clean_sf$hydrobasin <- NA_character_
    
    # Get species list
    species_list <- unique(cd$species)
    species_list <- species_list[!is.na(species_list) & nzchar(species_list)]
    
    log_info("Processing %d species in batches...", length(species_list), module = module)
    
    # --- PER-SPECIES PROCESSING FUNCTION ---
    process_species_hydrobasin <- function(sp) {
      library(sf)
      library(dplyr)
      
      sp_idx <- which(cd$species == sp)
      if (length(sp_idx) == 0) return(NULL)
      
      sp_pts <- result$clean_sf[sp_idx, , drop = FALSE]
      
      cat_val <- if ("iucn_category" %in% names(cd)) {
        cd$iucn_category[sp_idx][1]
      } else if ("category" %in% names(cd)) {
        cd$category[sp_idx][1]
      } else if ("distribution_category" %in% names(cd)) {
        cd$distribution_category[sp_idx][1]
      } else {
        NA_character_
      }
      
      if (is.null(cat_val) || length(cat_val) == 0 || is.na(cat_val)) {
        cat_val <- "regional"
      }
      
      lvl <- if(cat_val %in% names(level_for_cat)) level_for_cat[[cat_val]] else default_level
      
      lyr <- switch(as.character(lvl), "10"=lyr_L10, "8"=lyr_L08, "6"=lyr_L06, lyr_L08)
      
      # ─── BBOX-CROP DECISION ───────────────────────────────────────
      # Skip cropping when:
      #   (a) layer is small enough to use whole (L6 = 16k features)
      #   (b) species bbox spans more than 90° in either dimension
      #       (multi-continental → crop produces invalid geometry)
      bb <- sf::st_bbox(sp_pts)
      bb_width  <- as.numeric(bb["xmax"] - bb["xmin"])
      bb_height <- as.numeric(bb["ymax"] - bb["ymin"])
      
      skip_crop <- (nrow(lyr) < 50000) || (bb_width > 90) || (bb_height > 90)
      
      if (skip_crop) {
        lyr_small <- lyr
        if (bb_width > 90 || bb_height > 90) {
          log_info("  [%s] Multi-continental species (bbox %.0f° × %.0f°) — using full global L%d layer (%d features)",
                   sp, bb_width, bb_height, lvl, nrow(lyr), module = module)
        }
      } else {
        bb_poly <- sf::st_as_sfc(bb)
        ea <- 6933
        bb_ea <- tryCatch(sf::st_transform(bb_poly, ea), error = function(e) NULL)
        
        if (!is.null(bb_ea)) {
          buf_ea <- sf::st_buffer(bb_ea, dist = bbox_expand_km * 1000)
          buf_orig <- tryCatch(sf::st_transform(buf_ea, target_crs), error = function(e) NULL)
          
          if (!is.null(buf_orig) && all(sf::st_is_valid(buf_orig))) {
            lyr_small <- try(suppressWarnings(sf::st_crop(lyr, buf_orig)), silent = TRUE)
          } else {
            lyr_small <- lyr  # Invalid buffer geometry → fall back to full layer
          }
        } else {
          lyr_small <- lyr
        }
        
        # Safety net: if crop produced empty or errored, fall back to full layer
        if (inherits(lyr_small, "try-error") || nrow(lyr_small) == 0) {
          log_warn("  [%s] Bbox crop failed/empty — falling back to full L%d layer", 
                   sp, lvl, module = module)
          lyr_small <- lyr
        }
      }
      
      # Diagnostic: log layer size used for this species
      log_debug("  [%s] L%d: using %d basin features (full=%d)", 
                sp, lvl, nrow(lyr_small), nrow(lyr), module = module)
      
      
      basins_matched <- try(sf::st_filter(lyr_small, sp_pts), silent=TRUE)
      
      if (inherits(basins_matched, "try-error") || nrow(basins_matched) == 0) {
        labels <- rep(NA_character_, nrow(sp_pts))
      } else {
        ix <- try(sf::st_intersects(sp_pts, basins_matched), silent=TRUE)
        
        if (inherits(ix, "try-error")) {
          labels <- rep(NA_character_, nrow(sp_pts))
        } else {
          labels <- character(nrow(sp_pts))
          for(i in seq_along(sp_idx)) {
            match_idx <- ix[[i]]
            if(length(match_idx) > 0) {
              basin_ids <- basins_matched$HB_LABEL[match_idx]
              basin_ids_with_level <- paste0("L", lvl, ":", basin_ids)
              labels[i] <- paste(unique(basin_ids_with_level), collapse=" | ")
            } else {
              labels[i] <- NA_character_
            }
          }
        }
      }
      
      if (nearest_fallback && any(is.na(labels)) && 
          !inherits(basins_matched, "try-error") && nrow(basins_matched) > 0) {
        miss <- which(is.na(labels))
        nn <- sf::st_nearest_feature(sp_pts[miss,], basins_matched)
        
        dists <- sf::st_distance(sp_pts[miss,], basins_matched[nn,], by_element=TRUE)
        valid_nn <- as.numeric(dists) <= (NEAREST_MAX_KM * 1000)
        
        if (any(valid_nn)) {
          nearest_ids <- basins_matched$HB_LABEL[nn[valid_nn]]
          nearest_ids_with_level <- paste0("L", lvl, ":", nearest_ids)
          labels[miss[valid_nn]] <- nearest_ids_with_level
        }
      }
      
      n_assigned <- sum(!is.na(labels))
      n_total <- length(labels)
      if (n_assigned < n_total * 0.8) {
        log_warn("  [%s] LOW ASSIGNMENT: %d/%d points (%.0f%%)",
                 sp, n_assigned, n_total, 100 * n_assigned / n_total,
                 module = module)
      }

      return(list(
        species = sp,
        indices = sp_idx,
        labels = labels
      ))
    }
    
    # CRITICAL: Process in batches with progress bar
    pb <- create_progress_bar(length(species_list))

    results_list <- process_in_batches(
      items = species_list,
      batch_size = batch_size,
      process_fn = function(sp) {
        pb$tick()
        process_species_hydrobasin(sp)
      }
    )

    pb$terminate()

    # Assign results
    log_info("Assigning results back...", module = module)
    for (res in results_list) {
      if (!is.null(res) && !is.null(res$indices)) {
        result$clean_sf$hydrobasin[res$indices] <- res$labels
        result$clean_data$hydrobasin[res$indices] <- res$labels
      }
    }
    
    
    # --- SAVE OUTPUTS (BRANCH-SPECIFIC) ---
    write_tsv(result$clean_data, out_tsv, row.names=FALSE)
    saveRDS(result$clean_sf, out_rds)
    
    result$files_created <- c(result$files_created, out_tsv, out_rds)
    
    assigned_count <- sum(!is.na(result$clean_data$hydrobasin))
    log_info("HydroBASINS complete: %d/%d records assigned", 
             assigned_count, nrow(result$clean_data), module=module)
    
    return(result)
  })
}