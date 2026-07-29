#### MODULE 1D: FRAGMENTATION ANALYSIS ####
analyze_fragmentation <- function(result, output_dir, category_filter = NULL) {
  module <- "MODULE1D_FRAG"
  
  with_log_section(module, {
    log_info("=== MODULE 1D: FRAGMENTATION ANALYSIS ===", module = module)
    
    cd <- result$clean_data
    
    # Check for required columns
    if (!"iucn_category" %in% names(cd) && !"category" %in% names(cd)) {
      log_warn("No category column found. Skipping fragmentation.", module = module)
      return(result)
    }
    
    # Apply category filter if provided
    if (!is.null(category_filter)) {
      log_info("Filtering to categories: %s", paste(category_filter, collapse = ", "), module = module)
      
      cat_col <- if ("iucn_category" %in% names(cd)) "iucn_category" else "category"
      cd <- cd[cd[[cat_col]] %in% category_filter, ]
      
      if (nrow(cd) == 0) {
        log_info("No species match filter criteria. Skipping fragmentation.", module = module)
        result$fragmentation <- data.frame(
          species = character(),
          computed = logical(),
          scope = character(),
          status = character(),
          n_clusters = integer(),
          cluster_sizes_n = character(),
          mean_distance_km = numeric(),
          stringsAsFactors = FALSE
        )
        return(result)
      }
    }
    
    species_list <- unique(cd$species)
    log_info("Analyzing fragmentation for %d species...", length(species_list), module = module)
    
    frag_results <- list()
    ea_crs <- 6933  # Equal Area projection
    
    for (sp in species_list) {
      sp_dat <- cd[cd$species == sp, ]
      
      # Get category
      cat_val <- if ("iucn_category" %in% names(sp_dat)) {
        sp_dat$iucn_category[1]
      } else if ("category" %in% names(sp_dat)) {
        sp_dat$category[1]
      } else {
        "unknown"
      }
      
      n_recs <- nrow(sp_dat)
      
      # Default: Not computed
      res <- list(
        species = sp,
        computed = FALSE,
        scope = "not_applicable",
        status = "not_computed",
        n_clusters = NA_integer_,
        cluster_sizes_n = NA_character_,
        mean_distance_km = NA_real_
      )
      
      # Skip cosmopolitan
      if (cat_val == "cosmopolitan") {
        res$scope <- "not_applicable_for_cosmopolitan_ranges"
        frag_results[[sp]] <- res
        next
      }
      
      # Need at least 5 points
      if (n_recs < 5) {
        res$scope <- "insufficient_data_points"
        frag_results[[sp]] <- res
        next
      }
      
      # Run analysis
      tryCatch({
        valid_pts <- sp_dat[!is.na(sp_dat$longitude) & !is.na(sp_dat$latitude), ]
        
        if (nrow(valid_pts) < 5) {
          res$scope <- "insufficient_valid_coordinates"
        } else {
          pts_sf <- sf::st_as_sf(valid_pts, coords = c("longitude", "latitude"), crs = 4326)
          pts_ea <- sf::st_transform(pts_sf, ea_crs)
          
          # Distance matrix
          dist_mat <- sf::st_distance(pts_ea)
          dist_vals <- as.numeric(dist_mat[lower.tri(dist_mat)])
          d_mean <- mean(dist_vals, na.rm = TRUE)
          
          # Hierarchical clustering
          hc <- hclust(as.dist(dist_mat), method = "complete")
          clusters <- cutree(hc, h = d_mean)
          
          n_clust <- length(unique(clusters))
          
          # Cluster sizes
          counts <- sort(table(clusters), decreasing = TRUE)
          total <- sum(counts)
          size_strs <- vapply(counts, function(n) {
            pct <- round((n / total) * 100, 0)
            paste0(n, "(", pct, "%)")
          }, character(1))
          
          res$computed <- TRUE
          res$scope <- "endemic_or_regional"
          res$mean_distance_km <- round(d_mean / 1000, 2)
          res$n_clusters <- n_clust
          res$cluster_sizes_n <- paste(size_strs, collapse = ", ")
          res$status <- if (n_clust > 1) "detected" else "none_detected"
        }
      }, error = function(e) {
        log_warn("Fragmentation analysis failed for %s: %s", sp, conditionMessage(e), module = module)
        res$status <- "error_in_computation"
      })
      
      frag_results[[sp]] <- res
    }
    
    # Convert to dataframe
    frag_df <- do.call(rbind, lapply(frag_results, as.data.frame, stringsAsFactors = FALSE))
    
    # Save results
    out_tsv <- file.path(output_dir, "fragmentation_analysis_indigenous.tsv")
    write_tsv(frag_df, out_tsv)
    log_info("Saved fragmentation analysis to: %s", out_tsv, module = module)
    
    # Store in result
    result$fragmentation <- frag_df
    result$files_created <- c(result$files_created %||% character(), out_tsv)
    
    # Log summary
    status_counts <- table(frag_df$status)
    log_info("Fragmentation summary:", module = module)
    log_info("  Detected: %d species", status_counts["detected"] %||% 0, module = module)
    log_info("  None detected: %d species", status_counts["none_detected"] %||% 0, module = module)
    log_info("  Not computed: %d species", sum(status_counts[!names(status_counts) %in% c("detected", "none_detected")]), module = module)
    
    return(result)
  })
}