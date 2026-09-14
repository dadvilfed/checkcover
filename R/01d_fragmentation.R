#### MODULE 1D: SPATIAL CLUSTERING ANALYSIS ####
#
#' @param threshold_km Absolute separation threshold in kilometres. Two points
#'   fall in the same cluster when a chain of points links them with no gap
#'   wider than this. PROVISIONAL DEFAULT — pending an ecologically justified
#'   value from Lucian; it is a configuration choice, not a property of the
#'   workflow, and is recorded in every output so a package always states the
#'   threshold it was computed under.
#' @param linkage Agglomeration method. "single" answers "are there gaps wider
#'   than the threshold?"; see the note at the cutree() call before changing it.
analyze_fragmentation <- function(result, output_dir, category_filter = NULL,
                                  threshold_km = NULL,
                                  method = NULL,
                                  linkage = NULL) {
  module <- "MODULE1D_FRAG"

  # Config is the source of truth; arguments override it for tests.
  cfg <- if (exists("CONFIG", envir = globalenv())) get("CONFIG", envir = globalenv()) else NULL
  threshold_km <- threshold_km %||% cfg$clustering$threshold_km %||% 10
  clustering_linkage <- linkage %||% cfg$clustering$linkage %||% "single"
  clustering_method  <- method  %||% cfg$clustering$method  %||% "basin"
  clustering_method  <- match.arg(clustering_method, c("basin", "euclidean"))
  threshold_m  <- threshold_km * 1000

  with_log_section(module, {
    log_info("=== MODULE 1D: SPATIAL CLUSTERING ANALYSIS ===", module = module)
    log_info("Clustering: method=%s, threshold=%.2f km, linkage=%s",
             clustering_method, threshold_km, clustering_linkage, module = module)
    
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
          threshold_km = numeric(),
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
        mean_distance_km = NA_real_,
        threshold_km = threshold_km,
        # Recorded per species, because whether basin connectivity was actually
        # applied depends on the species having hydrobasin assignments.
        method = clustering_method,
        basin_connectivity = NA,
        n_basin_units = NA_integer_
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

          # Distance matrix (metres, equal-area CRS)
          dist_mat <- matrix(as.numeric(sf::st_distance(pts_ea)),
                             nrow = nrow(pts_ea))
          dist_vals <- dist_mat[lower.tri(dist_mat)]
          d_mean <- mean(dist_vals, na.rm = TRUE)

          # ── Basin-aware connectivity (Lucian, 2026-09) ────────────────────
          # Euclidean distance is the wrong metric for freshwater crayfish. Two
          # populations 5 km apart in separate catchments are more disconnected
          # than two 50 km apart along one river, and no purely spatial
          # threshold can express that.
          #
          # Records sharing a HydroBASINS unit are therefore connected
          # regardless of distance; records in different units are separate
          # unless the distance rule below links them. Setting the pairwise
          # distance to zero for same-basin pairs and cutting a SINGLE-linkage
          # tree at the threshold expresses exactly that: single linkage merges
          # on any pair within the cut height, so the result is the connected
          # components of "same basin OR within threshold_km".
          #
          # The basin level is whatever Module 2F already assigned for this
          # species' category (L10 endemic, L08 regional, L06 cosmopolitan), so
          # resolution matches range extent without a second rule here.
          basin_used <- FALSE
          n_basin_units <- NA_integer_
          if (identical(clustering_method, "basin") &&
              "hydrobasin" %in% names(valid_pts)) {
            hb <- as.character(valid_pts$hydrobasin)
            sets <- strsplit(ifelse(is.na(hb), "", hb), "\\s*\\|\\s*")
            sets <- lapply(sets, function(s) s[nzchar(s)])
            units <- unique(unlist(sets))
            if (length(units) > 0) {
              for (u in units) {
                ix <- which(vapply(sets, function(s) u %in% s, logical(1)))
                if (length(ix) > 1L) dist_mat[ix, ix] <- 0
              }
              basin_used <- TRUE
              n_basin_units <- length(units)
            }
          }
          if (identical(clustering_method, "basin") && !basin_used) {
            # Never silently fall back to a different metric: a package must not
            # claim basin-aware clustering it did not perform.
            log_warn("  %s: no hydrobasin assignments — clustering by distance only.",
                     sp, module = module)
          }

          # ── Spatial clustering ─────────────────────────────────────────────
          # The cut height MUST be an absolute distance, never a statistic of
          # the points themselves.
          #
          # Until 2026-09 this cut at h = mean(pairwise distance). With complete
          # linkage the root merge height IS the maximum pairwise distance, and
          # the mean is below the maximum whenever distances vary at all — so
          # the root merge was always severed and a single cluster was
          # UNREACHABLE BY CONSTRUCTION. Over 1,000 simulated draws of five
          # points within one metre, the old rule returned 1 cluster zero times.
          # Worse, being scale-free it tracked sample size rather than spatial
          # structure (n=5 -> ~2 clusters, n=200 -> ~11) and was anti-correlated
          # with real fragmentation: one tight blob scored 8 clusters while two
          # genuinely separated blobs scored 2. Reported by Reviewer 1,
          # Ecological Informatics, 2026-09.
          #
          # Single linkage, not complete: the question this metric answers is
          # "are there gaps wider than the threshold?", which is a connectivity
          # property. Complete linkage constrains cluster DIAMETER instead, so
          # it splits a long river system into many clusters purely because it
          # is long — conflating extent with fragmentation. Single linkage is
          # also what makes the same-basin-distance-zero trick above resolve to
          # connected components; complete linkage would not.
          clusters <- cutree(
            hclust(as.dist(dist_mat), method = clustering_linkage),
            h = threshold_m
          )

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
          res$threshold_km     <- threshold_km
          res$basin_connectivity <- basin_used
          res$n_basin_units      <- n_basin_units
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
    
    # ── Summary ───────────────────────────────────────────────────────────
    # The single-cluster count is reported explicitly and prominently. Under
    # the rule in force up to v1.2 it was ZERO by construction, for every
    # species, and that went unquestioned. If it is ever zero again on a real
    # cohort, treat it as a symptom rather than a result (Lucian, 2026-09).
    status_counts <- table(frag_df$status)
    computed  <- frag_df[isTRUE(frag_df$computed) | frag_df$computed %in% TRUE, , drop = FALSE]
    n_one     <- sum(computed$n_clusters == 1L, na.rm = TRUE)
    n_multi   <- sum(computed$n_clusters >  1L, na.rm = TRUE)
    n_basin   <- sum(computed$basin_connectivity %in% TRUE)

    log_info("Spatial clustering summary:", module = module)
    log_info("  Computed: %d species (%d with basin connectivity applied)",
             nrow(computed), n_basin, module = module)
    log_info("  Single cluster: %d species", n_one, module = module)
    log_info("  Multiple clusters: %d species", n_multi, module = module)
    log_info("  Not computed: %d species",
             sum(status_counts[!names(status_counts) %in% c("detected", "none_detected")]),
             module = module)

    cat("\n")
    cat("  Spatial clustering\n")
    cat("  ------------------\n")
    cat(sprintf("  method                    %s (threshold %.2f km, %s linkage)\n",
                clustering_method, threshold_km, clustering_linkage))
    cat(sprintf("  computed                  %d species\n", nrow(computed)))
    cat(sprintf("    basin connectivity used %d\n", n_basin))
    cat(sprintf("    single cluster          %d\n", n_one))
    cat(sprintf("    multiple clusters       %d\n", n_multi))
    if (nrow(computed) > 0 && n_one == 0) {
      cat("\n")
      cat("  [!] NO species resolved into a single cluster.\n")
      cat("      Up to v1.2 that outcome was guaranteed by the formula rather\n")
      cat("      than observed in the data. Verify before reporting it.\n")
    }
    cat("\n")

    return(result)
  })
}