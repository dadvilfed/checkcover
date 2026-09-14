#### MODULE 1C: SPLIT DATA BY POPULATION STATUS ####

#' Split occurrence data into indigenous and non-indigenous branches
#' @param result List from ingest_clean() with clean_data and clean_sf
#' @param output_dir Output directory for split files
#' @return List with result_indigenous, result_non_indigenous, and result_combined (with population_type)
split_by_population <- function(result, output_dir = "checkover_output") {
  module <- "MODULE1C_SPLIT"
  
  with_log_section(module, {
    log_info("=== MODULE 1C: SPLIT BY POPULATION STATUS ===", module = module)
    
    # Preconditions
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      log_error("Expected result with clean_data and clean_sf.", module = module)
      stop("Expected result with clean_data and clean_sf.")
    }
    
    cd <- result$clean_data
    sf_pts <- result$clean_sf
    
    if (nrow(cd) == 0) {
      log_warn("No data to split. Returning empty results.", module = module)
      return(list(
        result_indigenous = list(clean_data = cd[0,], clean_sf = sf_pts[0,]),
        result_non_indigenous = list(clean_data = cd[0,], clean_sf = sf_pts[0,]),
        result_combined = result
      ))
    }
    
    log_info("Input: %d records, %d species", 
             nrow(cd), length(unique(cd$species)), module = module)
    
    # Detect population type
    log_info("Detecting population types...", module = module)
    cd$population_type <- detect_population_type(cd)
    sf_pts$population_type <- cd$population_type
    
    # Update the original result object with population_type
    result$clean_data <- cd
    result$clean_sf <- sf_pts
    
    # ── Records without a usable establishmentMeans ──────────────────────────
    # cheCkOVER processes two population streams, indigenous and non-indigenous,
    # because that is what World of Crayfish records. Per the WoC submission
    # guidelines, establishmentMeans is MANDATORY and takes exactly two values;
    # the five-value vocabulary (native, type locality, introduced, invasive,
    # cryptogenic) belongs to the separate occurrenceOrigin field, and the two
    # axes are deliberately never merged. A record with no establishmentMeans
    # therefore cannot be assigned to a stream, and is excluded.
    #
    # That is by design, but it used to happen behind a single log line. A
    # dataset exported from GBIF or a similar aggregator often leaves the column
    # empty even though it is present, in which case EVERY record is excluded
    # and the run completes with empty outputs and no obvious cause (Reviewer 1,
    # Ecological Informatics, 2026-09). The exclusion is now stated on the
    # console, in proportion, with the likely cause named.
    na_count <- sum(is.na(cd$population_type))
    if (na_count > 0) {
      pct <- 100 * na_count / max(nrow(cd), 1)
      log_warn("Excluding %d of %d records (%.1f%%): no usable establishmentMeans.",
               na_count, nrow(cd), pct, module = module)

      cat("\n")
      cat("  [!] establishmentMeans missing or unrecognised\n")
      cat(sprintf("      %d of %d records (%.1f%%) are excluded from all analysis.\n",
                  na_count, nrow(cd), pct))
      cat("      cheCkOVER splits occurrences into indigenous and non-indigenous\n")
      cat("      streams; a record with neither value cannot enter either stream.\n")
      cat("      Accepted values: 'indigenous' or 'non-indigenous'.\n")
      cat("      Datasets exported from GBIF and similar aggregators frequently\n")
      cat("      carry this column but leave it empty. If that is your source,\n")
      cat("      populate establishmentMeans before running -- see the WoC\n")
      cat("      submission guidelines. Note it is a DIFFERENT field from\n")
      cat("      occurrenceOrigin (native / type locality / introduced /\n")
      cat("      invasive / cryptogenic), which is not a substitute for it.\n")
      if (na_count == nrow(cd)) {
        cat("\n")
        cat("      ALL records lack establishmentMeans. Every downstream output\n")
        cat("      will be empty. Stopping would hide the rest of the report, so\n")
        cat("      the run continues -- but nothing meaningful will be produced.\n")
        log_error("All %d records lack establishmentMeans; outputs will be empty.",
                  na_count, module = module)
      }
      cat("\n")

      cd <- cd[!is.na(cd$population_type), ]
      sf_pts <- sf_pts[!is.na(sf_pts$population_type), ]
    }
    
    # Count by type
    type_summary <- table(cd$population_type)
    log_info("Indigenous records: %d", type_summary["indigenous"] %||% 0, module = module)
    log_info("Non-indigenous records: %d", type_summary["non-indigenous"] %||% 0, module = module)
    
    # Split data
    log_info("Splitting data into two branches...", module = module)
    
    indigenous_mask <- cd$population_type == "indigenous"
    non_indigenous_mask <- cd$population_type == "non-indigenous"
    
    cd_indigenous <- cd[indigenous_mask, ]
    cd_non_indigenous <- cd[non_indigenous_mask, ]
    
    sf_indigenous <- sf_pts[indigenous_mask, ]
    sf_non_indigenous <- sf_pts[non_indigenous_mask, ]
    
    log_info("Branch A (Indigenous): %d records, %d species",
             nrow(cd_indigenous), 
             length(unique(cd_indigenous$species)),
             module = module)
    
    log_info("Branch B (Non-indigenous): %d records, %d species",
             nrow(cd_non_indigenous),
             length(unique(cd_non_indigenous$species)),
             module = module)
    
    # Create result objects
    result_indigenous <- list(
      clean_data = cd_indigenous,
      clean_sf = sf_indigenous,
      summary_stats = list(
        total_records = nrow(cd_indigenous),
        unique_species = length(unique(cd_indigenous$species))
      )
    )
    
    result_non_indigenous <- list(
      clean_data = cd_non_indigenous,
      clean_sf = sf_non_indigenous,
      summary_stats = list(
        total_records = nrow(cd_non_indigenous),
        unique_species = length(unique(cd_non_indigenous$species))
      )
    )
    
    # Save split data for debugging
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    write_tsv(cd_indigenous, 
              file.path(output_dir, "split_indigenous.tsv"))
    write_tsv(cd_non_indigenous, 
              file.path(output_dir, "split_non_indigenous.tsv"))
    
    log_info("Saved split files for debugging.", module = module)
    
    log_info("Split complete. Returning three result objects.", module = module)
    
    return(list(
      result_indigenous = result_indigenous,
      result_non_indigenous = result_non_indigenous,
      result_combined = result  # ✅ NEW: Return the modified original with population_type
    ))
  })
}