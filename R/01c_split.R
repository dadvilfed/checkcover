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
    
    # Check for NAs
    na_count <- sum(is.na(cd$population_type))
    if (na_count > 0) {
      log_warn("Found %d records with unknown population type. Excluding from analysis.", 
               na_count, module = module)
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