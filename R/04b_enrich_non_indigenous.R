#### MODULE 4B: SPATIAL ENRICHMENT WRAPPER (NON-INDIGENOUS) ####

#' Wrapper to enrich non-indigenous data with spatial layers
#' @param result_non_indigenous Result object with metrics and categories
#' @param output_dir Output directory
#' @param cache_dir Shared cache directory
#' @param config Configuration list
#' @param hydrobasin_names HydroBASINS name lookup table
#' @return Enriched result_non_indigenous
enrich_non_indigenous_spatial <- function(result_non_indigenous, 
                                          output_dir, 
                                          cache_dir,
                                          config,
                                          hydrobasin_names = NULL) {
  module <- "MODULE4B_ENRICH_NON_IND"
  
  with_log_section(module, {
    log_info("=== MODULE 4B: NON-INDIGENOUS SPATIAL ENRICHMENT ===", module = module)
    
    # Empty-data short-circuit. Must come BEFORE population_type validation —
    # otherwise sparse-versioning runs that have no non-indigenous active
    # species (e.g. only an indigenous-only species was reprocessed) trip
    # the "wrong population type" check on a character(0) vector.
    if (nrow(result_non_indigenous$clean_data) == 0) {
      log_warn("No non-indigenous data to enrich.", module = module)
      return(result_non_indigenous)
    }
    
    # CRITICAL VALIDATION: Ensure we have the right data
    if (!"population_type" %in% names(result_non_indigenous$clean_data)) {
      log_error("Missing population_type column in result_non_indigenous!", module = module)
      stop("Data corruption detected: missing population_type column")
    }
    
    pop_types <- unique(result_non_indigenous$clean_data$population_type)
    if (!"non-indigenous" %in% pop_types) {
      log_error("CRITICAL ERROR: result_non_indigenous contains wrong population type!", module = module)
      log_error("Expected: 'non-indigenous', Found: %s", paste(pop_types, collapse = ", "), module = module)
      stop("Data corruption: wrong population type in result_non_indigenous")
    }
    
    if ("indigenous" %in% pop_types) {
      log_error("CRITICAL ERROR: result_non_indigenous contains INDIGENOUS records!", module = module)
      stop("Data corruption: indigenous records found in non-indigenous object")
    }
    
    log_info("Validation passed: Object contains only non-indigenous records.", module = module)
    
    if (nrow(result_non_indigenous$clean_data) == 0) {
      log_warn("No non-indigenous data to enrich.", module = module)
      return(result_non_indigenous)
    }
    
    log_info("Enriching %d non-indigenous records across %d species...",
             nrow(result_non_indigenous$clean_data),
             length(unique(result_non_indigenous$clean_data$species)),
             module = module)
    
    log_info("Note: GADM enrichment already completed in pre-split phase.", module = module)
    log_info("Note: Fragmentation analysis NOT applicable for non-indigenous populations.", module = module)
    
    # Store original to compare after each step
    original_species <- unique(result_non_indigenous$clean_data$species)
    original_records <- nrow(result_non_indigenous$clean_data)
    
    # Step 1: TEOW
    log_info("Step 1/4: Enriching with TEOW...", module = module)
    result_non_indigenous <- enrich_with_teow(
      result_non_indigenous,
      output_dir = output_dir
    )
    
    # Validate after TEOW
    if (nrow(result_non_indigenous$clean_data) != original_records) {
      log_error("Record count changed after TEOW! Before: %d, After: %d",
                original_records, nrow(result_non_indigenous$clean_data), module = module)
    }
    
    # Step 2: FEOW
    log_info("Step 2/4: Enriching with FEOW...", module = module)
    result_non_indigenous <- enrich_with_feow(
      result_non_indigenous,
      output_dir = output_dir,
      feow_source = config$spatial$feow_source,
      feow_shp_path = config$spatial$feow_path
    )
    
    # Validate after FEOW
    if (nrow(result_non_indigenous$clean_data) != original_records) {
      log_error("Record count changed after FEOW! Before: %d, After: %d",
                original_records, nrow(result_non_indigenous$clean_data), module = module)
    }
    
    # Step 3: WDPA
    log_info("Step 3/4: Enriching with WDPA...", module = module)
    result_non_indigenous <- enrich_with_wdpa(
      result_non_indigenous,
      output_dir = output_dir,
      cache_dir = cache_dir
    )
    
    # Validate after WDPA
    if (nrow(result_non_indigenous$clean_data) != original_records) {
      log_error("Record count changed after WDPA! Before: %d, After: %d",
                original_records, nrow(result_non_indigenous$clean_data), module = module)
    }
    
    # Step 4: HydroBASINS
    log_info("Step 4/4: Enriching with HydroBASINS...", module = module)
    result_non_indigenous <- enrich_with_hydrobasins_merged(
      result_non_indigenous,
      hydro_dir = config$spatial$hydro_dir,
      global_files = config$spatial$hydro_files,
      output_dir = output_dir,
      cache_dir = cache_dir,
      reuse_cached = TRUE,
      parallel = FALSE
    )
    
    # Final validation
    final_species <- unique(result_non_indigenous$clean_data$species)
    final_records <- nrow(result_non_indigenous$clean_data)
    
    if (final_records != original_records) {
      log_error("RECORD COUNT MISMATCH! Original: %d, Final: %d",
                original_records, final_records, module = module)
    }
    
    if (length(final_species) != length(original_species)) {
      log_error("SPECIES COUNT MISMATCH! Original: %d, Final: %d",
                length(original_species), length(final_species), module = module)
    }
    
    # Save enriched data
    enriched_file <- file.path(output_dir, "non_indigenous_enriched.tsv")
    write_tsv(result_non_indigenous$clean_data, enriched_file)
    log_info("Saved enriched non-indigenous data to: %s", enriched_file, module = module)
    
    log_info("Non-indigenous spatial enrichment complete.", module = module)
    
    return(result_non_indigenous)
  })
}