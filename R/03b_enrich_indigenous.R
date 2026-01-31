#### MODULE 3B: SPATIAL ENRICHMENT WRAPPER (INDIGENOUS) ####

#' Wrapper to enrich indigenous data with remaining spatial layers
#' @param result_indigenous Result object with metrics and categories
#' @param output_dir Output directory
#' @param cache_dir Shared cache directory
#' @param config Configuration list
#' @param hydrobasin_names HydroBASINS name lookup table
#' @return Enriched result_indigenous
enrich_indigenous_spatial <- function(result_indigenous, 
                                      output_dir, 
                                      cache_dir,
                                      config,
                                      hydrobasin_names = NULL) {
  module <- "MODULE3B_ENRICH_IND"
  
  with_log_section(module, {
    
    log_info("=== MODULE 3B: INDIGENOUS SPATIAL ENRICHMENT (remaining layers) ===", module = module)
    # CRITICAL VALIDATION: Ensure we have the right data
    if (!"population_type" %in% names(result_indigenous$clean_data)) {
      log_error("Missing population_type column in result_indigenous!", module = module)
      stop("Data corruption detected: missing population_type column")
    }
    
    pop_types <- unique(result_indigenous$clean_data$population_type)
    if (!"indigenous" %in% pop_types) {
      log_error("CRITICAL ERROR: result_indigenous contains wrong population type!", module = module)
      log_error("Expected: 'indigenous', Found: %s", paste(pop_types, collapse = ", "), module = module)
      stop("Data corruption: wrong population type in result_indigenous")
    }
    
    if ("non-indigenous" %in% pop_types) {
      log_error("CRITICAL ERROR: result_indigenous contains NON-INDIGENOUS records!", module = module)
      stop("Data corruption: non-indigenous records found in indigenous object")
    }
    
    log_info("Validation passed: Object contains only indigenous records.", module = module)
    
    if (nrow(result_indigenous$clean_data) == 0) {
      log_warn("No indigenous data to enrich.", module = module)
      return(result_indigenous)
    }
    
    log_info("Enriching %d indigenous records across %d species...",
             nrow(result_indigenous$clean_data),
             length(unique(result_indigenous$clean_data$species)),
             module = module)
    
    # Note: GADM already done before split
    log_info("Note: GADM enrichment already completed in pre-split phase.", module = module)
    
    # Step 1: TEOW (Terrestrial Ecoregions)
    log_info("Step 1/4: Enriching with TEOW...", module = module)
    result_indigenous <- enrich_with_teow(
      result_indigenous,
      output_dir = output_dir
    )
    
    # Step 2: FEOW (Freshwater Ecoregions)
    log_info("Step 2/4: Enriching with FEOW...", module = module)
    result_indigenous <- enrich_with_feow(
      result_indigenous,
      output_dir = output_dir,
      feow_source = config$spatial$feow_source,
      feow_shp_path = config$spatial$feow_path
    )
    
    # Step 3: WDPA (Protected Areas)
    log_info("Step 3/4: Enriching with WDPA...", module = module)
    result_indigenous <- enrich_with_wdpa(
      result_indigenous,
      output_dir = output_dir,
      cache_dir = cache_dir
    )
    
    # Step 4: HydroBASINS (Species-specific levels)
    log_info("Step 4/4: Enriching with HydroBASINS...", module = module)
    result_indigenous <- enrich_with_hydrobasins_merged(
      result_indigenous,
      hydro_dir = config$spatial$hydro_dir,
      global_files = config$spatial$hydro_files,
      output_dir = output_dir,
      cache_dir = cache_dir,
      reuse_cached = TRUE,
      parallel = FALSE
    )
    
    # Save enriched data
    enriched_file <- file.path(output_dir, "indigenous_enriched.tsv")
    write_tsv(result_indigenous$clean_data, enriched_file)
    log_info("Saved enriched indigenous data to: %s", enriched_file, module = module)
    
    log_info("Indigenous spatial enrichment complete.", module = module)
    
    return(result_indigenous)
  })
}