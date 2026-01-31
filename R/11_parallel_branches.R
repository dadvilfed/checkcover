#### MODULE 11: PARALLEL BRANCH EXECUTION ####

#' Wrapper functions for parallel execution of Branch A (Indigenous) 
#' and Branch B (Non-indigenous) pipelines

#' Process Indigenous Branch (Branch A)
#' Encapsulates all processing steps for indigenous species
#' @param result_indigenous Split indigenous result object
#' @param output_dir Output directory
#' @param shared_cache Shared cache directory
#' @param config CONFIG object
#' @param hydrobasin_names HydroBASINS name lookup
#' @param vernacular_lookup Vernacular names lookup
#' @return List with processed result and reports
process_branch_indigenous <- function(result_indigenous,
                                      output_dir,
                                      shared_cache,
                                      config,
                                      hydrobasin_names,
                                      vernacular_lookup) {
  module <- "BRANCH_A"
  
  log_info("=== BRANCH A: INDIGENOUS PROCESSING START ===", module = module)
  start_time <- Sys.time()
  
  tryCatch({
    # Step 1: Calculate Metrics & IUCN Categorization
    log_info("Step 1: Calculating indigenous metrics...", module = module)
    result_indigenous <- calculate_indigenous_metrics(
      result_indigenous,
      output_dir = output_dir
    )
    
    # Step 2: Fragmentation Analysis (endemic/regional only)
    log_info("Step 2: Analyzing fragmentation...", module = module)
    result_indigenous <- analyze_fragmentation(
      result_indigenous,
      output_dir = output_dir,
      category_filter = c("endemic", "regional")
    )
    
    # Step 3: Spatial Enrichment
    log_info("Step 3: Enriching with spatial layers...", module = module)
    result_indigenous <- enrich_indigenous_spatial(
      result_indigenous,
      output_dir = output_dir,
      cache_dir = shared_cache,
      config = config,
      hydrobasin_names = hydrobasin_names
    )
    
    # Step 4: Generate Reports
    log_info("Step 4: Generating reports...", module = module)
    indigenous_reports <- generate_indigenous_reports(
      result_indigenous,
      vernacular_lookup = vernacular_lookup,
      output_dir = output_dir
    )
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    log_info("=== BRANCH A COMPLETE in %.1f seconds ===", as.numeric(elapsed), module = module)
    
    return(list(
      result = result_indigenous,
      reports = indigenous_reports,
      elapsed_seconds = as.numeric(elapsed),
      success = TRUE
    ))
    
  }, error = function(e) {
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    log_error("BRANCH A FAILED after %.1f seconds: %s", 
              as.numeric(elapsed), conditionMessage(e), module = module)
    
    return(list(
      result = result_indigenous,
      reports = NULL,
      elapsed_seconds = as.numeric(elapsed),
      success = FALSE,
      error = conditionMessage(e)
    ))
  })
}


#' Process Non-Indigenous Branch (Branch B)
#' Encapsulates all processing steps for non-indigenous species
#' @param result_non_indigenous Split non-indigenous result object
#' @param output_dir Output directory
#' @param shared_cache Shared cache directory
#' @param config CONFIG object
#' @param hydrobasin_names HydroBASINS name lookup
#' @param vernacular_lookup Vernacular names lookup
#' @return List with processed result and reports
process_branch_non_indigenous <- function(result_non_indigenous,
                                          output_dir,
                                          shared_cache,
                                          config,
                                          hydrobasin_names,
                                          vernacular_lookup) {
  module <- "BRANCH_B"
  
  log_info("=== BRANCH B: NON-INDIGENOUS PROCESSING START ===", module = module)
  start_time <- Sys.time()
  
  tryCatch({
    # Step 1: Calculate Metrics & Categorization (local/widespread)
    log_info("Step 1: Calculating non-indigenous metrics...", module = module)
    result_non_indigenous <- calculate_non_indigenous_metrics(
      result_non_indigenous,
      output_dir = output_dir
    )
    
    # Step 2: Spatial Enrichment (NO fragmentation)
    log_info("Step 2: Enriching with spatial layers...", module = module)
    result_non_indigenous <- enrich_non_indigenous_spatial(
      result_non_indigenous,
      output_dir = output_dir,
      cache_dir = shared_cache,
      config = config,
      hydrobasin_names = hydrobasin_names
    )
    
    # Step 3: Generate Reports
    log_info("Step 3: Generating reports...", module = module)
    non_indigenous_reports <- generate_non_indigenous_reports(
      result_non_indigenous,
      vernacular_lookup = vernacular_lookup,
      output_dir = output_dir
    )
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    log_info("=== BRANCH B COMPLETE in %.1f seconds ===", as.numeric(elapsed), module = module)
    
    return(list(
      result = result_non_indigenous,
      reports = non_indigenous_reports,
      elapsed_seconds = as.numeric(elapsed),
      success = TRUE
    ))
    
  }, error = function(e) {
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    log_error("BRANCH B FAILED after %.1f seconds: %s", 
              as.numeric(elapsed), conditionMessage(e), module = module)
    
    return(list(
      result = result_non_indigenous,
      reports = NULL,
      elapsed_seconds = as.numeric(elapsed),
      success = FALSE,
      error = conditionMessage(e)
    ))
  })
}


#' Run both branches (parallel or sequential based on plan)
#' @param result_indigenous Indigenous result from split
#' @param result_non_indigenous Non-indigenous result from split
#' @param output_dir Output directory
#' @param shared_cache Shared cache directory
#' @param config CONFIG object
#' @param hydrobasin_names HydroBASINS name lookup
#' @param vernacular_lookup Vernacular names lookup
#' @param force_sequential Force sequential execution even if parallel is available
#' @return List with both branch results
run_parallel_branches <- function(result_indigenous,
                                  result_non_indigenous,
                                  output_dir,
                                  shared_cache,
                                  config,
                                  hydrobasin_names,
                                  vernacular_lookup,
                                  force_sequential = FALSE) {
  module <- "PARALLEL_BRANCHES"
  
  # Check current parallel configuration
  par_status <- parallel_status()
  use_parallel <- par_status$is_parallel && !force_sequential
  
  # Check if both branches have data
  has_indigenous <- nrow(result_indigenous$clean_data) > 0
  has_non_indigenous <- nrow(result_non_indigenous$clean_data) > 0
  
  log_info("Branch status - Indigenous: %d records, Non-indigenous: %d records",
           nrow(result_indigenous$clean_data),
           nrow(result_non_indigenous$clean_data),
           module = module)
  
  start_time <- Sys.time()
  
  # Handle cases where only one branch has data
  if (!has_indigenous && !has_non_indigenous) {
    log_warn("No data in either branch!", module = module)
    return(list(
      branch_a = list(result = result_indigenous, reports = list(), success = FALSE),
      branch_b = list(result = result_non_indigenous, reports = list(), success = FALSE),
      parallel_execution = FALSE
    ))
  }
  
  if (!has_indigenous) {
    log_info("Only non-indigenous data present. Running Branch B only.", module = module)
    branch_b <- process_branch_non_indigenous(
      result_non_indigenous, output_dir, shared_cache, config,
      hydrobasin_names, vernacular_lookup
    )
    return(list(
      branch_a = list(result = result_indigenous, reports = list(), success = TRUE, elapsed_seconds = 0),
      branch_b = branch_b,
      parallel_execution = FALSE
    ))
  }
  
  if (!has_non_indigenous) {
    log_info("Only indigenous data present. Running Branch A only.", module = module)
    branch_a <- process_branch_indigenous(
      result_indigenous, output_dir, shared_cache, config,
      hydrobasin_names, vernacular_lookup
    )
    return(list(
      branch_a = branch_a,
      branch_b = list(result = result_non_indigenous, reports = list(), success = TRUE, elapsed_seconds = 0),
      parallel_execution = FALSE
    ))
  }
  
  # Both branches have data - execute in parallel or sequential
  if (use_parallel) {
    log_info("=== EXECUTING BRANCHES IN PARALLEL ===", module = module)
    log_info("Workers available: %d", par_status$workers, module = module)
    
    # Create futures for both branches
    future_a <- future::future({
      process_branch_indigenous(
        result_indigenous, output_dir, shared_cache, config,
        hydrobasin_names, vernacular_lookup
      )
    }, seed = TRUE)
    
    future_b <- future::future({
      process_branch_non_indigenous(
        result_non_indigenous, output_dir, shared_cache, config,
        hydrobasin_names, vernacular_lookup
      )
    }, seed = TRUE)
    
    # Collect results (blocks until both complete)
    log_info("Waiting for both branches to complete...", module = module)
    branch_a <- future::value(future_a)
    branch_b <- future::value(future_b)
    
    parallel_execution <- TRUE
    
  } else {
    log_info("=== EXECUTING BRANCHES SEQUENTIALLY ===", module = module)
    
    branch_a <- process_branch_indigenous(
      result_indigenous, output_dir, shared_cache, config,
      hydrobasin_names, vernacular_lookup
    )
    
    branch_b <- process_branch_non_indigenous(
      result_non_indigenous, output_dir, shared_cache, config,
      hydrobasin_names, vernacular_lookup
    )
    
    parallel_execution <- FALSE
  }
  
  total_elapsed <- difftime(Sys.time(), start_time, units = "secs")
  
  # Summary
  log_info("=== BRANCH EXECUTION SUMMARY ===", module = module)
  log_info("  Mode: %s", if(parallel_execution) "PARALLEL" else "SEQUENTIAL", module = module)
  log_info("  Branch A (Indigenous): %.1f sec, Success: %s", 
           branch_a$elapsed_seconds, branch_a$success, module = module)
  log_info("  Branch B (Non-indigenous): %.1f sec, Success: %s", 
           branch_b$elapsed_seconds, branch_b$success, module = module)
  log_info("  Total wall-clock time: %.1f sec", as.numeric(total_elapsed), module = module)
  
  if (parallel_execution) {
    sequential_time <- branch_a$elapsed_seconds + branch_b$elapsed_seconds
    speedup <- sequential_time / as.numeric(total_elapsed)
    log_info("  Estimated sequential time: %.1f sec", sequential_time, module = module)
    log_info("  Parallel speedup: %.2fx", speedup, module = module)
  }
  
  return(list(
    branch_a = branch_a,
    branch_b = branch_b,
    parallel_execution = parallel_execution,
    total_elapsed_seconds = as.numeric(total_elapsed)
  ))
}