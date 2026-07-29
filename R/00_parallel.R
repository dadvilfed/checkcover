#### MODULE 00: PARALLEL PROCESSING UTILITIES ####

#' Parallel processing utilities for cheCkOVER
#' Provides wrapper functions for parallel execution with proper error handling,
#' progress reporting, and memory management.

library(future)
library(future.apply)

#' Initialize parallel backend
#' @param workers Number of workers ("auto" for automatic detection)
#' @param max_memory_mb Maximum memory per worker in MB
#' @param force_sequential If TRUE, disable parallelization
#' @return List with parallelization info
init_parallel <- function(workers = "auto",
                          max_memory_mb = 1500,
                          force_sequential = FALSE) {
  module <- "PARALLEL_INIT"
  
  # Set memory limit for futures
  options(future.globals.maxSize = max_memory_mb * 1024^2)
  
  if (force_sequential) {
    plan(sequential)
    log_info("Parallel processing DISABLED (sequential mode)", module = module)
    return(list(
      enabled = FALSE,
      workers = 1,
      backend = "sequential"
    ))
  }
  
  # Determine number of workers
  if (workers == "auto") {
    available_cores <- parallel::detectCores(logical = FALSE)
    workers_to_use <- max(1, available_cores - 1)
  } else {
    workers_to_use <- as.integer(workers)
  }
  
  # Select backend based on OS
  if (.Platform$OS.type == "windows") {
    plan(multisession, workers = workers_to_use)
    backend <- "multisession"
    log_info("Parallel: MULTISESSION with %d workers (Windows)", workers_to_use, module = module)
  } else {
    plan(multicore, workers = workers_to_use)
    backend <- "multicore"
    log_info("Parallel: MULTICORE with %d workers (Linux/macOS)", workers_to_use, module = module)
  }
  
  return(list(
    enabled = TRUE,
    workers = workers_to_use,
    backend = backend
  ))
}


#' Parallel lapply with progress and error handling
#' @param X Vector or list to iterate over
#' @param FUN Function to apply
#' @param ... Additional arguments to FUN
#' @param .parallel Whether to use parallel processing
#' @param .progress Show progress bar
#' @param .error_value Value to return on error (NULL to propagate errors)
#' @param .module Module name for logging
#' @return List of results
parallel_lapply <- function(X, FUN, ...,
                            .parallel = TRUE,
                            .progress = TRUE,
                            .error_value = NULL,
                            .module = "PARALLEL") {
  
  n <- length(X)
  
  if (n == 0) return(list())
  
  # Wrap function with error handling
  safe_fun <- function(x, ...) {
    tryCatch({
      FUN(x, ...)
    }, error = function(e) {
      if (is.null(.error_value)) {
        stop(e)
      } else {
        log_warn("Error processing item: %s", conditionMessage(e), module = .module)
        return(.error_value)
      }
    })
  }
  
  # Choose execution method
  if (.parallel && inherits(plan(), "multisession") || inherits(plan(), "multicore")) {
    # Parallel execution
    if (.progress) {
      log_info("Processing %d items in parallel...", n, module = .module)
    }
    
    results <- future_lapply(
      X, 
      safe_fun, 
      ...,
      future.seed = TRUE,
      future.scheduling = 1.0  # Dynamic scheduling
    )
    
  } else {
    # Sequential execution with optional progress
    if (.progress) {
      log_info("Processing %d items sequentially...", n, module = .module)
      pb <- progress::progress_bar$new(
        format = "  [:bar] :current/:total (:percent) ETA: :eta",
        total = n,
        clear = FALSE,
        width = 60
      )
    }
    
    results <- lapply(seq_along(X), function(i) {
      if (.progress) pb$tick()
      safe_fun(X[[i]], ...)
    })
  }
  
  # Restore names if present
  if (!is.null(names(X))) {
    names(results) <- names(X)
  }
  
  return(results)
}


#' Execute two functions in parallel (for Branch A/B)
#' @param fun_a Function A (indigenous branch)
#' @param fun_b Function B (non-indigenous branch)
#' @param args_a Arguments for function A
#' @param args_b Arguments for function B
#' @return List with results from both branches
parallel_branches <- function(fun_a, fun_b, args_a, args_b) {
  module <- "PARALLEL_BRANCHES"
  
  # Check if parallel is enabled
  current_plan <- plan()
  is_parallel <- !inherits(current_plan, "sequential")
  
  if (is_parallel) {
    log_info("Executing Branch A and Branch B in PARALLEL...", module = module)
    
    start_time <- Sys.time()
    
    # Create futures for both branches
    future_a <- future({
      do.call(fun_a, args_a)
    }, seed = TRUE)
    
    future_b <- future({
      do.call(fun_b, args_b)
    }, seed = TRUE)
    
    # Collect results
    result_a <- value(future_a)
    result_b <- value(future_b)
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    log_info("Both branches completed in %.1f seconds", as.numeric(elapsed), module = module)
    
  } else {
    log_info("Executing Branch A and Branch B SEQUENTIALLY...", module = module)
    
    start_time <- Sys.time()
    
    log_info("Starting Branch A...", module = module)
    result_a <- do.call(fun_a, args_a)
    time_a <- difftime(Sys.time(), start_time, units = "secs")
    log_info("Branch A completed in %.1f seconds", as.numeric(time_a), module = module)
    
    log_info("Starting Branch B...", module = module)
    start_b <- Sys.time()
    result_b <- do.call(fun_b, args_b)
    time_b <- difftime(Sys.time(), start_b, units = "secs")
    log_info("Branch B completed in %.1f seconds", as.numeric(time_b), module = module)
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    log_info("Total sequential time: %.1f seconds", as.numeric(elapsed), module = module)
  }
  
  return(list(
    branch_a = result_a,
    branch_b = result_b
  ))
}


#' Batch processing with memory management
#' @param items Vector/list of items to process
#' @param process_fun Function to process each item
#' @param batch_size Number of items per batch
#' @param aggressive_gc Run gc() after each batch
#' @param ... Additional arguments to process_fun
#' @return Combined results from all batches
batch_process <- function(items, process_fun, 
                          batch_size = 50,
                          aggressive_gc = TRUE,
                          ...) {
  module <- "BATCH_PROCESS"
  
  n <- length(items)
  if (n == 0) return(list())
  
  # Calculate batches
  n_batches <- ceiling(n / batch_size)
  
  log_info("Processing %d items in %d batches (size=%d)", 
           n, n_batches, batch_size, module = module)
  
  all_results <- list()
  
  for (batch_num in seq_len(n_batches)) {
    # Get batch indices
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, n)
    
    batch_items <- items[start_idx:end_idx]
    
    log_info("Batch %d/%d: items %d-%d", 
             batch_num, n_batches, start_idx, end_idx, module = module)
    
    # Process batch (can be parallel within batch)
    batch_results <- parallel_lapply(
      batch_items,
      process_fun,
      ...,
      .module = module
    )
    
    # Append results
    all_results <- c(all_results, batch_results)
    
    # Memory cleanup
    if (aggressive_gc) {
      gc(verbose = FALSE)
    }
  }
  
  return(all_results)
}


#' Get current parallel status
#' @return List with current parallel configuration
parallel_status <- function() {
  current_plan <- plan()
  
  list(
    plan_class = class(current_plan)[1],
    is_parallel = !inherits(current_plan, "sequential"),
    workers = nbrOfWorkers(),
    available_cores = parallel::detectCores(logical = FALSE),
    max_memory_mb = getOption("future.globals.maxSize") / 1024^2
  )
}


#' Shutdown parallel workers
#' @return NULL
shutdown_parallel <- function() {
  plan(sequential)
  gc()
  log_info("Parallel workers shut down", module = "PARALLEL")
  invisible(NULL)
}