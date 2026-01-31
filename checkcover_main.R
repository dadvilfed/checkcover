#!/usr/bin/env Rscript
#### cheCkCOVER MAIN ORCHESTRATION SCRIPT ####
# This script runs the entire analysis pipeline

# Clear workspace
rm(list = ls())
gc()

# Get script directory (works with source() or Rscript)
if (sys.nframe() == 0) {
  script_dir <- getwd()
} else {
  script_dir <- getSrcDirectory(function(x) {x})
}

setwd(script_dir)

cat("\n")
cat("==============================================================\n")
cat("   cheCkCOVER: Biodiversity Occurrence Analysis Framework     \n")
cat("==============================================================\n\n")

# Step 1: Load configuration
cat("Loading configuration...\n")
source("config.R")

# Step 2: Initialize logging FIRST (before loading packages)
cat("Initializing logger...\n")
source("R/00_logging.R")
init_logger(
  log_dir = file.path(CONFIG$root_output_dir, "logs"),
  log_level = "INFO"
)

log_info("========================================", module = "MAIN")
log_info("cheCkCOVER Analysis Started", module = "MAIN")
log_info("========================================", module = "MAIN")

# Step 3: Install and load packages
cat("Loading required packages...\n")
source("R/00_helpers.R")

install_missing <- function(packages, module = "PKG_LOADER") {
  installed <- utils::installed.packages()[, "Package"]
  missing <- setdiff(packages, installed)
  
  if (length(missing) == 0L) {
    log_info("All required packages installed.", module = module)
    return(invisible(NULL))
  }
  
  log_info("Installing %d missing packages: %s", 
           length(missing), paste(missing, collapse = ", "), module = module)
  
  for (pkg in missing) {
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      log_info("Installed '%s'.", pkg, module = module)
    }, error = function(e) {
      log_error("Failed to install '%s': %s", pkg, conditionMessage(e), module = module)
    })
  }
}

load_packages <- function(packages, module = "PKG_LOADER") {
  with_log_section(module, {
    install_missing(packages, module = module)
    
    loaded <- vapply(packages, FUN.VALUE = logical(1), FUN = function(pkg) {
      ok <- require(pkg, character.only = TRUE, quietly = TRUE)
      if (ok) {
        ver <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "unknown")
        log_debug("Loaded '%s' (v%s).", pkg, ver, module = module)
      } else {
        log_error("Failed to load '%s'.", pkg, module = module)
      }
      ok
    })
    
    if (!all(loaded)) {
      failed <- packages[!loaded]
      log_error("Failed packages: %s", paste(failed, collapse = ", "), module = module)
      stop("Cannot proceed without required packages.")
    }
    
    log_info("Successfully loaded all %d packages.", length(packages), module = module)
  })
}

load_packages(REQUIRED_PACKAGES)

# # Step 4: Setup parallelization
# cat("Configuring parallel processing...\n")
# 
# library(future)
# library(future.apply)
# 
# # CRITICAL: Set memory limit for workers
# options(future.globals.maxSize = CONFIG$memory$max_worker_memory * 1024^2)
# 
# if (CONFIG$parallel$force_sequential) {
#   plan(sequential)
#   log_info("Parallel processing DISABLED (force_sequential = TRUE)", module = "MAIN")
# } else {
#   if (CONFIG$parallel$workers == "auto") {
#     available_cores <- parallel::detectCores(logical = FALSE)
#     workers_to_use <- max(1, available_cores - 1)
#   } else {
#     workers_to_use <- as.integer(CONFIG$parallel$workers)
#   }
#   
#   if (.Platform$OS.type == "windows") {
#     plan(multisession, workers = workers_to_use)
#     log_info("Parallel: MULTISESSION with %d workers (Windows)", workers_to_use, module = "MAIN")
#   } else {
#     plan(multicore, workers = workers_to_use)
#     log_info("Parallel: MULTICORE with %d workers (Linux)", workers_to_use, module = "MAIN")
#   }
# }

# Step 4: Setup parallelization
cat("Configuring parallel processing...\n")

library(future)
library(future.apply)

# Load parallel utilities
source("R/00_parallel.R")

# Initialize parallel backend
PARALLEL_INFO <- init_parallel(
  workers = CONFIG$parallel$workers,
  max_memory_mb = CONFIG$memory$max_worker_memory,
  force_sequential = CONFIG$parallel$force_sequential
)

log_info("Parallel configuration:", module = "MAIN")
log_info("  Enabled: %s", PARALLEL_INFO$enabled, module = "MAIN")
log_info("  Workers: %d", PARALLEL_INFO$workers, module = "MAIN")
log_info("  Backend: %s", PARALLEL_INFO$backend, module = "MAIN")


# Step 5: Initialize Run Manager
cat("Initializing run manager...\n")

init_run_manager <- function(input_file, config_list, root_output_dir) {
  module <- "RUN_MANAGER"
  
  if (!dir.exists(root_output_dir)) dir.create(root_output_dir, recursive = TRUE)
  
  if (!is.null(config_list$version)) {
    run_id <- paste0("run_", config_list$version)
  } else {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    input_hash <- substr(digest::digest(list(input_file, config_list), algo = "xxhash64"), 1, 6)
    run_id <- paste0("run_", timestamp, "_", input_hash)
  }
  
  runs_root <- file.path(root_output_dir, "runs")
  run_dir <- file.path(runs_root, run_id)
  
  run_status <- "NEW"
  if (dir.exists(run_dir)) {
    run_status <- "RESUME"
    log_info("Run folder exists. RESUME mode: %s", run_id, module = module)
  } else {
    log_info("NEW run: %s", run_id, module = module)
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  registry_file <- file.path(root_output_dir, "_registry.json")
  reg_entry <- list(
    run_id = run_id,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    input_file = basename(input_file),
    status = run_status,
    path = run_dir
  )
  
  current_reg <- list()
  if (file.exists(registry_file)) {
    try(current_reg <- jsonlite::read_json(registry_file, simplifyVector = FALSE), silent = TRUE)
  }
  current_reg[[length(current_reg) + 1]] <- reg_entry
  jsonlite::write_json(current_reg, registry_file, pretty = TRUE, auto_unbox = TRUE)
  
  return(list(
    run_id = run_id,
    run_dir = run_dir,
    status = run_status,
    shared_cache_dir = file.path(root_output_dir, "cache")
  ))
}

mark_run_complete <- function(run_dir) {
  file.create(file.path(run_dir, "_SUCCESS"))
}

run_env <- init_run_manager(
  input_file = CONFIG$input_file,
  config_list = CONFIG,
  root_output_dir = CONFIG$root_output_dir
)

SHARED_CACHE <- run_env$shared_cache_dir

# Step 6: Load all module scripts
cat("Loading analysis modules...\n")
module_files <- c(
  #"R/00_parallel.R", #NEW MODULE
  "R/01_ingest.R",
  "R/01b_vernacular.R",
  #"R/01c_metrics.R",
  "R/01c_split.R", # NEW MODULE
  "R/01d_fragmentation.R",
  "R/01d_scenario_detection.R", #NEW MODULE
  "R/02a_continents.R",
  "R/02b_gadm.R",
  "R/02c_teow.R",
  "R/02d_feow.R",
  "R/02e_wdpa.R",
  "R/02f_hydrobasins.R",
  #"R/03_maps.R",
  "R/03a_metrics_indigenous.R", #NEW MODULE
  "R/03b_enrich_indigenous.R", #NEW MODULE  
  "R/03c_reports_indigenous.R", #NEW MODULE
  #"R/04_reports.R",
  "R/04a_metrics_non_indigenous.R", #NEW MODULE
  "R/04b_enrich_non_indigenous.R", #NEW MODULE
  "R/04c_reports_non_indigenous.R", #NEW MODULE
  #"R/05_narratives.R",
  "R/05_merge_scenario3.R", # NEW MODULE
  #"R/06_citations.R",
  "R/06_narratives.R", #NEW MODULE
  #"R/07_export.R",
  "R/07_citations.R", #NEW MODULE
  "R/08_maps.R", #NEW MODULE
  "R/08_maps_parallel.R", #NEW MODULE
  "R/09_package_export.R", #NEW MODULE
  "R/10_canonical_narratives.R", #NEW MODULE
  "R/11_parallel_branches.R" #NEW MODULE
)

for (mf in module_files) {
  if (file.exists(mf)) {
    source(mf)
    log_info("Loaded: %s", mf, module = "MAIN")
  } else {
    log_error("Module file not found: %s", mf, module = "MAIN")
    stop("Cannot proceed without all modules.")
  }
}

# Step 7: Load dictionaries
cat("Loading dictionaries...\n")

load_hydrobasin_names <- function(tsv_path) {
  if (!file.exists(tsv_path)) {
    log_warn("HydroBASINS names not found: %s", tsv_path, module = "MAIN")
    return(NULL)
  }
  hb_names <- read.delim(tsv_path, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE, na.strings = c("", "NA"))
  names(hb_names) <- c("Basin_level", "HYBAS_ID", "Basin_name", "Subbasin_name")
  hb_names$HYBAS_ID <- as.character(hb_names$HYBAS_ID)
  hb_names$lookup_key_full <- paste0(hb_names$Basin_level, ":", hb_names$HYBAS_ID)
  hb_names$lookup_key_id <- hb_names$HYBAS_ID
  log_info("Loaded %d HydroBASINS names.", nrow(hb_names), module = "MAIN")
  return(hb_names)
}

HYDROBASIN_NAMES <- load_hydrobasin_names(CONFIG$dictionaries$hydrobasins)
VERNACULAR_LOOKUP <- NULL

# Step 8: RUN THE PIPELINE
cat("\n")
cat("==============================================================\n")
cat("   STARTING ANALYSIS PIPELINE                                \n")
cat("==============================================================\n\n")

pipeline_start_time <- Sys.time()  # <-- ADD THIS LINE HERE

if (run_env$status %in% c("NEW", "RESUME")) {
  
  log_info(">>> PIPELINE START (Run ID: %s) <<<", run_env$run_id, module = "MAIN")
  
  # ============================================================
  # PHASE 1: PRE-SPLIT PROCESSING
  # ============================================================
  
  # Module 1: Ingest & Clean
  cat("\n[MODULE 1] Ingesting and cleaning data...\n")
  result <- ingest_clean(
    CONFIG$input_file,
    output_dir = run_env$run_dir,
    resolve_taxonomy = CONFIG$taxonomy$resolve
  )
  
  # Module 1B: Vernacular Names (on full dataset)
  cat("\n[MODULE 1B] Generating vernacular names...\n")
  vernacular_lookup <- generate_vernacular_file(
    result,
    output_dir = run_env$run_dir,
    source = CONFIG$vernaculars$source,
    docx_path = CONFIG$vernaculars$path
  )
  VERNACULAR_LOOKUP <- list(wide = vernacular_lookup)
  
  # Module 2A: Continents (BEFORE split - on full dataset)
  cat("\n[MODULE 2A] Enriching with continents...\n")
  result <- enrich_with_continents(result, output_dir = run_env$run_dir)
  
  # Module 2B: GADM (BEFORE split) ✅ MOVED HERE
  cat("\n[MODULE 2B] Enriching with GADM (countries & admin units)...\n")
  result <- enrich_with_gadm(result, output_dir = run_env$run_dir, cache_dir = SHARED_CACHE)
  
  # ============================================================
  # PHASE 1: SPLIT INTO TWO BRANCHES
  # ============================================================
  
  # Module 1C: Split by Population Status
  cat("\n[MODULE 1C] Splitting data by population status...\n")
  split_results <- split_by_population(result, output_dir = run_env$run_dir)
  
  result_indigenous <- split_results$result_indigenous
  result_non_indigenous <- split_results$result_non_indigenous
  result <- split_results$result_combined  # ✅ Update original result with population_type
  
  # Module 1D: Detect Species Scenarios
  cat("\n[MODULE 1D] Detecting species scenarios...\n")
  scenario_table <- detect_species_scenarios(result, output_dir = run_env$run_dir)
  
  # Save scenario table for later use
  saveRDS(scenario_table, file.path(run_env$run_dir, "scenario_table.rds"))
  
  # # ============================================================
  # # PHASE 2: BRANCH A - INDIGENOUS PIPELINE
  # # ============================================================
  # 
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PHASE 2: BRANCH A - INDIGENOUS PROCESSING                 \n")
  # cat("==============================================================\n")
  # cat("\n")
  # 
  # # Module 3A: Calculate Metrics & IUCN Categorization
  # cat("\n[MODULE 3A] Calculating indigenous metrics...\n")
  # result_indigenous <- calculate_indigenous_metrics(
  #   result_indigenous,
  #   output_dir = run_env$run_dir
  # )
  # 
  # # Module 3B: Fragmentation Analysis (endemic/regional only)
  # cat("\n[MODULE 3B] Analyzing fragmentation (endemic/regional only)...\n")
  # result_indigenous <- analyze_fragmentation(
  #   result_indigenous,
  #   output_dir = run_env$run_dir,
  #   category_filter = c("endemic", "regional")  # Skip cosmopolitan
  # )
  # 
  # # Module 3C: Spatial Enrichment
  # cat("\n[MODULE 3C] Enriching with spatial layers...\n")
  # result_indigenous <- enrich_indigenous_spatial(
  #   result_indigenous,
  #   output_dir = run_env$run_dir,
  #   cache_dir = SHARED_CACHE,
  #   config = CONFIG,
  #   hydrobasin_names = HYDROBASIN_NAMES
  # )
  # 
  # # Module 3D: Generate Reports
  # cat("\n[MODULE 3D] Generating indigenous reports...\n")
  # indigenous_reports <- generate_indigenous_reports(
  #   result_indigenous,
  #   vernacular_lookup = VERNACULAR_LOOKUP,
  #   output_dir = run_env$run_dir
  # )
  # 
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PHASE 2 COMPLETE                                          \n")
  # cat("==============================================================\n")
  # cat("\n")
  # cat("Indigenous Processing Summary:\n")
  # cat("  Species processed:", length(unique(result_indigenous$clean_data$species)), "\n")
  # cat("  Endemic:", sum(result_indigenous$clean_data$iucn_category == "endemic", na.rm = TRUE), "records\n")
  # cat("  Regional:", sum(result_indigenous$clean_data$iucn_category == "regional", na.rm = TRUE), "records\n")
  # cat("  Cosmopolitan:", sum(result_indigenous$clean_data$iucn_category == "cosmopolitan", na.rm = TRUE), "records\n")
  # cat("\n")
  
  # ============================================================
  # PHASE 2-3: PARALLEL BRANCH PROCESSING (NEW!)
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  if (CONFIG$parallel$parallel_branches && PARALLEL_INFO$enabled) {
    cat("   PHASE 2-3: PARALLEL BRANCH PROCESSING                     \n")
  } else {
    cat("   PHASE 2-3: SEQUENTIAL BRANCH PROCESSING                   \n")
  }
  cat("==============================================================\n")
  cat("\n")
  
  # Use parallel branch execution wrapper
  branch_results <- run_parallel_branches(
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    output_dir = run_env$run_dir,
    shared_cache = SHARED_CACHE,
    config = CONFIG,
    hydrobasin_names = HYDROBASIN_NAMES,
    vernacular_lookup = VERNACULAR_LOOKUP,
    force_sequential = !CONFIG$parallel$parallel_branches
  )
  
  # Extract results
  result_indigenous <- branch_results$branch_a$result
  indigenous_reports <- branch_results$branch_a$reports
  
  result_non_indigenous <- branch_results$branch_b$result
  non_indigenous_reports <- branch_results$branch_b$reports
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 2-3 COMPLETE                                         \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Branch Execution Summary:\n")
  cat("  Mode:", if(branch_results$parallel_execution) "PARALLEL" else "SEQUENTIAL", "\n")
  cat("  Branch A (Indigenous): %.1f sec\n", branch_results$branch_a$elapsed_seconds)
  cat("  Branch B (Non-indigenous): %.1f sec\n", branch_results$branch_b$elapsed_seconds)
  cat("  Total wall-clock time: %.1f sec\n", branch_results$total_elapsed_seconds)
  if (branch_results$parallel_execution) {
    sequential_est <- branch_results$branch_a$elapsed_seconds + branch_results$branch_b$elapsed_seconds
    cat("  Parallel speedup: %.2fx\n", sequential_est / branch_results$total_elapsed_seconds)
  }
  cat("\n")
  
  # # ============================================================
  # # PRE-PHASE-3 VALIDATION
  # # ============================================================
  # 
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PRE-PHASE-3 VALIDATION                                     \n")
  # cat("==============================================================\n")
  # cat("\n")
  # 
  # cat("Validating data integrity before Phase 3...\n")
  # 
  # # Check indigenous object
  # cat("Indigenous object:\n")
  # cat("  Records:", nrow(result_indigenous$clean_data), "\n")
  # cat("  Species:", length(unique(result_indigenous$clean_data$species)), "\n")
  # cat("  Population types:", paste(unique(result_indigenous$clean_data$population_type), collapse = ", "), "\n")
  # 
  # # Check non-indigenous object
  # cat("\nNon-indigenous object:\n")
  # cat("  Records:", nrow(result_non_indigenous$clean_data), "\n")
  # cat("  Species:", length(unique(result_non_indigenous$clean_data$species)), "\n")
  # cat("  Population types:", paste(unique(result_non_indigenous$clean_data$population_type), collapse = ", "), "\n")
  # cat("  Species list:", paste(unique(result_non_indigenous$clean_data$species), collapse = ", "), "\n")
  # 
  # # Validation checks
  # indigenous_ok <- all(result_indigenous$clean_data$population_type == "indigenous")
  # non_indigenous_ok <- all(result_non_indigenous$clean_data$population_type == "non-indigenous")
  # 
  # if (!indigenous_ok) {
  #   cat("\n❌ ERROR: Indigenous object contains non-indigenous records!\n")
  #   stop("Data corruption detected before Phase 3")
  # }
  # 
  # if (!non_indigenous_ok) {
  #   cat("\n❌ ERROR: Non-indigenous object contains indigenous records!\n")
  #   stop("Data corruption detected before Phase 3")
  # }
  # 
  # cat("\n✓ Validation passed. Proceeding to Phase 3...\n")
  
  # ============================================================
  # PHASE 3: BRANCH B - NON-INDIGENOUS PIPELINE
  # ============================================================
  
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PHASE 3: BRANCH B - NON-INDIGENOUS PROCESSING             \n")
  # cat("==============================================================\n")
  # cat("\n")
  # 
  # # Module 4A: Calculate Metrics & Categorization (local/widespread)
  # cat("\n[MODULE 4A] Calculating non-indigenous metrics...\n")
  # result_non_indigenous <- calculate_non_indigenous_metrics(
  #   result_non_indigenous,
  #   output_dir = run_env$run_dir
  # )
  # 
  # # Module 4B: Spatial Enrichment (NO fragmentation)
  # cat("\n[MODULE 4B] Enriching with spatial layers...\n")
  # result_non_indigenous <- enrich_non_indigenous_spatial(
  #   result_non_indigenous,
  #   output_dir = run_env$run_dir,
  #   cache_dir = SHARED_CACHE,
  #   config = CONFIG,
  #   hydrobasin_names = HYDROBASIN_NAMES
  # )
  # 
  # # Module 4C: Generate Reports
  # cat("\n[MODULE 4C] Generating non-indigenous reports...\n")
  # non_indigenous_reports <- generate_non_indigenous_reports(
  #   result_non_indigenous,
  #   vernacular_lookup = VERNACULAR_LOOKUP,
  #   output_dir = run_env$run_dir
  # )
  # 
  # # ============================================================
  # # PHASE 3 COMPLETE
  # # ============================================================
  # 
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PHASE 3 COMPLETE                                          \n")
  # cat("==============================================================\n")
  # cat("\n")
  # cat("Non-Indigenous Processing Summary:\n")
  # cat("  Species processed:", length(unique(result_non_indigenous$clean_data$species)), "\n")
  # cat("  Local:", sum(result_non_indigenous$clean_data$category == "local", na.rm = TRUE), "records\n")
  # cat("  Widespread:", sum(result_non_indigenous$clean_data$category == "widespread", na.rm = TRUE), "records\n")
  # cat("\n")
  
  # ============================================================
  # PHASE 4: MERGE SCENARIO 3 SPECIES
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 4: MERGE SCENARIO 3 SPECIES                         \n")
  cat("==============================================================\n")
  cat("\n")
  
  cat("\n[MODULE 5] Merging Scenario 3 species (both population types)...\n")
  scenario3_merged <- merge_scenario3_reports(
    scenario_table = scenario_table,
    indigenous_reports = indigenous_reports,
    non_indigenous_reports = non_indigenous_reports,
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    output_dir = run_env$run_dir
  )
  
  # ============================================================
  # PHASE 5B: CANONICAL NARRATIVE GENERATION
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 5B: CANONICAL NARRATIVE GENERATION                  \n")
  cat("==============================================================\n")
  cat("\n")
  
  cat("\n[MODULE 10] Generating canonical geo-narratives (File_S1 template)...\n")
  canonical_narratives <- generate_canonical_narratives(
    scenario_table = scenario_table,
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    indigenous_reports = indigenous_reports,
    non_indigenous_reports = non_indigenous_reports,
    scenario3_merged = scenario3_merged,
    vernacular_lookup = VERNACULAR_LOOKUP,
    output_dir = run_env$run_dir,
    feow_lookup_path = CONFIG$dictionaries$feow,
    hydrobasin_names = HYDROBASIN_NAMES
  )
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 5B COMPLETE - CANONICAL NARRATIVES GENERATED        \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Canonical Narratives:\n")
  cat("  Species processed:", length(canonical_narratives$narratives), "\n")
  cat("  Files per species:\n")
  cat("    - Canonical markdown (full template)\n")
  cat("    - Formal narrative TXT (Section 5)\n")
  cat("    - Formal narrative JSON (Section 5)\n")
  cat("\n")
  
  # ============================================================
  # PHASE 5A: MAP GENERATION
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 5A: MAP GENERATION (SCENARIO-AWARE)                 \n")
  cat("==============================================================\n")
  cat("\n")

  cat("\n[MODULE 8] Generating maps for all scenarios...\n")
  all_maps <- generate_all_maps_seq(
    scenario_table = scenario_table,
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    output_dir = run_env$run_dir,
    cache_dir = SHARED_CACHE,  # ✅ ADD THIS LINE - Use shared cache for global HydroBASINS layers
    formats = CONFIG$reporting$formats,
    bbox_expand_km = CONFIG$spatial$hydro_bbox
  )

  # ============================================================
  # PHASE 5A COMPLETE
  # ============================================================

  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 5A COMPLETE - ALL MAPS GENERATED                    \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Maps Generated:\n")
  cat("  Species with maps:", length(all_maps$maps), "\n")
  cat("  Total map files:", all_maps$summary$total_files, "\n")
  cat("\n")
  cat("Map types per species:\n")
  cat("  - EOO (Extent of Occurrence) - Yellow polygons\n")
  cat("  - AOO (Area of Occupancy) - Yellow grid cells\n")
  cat("  - HydroBASINS - Color-coded by population type\n")
  cat("    * Native: Orange (#D48D00)\n")
  cat("    * Introduced: Purple (#4D0073)\n")
  cat("    * Mixed (Scenario 3): Red-Orange (#FF6600)\n")
  cat("\n")
  cat("Formats: GeoJSON, KML\n")
  cat("\n")
  
  # ============================================================
  # PHASE 5A: MAP GENERATION (PARALLEL SUPPORT)
  # ============================================================
  
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PHASE 5A: MAP GENERATION (SCENARIO-AWARE)                 \n")
  # cat("==============================================================\n")
  # cat("\n")
  # 
  # cat("\n[MODULE 8] Generating maps for all scenarios...\n")
  # all_maps <- generate_all_maps_par(
  #   scenario_table = scenario_table,
  #   result_indigenous = result_indigenous,
  #   result_non_indigenous = result_non_indigenous,
  #   output_dir = run_env$run_dir,
  #   cache_dir = SHARED_CACHE,
  #   formats = CONFIG$reporting$formats,
  #   bbox_expand_km = CONFIG$spatial$hydro_bbox,
  #   parallel_maps = CONFIG$parallel$parallel_maps && PARALLEL_INFO$enabled
  # )
  # 
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PHASE 5A COMPLETE - ALL MAPS GENERATED                    \n")
  # cat("==============================================================\n")
  # cat("\n")
  # cat("Maps Generated:\n")
  # cat("  Species with maps:", length(all_maps$maps), "\n")
  # cat("  Total map files:", all_maps$summary$total_files, "\n")
  # cat("  Time: %.1f seconds\n", all_maps$summary$elapsed_seconds)
  # cat("  Parallel:", all_maps$summary$parallel_enabled, "\n")
  # cat("\n")
  
  # ============================================================
  # PHASE 6: CITATION MANAGEMENT
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 6: CITATION MANAGEMENT                              \n")
  cat("==============================================================\n")
  cat("\n")
  
  # Load methods from summary if not already defined
  if (is.null(methods)) {
    summary_file <- file.path(run_env$run_dir, "reports", "dataset_summary_statistics.json")
    if (file.exists(summary_file)) {
      summary_data <- jsonlite::read_json(summary_file, simplifyVector = TRUE)
      methods <- summary_data$methods
    } else {
      methods <- list(
        taxonomy = "WoRMS via worrms package",
        vernacular = "Manual dictionary",
        continents = "Natural Earth medium scale",
        gadm = "GADM v4.1",
        teow = "ecoregions::worldecoregions",
        feow = "Local FEOW shapefile",
        wdpa = "wdpar package (cached)",
        hydrobasins = "Local HydroBASINS shapefiles (L6/L8/L10)"
      )
    }
  }
  
  cat("\n[MODULE 7] Generating citations for all species...\n")
  all_citations <- generate_all_citations(
    scenario_table = scenario_table,
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    output_dir = run_env$run_dir,
    script_run_time = Sys.time(),
    methods = methods
  )
  
  # ============================================================
  # PHASE 7: SPECIES PACKAGE EXPORT
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 7: SPECIES PACKAGE EXPORT                           \n")
  cat("==============================================================\n")
  cat("\n")
  
  cat("\n[MODULE 9] Creating individual species packages...\n")
  species_packages <- export_species_packages(
    scenario_table = scenario_table,
    all_maps = all_maps,
    canonical_narratives = canonical_narratives,  # ✅ CHANGED
    all_citations = all_citations,
    vernacular_lookup = VERNACULAR_LOOKUP,
    output_dir = run_env$run_dir
  )
  
  # ============================================================
  # PHASE 7 COMPLETE
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 7 COMPLETE - ALL SPECIES PACKAGED                   \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Species Packages:\n")
  cat("  Total species:", length(species_packages$packages), "\n")
  cat("  Total files:", species_packages$summary$total_files, "\n")
  cat("\n")
  cat("Package contents per species:\n")
  cat("  - Maps (GeoJSON + KML)\n")
  cat("  - Narratives (TXT + JSON)\n")
  cat("  - Citations (JSON, BibTeX, CSV, CFF)\n")
  cat("  - Metadata (JSON)\n")
  cat("  - Manifest (CSV)\n")
  cat("  - README (Markdown)\n")
  cat("\n")
  
  # ============================================================
  # FINAL PIPELINE SUMMARY
  # ============================================================
  
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   PIPELINE COMPLETE - ALL PHASES FINISHED                   \n")
  # cat("==============================================================\n")
  # cat("\n")
  # cat("Total Processing Summary:\n")
  # cat("  Species analyzed:", nrow(scenario_table), "\n")
  # cat("  Indigenous-only:", sum(scenario_table$scenario == 1, na.rm = TRUE), "\n")
  # cat("  Non-indigenous-only:", sum(scenario_table$scenario == 2, na.rm = TRUE), "\n")
  # cat("  Combined (Scenario 3):", sum(scenario_table$scenario == 3, na.rm = TRUE), "\n")
  # cat("\n")
  # cat("Outputs Generated:\n")
  # cat("  Reports:", length(indigenous_reports$per_species) + length(non_indigenous_reports$per_species), "\n")
  # cat("  Narratives:", length(canonical_narratives$narratives), "\n")
  # cat("  Citation files:", length(all_citations$citations), "\n")
  # cat("  Map files:", all_maps$summary$total_files, "\n")
  # cat("  Species packages:", length(species_packages$packages), "\n")
  # cat("\n")
  # cat("Output Location:\n")
  # cat("  Run directory:", run_env$run_dir, "\n")
  # cat("  Packages:", file.path(run_env$run_dir, "species_packages"), "\n")
  # cat("\n")
  # cat("==============================================================\n")
  # cat("   SUCCESS - cheCkOVER PIPELINE COMPLETE                     \n")
  # cat("==============================================================\n")
  
  # ============================================================
  # FINAL PIPELINE SUMMARY
  # ============================================================
  
  pipeline_end_time <- Sys.time()
  total_pipeline_time <- difftime(pipeline_end_time, pipeline_start_time, units = "mins")
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PIPELINE COMPLETE - ALL PHASES FINISHED                   \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Total Processing Summary:\n")
  cat("  Species analyzed:", nrow(scenario_table), "\n")
  cat("  Indigenous-only:", sum(scenario_table$scenario == 1, na.rm = TRUE), "\n")
  cat("  Non-indigenous-only:", sum(scenario_table$scenario == 2, na.rm = TRUE), "\n")
  cat("  Combined (Scenario 3):", sum(scenario_table$scenario == 3, na.rm = TRUE), "\n")
  cat("\n")
  cat("Outputs Generated:\n")
  cat("  Reports:", length(indigenous_reports$per_species) + length(non_indigenous_reports$per_species), "\n")
  cat("  Narratives:", length(canonical_narratives$narratives), "\n")
  cat("  Citation files:", length(all_citations$citations), "\n")
  cat("  Map files:", all_maps$summary$total_files, "\n")
  cat("  Species packages:", length(species_packages$packages), "\n")
  cat("\n")
  cat("Performance:\n")
  cat("  Total time: %.1f minutes\n", as.numeric(total_pipeline_time), "\n")
  cat("  Parallel enabled:", PARALLEL_INFO$enabled, "\n")
  cat("  Workers used:", PARALLEL_INFO$workers, "\n")
  cat("\n")
  cat("Output Location:\n")
  cat("  Run directory:", run_env$run_dir, "\n")
  cat("  Packages:", file.path(run_env$run_dir, "species_packages"), "\n")
  cat("\n")
  cat("==============================================================\n")
  cat("   SUCCESS - cheCkOVER PIPELINE COMPLETE                     \n")
  cat("==============================================================\n")
  
  mark_run_complete(run_env$run_dir)
  log_info(">>> PIPELINE COMPLETE - ALL PHASES FINISHED <<<", module = "MAIN")
}
# Shutdown parallel workers
shutdown_parallel()
close_logger()
cat("\nDone! Check the output directory for results.\n\n")