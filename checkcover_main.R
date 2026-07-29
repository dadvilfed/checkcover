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
# -- Framework-version guard ------------------------------------------------
# Refuses to start if this framework_version already has output on disk.
# Prevents silently producing a multi-hour run mislabeled with the wrong
# version (e.g. forgetting to bump framework_version between databases).
# To genuinely re-run an existing version: delete its dir first, then run.
local({
  .fv      <- CONFIG$framework_version
  .fv_dir  <- file.path(CONFIG$root_output_dir, .fv)
  .fv_man  <- file.path(.fv_dir, "checkover", "manifest.json")
  if (file.exists(.fv_man)) {
    stop(sprintf(paste0(
      "\n\n  HALTED: framework_version '%s' already has output.\n",
      "  Found: %s\n\n",
      "  You probably forgot to bump CONFIG$framework_version for a new database.\n",
      "  - New database/version  -> set framework_version to the next number in config.R\n",
      "  - Intentional re-run    -> delete '%s' first, then run again\n"),
      .fv, .fv_man, .fv_dir),
      call. = FALSE)
  }
})
# ---------------------------------------------------------------------------

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
source("R/00_geo_canon.R")
source("R/00_dwc_fields.R")
source("R/00_spatial_sanitize.R")
source("R/00_run_context.R")
source("R/01e_change_detection.R")

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

# Step 4: Setup parallelization
cat("Configuring parallel processing...\n")

library(future)
library(future.apply)

# CRITICAL: Set memory limit for workers
options(future.globals.maxSize = CONFIG$memory$max_worker_memory * 1024^2)

if (CONFIG$parallel$force_sequential) {
  plan(sequential)
  log_info("Parallel processing DISABLED (force_sequential = TRUE)", module = "MAIN")
} else {
  if (CONFIG$parallel$workers == "auto") {
    available_cores <- parallel::detectCores(logical = FALSE)
    workers_to_use <- max(1, available_cores - 1)
  } else {
    workers_to_use <- as.integer(CONFIG$parallel$workers)
  }
  
  if (.Platform$OS.type == "windows") {
    plan(multisession, workers = workers_to_use)
    log_info("Parallel: MULTISESSION with %d workers (Windows)", workers_to_use, module = "MAIN")
  } else {
    plan(multicore, workers = workers_to_use)
    log_info("Parallel: MULTICORE with %d workers (Linux)", workers_to_use, module = "MAIN")
  }
}

# Step 5: Initialize Run Manager
cat("Initializing run manager...\n")

.compute_data_fingerprint <- function(input_file) {
  if (!file.exists(input_file)) return(NA_character_)
  digest::digest(file = input_file, algo = "xxhash64")
}

.find_latest_completed_run <- function(runs_root, base_prefix) {
  if (!dir.exists(runs_root)) return(NULL)
  
  all_dirs <- list.dirs(runs_root, full.names = TRUE, recursive = FALSE)
  pattern  <- paste0("^run_", base_prefix, "(_v\\d+)?$")
  matching <- all_dirs[grepl(pattern, basename(all_dirs))]
  if (length(matching) == 0L) return(NULL)
  
  completed <- matching[file.exists(file.path(matching, "_SUCCESS"))]
  if (length(completed) == 0L) return(NULL)
  
  # Extract version numbers, sort, pick latest
  ver_strings <- sub(paste0("^run_", base_prefix, "_v"), "", basename(completed))
  ver_strings[ver_strings == basename(completed)] <- "0"
  ver_nums <- suppressWarnings(as.integer(ver_strings))
  ver_nums[is.na(ver_nums)] <- 0L
  
  idx <- which.max(ver_nums)
  list(path = completed[idx], run_id = basename(completed[idx]),
       version = ver_nums[idx])
}

.read_stored_fingerprint <- function(run_dir) {
  fp_file <- file.path(run_dir, "_data_fingerprint.txt")
  if (!file.exists(fp_file)) return(NA_character_)
  trimws(readLines(fp_file, n = 1L, warn = FALSE))
}

.write_data_fingerprint <- function(run_dir, fingerprint) {
  writeLines(fingerprint, file.path(run_dir, "_data_fingerprint.txt"))
}

.update_registry <- function(root_output_dir, run_id, input_file, status,
                             run_dir, fingerprint) {
  registry_file <- file.path(root_output_dir, "_registry.json")
  current_reg <- list()
  if (file.exists(registry_file)) {
    try(current_reg <- jsonlite::read_json(registry_file,
                                           simplifyVector = FALSE),
        silent = TRUE)
  }
  current_reg[[length(current_reg) + 1L]] <- list(
    run_id      = run_id,
    timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    input_file  = basename(input_file),
    status      = status,
    fingerprint = fingerprint,
    path        = run_dir
  )
  jsonlite::write_json(current_reg, registry_file, pretty = TRUE,
                       auto_unbox = TRUE)
}

init_run_manager <- function(input_file, config_list, root_output_dir) {
  module <- "RUN_MANAGER"
  
  if (!dir.exists(root_output_dir)) dir.create(root_output_dir, recursive = TRUE)
  
  runs_root <- file.path(root_output_dir, "runs")
  if (!dir.exists(runs_root)) dir.create(runs_root, recursive = TRUE)
  
  base_prefix <- config_list$version %||% "default"
  
  # 1. Fingerprint the input file
  current_fp <- .compute_data_fingerprint(input_file)
  log_info("Input data fingerprint: %s", current_fp, module = module)
  
  # 2. Find latest completed run with this base prefix
  latest <- .find_latest_completed_run(runs_root, base_prefix)
  
  if (is.null(latest)) {
    # ── First run ever ──
    run_id  <- paste0("run_", base_prefix, "_v1")
    run_dir <- file.path(runs_root, run_id)
    
    # Check for incomplete run we can resume
    if (dir.exists(run_dir)) {
      stored_fp <- .read_stored_fingerprint(run_dir)
      if (!is.na(stored_fp) && !is.na(current_fp) && stored_fp == current_fp) {
        log_info("Resuming incomplete run (same data): %s", run_id, module = module)
        .update_registry(root_output_dir, run_id, input_file, "RESUME",
                         run_dir, current_fp)
        return(list(run_id = run_id, run_dir = run_dir, status = "RESUME",
                    data_changed = FALSE,
                    shared_cache_dir = file.path(root_output_dir, "cache")))
      }
    }
    
    log_info("First run: %s", run_id, module = module)
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    .write_data_fingerprint(run_dir, current_fp)
    .update_registry(root_output_dir, run_id, input_file, "NEW",
                     run_dir, current_fp)
    
    return(list(run_id = run_id, run_dir = run_dir, status = "NEW",
                data_changed = FALSE,
                shared_cache_dir = file.path(root_output_dir, "cache")))
  }
  
  # 3. Compare fingerprints against latest completed run
  stored_fp <- .read_stored_fingerprint(latest$path)
  
  if (!is.na(stored_fp) && !is.na(current_fp) && stored_fp == current_fp) {
    # ── Same data → RESUME ──
    log_info("Data unchanged. RESUME mode: %s", latest$run_id, module = module)
    .update_registry(root_output_dir, latest$run_id, input_file, "RESUME",
                     latest$path, current_fp)
    return(list(run_id = latest$run_id, run_dir = latest$path,
                status = "RESUME", data_changed = FALSE,
                shared_cache_dir = file.path(root_output_dir, "cache")))
  }
  
  # ── Data changed → auto-increment ──
  next_v  <- latest$version + 1L
  run_id  <- sprintf("run_%s_v%d", base_prefix, next_v)
  run_dir <- file.path(runs_root, run_id)
  
  log_info("Data CHANGED. New run: %s (was: %s)", run_id, latest$run_id,
           module = module)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  .write_data_fingerprint(run_dir, current_fp)
  .update_registry(root_output_dir, run_id, input_file, "NEW_DATA",
                   run_dir, current_fp)
  
  return(list(run_id = run_id, run_dir = run_dir, status = "NEW",
              data_changed = TRUE,
              shared_cache_dir = file.path(root_output_dir, "cache")))
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
  "R/01_ingest.R",
  "R/01b_vernacular.R",
  #"R/01c_metrics.R",
  "R/01c_split.R", # NEW MODULE
  "R/01d_fragmentation.R",
  "R/01d_scenario_detection.R", #NEW MODULE
  "R/02a_continents.R",
  "R/02b_gadm.R",
  "R/02c_geo_integrity.R",
  "R/02c_teow.R",
  "R/02d_feow.R",
  "R/02e_wdpa.R",
  "R/02f_hydrobasins.R",
  "R/03a_metrics_indigenous.R", #NEW MODULE
  "R/03b_enrich_indigenous.R", #NEW MODULE  
  "R/03c_reports_indigenous.R", #NEW MODULE
  "R/04_reports.R",
  "R/04a_metrics_non_indigenous.R", #NEW MODULE
  "R/04b_enrich_non_indigenous.R", #NEW MODULE
  "R/04c_reports_non_indigenous.R", #NEW MODULE
  "R/05_narratives.R",
  "R/05_merge_scenario3.R", # NEW MODULE
  "R/06_citations.R",
  "R/06_narratives.R", #NEW MODULE
  "R/07_export.R",
  "R/07_citations.R", #NEW MODULE
  "R/08_maps.R", #NEW MODULE
  "R/09_package_export.R", #NEW MODULE
  "R/10_canonical_narratives.R",
  "R/11_temporal_delta.R",       # TEMPORAL: Phase 1 core functions
  "R/12_temporal_outputs.R",     # TEMPORAL: Phase 2 rendering + maps
  "R/13_temporal_pipeline.R"     # TEMPORAL: Phase 3 per-species wrapper
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
  # Table_S3 has FIVE columns: the 5th (river_name) is the finest level-10 name
  # (e.g. Crișul Alb, Poprad, Casimcea). The old 4-name force renamed it to NA,
  # discarding the river granularity and collapsing endemics to their coarse
  # basin (Lucian, 2026-07). Name all present columns; keep river_name.
  base_names <- c("Basin_level", "HYBAS_ID", "Basin_name", "Subbasin_name", "river_name")
  names(hb_names)[seq_len(min(ncol(hb_names), length(base_names)))] <-
    base_names[seq_len(min(ncol(hb_names), length(base_names)))]
  if (!"river_name" %in% names(hb_names)) hb_names$river_name <- NA_character_
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

if (run_env$status %in% c("NEW", "RESUME")) {
  # ==============================================================
  #    STARTING ANALYSIS PIPELINE                                
  # ==============================================================
  
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
  
  # ── Sparse-versioning bootstrap (Session 3, refactor) ──────────────────────
  # Initialize the RunContext for downstream phase coordination. After this
  # block, ctx$all_species is populated and clean_occurrences.tsv has been
  # mirrored to the scaffolding dir for Phase 1.5 to read.
  ctx <- RunContext_init(CONFIG, run_id = run_env$run_id)
  ctx$all_species <- sort(unique(result$clean_data$species[!is.na(result$clean_data$species)]))
  
  if (!dir.exists(ctx$current_scaffolding_dir)) {
    dir.create(ctx$current_scaffolding_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(file.path(run_env$run_dir, "clean_occurrences.tsv"),
            file.path(ctx$current_scaffolding_dir, "clean_occurrences.tsv"),
            overwrite = TRUE)
  
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
  
  # Module 2B: GADM (BEFORE split) âœ… MOVED HERE
  cat("\n[MODULE 2B] Enriching with GADM (countries & admin units)...\n")
  result <- enrich_with_gadm(result, output_dir = run_env$run_dir, cache_dir = SHARED_CACHE)

  # Module 2C: Geographic integrity report (native vs fallback vs unresolved,
  # rejected nearest-land snaps, canonical-vocabulary compliance). Runs on the
  # combined dataset before the population split so the accounting covers every
  # record exactly once.
  cat("\n[MODULE 2C] Geographic integrity report...\n")
  result <- report_geographic_integrity(result, output_dir = run_env$run_dir)

  # ============================================================
  # PHASE 1: SPLIT INTO TWO BRANCHES
  # ============================================================
  
  # Module 1C: Split by Population Status
  cat("\n[MODULE 1C] Splitting data by population status...\n")
  split_results <- split_by_population(result, output_dir = run_env$run_dir)
  
  result_indigenous <- split_results$result_indigenous
  result_non_indigenous <- split_results$result_non_indigenous
  result <- split_results$result_combined  # âœ… Update original result with population_type
  
  # Module 1D: Detect Species Scenarios
  cat("\n[MODULE 1D] Detecting species scenarios...\n")
  scenario_table <- detect_species_scenarios(result, output_dir = run_env$run_dir)
  
  # Save scenario table for later use
  saveRDS(scenario_table, file.path(run_env$run_dir, "scenario_table.rds"))
  
  # ============================================================
  # PHASE 1.5: CHANGE DETECTION (sparse-versioning gate)
  # ============================================================
  #
  # From here on, all downstream modules see only species in ctx$active_species.
  # This is the change-detection cutoff — unchanged species exit the pipeline
  # here and have their artifacts referenced from a prior version via the
  # manifest written by Module 9.
  
  cat("\n[PHASE 1.5] Detecting per-species changes...\n")
  ctx <- detect_species_changes(ctx)
  
  n_total   <- length(ctx$all_species)
  n_active  <- length(ctx$active_species)
  n_skipped <- n_total - n_active
  log_info(sprintf(
    "Active species cutoff applied: %d of %d species will be processed downstream. Skipped: %d (unchanged since prior snapshot).",
    n_active, n_total, n_skipped), module = "MAIN")

  # Loud diagnostic for the "nothing to do" case. With 0 active species every
  # downstream module receives an empty cohort — the run will complete but
  # produce no new packages (all inherited from the prior version). If you
  # EXPECTED changes (e.g. new extinction claims in this snapshot), this almost
  # always means the current input is fingerprint-identical to the prior version
  # for every species — verify the input file actually changed and that the
  # extinction/occurrence columns are populated. See <v>/checkover/fingerprints/.
  if (n_active == 0L) {
    log_warn(paste0(
      "0 active species: every species is fingerprint-identical to the prior ",
      "version. No new packages will be generated this run. If you expected ",
      "changes, confirm the input differs from the prior snapshot."),
      module = "MAIN")
    cat("\n[!] 0 active species — nothing changed vs the prior version. ",
        "Run will finish without generating new packages.\n", sep = "")
  }
  
  # Apply the filter to the inputs every downstream module reads from.
  # Modules themselves are unchanged — they process whatever is in their input.
  # IMPORTANT: each `result_*` object carries BOTH clean_data (data.frame) AND
  # clean_sf (paired sf object with the same row count). They must be filtered
  # in lockstep — otherwise Module 2C/2D/2E/2F crash with row-count mismatches
  # because they intersect on clean_sf but write back to clean_data.
  if (n_active < n_total) {
    keep_df <- function(df, col = "species") df[df[[col]] %in% ctx$active_species, , drop = FALSE]
    keep_sf <- function(sf_obj, col = "species") sf_obj[sf_obj[[col]] %in% ctx$active_species, , drop = FALSE]
    
    result$clean_data                  <- keep_df(result$clean_data)
    result$clean_sf                    <- keep_sf(result$clean_sf)
    result_indigenous$clean_data       <- keep_df(result_indigenous$clean_data)
    result_indigenous$clean_sf         <- keep_sf(result_indigenous$clean_sf)
    result_non_indigenous$clean_data   <- keep_df(result_non_indigenous$clean_data)
    result_non_indigenous$clean_sf     <- keep_sf(result_non_indigenous$clean_sf)
    scenario_table                     <- keep_df(scenario_table)
    
    saveRDS(scenario_table, file.path(run_env$run_dir, "scenario_table.rds"))
  }
  
  # ── Apply extinction masking BEFORE Phase 2 metrics computation ────────────
  # Each species' is_extinct=TRUE records become temporal_status="extinct";
  # records within 500m and predating the extinction become "suppressed".
  # Downstream metrics modules (3A/3C/4A/4C) filter on temporal_status==
  # "active" so extinct + suppressed records don't pollute AOO/EOO/counts.
  cat("\n[EXTINCTION MASKING] Applying 500m suppression mask per active species...\n")
  masked <- apply_extinction_masking_to_branches(
    result_indigenous     = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    active_species        = ctx$active_species
  )
  result_indigenous     <- masked$result_indigenous
  result_non_indigenous <- masked$result_non_indigenous
  rm(masked)
  
  # ============================================================
  # PHASE 2: BRANCH A - INDIGENOUS PIPELINE
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 2: BRANCH A - INDIGENOUS PROCESSING                 \n")
  cat("==============================================================\n")
  cat("\n")
  
  # Module 3A: Calculate Metrics & IUCN Categorization
  cat("\n[MODULE 3A] Calculating indigenous metrics...\n")
  result_indigenous <- calculate_indigenous_metrics(
    result_indigenous,
    output_dir = run_env$run_dir
  )
  
  # Module 3B: Fragmentation Analysis (endemic/regional only)
  cat("\n[MODULE 3B] Analyzing fragmentation (endemic/regional only)...\n")
  result_indigenous <- analyze_fragmentation(
    result_indigenous,
    output_dir = run_env$run_dir,
    category_filter = c("endemic", "regional")  # Skip cosmopolitan
  )
  
  # Module 3C: Spatial Enrichment
  cat("\n[MODULE 3C] Enriching with spatial layers...\n")
  result_indigenous <- enrich_indigenous_spatial(
    result_indigenous,
    output_dir = run_env$run_dir,
    cache_dir = SHARED_CACHE,
    config = CONFIG,
    hydrobasin_names = HYDROBASIN_NAMES
  )
  
  # Module 3D: Generate Reports
  cat("\n[MODULE 3D] Generating indigenous reports...\n")
  indigenous_reports <- generate_indigenous_reports(
    result_indigenous,
    vernacular_lookup = VERNACULAR_LOOKUP,
    output_dir = run_env$run_dir
  )
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 2 COMPLETE                                          \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Indigenous Processing Summary:\n")
  cat("  Species processed:", length(unique(result_indigenous$clean_data$species)), "\n")
  cat("  Endemic:", sum(result_indigenous$clean_data$iucn_category == "endemic", na.rm = TRUE), "records\n")
  cat("  Regional:", sum(result_indigenous$clean_data$iucn_category == "regional", na.rm = TRUE), "records\n")
  cat("  Cosmopolitan:", sum(result_indigenous$clean_data$iucn_category == "cosmopolitan", na.rm = TRUE), "records\n")
  cat("\n")
  
  # ============================================================
  # PRE-PHASE-3 VALIDATION
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PRE-PHASE-3 VALIDATION                                     \n")
  cat("==============================================================\n")
  cat("\n")
  
  cat("Validating data integrity before Phase 3...\n")
  
  # Check indigenous object
  cat("Indigenous object:\n")
  cat("  Records:", nrow(result_indigenous$clean_data), "\n")
  cat("  Species:", length(unique(result_indigenous$clean_data$species)), "\n")
  cat("  Population types:", paste(unique(result_indigenous$clean_data$population_type), collapse = ", "), "\n")
  
  # Check non-indigenous object
  cat("\nNon-indigenous object:\n")
  cat("  Records:", nrow(result_non_indigenous$clean_data), "\n")
  cat("  Species:", length(unique(result_non_indigenous$clean_data$species)), "\n")
  cat("  Population types:", paste(unique(result_non_indigenous$clean_data$population_type), collapse = ", "), "\n")
  cat("  Species list:", paste(unique(result_non_indigenous$clean_data$species), collapse = ", "), "\n")
  
  # Validation checks
  indigenous_ok <- all(result_indigenous$clean_data$population_type == "indigenous")
  non_indigenous_ok <- all(result_non_indigenous$clean_data$population_type == "non-indigenous")
  
  if (!indigenous_ok) {
    cat("\nâŒ ERROR: Indigenous object contains non-indigenous records!\n")
    stop("Data corruption detected before Phase 3")
  }
  
  if (!non_indigenous_ok) {
    cat("\nâŒ ERROR: Non-indigenous object contains indigenous records!\n")
    stop("Data corruption detected before Phase 3")
  }
  
  cat("\nâœ“ Validation passed. Proceeding to Phase 3...\n")
  
  # ============================================================
  # PHASE 3: BRANCH B - NON-INDIGENOUS PIPELINE
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 3: BRANCH B - NON-INDIGENOUS PROCESSING             \n")
  cat("==============================================================\n")
  cat("\n")
  
  # Module 4A: Calculate Metrics & Categorization (local/widespread)
  cat("\n[MODULE 4A] Calculating non-indigenous metrics...\n")
  result_non_indigenous <- calculate_non_indigenous_metrics(
    result_non_indigenous,
    output_dir = run_env$run_dir
  )
  
  # Module 4B: Spatial Enrichment (NO fragmentation)
  cat("\n[MODULE 4B] Enriching with spatial layers...\n")
  result_non_indigenous <- enrich_non_indigenous_spatial(
    result_non_indigenous,
    output_dir = run_env$run_dir,
    cache_dir = SHARED_CACHE,
    config = CONFIG,
    hydrobasin_names = HYDROBASIN_NAMES
  )
  
  # Module 4C: Generate Reports
  cat("\n[MODULE 4C] Generating non-indigenous reports...\n")
  non_indigenous_reports <- generate_non_indigenous_reports(
    result_non_indigenous,
    vernacular_lookup = VERNACULAR_LOOKUP,
    output_dir = run_env$run_dir
  )
  
  # ============================================================
  # PHASE 3 COMPLETE
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 3 COMPLETE                                          \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Non-Indigenous Processing Summary:\n")
  cat("  Species processed:", length(unique(result_non_indigenous$clean_data$species)), "\n")
  cat("  Local:", sum(result_non_indigenous$clean_data$category == "local", na.rm = TRUE), "records\n")
  cat("  Widespread:", sum(result_non_indigenous$clean_data$category == "widespread", na.rm = TRUE), "records\n")
  cat("\n")
  
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
  # PIPELINE SUMMARY
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PIPELINE SUMMARY - ALL PHASES COMPLETE                    \n")
  cat("==============================================================\n")
  cat("\n")
  cat("Data Processing:\n")
  cat("  Total species:", length(unique(c(result_indigenous$clean_data$species, 
                                          result_non_indigenous$clean_data$species))), "\n")
  cat("  Total records:", nrow(result_indigenous$clean_data) + nrow(result_non_indigenous$clean_data), "\n")
  cat("\n")
  cat("Population Analysis:\n")
  cat("  Scenario 1 (Indigenous only):", sum(scenario_table$scenario == 1, na.rm = TRUE), "species\n")
  cat("  Scenario 2 (Non-indigenous only):", sum(scenario_table$scenario == 2, na.rm = TRUE), "species\n")
  cat("  Scenario 3 (Both):", sum(scenario_table$scenario == 3, na.rm = TRUE), "species\n")
  cat("\n")
  cat("Indigenous Reports:\n")
  cat("  Per-species:", length(indigenous_reports$per_species), "files\n")
  cat("  Group summary: 1 file\n")
  cat("\n")
  cat("Non-Indigenous Reports:\n")
  cat("  Per-species:", length(non_indigenous_reports$per_species), "files\n")
  cat("  Group summary: 1 file\n")
  cat("\n")
  cat("Merged Reports (Scenario 3):\n")
  cat("  Merged species:", length(scenario3_merged$merged_species), "files\n")
  cat("\n")
  
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
    hydrobasin_names = HYDROBASIN_NAMES,
    # Bind narrative front-matter version to the run's framework_version — never
    # let it default to "1.0"/"v1.0" and never carry a stale label forward
    # (Lucian bug 0). package_metadata / SEB already use this same value.
    checkover_version = ctx$framework_version,
    output_version    = paste0("v", ctx$framework_version),
    # Pass the RunContext so the narrative trend reads the SAME prior-version AOO
    # as package_metadata.json (single-source trend, Lucian bug 5).
    ctx               = ctx
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
  # PHASE 5C: TEMPORAL CHANGE DETECTION (per-species, selective)
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 5C: TEMPORAL CHANGE DETECTION                       \n")
  cat("==============================================================\n")
  cat("\n")
  
  log_info("=== PHASE 5C: TEMPORAL CHANGE DETECTION ===", module = "MAIN")
  
  # Source temporal modules (11, 12, 13 should already be in module_files,
  # but guard against double-sourcing)
  if (!exists("compare_species_to_baseline", mode = "function")) {
    source("R/11_temporal_delta.R")
    source("R/12_temporal_outputs.R")
    source("R/13_temporal_pipeline.R")
  }
  
  temporal_baselines <- 0L
  temporal_updates   <- 0L
  temporal_skipped   <- 0L
  temporal_errors    <- 0L
  species_skipped_by_temporal <- character(0)
  
  all_species_temporal <- unique(scenario_table$species)
  all_species_temporal <- all_species_temporal[!is.na(all_species_temporal)]
  
  for (sp in all_species_temporal) {
    sp_clean <- clean_species_name(sp)
    scn <- scenario_table$scenario[scenario_table$species == sp][1]
    
    # Locate the canonical markdown from Module 10. Module 10 writes to
    # <run_dir>/narratives_canonical/<make_package_id(sp)>_canonical.md — this
    # lookup previously used the wrong directory ("canonical") AND a different
    # slug function (clean_species_name), so it never found the file and every
    # species was skipped ("No canonical MD ..." → 0 temporal baselines).
    canonical_slug <- make_package_id(sp)
    canonical_path <- file.path(run_env$run_dir, "narratives_canonical",
                                paste0(canonical_slug, "_canonical.md"))
    if (!file.exists(canonical_path)) {
      # Back-compat: also try the legacy location/slug in case of an old run dir.
      legacy_path <- file.path(run_env$run_dir, "canonical",
                               paste0(sp_clean, "_canonical.md"))
      if (file.exists(legacy_path)) {
        canonical_path <- legacy_path
      } else {
        log_warn("No canonical MD for %s (looked in narratives_canonical/), skipping temporal.",
                 sp, module = "MAIN")
        next
      }
    }
    baseline_md <- paste(readLines(canonical_path), collapse = "\n")
    
    # Pull current records for this species from the pipeline result objects
    ind_occ <- if (scn %in% c(1L, 3L)) {
      result_indigenous$clean_data %>% dplyr::filter(species == sp)
    } else NULL
    ni_occ <- if (scn %in% c(2L, 3L)) {
      result_non_indigenous$clean_data %>% dplyr::filter(species == sp)
    } else NULL
    
    # ── Per-species comparison against saved baseline ──
    art <- detect_prior_artifacts(sp_clean, CONFIG$root_output_dir)
    
    if (art$artifacts_exist) {
      # Load the previous snapshot for comparison
      prev <- tryCatch(
        load_previous_version(sp_clean, art$latest_version, CONFIG$root_output_dir),
        error = function(e) NULL
      )
      
      if (!is.null(prev)) {
        # Combine current records for comparison
        current_all <- dplyr::bind_rows(ind_occ, ni_occ)
        comparison  <- compare_species_to_baseline(current_all,
                                                   prev$occurrences_previous)
        
        if (comparison$status == "unchanged") {
          # ── Data identical: SKIP — no new version created ──
          species_skipped_by_temporal <- c(species_skipped_by_temporal, sp)
          temporal_skipped <- temporal_skipped + 1L
          next
        }
        
        log_info("  %s: %s", sp, comparison$details, module = "MAIN")
      }
    }
    
    # ── Process temporal (baseline for new species, or update for changed) ──
    res <- tryCatch({
      process_species_temporal(
        species_clean              = sp_clean,
        scenario                   = scn,
        occurrences_indigenous     = ind_occ,
        occurrences_non_indigenous = ni_occ,
        baseline_canonical_md      = baseline_md,
        root_output_dir            = CONFIG$root_output_dir
      )
    }, error = function(e) {
      log_error("Temporal FAILED for %s: %s", sp, conditionMessage(e),
                module = "MAIN")
      temporal_errors <<- temporal_errors + 1L
      NULL
    })
    
    if (!is.null(res)) {
      if (res$is_baseline) temporal_baselines <- temporal_baselines + 1L
      else                 temporal_updates   <- temporal_updates   + 1L
    }
    
    # Per-species GC
    rm(res); gc(verbose = FALSE)
  }
  
  log_info("Phase 5C complete: %d baselines, %d updates, %d skipped, %d errors.",
           temporal_baselines, temporal_updates, temporal_skipped, temporal_errors,
           module = "MAIN")
  
  cat("\nPhase 5C Summary:\n")
  cat("  Baselines (new species):", temporal_baselines, "\n")
  cat("  Updates (data changed):", temporal_updates, "\n")
  cat("  Skipped (data unchanged):", temporal_skipped, "\n")
  cat("  Errors:", temporal_errors, "\n")
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
  all_maps <- generate_all_maps(
    scenario_table = scenario_table,
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    output_dir = run_env$run_dir,
    cache_dir = SHARED_CACHE,  # âœ… ADD THIS LINE - Use shared cache for global HydroBASINS layers
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
  # PHASE 6: CITATION MANAGEMENT
  # ============================================================
  
  cat("\n")
  cat("==============================================================\n")
  cat("   PHASE 6: CITATION MANAGEMENT                              \n")
  cat("==============================================================\n")
  cat("\n")
  
  methods <- NULL   # ← ADD THIS LINE

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
  
  # Pass temporal record tags if available (from Phase 5C)
  # On first run with no temporal changes, this is an empty data.frame
  # → all references tagged "baseline".
  temporal_tags_for_citations <- if (exists("temporal_record_tags") &&
                                     is.data.frame(temporal_record_tags) &&
                                     nrow(temporal_record_tags) > 0L) {
    temporal_record_tags
  } else NULL
  
  all_citations <- generate_all_citations(
    scenario_table = scenario_table,
    result_indigenous = result_indigenous,
    result_non_indigenous = result_non_indigenous,
    output_dir = run_env$run_dir,
    script_run_time = Sys.time(),
    methods = methods,
    temporal_record_tags = temporal_tags_for_citations
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
    ctx = ctx,
    scenario_table = scenario_table,
    all_maps = all_maps,
    canonical_narratives = canonical_narratives,  # ✅ CHANGED
    all_citations = all_citations,
    vernacular_lookup = VERNACULAR_LOOKUP,
    reports_dir = run_env$run_dir
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
  if (isTRUE(CONFIG$temporal$enabled) && exists("temporal_results_summary")) {
    cat("  Temporal baselines (v1.0):", temporal_results_summary$baselines_created, "\n")
    cat("  Temporal updates (v1.X):", temporal_results_summary$updates_created, "\n")
    if (length(temporal_results_summary$errors) > 0L) {
      cat("  Temporal errors:", length(temporal_results_summary$errors), "\n")
    }
  }
  cat("\n")
  cat("Output Location:\n")
  cat("  Run directory:", run_env$run_dir, "\n")
  cat("  Packages:", ctx$current_version_dir, "\n")
  cat("  Manifest:", file.path(ctx$current_scaffolding_dir, "manifest.json"), "\n")  
  if (isTRUE(CONFIG$temporal$enabled)) {
    cat("  Temporal:", file.path(CONFIG$root_output_dir, "temporal"), "\n")
  }
  cat("\n")
  cat("==============================================================\n")
  cat("   SUCCESS - cheCkOVER PIPELINE COMPLETE                     \n")
  cat("==============================================================\n")
  
  mark_run_complete(run_env$run_dir)
  log_info(">>> PIPELINE COMPLETE - ALL PHASES FINISHED <<<", module = "MAIN")
}
close_logger()
cat("\nDone! Check the output directory for results.\n\n")