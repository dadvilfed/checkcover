#### MODULE 5: MERGE SCENARIO 3 SPECIES (BOTH POPULATIONS) ####

#' Merge indigenous and non-indigenous reports for Scenario 3 species
#' @param scenario_table Scenario detection table from Module 1D
#' @param indigenous_reports Reports from Branch A
#' @param non_indigenous_reports Reports from Branch B
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @return List of merged reports
merge_scenario3_reports <- function(scenario_table,
                                    indigenous_reports,
                                    non_indigenous_reports,
                                    result_indigenous,
                                    result_non_indigenous,
                                    output_dir = "checkover_output") {
  module <- "MODULE5_MERGE"
  
  with_log_section(module, {
    log_info("=== MODULE 5: MERGE SCENARIO 3 SPECIES ===", module = module)
    
    # Identify Scenario 3 species
    scenario3_species <- scenario_table$species[scenario_table$scenario == 3]
    scenario3_species <- scenario3_species[!is.na(scenario3_species)]
    
    if (length(scenario3_species) == 0) {
      log_info("No Scenario 3 species detected (no species with both population types).", 
               module = module)
      log_info("Skipping merge - all species are either fully indigenous or fully non-indigenous.", 
               module = module)
      return(list(
        merged_species = character(0),
        reports = list(),
        summary = list(
          total_merged = 0,
          note = "No species with both indigenous and non-indigenous populations found"
        )
      ))
    }
    
    log_info("Found %d Scenario 3 species (both indigenous AND non-indigenous):", 
             length(scenario3_species), module = module)
    for (sp in scenario3_species) {
      log_info("  - %s", sp, module = module)
    }
    
    # Create merged reports directory
    reports_dir <- file.path(output_dir, "reports", "merged_scenario3")
    if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
    
    merged_reports <- list()
    
    # Process each Scenario 3 species
    for (sp in scenario3_species) {
      log_info("Merging reports for: %s", sp, module = module)
      
      sp_clean <- make_package_id(sp)
      
      # Load indigenous report
      ind_report_file <- file.path(output_dir, "reports", "indigenous", paste0(sp_clean, ".json"))
      ind_report <- NULL
      if (file.exists(ind_report_file)) {
        ind_report <- jsonlite::read_json(ind_report_file, simplifyVector = TRUE)
      } else {
        log_warn("  Indigenous report not found for %s", sp, module = module)
      }
      
      # Load non-indigenous report
      non_ind_report_file <- file.path(output_dir, "reports", "non_indigenous", paste0(sp_clean, ".json"))
      non_ind_report <- NULL
      if (file.exists(non_ind_report_file)) {
        non_ind_report <- jsonlite::read_json(non_ind_report_file, simplifyVector = TRUE)
      } else {
        log_warn("  Non-indigenous report not found for %s", sp, module = module)
      }
      
      if (is.null(ind_report) && is.null(non_ind_report)) {
        log_error("  No reports found for %s. Skipping.", sp, module = module)
        next
      }
      
      # Extract data from both branches
      ind_data <- if (!is.null(result_indigenous$clean_data)) {
        result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
      } else NULL
      
      non_ind_data <- if (!is.null(result_non_indigenous$clean_data)) {
        result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
      } else NULL
      
      # Build merged report
      merged_report <- list(
        species = sp,
        scenario = 3,
        population_types = "both (indigenous + non-indigenous)",
        vernacular_names = ind_report$vernacular_names %||% non_ind_report$vernacular_names,
        
        # Indigenous population (native range)
        indigenous_population = if (!is.null(ind_report)) {
          list(
            present = TRUE,
            metrics = ind_report$metrics,
            temporal = ind_report$temporal,
            spatial_context = ind_report$spatial_context,
            counts = ind_report$counts,
            conservation = ind_report$conservation,
            spatial_clustering = ind_report$spatial_clustering %||% ind_report$fragmentation
          )
        } else {
          list(present = FALSE)
        },
        
        # Non-indigenous population (invaded range)
        non_indigenous_population = if (!is.null(non_ind_report)) {
          list(
            present = TRUE,
            metrics = non_ind_report$metrics,
            temporal = non_ind_report$temporal,
            spatial_context = non_ind_report$spatial_context,
            counts = non_ind_report$counts,
            conservation = non_ind_report$conservation,
            notes = non_ind_report$notes
          )
        } else {
          list(present = FALSE)
        },
        
        # Combined statistics
        combined_summary = list(
          total_records = (if (!is.null(ind_data)) nrow(ind_data) else 0) + 
            (if (!is.null(non_ind_data)) nrow(non_ind_data) else 0),
          indigenous_records = if (!is.null(ind_data)) nrow(ind_data) else 0,
          non_indigenous_records = if (!is.null(non_ind_data)) nrow(non_ind_data) else 0,
          
          total_countries = length(unique(c(
            if (!is.null(ind_data)) ind_data$country else character(0),
            if (!is.null(non_ind_data)) non_ind_data$country else character(0)
          ))),
          
          total_continents = length(unique(c(
            if (!is.null(ind_data)) ind_data$continents else character(0),
            if (!is.null(non_ind_data)) non_ind_data$continents else character(0)
          ))),
          
          invasion_status = list(
            native_countries = if (!is.null(ind_data)) {
              length(unique(ind_data$country[!is.na(ind_data$country)]))
            } else 0,
            invaded_countries = if (!is.null(non_ind_data)) {
              length(unique(non_ind_data$country[!is.na(non_ind_data$country)]))
            } else 0
          )
        )
      )
      
      # Save merged report
      merged_file <- file.path(reports_dir, paste0(sp_clean, "_merged.json"))
      jsonlite::write_json(merged_report, merged_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
      
      merged_reports[[sp]] <- merged_file
      log_info("  Saved merged report to: %s", basename(merged_file), module = module)
    }
    
    # Create summary
    summary_report <- list(
      scenario = 3,
      description = "Species with both indigenous and non-indigenous populations",
      total_species = length(scenario3_species),
      species_list = scenario3_species,
      reports_generated = length(merged_reports)
    )
    
    summary_file <- file.path(reports_dir, "scenario3_summary.json")
    jsonlite::write_json(summary_report, summary_file, pretty = TRUE, auto_unbox = TRUE)
    
    log_info("Merged %d Scenario 3 species reports.", length(merged_reports), module = module)
    log_info("Saved summary to: %s", summary_file, module = module)
    
    return(list(
      merged_species = scenario3_species,
      reports = merged_reports,
      summary = summary_report
    ))
  })
}