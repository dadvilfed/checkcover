#### MODULE 1D: SCENARIO DETECTION ####

#' Detect which scenario each species belongs to
#' @param result List with clean_data containing population_type column
#' @param output_dir Output directory for scenario table
#' @return Data frame with species scenarios
detect_species_scenarios <- function(result, output_dir = "checkover_output") {
  module <- "MODULE1D_SCENARIOS"
  
  with_log_section(module, {
    log_info("=== MODULE 1D: SPECIES SCENARIO DETECTION ===", module = module)
    
    cd <- result$clean_data
    
    if (nrow(cd) == 0) {
      log_warn("No data for scenario detection.", module = module)
      return(data.frame(
        species = character(),
        scenario = integer(),
        indigenous = integer(),
        `non-indigenous` = integer(),
        total_records = integer(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    
    if (!"population_type" %in% names(cd)) {
      log_error("Data missing 'population_type' column. Run split module first.", 
                module = module)
      stop("Missing population_type column.")
    }
    
    log_info("Creating scenario table for %d species...",
             length(unique(cd$species)),
             module = module)
    
    # Create scenario table
    scenario_table <- create_scenario_table(cd)
    
    # Summary statistics
    scenario_counts <- table(scenario_table$scenario)
    
    log_info("=== SCENARIO SUMMARY ===", module = module)
    log_info("Scenario 1 (Indigenous only): %d species",
             scenario_counts["1"] %||% 0,
             module = module)
    log_info("Scenario 2 (Non-indigenous only): %d species",
             scenario_counts["2"] %||% 0,
             module = module)
    log_info("Scenario 3 (Both): %d species",
             scenario_counts["3"] %||% 0,
             module = module)
    
    # Detailed breakdown
    if ("3" %in% names(scenario_counts) && scenario_counts["3"] > 0) {
      scenario3_species <- scenario_table[scenario_table$scenario == 3, ]
      log_info("Scenario 3 details:", module = module)
      log_info("  - Total records: %d indigenous + %d non-indigenous",
               sum(scenario3_species$indigenous),
               sum(scenario3_species$`non-indigenous`),
               module = module)
      log_info("  - Avg records per species: %.1f indigenous, %.1f non-indigenous",
               mean(scenario3_species$indigenous),
               mean(scenario3_species$`non-indigenous`),
               module = module)
    }
    
    # Save scenario table
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    scenario_file <- file.path(output_dir, "species_scenario_table.tsv")
    write_tsv(scenario_table, scenario_file)
    
    log_info("Saved scenario table to: %s", scenario_file, module = module)
    
    # Save JSON summary
    scenario_json <- list(
      total_species = nrow(scenario_table),
      scenario_1_count = scenario_counts["1"] %||% 0,
      scenario_2_count = scenario_counts["2"] %||% 0,
      scenario_3_count = scenario_counts["3"] %||% 0,
      scenario_1_species = get_species_by_scenario(scenario_table, 1),
      scenario_2_species = get_species_by_scenario(scenario_table, 2),
      scenario_3_species = get_species_by_scenario(scenario_table, 3)
    )
    
    jsonlite::write_json(
      scenario_json,
      file.path(output_dir, "species_scenarios_summary.json"),
      pretty = TRUE,
      auto_unbox = TRUE
    )
    
    log_info("Scenario detection complete.", module = module)
    
    return(scenario_table)
  })
}