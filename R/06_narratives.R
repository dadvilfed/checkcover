#### MODULE 6: NARRATIVE GENERATION (ALL SCENARIOS) ####

#' Generate comprehensive narratives for all species
#' @param scenario_table Scenario detection table
#' @param indigenous_reports Indigenous reports list
#' @param non_indigenous_reports Non-indigenous reports list
#' @param scenario3_merged Merged Scenario 3 reports
#' @param vernacular_lookup Vernacular names lookup
#' @param output_dir Output directory
#' @param feow_lookup_path Path to FEOW name lookup
#' @param hydrobasin_names HydroBASINS name lookup table
#' @return List of generated narratives
generate_all_narratives <- function(scenario_table,
                                    indigenous_reports,
                                    non_indigenous_reports,
                                    scenario3_merged,
                                    vernacular_lookup = NULL,
                                    output_dir = "checkover_output",
                                    feow_lookup_path = NULL,
                                    hydrobasin_names = NULL) {
  module <- "MODULE6_NARRATIVES"
  
  with_log_section(module, {
    log_info("=== MODULE 6: NARRATIVE GENERATION (ALL SCENARIOS) ===", module = module)
    
    # Create narratives directory
    narratives_dir <- file.path(output_dir, "narratives")
    if (!dir.exists(narratives_dir)) dir.create(narratives_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Load FEOW lookup if available
    feow_map <- NULL
    if (!is.null(feow_lookup_path) && file.exists(feow_lookup_path)) {
      log_info("Loading FEOW name lookup from: %s", feow_lookup_path, module = module)
      ext <- tolower(tools::file_ext(feow_lookup_path))
      tryCatch({
        if (ext %in% c("xlsx", "xls")) {
          if (requireNamespace("readxl", quietly = TRUE)) {
            feow_map <- as.data.frame(readxl::read_excel(feow_lookup_path))
          }
        } else {
          feow_map <- read.csv(feow_lookup_path, stringsAsFactors = FALSE)
        }
        if (!is.null(feow_map)) {
          names(feow_map) <- toupper(names(feow_map))
          if ("ID" %in% names(feow_map) && "ECOREGION" %in% names(feow_map)) {
            feow_map$ID <- as.integer(feow_map$ID)
          }
        }
      }, error = function(e) {
        log_warn("Failed to load FEOW lookup: %s", conditionMessage(e), module = module)
      })
    }
    
    # Extract vernacular lookup
    vern_map <- NULL
    if (!is.null(vernacular_lookup)) {
      if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup)) {
        vern_map <- vernacular_lookup$wide
      } else if (is.data.frame(vernacular_lookup)) {
        vern_map <- vernacular_lookup
      }
    }
    
    all_narratives <- list()
    
    # --- SCENARIO 1: INDIGENOUS ONLY ---
    scenario1_species <- scenario_table$species[scenario_table$scenario == 1]
    scenario1_species <- scenario1_species[!is.na(scenario1_species)]
    
    if (length(scenario1_species) > 0) {
      log_info("Generating narratives for %d Scenario 1 species (indigenous only)...", 
               length(scenario1_species), module = module)
      
      for (sp in scenario1_species) {
        narrative <- .build_indigenous_narrative(
          sp, indigenous_reports, vern_map, feow_map, hydrobasin_names
        )
        
        if (!is.null(narrative)) {
          sp_clean <- make_package_id(sp)
          
          # Save as text
          txt_file <- file.path(narratives_dir, paste0(sp_clean, "_indigenous_narrative.txt"))
          writeLines(narrative$text, txt_file)
          
          # Save as JSON with metadata
          json_file <- file.path(narratives_dir, paste0(sp_clean, "_indigenous_narrative.json"))
          jsonlite::write_json(narrative, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
          
          all_narratives[[sp]] <- list(text = txt_file, json = json_file)
        }
      }
    }
    
    # --- SCENARIO 2: NON-INDIGENOUS ONLY ---
    scenario2_species <- scenario_table$species[scenario_table$scenario == 2]
    scenario2_species <- scenario2_species[!is.na(scenario2_species)]
    
    if (length(scenario2_species) > 0) {
      log_info("Generating narratives for %d Scenario 2 species (non-indigenous only)...", 
               length(scenario2_species), module = module)
      
      for (sp in scenario2_species) {
        narrative <- .build_non_indigenous_narrative(
          sp, non_indigenous_reports, vern_map, feow_map, hydrobasin_names
        )
        
        if (!is.null(narrative)) {
          sp_clean <- make_package_id(sp)
          
          txt_file <- file.path(narratives_dir, paste0(sp_clean, "_non_indigenous_narrative.txt"))
          writeLines(narrative$text, txt_file)
          
          json_file <- file.path(narratives_dir, paste0(sp_clean, "_non_indigenous_narrative.json"))
          jsonlite::write_json(narrative, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
          
          all_narratives[[sp]] <- list(text = txt_file, json = json_file)
        }
      }
    }
    
    # --- SCENARIO 3: BOTH POPULATIONS ---
    scenario3_species <- scenario_table$species[scenario_table$scenario == 3]
    scenario3_species <- scenario3_species[!is.na(scenario3_species)]
    
    if (length(scenario3_species) > 0) {
      log_info("Generating narratives for %d Scenario 3 species (both populations)...", 
               length(scenario3_species), module = module)
      
      for (sp in scenario3_species) {
        narrative <- .build_scenario3_narrative(
          sp, scenario3_merged, vern_map, feow_map, hydrobasin_names
        )
        
        if (!is.null(narrative)) {
          sp_clean <- make_package_id(sp)
          
          txt_file <- file.path(narratives_dir, paste0(sp_clean, "_combined_narrative.txt"))
          writeLines(narrative$text, txt_file)
          
          json_file <- file.path(narratives_dir, paste0(sp_clean, "_combined_narrative.json"))
          jsonlite::write_json(narrative, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
          
          all_narratives[[sp]] <- list(text = txt_file, json = json_file)
        }
      }
    }
    
    # --- SUMMARY DOCUMENT ---
    summary_narrative <- .build_summary_narrative(scenario_table, all_narratives)
    summary_file <- file.path(narratives_dir, "dataset_narrative_summary.txt")
    writeLines(summary_narrative, summary_file)
    
    log_info("Generated %d species narratives.", length(all_narratives), module = module)
    log_info("Saved summary narrative to: %s", summary_file, module = module)
    
    return(list(
      narratives = all_narratives,
      summary = summary_file
    ))
  })
}


# --- HELPER: INDIGENOUS NARRATIVE ---
.build_indigenous_narrative <- function(sp, reports, vern_map, feow_map, hb_names) {
  sp_clean <- make_package_id(sp)
  report_file <- file.path(dirname(dirname(reports$per_species[[1]])), "indigenous", paste0(sp_clean, ".json"))
  
  if (!file.exists(report_file)) return(NULL)
  
  report <- jsonlite::read_json(report_file, simplifyVector = TRUE)
  
  # Extract vernacular
  vern_str <- NA_character_
  if (!is.null(vern_map) && "species" %in% names(vern_map)) {
    idx <- match(sp, vern_map$species)
    if (!is.na(idx) && "vernacular_string" %in% names(vern_map)) {
      raw_v <- vern_map$vernacular_string[idx]
      if (!is.na(raw_v) && nzchar(raw_v)) {
        vern_str <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
      }
    }
  }
  
  # Build narrative sections
  intro <- if (!is.na(vern_str) && nzchar(vern_str)) {
    sprintf("**%s** (common names: %s) is an indigenous freshwater crayfish species.", sp, vern_str)
  } else {
    sprintf("**%s** is an indigenous freshwater crayfish species.", sp)
  }
  
  # Geographic extent
  eoo <- report$metrics$eoo_km2
  eoo_str <- if (!is.na(eoo)) sprintf("%s km²", format(round(eoo, 0), big.mark = ",")) else "undetermined"
  
  category <- report$metrics$iucn_category
  countries <- report$spatial_context$countries
  
  geo_text <- sprintf(
    "The species is categorized as **%s** based on its geographic extent (EOO: %s). It has been recorded in the following countries: %s.",
    category, eoo_str, countries
  )
  
  # Hydrographic basins (with names if available)
  basins_raw <- report$spatial_context$hydrobasins
  if (!is.na(basins_raw) && nzchar(basins_raw)) {
    basin_codes <- strsplit(basins_raw, " \\| ")[[1]]
    if (!is.null(hb_names) && length(basin_codes) > 0) {
      basin_names <- sapply(basin_codes, function(code) {
        idx <- match(code, hb_names$lookup_key_full)
        if (is.na(idx)) idx <- match(sub("^L\\d+:", "", code), hb_names$lookup_key_id)
        if (!is.na(idx)) {
          basin <- hb_names$Basin_name[idx]
          subbasin <- hb_names$Subbasin_name[idx]
          if (!is.na(subbasin) && nzchar(subbasin) && basin != subbasin) {
            return(paste0(basin, " - ", subbasin))
          } else {
            return(basin)
          }
        }
        return(code)
      }, USE.NAMES = FALSE)
      
      if (length(basin_names) > 3) {
        basins_text <- paste(paste(head(basin_names, 3), collapse = ", "), 
                             sprintf("and %d others", length(basin_names) - 3))
      } else {
        basins_text <- paste(basin_names, collapse = ", ")
      }
    } else {
      basins_text <- "multiple hydrographic basins"
    }
  } else {
    basins_text <- "undetermined hydrographic basins"
  }
  
  geo_text <- paste(geo_text, sprintf("It inhabits %s.", basins_text))
  
  # Ecoregions
  teow <- report$spatial_context$ecoregions_teow
  feow <- report$spatial_context$ecoregions_feow
  
  eco_text <- ""
  if (!is.na(teow) && nzchar(teow)) {
    teow_list <- strsplit(teow, " \\| ")[[1]]
    top_teow <- head(teow_list, 3)
    eco_text <- sprintf("Ecologically, it is associated with terrestrial ecoregions including %s", 
                        paste(top_teow, collapse = ", "))
    if (length(teow_list) > 3) eco_text <- paste0(eco_text, " and others.")
    else eco_text <- paste0(eco_text, ".")
  }
  
  if (!is.na(feow) && nzchar(feow)) {
    # Resolve FEOW names if lookup available
    feow_list <- strsplit(feow, " \\| ")[[1]]
    if (!is.null(feow_map) && "ID" %in% names(feow_map)) {
      feow_ids <- suppressWarnings(as.integer(feow_list))
      feow_names <- sapply(feow_ids, function(id) {
        if (is.na(id)) return(NA_character_)
        idx <- match(id, feow_map$ID)
        if (!is.na(idx) && "ECOREGION" %in% names(feow_map)) {
          return(feow_map$ECOREGION[idx])
        }
        return(as.character(id))
      })
      feow_list <- feow_names[!is.na(feow_names)]
    }
    
    top_feow <- head(feow_list, 3)
    eco_text <- paste(eco_text, sprintf("Freshwater habitats include %s.", paste(top_feow, collapse = ", ")))
  }
  
  # Conservation
  prot_pct <- report$conservation$protection_percentage
  prot_recs <- report$conservation$n_protected_records
  
  cons_text <- sprintf("%d occurrence records (%.1f%%) fall within designated protected areas.", 
                       prot_recs, prot_pct)
  
  pas <- report$spatial_context$protected_areas
  if (!is.na(pas) && nzchar(pas)) {
    pa_list <- strsplit(pas, " \\| ")[[1]]
    top_pa <- head(pa_list, 3)
    cons_text <- paste(cons_text, sprintf("Key protected areas include: %s.", paste(top_pa, collapse = ", ")))
  }
  
  # Fragmentation
  frag <- report$fragmentation
  frag_text <- ""
  if (!is.null(frag) && frag$computed) {
    if (frag$status == "detected") {
      frag_text <- sprintf(
        "**Distributional Fragmentation:** Detected. The species distribution shows %d spatial clusters (%s of all localities).",
        frag$n_clusters, frag$cluster_sizes_n
      )
    } else if (frag$status == "none_detected") {
      frag_text <- "**Distributional Fragmentation:** None detected. All known localities form a single spatial cluster."
    }
  } else {
    frag_text <- "**Distributional Fragmentation:** Not computed (insufficient data or not applicable for cosmopolitan ranges)."
  }
  
  # Assemble full narrative
  full_text <- paste(
    "# Indigenous Population Narrative",
    "",
    "## Species Overview",
    intro,
    "",
    "## Geographic Distribution",
    geo_text,
    "",
    "## Ecological Context",
    eco_text,
    "",
    "## Conservation Status",
    cons_text,
    "",
    "## Spatial Structure",
    frag_text,
    sep = "\n"
  )
  
  return(list(
    species = sp,
    scenario = 1,
    text = full_text,
    metadata = report
  ))
}


# --- HELPER: NON-INDIGENOUS NARRATIVE ---
.build_non_indigenous_narrative <- function(sp, reports, vern_map, feow_map, hb_names) {
  sp_clean <- make_package_id(sp)
  report_file <- file.path(dirname(dirname(reports$per_species[[1]])), "non_indigenous", paste0(sp_clean, ".json"))
  
  if (!file.exists(report_file)) return(NULL)
  
  report <- jsonlite::read_json(report_file, simplifyVector = TRUE)
  
  # Extract vernacular
  vern_str <- NA_character_
  if (!is.null(vern_map) && "species" %in% names(vern_map)) {
    idx <- match(sp, vern_map$species)
    if (!is.na(idx) && "vernacular_string" %in% names(vern_map)) {
      raw_v <- vern_map$vernacular_string[idx]
      if (!is.na(raw_v) && nzchar(raw_v)) {
        vern_str <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
      }
    }
  }
  
  intro <- if (!is.na(vern_str) && nzchar(vern_str)) {
    sprintf("**%s** (common names: %s) is recorded as a non-indigenous freshwater crayfish species in the analyzed region.", sp, vern_str)
  } else {
    sprintf("**%s** is recorded as a non-indigenous freshwater crayfish species in the analyzed region.", sp)
  }
  
  # Invasion extent
  eoo <- report$metrics$eoo_km2
  eoo_str <- if (!is.na(eoo)) sprintf("%s km²", format(round(eoo, 0), big.mark = ",")) else "undetermined"
  
  category <- report$metrics$category
  countries <- report$spatial_context$countries
  
  inv_text <- sprintf(
    "The non-indigenous population is categorized as **%s** based on its invasion extent (EOO: %s). Records exist in: %s.",
    category, eoo_str, countries
  )
  
  # Similar structure to indigenous but focused on invasion
  full_text <- paste(
    "# Non-Indigenous Population Narrative",
    "",
    "## Species Overview",
    intro,
    "",
    "## Invasion Extent",
    inv_text,
    "",
    "## Notes",
    "Fragmentation analysis is not applicable for non-indigenous populations.",
    sep = "\n"
  )
  
  return(list(
    species = sp,
    scenario = 2,
    text = full_text,
    metadata = report
  ))
}


# --- HELPER: SCENARIO 3 NARRATIVE ---
.build_scenario3_narrative <- function(sp, merged, vern_map, feow_map, hb_names) {
  sp_clean <- make_package_id(sp)
  
  if (!sp %in% names(merged$reports)) return(NULL)
  
  report_file <- merged$reports[[sp]]
  if (!file.exists(report_file)) return(NULL)
  
  report <- jsonlite::read_json(report_file, simplifyVector = TRUE)
  
  intro <- sprintf(
    "**%s** exhibits both indigenous and non-indigenous populations, representing a complex biogeographic scenario.",
    sp
  )
  
  full_text <- paste(
    "# Combined Population Narrative (Scenario 3)",
    "",
    "## Species Overview",
    intro,
    "",
    "## Native Range (Indigenous Population)",
    sprintf("EOO: %s km²", format(round(report$indigenous_population$metrics$eoo_km2, 0), big.mark = ",")),
    sprintf("Category: %s", report$indigenous_population$metrics$iucn_category),
    "",
    "## Invaded Range (Non-Indigenous Population)",
    sprintf("EOO: %s km²", format(round(report$non_indigenous_population$metrics$eoo_km2, 0), big.mark = ",")),
    sprintf("Category: %s", report$non_indigenous_population$metrics$category),
    "",
    "## Invasion Summary",
    sprintf("Native countries: %d", report$combined_summary$invasion_status$native_countries),
    sprintf("Invaded countries: %d", report$combined_summary$invasion_status$invaded_countries),
    sep = "\n"
  )
  
  return(list(
    species = sp,
    scenario = 3,
    text = full_text,
    metadata = report
  ))
}


# --- HELPER: SUMMARY NARRATIVE ---
.build_summary_narrative <- function(scenario_table, narratives) {
  total_species <- nrow(scenario_table)
  s1 <- sum(scenario_table$scenario == 1, na.rm = TRUE)
  s2 <- sum(scenario_table$scenario == 2, na.rm = TRUE)
  s3 <- sum(scenario_table$scenario == 3, na.rm = TRUE)
  
  summary <- paste(
    "# Dataset Narrative Summary",
    "",
    sprintf("This dataset contains %d species across three scenarios:", total_species),
    "",
    sprintf("- **Scenario 1** (Indigenous only): %d species", s1),
    sprintf("- **Scenario 2** (Non-indigenous only): %d species", s2),
    sprintf("- **Scenario 3** (Both populations): %d species", s3),
    "",
    sprintf("Generated %d comprehensive species narratives.", length(narratives)),
    sep = "\n"
  )
  
  return(summary)
}