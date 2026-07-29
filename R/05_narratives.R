#### MODULE 5: NARRATIVE BUILDER ####

narrative_builder <- function(result, detailed_reports, vernacular_result = NULL,
                              type_localities = NULL, output_dir = "checkover_output",
                              species_list = NULL, methods = NULL,
                              script_run_time = Sys.time(), hydrobasin_names = NULL) {
  module <- "MODULE5_NARRATIVES"
  
  with_log_section(module, {
    log_info("=== MODULE 5: NARRATIVE BUILDER ===", module = module)
    
    cd <- result$clean_data
    if (is.null(species_list)) species_list <- unique(cd$species)
    species_list <- species_list[!is.na(species_list)]
    
    narratives_dir <- file.path(output_dir, "narratives")
    if (!dir.exists(narratives_dir)) dir.create(narratives_dir, recursive = TRUE)
    
    generated <- list()
    
    cols <- names(detailed_reports)
    has_history <- "eoo_historical_km2" %in% cols && "eoo_current_km2" %in% cols
    
    pb <- create_progress_bar(length(species_list))
    
    for (sp in species_list) {
      pb$tick()
      
      dr <- detailed_reports[detailed_reports$species == sp, ]
      if (nrow(dr) == 0) next
      
      # Extract metrics
      n_countries <- as.integer(dr$countries_count)
      countries_txt <- as.character(dr$countries_list)
      dist_cat <- as.character(dr$distribution_category)
      n_basins <- as.integer(dr$basins_count)
      
      get_num <- function(x) {
        val <- suppressWarnings(as.numeric(x))
        if (length(val) == 0 || !is.finite(val)) return(NA_real_)
        return(val)
      }
      
      if (has_history) {
        eoo_curr <- get_num(dr$eoo_current_km2)
        eoo_hist <- get_num(dr$eoo_historical_km2)
        aoo_curr <- get_num(dr$aoo_current_km2)
        ext_sig <- as.character(dr$extirpation_signature)
      } else {
        eoo_curr <- get_num(dr$eoo_km2)
        eoo_hist <- eoo_curr
        aoo_curr <- get_num(dr$aoo_km2)
        if (is.na(aoo_curr)) aoo_curr <- get_num(dr$aoo_k2)
        ext_sig <- NA_character_
      }
      
      # Extract vernacular
      v_str <- NA_character_
      if (!is.null(vernacular_result)) {
        if (is.list(vernacular_result) && "wide" %in% names(vernacular_result)) {
          vern_df <- vernacular_result$wide
        } else if (is.data.frame(vernacular_result)) {
          vern_df <- vernacular_result
        } else {
          vern_df <- NULL
        }
        
        if (!is.null(vern_df) && "species" %in% names(vern_df)) {
          idx <- match(sp, vern_df$species)
          if (!is.na(idx) && "vernacular_string" %in% names(vern_df)) {
            raw_v <- vern_df$vernacular_string[idx]
            if (!is.na(raw_v) && nzchar(raw_v)) {
              v_str <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
            }
          }
        }
      }
      
      # 1. GEOGRAPHIC NARRATIVE
      eoo_str <- if (!is.na(eoo_curr)) {
        paste0(format(round(eoo_curr, 0), big.mark = ","), " km²")
      } else {
        "undetermined"
      }
      
      # Get basin names from detailed_reports
      basins_text <- as.character(dr$hydrobasins_list)
      if (is.na(basins_text) || !nzchar(basins_text)) {
        basins_text <- "undetermined hydrographic basins"
      } else {
        basin_entries <- strsplit(basins_text, " \\| ")[[1]]
        if (length(basin_entries) > 3) {
          basin_summary <- paste(length(basin_entries), "hydrographic basins including",
                                 paste(head(basin_entries, 3), collapse = ", "), "and others")
        } else {
          basin_summary <- paste(basin_entries, collapse = ", ")
        }
        basins_text <- basin_summary
      }
      
      geo_text <- glue::glue(
        "{sp} is recorded in {n_countries} countries: {countries_txt}. ",
        "It is categorized as **{dist_cat}** based on its current geographic extent (EOO: {eoo_str}). ",
        "The species inhabits {basins_text}."
      )
      
      # Add vernacular if available
      if (!is.na(v_str) && nzchar(v_str)) {
        geo_text <- paste0("**", sp, "** (common names: ", v_str, ")\n\n", geo_text)
      }
      
      # Add Extinction Context
      if (!is.na(eoo_hist) && !is.na(eoo_curr) && eoo_hist > eoo_curr) {
        loss_pct <- round(((eoo_hist - eoo_curr) / eoo_hist) * 100, 1)
        geo_text <- paste0(geo_text, glue::glue(
          "\n\n**Range Contraction Detected:** Historical EOO was {format(round(eoo_hist, 0), big.mark = ',')} km². ",
          "The species has lost approximately {loss_pct}% of its extent. ",
          "Extirpated locations: {ifelse(is.na(ext_sig) | ext_sig == '', 'Unspecified', ext_sig)}."
        ))
      }
      
      # 2. ECOLOGICAL NARRATIVE
      eco_list <- strsplit(as.character(dr$ecoregions_list), " \\| ")[[1]]
      feow_list <- strsplit(as.character(dr$feow_list), " \\| ")[[1]]
      
      eco_text <- ""
      if (length(eco_list) > 0 && !all(is.na(eco_list))) {
        top_eco <- head(eco_list, 3)
        eco_text <- glue::glue("Ecologically, it is associated with terrestrial ecoregions such as *{paste(top_eco, collapse = ', ')}*")
        if (length(eco_list) > 3) eco_text <- paste0(eco_text, " and others.") else eco_text <- paste0(eco_text, ".")
      } else {
        eco_text <- "Terrestrial ecoregion associations are undefined."
      }
      
      if (length(feow_list) > 0 && !all(is.na(feow_list))) {
        top_feow <- head(feow_list, 3)
        eco_text <- paste(eco_text, glue::glue(" Freshwater habitats include *{paste(top_feow, collapse = ', ')}*."))
      }
      
      # 3. CONSERVATION NARRATIVE
      prot_pct <- get_num(dr$protection_pct)
      if (is.na(prot_pct)) prot_pct <- 0
      
      prot_recs <- get_num(dr$protected_records)
      if (is.na(prot_recs)) prot_recs <- 0
      
      pa_list <- strsplit(as.character(dr$protected_areas_list), " \\| ")[[1]]
      
      cons_text <- glue::glue("{prot_recs} occurrence records ({prot_pct}%) fall within designated protected areas.")
      
      if (length(pa_list) > 0 && !all(is.na(pa_list))) {
        top_pa <- head(pa_list, 3)
        cons_text <- paste(cons_text, glue::glue("Key protected areas include: *{paste(top_pa, collapse = ', ')}*."))
      } else {
        cons_text <- paste(cons_text, "No records were matched to specific protected areas in the WDPA database.")
      }
      
      # 4. TYPE LOCALITY
      sp_rows <- cd[cd$species == sp & isTRUE(cd$is_type_locality), ]
      if (nrow(sp_rows) > 0) {
        tl_row <- sp_rows[1, ]
        lat <- round(tl_row$latitude, 4)
        lon <- round(tl_row$longitude, 4)
        tl_text <- glue::glue("\n\n**Type Locality:** {lat}, {lon}.")
        geo_text <- paste(geo_text, tl_text)
      }
      
      # 5. FRAGMENTATION ANALYSIS
      frag_text <- ""
      if (!is.null(result$fragmentation)) {
        frag_row <- result$fragmentation[result$fragmentation$species == sp, ]
        
        if (nrow(frag_row) > 0) {
          if (frag_row$scope == "not_applicable_for_cosmopolitan_ranges") {
            frag_text <- "Spatial clustering signal: not computed, as this species is classified as cosmopolitan."
          } else if (as.logical(frag_row$computed)) {
            if (frag_row$status == "none_detected") {
              frag_text <- "Spatial clustering signal: none detected (all known localities form a single spatial cluster under the mean-distance threshold)."
            } else if (frag_row$status == "detected") {
              frag_text <- glue::glue(
                "Spatial clustering signal: detected ({frag_row$n_clusters} spatial clusters identified under the mean-distance threshold; cluster sizes: {frag_row$cluster_sizes_n} of all localities)."
              )
            }
          } else {
            frag_text <- "Spatial clustering signal: not computed (insufficient data points)."
          }
        }
      }
      
      # 6. ASSEMBLE
      full_text <- paste(
        "### Distribution Summary", geo_text,
        "\n\n### Ecological Context", eco_text,
        "\n\n### Conservation Status", cons_text,
        "\n\n### Spatial Structure", frag_text,
        sep = "\n"
      )
      
      sp_clean <- make_package_id(sp)
      
      txt_path <- file.path(narratives_dir, paste0(sp_clean, "_narrative.txt"))
      json_path <- file.path(narratives_dir, paste0(sp_clean, "_eco_narrative.json"))
      
      writeLines(full_text, txt_path)
      
      json_out <- list(
        narrative = full_text,
        vernacular_names = v_str,
        metrics = as.list(dr)
      )
      jsonlite::write_json(json_out, json_path, pretty = TRUE, auto_unbox = TRUE)
      
      generated[[sp]] <- list(
        text_file = txt_path,
        json_file = json_path
      )
    }
    
    pb$terminate()
    
    log_info("Generated narratives for %d species.", length(generated), module = module)
    
    return(generated)
  })
}