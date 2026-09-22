#### MODULE 4C: NON-INDIGENOUS REPORTS ####

#' Generate detailed JSON reports for non-indigenous species
#' @param result_non_indigenous Enriched non-indigenous result (list with $clean_data;
#'                              clean_data must contain taxonomy columns from Module 1)
#' @param vernacular_lookup     Vernacular names lookup (list with $wide, or data frame)
#' @param output_dir            Output directory
#' @param feow_lookup_path      Path to FEOW TSV lookup (ID | Realm | Major Habitat Type | Ecoregion)
#' @param hydrobasin_names      HydroBASINS name lookup data frame (Basin_level | HYBAS_ID |
#'                              Basin_name | Subbasin_name)
#' @return List of generated reports
generate_non_indigenous_reports <- function(result_non_indigenous,
                                            vernacular_lookup  = NULL,
                                            output_dir         = "checkover_output",
                                            feow_lookup_path   = NULL,
                                            hydrobasin_names   = NULL) {
  module <- "MODULE4C_REPORTS_NON_IND"
  
  with_log_section(module, {
    log_info("=== MODULE 4C: NON-INDIGENOUS REPORTS ===", module = module)
    
    cd <- result_non_indigenous$clean_data
    
    if (nrow(cd) == 0) {
      log_warn("No data for reports.", module = module)
      return(list())
    }
    
    # Create reports directory
    reports_dir <- file.path(output_dir, "reports", "non_indigenous")
    if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
    
    species_list <- unique(cd$species)
    log_info("Generating reports for %d species...", length(species_list), module = module)
    
    # ---------- Shared lookups ----------
    vern_map <- .extract_vern_map_3c(vernacular_lookup)
    feow_map <- .load_feow_map_3c(feow_lookup_path, module)
    
    reports_generated <- list()
    
    for (sp in species_list) {
      sp_data_all <- cd[cd$species == sp, ]
      sp_data <- if ("temporal_status" %in% names(sp_data_all)) {
        sp_data_all[sp_data_all$temporal_status == "active", , drop = FALSE]
      } else sp_data_all
      sp_clean <- make_package_id(sp)
      
      # --- Vernacular ---
      vern_str <- .get_vern_str_3c(sp, vern_map)
      
      # --- Higher taxonomy ---
      tax_order    <- .first_val(sp_data$order)
      tax_superfam <- .first_val(sp_data$superfamily)
      tax_family   <- .first_val(sp_data$family)
      higher_taxonomy <- .build_higher_taxonomy(tax_order, tax_superfam, tax_family)

      # ── ZERO-ACTIVE (TOTAL EXTINCTION) TERMINAL STATE ──────────────────────
      # All non-indigenous occurrences extirpated. Valid terminal state: close on
      # zero, no classification, no spatial enumeration, mandatory disclaimer
      # (see 03c for rationale; Lucian, 2026-06).
      if (nrow(sp_data) == 0L && nrow(sp_data_all) > 0L) {
        tax_order    <- .first_val(sp_data_all$order)
        tax_superfam <- .first_val(sp_data_all$superfamily)
        tax_family   <- .first_val(sp_data_all$family)
        higher_taxonomy <- .build_higher_taxonomy(tax_order, tax_superfam, tax_family)

        ext_idx0   <- which(!is.na(sp_data_all$is_extinct) & sp_data_all$is_extinct == TRUE)
        ext_years  <- suppressWarnings(as.integer(sp_data_all$year[ext_idx0]))
        ext_years  <- ext_years[is.finite(ext_years)]
        last_ext_y <- if (length(ext_years) > 0L) max(ext_years) else NA_integer_
        first_ext_y<- if (length(ext_years) > 0L) min(ext_years) else NA_integer_
        n_ext_loc  <- if (length(ext_idx0) > 0L)
          length(unique(paste(sp_data_all$longitude[ext_idx0], sp_data_all$latitude[ext_idx0]))) else 0L

        report <- list(
          species          = sp,
          population_type  = "non-indigenous",
          vernacular_names = vern_str,
          status           = "Extinct",
          terminal_state   = "zero_active",
          taxonomy = list(
            order       = if (!is.na(tax_order))    tax_order    else NULL,
            superfamily = if (!is.na(tax_superfam)) tax_superfam else NULL,
            family      = if (!is.na(tax_family))   tax_family   else NULL,
            higher_taxonomy = if (!is.na(higher_taxonomy)) higher_taxonomy else NULL
          ),
          metrics = list(
            n_records          = 0L,
            eoo_km2            = NA_real_,
            aoo_km2            = 0,
            category           = "Extinct",
            hydrobasins_level  = NA_integer_,
            occurrence_origins = NA_character_
          ),
          temporal = list(
            year_min      = first_ext_y,
            year_max      = last_ext_y,
            year_range    = if (!is.na(first_ext_y) && !is.na(last_ext_y)) last_ext_y - first_ext_y + 1 else NA_integer_,
            first_record  = first_ext_y,
            pct_post_2000 = NA_real_
          ),
          spatial_context = list(
            continents = "", countries = "", admin_units = "",
            ecoregions_teow = "", ecoregions_feow = "",
            protected_areas = "", hydrobasins = ""
          ),
          counts = list(
            n_continents = 0L, n_countries = 0L,
            n_ecoregions_teow = 0L, n_ecoregions_feow = 0L,
            n_hydrobasins = 0L, n_distinct_protected_areas = 0L,
            n_extinctions = n_ext_loc
          ),
          conservation = list(
            n_protected_records = 0L,
            protection_percentage = NA_real_
          ),
          notes = list(clustering_analysis = "Not applicable for non-indigenous populations"),
          extinction_year = last_ext_y,
          integrity_flag  = "MAX",
          disclaimer      = CHECKOVER_EXTINCTION_DISCLAIMER
        )

        json_file <- file.path(reports_dir, paste0(sp_clean, ".json"))
        jsonlite::write_json(report, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
        reports_generated[[sp]] <- json_file
        log_info("  %s: zero-active terminal state (Extinct, non-indigenous) — %d extirpated localit%s",
                 sp, n_ext_loc, if (n_ext_loc != 1L) "ies" else "y", module = module)
        next
      }

      # --- Resolve FEOW IDs -> names ---
      feow_raw      <- sp_data$freshwater_ecoregion[!is.na(sp_data$freshwater_ecoregion)]
      feow_resolved <- .resolve_feow_3c(feow_raw, feow_map)
      feow_unique   <- sort(unique(feow_resolved[nzchar(feow_resolved)]))
      
      # --- Hydrographic basins: units vs named basins (see 03c for rationale) ---
      basin_cells    <- sp_data$hydrobasin[!is.na(sp_data$hydrobasin) & nzchar(sp_data$hydrobasin)]
      basin_codes    <- unique(unlist(strsplit(basin_cells, "\\s*\\|\\s*")))
      basin_codes    <- basin_codes[nzchar(basin_codes)]
      n_basin_units  <- length(basin_codes)
      basin_resolved <- .resolve_basin_3c(basin_codes, hydrobasin_names)
      basin_unique   <- sort(unique(basin_resolved[nzchar(basin_resolved)]))
      n_named_basins <- length(basin_unique)
      
      # --- Occurrence origins ---
      orig_col <- intersect(c("origin", "occurrence_origin", "status"), names(sp_data))[1]
      origins  <- if (!is.na(orig_col))
        paste(sort(unique(sp_data[[orig_col]][!is.na(sp_data[[orig_col]])])), collapse = " | ")
      else NA_character_
      
      # --- v3 metadata derivations: distinct PAs and extinction localities ---
      pa_split <- unlist(strsplit(
        sp_data$protected_area[!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)],
        "\\s*\\|\\s*"
      ))
      n_distinct_pas <- length(unique(pa_split[nzchar(pa_split)]))
      
      # Extinctions: use FULL slice — extinct records are what we're counting
      ext_idx <- which(!is.na(sp_data_all$is_extinct) & sp_data_all$is_extinct == TRUE)
      n_extinctions <- if (length(ext_idx) > 0L) {
        length(unique(paste(sp_data_all$longitude[ext_idx], sp_data_all$latitude[ext_idx])))
      } else 0L
      
      # --- Build report ---
      report <- list(
        species         = sp,
        population_type = "non-indigenous",
        vernacular_names = vern_str,
        
        taxonomy = list(
          order       = if (!is.na(tax_order))    tax_order    else NULL,
          superfamily = if (!is.na(tax_superfam)) tax_superfam else NULL,
          family      = if (!is.na(tax_family))   tax_family   else NULL,
          higher_taxonomy = if (!is.na(higher_taxonomy)) higher_taxonomy else NULL
        ),
        
        metrics = list(
          n_records         = nrow(sp_data),
          eoo_km2           = sp_data$eoo_km2[1],
          aoo_km2           = sp_data$aoo_km2[1],
          category          = sp_data$category[1],   # "local" or "widespread"
          hydrobasins_level = sp_data$hydrobasins_level[1],
          occurrence_origins = origins
        ),
        
        temporal = list(
          year_min   = min(sp_data$year, na.rm = TRUE),
          year_max   = max(sp_data$year, na.rm = TRUE),
          year_range = max(sp_data$year, na.rm = TRUE) - min(sp_data$year, na.rm = TRUE) + 1,
          first_record = min(sp_data$year, na.rm = TRUE),
          # post-2000 = strictly year > 2000 (see 03c; boundary shared with narrative)
        pct_post_2000 = if (nrow(sp_data) > 0L) round(100 * sum(sp_data$year > 2000, na.rm = TRUE) / nrow(sp_data), 1) else NA_real_
        ),
        
        spatial_context = list(
          # geo_usable() strips NA/blank AND the `unresolved` sentinel.
          continents      = paste(sort(unique(geo_usable(sp_data$continents))), collapse = " | "),
          countries       = paste(sort(unique(geo_usable(sp_data$country))),    collapse = " | "),
          admin_units     = paste(sort(unique(geo_usable(sp_data$admin_1))),    collapse = " | "),
          ecoregions_teow = paste(sort(unique(sp_data$ecoregion[!is.na(sp_data$ecoregion)])), collapse = " | "),
          ecoregions_feow = paste(feow_unique, collapse = " | "),
          protected_areas = paste(sort(unique(sp_data$protected_area[!is.na(sp_data$protected_area)])), collapse = " | "),
          hydrobasins       = paste(basin_unique, collapse = " | "),  # named basins
          hydrobasin_units  = paste(sort(basin_codes), collapse = " | ")
        ),

        counts = list(
          n_continents      = n_distinct_geo(sp_data$continents),
          n_countries       = n_distinct_geo(sp_data$country),
          n_ecoregions_teow = length(unique(sp_data$ecoregion[!is.na(sp_data$ecoregion)])),
          n_ecoregions_feow = length(feow_unique),
          n_hydrobasins     = n_basin_units,     # fine UNIT count -> basins_count
          n_named_basins    = n_named_basins,    # distinct river/basin names
          n_distinct_protected_areas = n_distinct_pas,
          n_extinctions     = n_extinctions
        ),
        
        conservation = list(
          n_protected_records   = sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)),
          protection_percentage = round(
            sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)) / nrow(sp_data) * 100, 1)
        ),
        
        notes = list(
          clustering_analysis = "Not applicable for non-indigenous populations"
        )
      )
      
      json_file <- file.path(reports_dir, paste0(sp_clean, ".json"))
      jsonlite::write_json(report, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
      reports_generated[[sp]] <- json_file
    }
    
    log_info("Generated %d per-species reports.", length(reports_generated), module = module)
    
    # Group summary report (unchanged)
    summary_report <- list(
      population_type = "non-indigenous",
      total_species   = length(species_list),
      total_records   = nrow(cd),
      
      categorization = list(
        local      = sum(cd$category == "local",      na.rm = TRUE) / nrow(cd) * 100,
        widespread = sum(cd$category == "widespread", na.rm = TRUE) / nrow(cd) * 100
      ),
      
      temporal = list(
        oldest_record = min(cd$year, na.rm = TRUE),
        newest_record = max(cd$year, na.rm = TRUE)
      ),
      
      conservation = list(
        avg_protection_pct = round(mean(
          tapply(!is.na(cd$protected_area) & nzchar(cd$protected_area),
                 cd$species, mean, na.rm = TRUE) * 100), 1)
      ),
      
      invasion_extent = list(
        total_countries_invaded = n_distinct_geo(cd$country),
        total_continents        = n_distinct_geo(cd$continents)
      )
    )
    
    summary_file <- file.path(reports_dir, "group_summary.json")
    jsonlite::write_json(summary_report, summary_file, pretty = TRUE, auto_unbox = TRUE)
    log_info("Saved group summary to: %s", summary_file, module = module)
    log_info("Non-indigenous reports complete.", module = module)
    
    return(list(
      per_species   = reports_generated,
      group_summary = summary_file
    ))
  })
}


# ===========================================================================
# MODULE-PRIVATE HELPERS
# ===========================================================================
# .extract_vern_map_3c(), .get_vern_str_3c(), .first_val(),
# .build_higher_taxonomy(), .load_feow_map_3c(), .resolve_feow_3c() and
# .resolve_basin_3c() are defined ONCE, in 03c_reports_indigenous.R, which is
# sourced before this file. They used to be copied here too; the copies were
# identical, but the later one silently wins, so an edit to either copy could
# be undone without a trace (tests/test_single_definitions.R forbids it now).
