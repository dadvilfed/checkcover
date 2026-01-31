#### MODULE 3C: INDIGENOUS REPORTS ####

#' Generate detailed JSON reports for indigenous species
#' @param result_indigenous Enriched indigenous result
#' @param vernacular_lookup Vernacular names lookup
#' @param output_dir Output directory
#' @return List of generated reports
generate_indigenous_reports <- function(result_indigenous,
                                        vernacular_lookup = NULL,
                                        output_dir = "checkover_output") {
  module <- "MODULE3C_REPORTS_IND"

  with_log_section(module, {
    log_info("=== MODULE 3C: INDIGENOUS REPORTS ===", module = module)

    cd <- result_indigenous$clean_data

    if (nrow(cd) == 0) {
      log_warn("No data for reports.", module = module)
      return(list())
    }

    # Create reports directory
    reports_dir <- file.path(output_dir, "reports", "indigenous")
    if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

    species_list <- unique(cd$species)
    log_info("Generating reports for %d species...", length(species_list), module = module)

    # Extract vernacular lookup
    vern_map <- NULL
    if (!is.null(vernacular_lookup)) {
      if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup)) {
        vern_map <- vernacular_lookup$wide
      } else if (is.data.frame(vernacular_lookup)) {
        vern_map <- vernacular_lookup
      }
    }

    # Fragmentation data
    frag_df <- result_indigenous$fragmentation %||% NULL

    reports_generated <- list()

    # Per-species reports
    for (sp in species_list) {
      sp_data <- cd[cd$species == sp, ]
      sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)

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

      # Extract fragmentation
      frag_block <- list(computed = FALSE, status = "not_computed")
      if (!is.null(frag_df)) {
        fr <- frag_df[frag_df$species == sp, ]
        if (nrow(fr) > 0) {
          frag_block <- list(
            computed = as.logical(fr$computed),
            scope = as.character(fr$scope),
            status = as.character(fr$status),
            n_clusters = if (!is.na(fr$n_clusters)) as.integer(fr$n_clusters) else NA,
            cluster_sizes = if (!is.na(fr$cluster_sizes_n)) as.character(fr$cluster_sizes_n) else NA,
            mean_threshold_km = if (!is.na(fr$mean_distance_km)) as.numeric(fr$mean_distance_km) else NA
          )
        }
      }

      # Build report
      report <- list(
        species = sp,
        population_type = "indigenous",
        vernacular_names = vern_str,

        metrics = list(
          n_records = nrow(sp_data),
          eoo_km2 = sp_data$eoo_km2[1],
          aoo_km2 = sp_data$aoo_km2[1],
          iucn_category = sp_data$iucn_category[1],
          hydrobasins_level = sp_data$hydrobasins_level[1]
        ),

        temporal = list(
          year_min = min(sp_data$year, na.rm = TRUE),
          year_max = max(sp_data$year, na.rm = TRUE),
          year_range = max(sp_data$year, na.rm = TRUE) - min(sp_data$year, na.rm = TRUE) + 1
        ),

        spatial_context = list(
          continents = paste(sort(unique(sp_data$continents[!is.na(sp_data$continents)])), collapse = " | "),
          countries = paste(sort(unique(sp_data$country[!is.na(sp_data$country)])), collapse = " | "),
          admin_units = paste(sort(unique(sp_data$admin_1[!is.na(sp_data$admin_1)])), collapse = " | "),
          ecoregions_teow = paste(sort(unique(sp_data$ecoregion[!is.na(sp_data$ecoregion)])), collapse = " | "),
          ecoregions_feow = paste(sort(unique(sp_data$freshwater_ecoregion[!is.na(sp_data$freshwater_ecoregion)])), collapse = " | "),
          protected_areas = paste(sort(unique(sp_data$protected_area[!is.na(sp_data$protected_area)])), collapse = " | "),
          hydrobasins = paste(sort(unique(sp_data$hydrobasin[!is.na(sp_data$hydrobasin)])), collapse = " | ")
        ),

        counts = list(
          n_continents = length(unique(sp_data$continents[!is.na(sp_data$continents)])),
          n_countries = length(unique(sp_data$country[!is.na(sp_data$country)])),
          n_ecoregions_teow = length(unique(sp_data$ecoregion[!is.na(sp_data$ecoregion)])),
          n_ecoregions_feow = length(unique(sp_data$freshwater_ecoregion[!is.na(sp_data$freshwater_ecoregion)])),
          n_hydrobasins = length(unique(sp_data$hydrobasin[!is.na(sp_data$hydrobasin)]))
        ),

        conservation = list(
          n_protected_records = sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)),
          protection_percentage = round((sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)) / nrow(sp_data)) * 100, 1)
        ),

        fragmentation = frag_block
      )

      # Save JSON
      json_file <- file.path(reports_dir, paste0(sp_clean, ".json"))
      jsonlite::write_json(report, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")

      reports_generated[[sp]] <- json_file
    }

    log_info("Generated %d per-species reports.", length(reports_generated), module = module)

    # Group summary report
    summary_report <- list(
      population_type = "indigenous",
      total_species = length(species_list),
      total_records = nrow(cd),

      categorization = list(
        endemic = sum(cd$iucn_category == "endemic", na.rm = TRUE) / nrow(cd) * 100,
        regional = sum(cd$iucn_category == "regional", na.rm = TRUE) / nrow(cd) * 100,
        cosmopolitan = sum(cd$iucn_category == "cosmopolitan", na.rm = TRUE) / nrow(cd) * 100
      ),

      temporal = list(
        oldest_record = min(cd$year, na.rm = TRUE),
        newest_record = max(cd$year, na.rm = TRUE)
      ),

      conservation = list(
        avg_protection_pct = round(mean(
          tapply(!is.na(cd$protected_area) & nzchar(cd$protected_area), cd$species, mean, na.rm = TRUE) * 100
        ), 1)
      )
    )

    summary_file <- file.path(reports_dir, "group_summary.json")
    jsonlite::write_json(summary_report, summary_file, pretty = TRUE, auto_unbox = TRUE)

    log_info("Saved group summary to: %s", summary_file, module = module)
    log_info("Indigenous reports complete.", module = module)

    return(list(
      per_species = reports_generated,
      group_summary = summary_file
    ))
  })
}