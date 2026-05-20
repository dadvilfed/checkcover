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
      
      # --- Resolve FEOW IDs -> names ---
      feow_raw      <- sp_data$freshwater_ecoregion[!is.na(sp_data$freshwater_ecoregion)]
      feow_resolved <- .resolve_feow_3c(feow_raw, feow_map)
      feow_unique   <- sort(unique(feow_resolved[nzchar(feow_resolved)]))
      
      # --- Resolve hydrobasin codes -> names ---
      basin_raw      <- sp_data$hydrobasin[!is.na(sp_data$hydrobasin)]
      basin_resolved <- .resolve_basin_3c(basin_raw, hydrobasin_names)
      basin_unique   <- sort(unique(basin_resolved[nzchar(basin_resolved)]))
      
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
          pct_post_2000 = if (nrow(sp_data) > 0L) round(100 * sum(sp_data$year >= 2000, na.rm = TRUE) / nrow(sp_data), 1) else NA_real_
        ),
        
        spatial_context = list(
          continents      = paste(sort(unique(sp_data$continents[!is.na(sp_data$continents)])), collapse = " | "),
          countries       = paste(sort(unique(sp_data$country[!is.na(sp_data$country)])),     collapse = " | "),
          admin_units     = paste(sort(unique(sp_data$admin_1[!is.na(sp_data$admin_1)])),     collapse = " | "),
          ecoregions_teow = paste(sort(unique(sp_data$ecoregion[!is.na(sp_data$ecoregion)])), collapse = " | "),
          ecoregions_feow = paste(feow_unique, collapse = " | "),
          protected_areas = paste(sort(unique(sp_data$protected_area[!is.na(sp_data$protected_area)])), collapse = " | "),
          hydrobasins     = paste(basin_unique, collapse = " | ")
        ),
        
        counts = list(
          n_continents      = length(unique(sp_data$continents[!is.na(sp_data$continents)])),
          n_countries       = length(unique(sp_data$country[!is.na(sp_data$country)])),
          n_ecoregions_teow = length(unique(sp_data$ecoregion[!is.na(sp_data$ecoregion)])),
          n_ecoregions_feow = length(feow_unique),
          n_hydrobasins     = length(basin_unique),
          n_distinct_protected_areas = n_distinct_pas,
          n_extinctions     = n_extinctions
        ),
        
        conservation = list(
          n_protected_records   = sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)),
          protection_percentage = round(
            sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)) / nrow(sp_data) * 100, 1)
        ),
        
        notes = list(
          fragmentation_analysis = "Not applicable for non-indigenous populations"
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
        total_countries_invaded = length(unique(cd$country[!is.na(cd$country)])),
        total_continents        = length(unique(cd$continents[!is.na(cd$continents)]))
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
# (Shared with 03c via sourcing; safe to define here as well since R uses
#  the most recently defined version.)
# ===========================================================================

.extract_vern_map_3c <- function(vernacular_lookup) {
  if (is.null(vernacular_lookup)) return(NULL)
  if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup))
    return(vernacular_lookup$wide)
  if (is.data.frame(vernacular_lookup)) return(vernacular_lookup)
  NULL
}

.get_vern_str_3c <- function(sp, vern_map) {
  if (is.null(vern_map) || !"species" %in% names(vern_map)) return(NA_character_)
  idx <- match(sp, vern_map$species)
  if (is.na(idx) || !"vernacular_string" %in% names(vern_map)) return(NA_character_)
  raw_v <- vern_map$vernacular_string[idx]
  if (is.na(raw_v) || !nzchar(raw_v)) return(NA_character_)
  gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
}

.first_val <- function(x) {
  v <- x[!is.na(x) & nzchar(x)]
  if (length(v) == 0) return(NA_character_)
  v[1]
}

.build_higher_taxonomy <- function(order, superfamily, family) {
  parts <- c(order, superfamily, family)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = " > ")
}

.load_feow_map_3c <- function(feow_lookup_path, module) {
  if (is.null(feow_lookup_path) || !file.exists(feow_lookup_path)) return(NULL)
  feow_map <- tryCatch(
    read.delim(feow_lookup_path, stringsAsFactors = FALSE,
               encoding = "UTF-8", quote = ""),
    error = function(e) {
      log_warn("Failed to load FEOW lookup: %s", conditionMessage(e), module = module)
      NULL
    }
  )
  if (is.null(feow_map)) return(NULL)
  names(feow_map) <- toupper(names(feow_map))
  if ("ID" %in% names(feow_map)) feow_map$ID <- as.integer(feow_map$ID)
  log_info("FEOW lookup loaded: %d rows", nrow(feow_map), module = module)
  feow_map
}

.resolve_feow_3c <- function(x, feow_map) {
  if (is.null(feow_map) || length(x) == 0) return(x)
  id_col   <- intersect(c("ID", "FEOW_ID"),                       names(feow_map))[1]
  name_col <- intersect(c("ECOREGION", "NAME", "ECOREGION_NAME"), names(feow_map))[1]
  if (is.na(id_col) || is.na(name_col)) return(x)
  ids       <- suppressWarnings(as.integer(x))
  match_idx <- match(ids, feow_map[[id_col]])
  resolved  <- feow_map[[name_col]][match_idx]
  ok        <- !is.na(resolved) & nzchar(resolved)
  x[ok]     <- resolved[ok]
  x
}

.resolve_basin_3c <- function(x, hb_lookup) {
  if (is.null(hb_lookup) || length(x) == 0) return(x)
  req <- c("Basin_level", "HYBAS_ID", "Basin_name", "Subbasin_name")
  if (!all(req %in% names(hb_lookup))) return(x)
  hb_lookup$lookup_key <- paste0(hb_lookup$Basin_level, ":", hb_lookup$HYBAS_ID)
  
  vapply(x, function(code) {
    if (is.na(code) || !nzchar(code)) return(code)
    idx <- match(code, hb_lookup$lookup_key)
    if (is.na(idx)) {
      id_only <- sub("^L\\d+:", "", code)
      idx     <- match(id_only, as.character(hb_lookup$HYBAS_ID))
    }
    if (is.na(idx)) return(code)
    basin    <- hb_lookup$Basin_name[idx]
    subbasin <- hb_lookup$Subbasin_name[idx]
    if (is.na(basin) || !nzchar(basin)) return(code)
    if (is.na(subbasin) || !nzchar(subbasin) || basin == subbasin) basin
    else paste0(basin, " - ", subbasin)
  }, character(1), USE.NAMES = FALSE)
}

# NULL coalescing (safe re-definition)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}