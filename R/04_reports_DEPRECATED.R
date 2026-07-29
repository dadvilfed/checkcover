#### MODULE 4: BUILD REPORTS ####

# Helper: Load HydroBASINS name lookup
load_hydrobasin_names <- function(tsv_path) {
  if (!file.exists(tsv_path)) {
    log_warn("HydroBASINS name lookup not found: %s", tsv_path, module = "MODULE4_REPORTS")
    return(NULL)
  }
  
  log_info("Loading HydroBASINS name lookup from: %s", tsv_path, module = "MODULE4_REPORTS")
  
  hb_names <- tryCatch({
    read.delim(tsv_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
               na.strings = c("", "NA"))
  }, error = function(e) {
    log_error("Failed to read HydroBASINS names: %s", conditionMessage(e), 
              module = "MODULE4_REPORTS")
    return(NULL)
  })
  
  names(hb_names) <- c("Basin_level", "HYBAS_ID", "Basin_name", "Subbasin_name")
  hb_names$HYBAS_ID <- as.character(hb_names$HYBAS_ID)
  hb_names$lookup_key_full <- paste0(hb_names$Basin_level, ":", hb_names$HYBAS_ID)
  hb_names$lookup_key_id <- hb_names$HYBAS_ID
  
  log_info("Loaded %d HydroBASINS name mappings.", nrow(hb_names), module = "MODULE4_REPORTS")
  return(hb_names)
}

# Helper: Resolve basin names from codes
resolve_basin_names <- function(basin_codes, hb_lookup) {
  if (is.null(hb_lookup) || length(basin_codes) == 0) return(basin_codes)
  
  sapply(basin_codes, function(code) {
    if (is.na(code) || !nzchar(code)) return(NA_character_)
    
    # Try full format first
    idx <- match(code, hb_lookup$lookup_key_full)
    
    # Fallback to ID only
    if (is.na(idx)) {
      if (grepl("^L\\d+:", code)) {
        id_part <- sub("^L\\d+:", "", code)
      } else {
        id_part <- code
      }
      idx <- match(id_part, hb_lookup$lookup_key_id)
    }
    
    if (is.na(idx)) return(code)
    
    basin <- hb_lookup$Basin_name[idx]
    subbasin <- hb_lookup$Subbasin_name[idx]
    level <- hb_lookup$Basin_level[idx]
    hybas_id <- hb_lookup$HYBAS_ID[idx]
    
    if (is.na(basin) || !nzchar(basin)) return(code)
    
    if (is.na(subbasin) || !nzchar(subbasin) || basin == subbasin) {
      return(paste0(basin, " (", level, ":", hybas_id, ")"))
    } else {
      return(paste0(basin, " - ", subbasin, " (", level, ":", hybas_id, ")"))
    }
  }, USE.NAMES = FALSE)
}

# Helper: Compute species metrics if missing
compute_species_metrics <- function(cd) {
  stopifnot(all(c("species", "longitude", "latitude") %in% names(cd)))
  
  cd$longitude <- suppressWarnings(as.numeric(cd$longitude))
  cd$latitude <- suppressWarnings(as.numeric(cd$latitude))
  cd <- cd[is.finite(cd$longitude) & is.finite(cd$latitude), , drop = FALSE]
  
  if (!nrow(cd)) {
    return(dplyr::tibble(species = character(), eoo_km2 = numeric(), aoo_km2 = numeric()))
  }
  
  dplyr::group_by(cd, species) %>%
    dplyr::summarise(
      eoo_km2 = {
        coords <- unique(data.frame(lon = longitude, lat = latitude))
        if (nrow(coords) >= 3) {
          hull <- sf::st_convex_hull(sf::st_union(sf::st_as_sf(
            coords, coords = c("lon", "lat"), crs = 4326
          )))
          as.numeric(sf::st_area(hull)) / 1e6
        } else {
          NA_real_
        }
      },
      aoo_km2 = {
        coords <- data.frame(lon = longitude, lat = latitude)
        if (!nrow(coords)) {
          NA_real_
        } else {
          grid <- 0.018
          length(unique(paste(floor(coords$lon / grid), floor(coords$lat / grid)))) * 4
        }
      },
      .groups = "drop"
    )
}

# Helper: Infer provenance
infer_provenance <- function(output_dir = "checkover_output") {
  cache_dir <- file.path(output_dir, "cache")
  prov <- list(
    taxonomy_method = NA_character_,
    vernacular_method = NA_character_,
    teow_method = NA_character_,
    feow_method = NA_character_,
    wdpa_method = NA_character_,
    hydrobasins_method = NA_character_
  )
  
  prov$teow_method <- if (file.exists(file.path(cache_dir, "teow_worldecoregions_min.rds"))) {
    "ecoregions::worldecoregions (cached)"
  } else {
    NA_character_
  }
  
  prov$feow_method <- if (file.exists(file.path(cache_dir, "feow_min.rds"))) {
    "feowR::read_feow or local FEOW file (cached)"
  } else {
    NA_character_
  }
  
  local_wdpa <- length(list.files("spatial_data/wdpa", pattern = "\\.shp$",
                                  recursive = TRUE, full.names = TRUE)) > 0
  wdpar_cache <- length(list.files(cache_dir, pattern = "^wdpa_.*\\.rds$",
                                   full.names = TRUE)) > 0
  
  prov$wdpa_method <- if (local_wdpa) {
    "local WDPA shapefiles"
  } else if (wdpar_cache) {
    "wdpar::wdpa_fetch (cached)"
  } else {
    NA_character_
  }
  
  if (file.exists(file.path(cache_dir, "hydro_lev10_merged.rds")) ||
      file.exists(file.path(cache_dir, "hydro_lev08_merged.rds")) ||
      file.exists(file.path(cache_dir, "hydro_lev06_merged.rds"))) {
    prov$hydrobasins_method <- "local HydroBASINS shapefiles merged per level (L10/L8/L6)"
  }
  
  prov
}

# Main report builder
build_reports <- function(result, vernacular_result = NULL, output_dir = "checkover_output",
                          script_run_time = Sys.time(), provenance = NULL,
                          feow_lookup_path = NULL, hydrobasin_names = NULL) {
  module <- "MODULE4_REPORTS"
  
  with_log_section(module, {
    log_info("=== MODULE 4: BUILD REPORTS ===", module = module)
    
    if (!all(c("clean_data", "clean_sf") %in% names(result))) stop("Missing data")
    cd <- result$clean_data
    
    reports_dir <- file.path(output_dir, "reports")
    species_dir <- file.path(reports_dir, "species")
    if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE)
    if (!dir.exists(species_dir)) dir.create(species_dir, recursive = TRUE)
    
    # Load FEOW lookup
    feow_map <- NULL
    if (!is.null(feow_lookup_path) && file.exists(feow_lookup_path)) {
      log_info("Loading FEOW dictionary from: %s", feow_lookup_path, module = module)
      ext <- tolower(tools::file_ext(feow_lookup_path))
      tryCatch({
        if (ext == "tsv") {
          # TSV format (preferred)
          log_info("Reading FEOW dictionary as TSV...", module = module)
          feow_raw <- read_tsv(feow_lookup_path)
          
          # Expected format: ID | Realm | Major Habitat Type | Ecoregion
          # We only need: ID and Ecoregion
          if ("ID" %in% names(feow_raw) && "Ecoregion" %in% names(feow_raw)) {
            feow_map <- feow_raw[, c("ID", "Ecoregion"), drop = FALSE]
            names(feow_map) <- c("Ecoregion_ID", "Ecoregion_Name")
            feow_map$Ecoregion_ID <- as.integer(feow_map$Ecoregion_ID)
            log_info("Loaded %d FEOW ecoregions from TSV.", nrow(feow_map), module = module)
          } else {
            log_warn("FEOW TSV missing expected columns (ID, Ecoregion)", module = module)
          }
          
        } else if (ext == "csv") {
          # CSV format
          log_info("Reading FEOW dictionary as CSV...", module = module)
          feow_raw <- read.csv(feow_lookup_path, stringsAsFactors = FALSE)
          
          # Try to find ID and Ecoregion columns (case-insensitive)
          names_upper <- toupper(names(feow_raw))
          id_col <- names(feow_raw)[which(names_upper == "ID")[1]]
          eco_col <- names(feow_raw)[which(names_upper == "ECOREGION")[1]]
          
          if (!is.na(id_col) && !is.na(eco_col)) {
            feow_map <- feow_raw[, c(id_col, eco_col), drop = FALSE]
            names(feow_map) <- c("Ecoregion_ID", "Ecoregion_Name")
            feow_map$Ecoregion_ID <- as.integer(feow_map$Ecoregion_ID)
            log_info("Loaded %d FEOW ecoregions from CSV.", nrow(feow_map), module = module)
          } else {
            log_warn("FEOW CSV missing expected columns (ID, ECOREGION)", module = module)
          }
          
        } else if (ext %in% c("xlsx", "xls")) {
          # Excel format (legacy)
          log_info("Reading FEOW dictionary as Excel...", module = module)
          if (requireNamespace("readxl", quietly = TRUE)) {
            feow_raw <- as.data.frame(readxl::read_excel(feow_lookup_path))
            
            # Try to find ID and Ecoregion columns (case-insensitive)
            names_upper <- toupper(names(feow_raw))
            id_col <- names(feow_raw)[which(names_upper == "ID")[1]]
            eco_col <- names(feow_raw)[which(names_upper == "ECOREGION")[1]]
            
            if (!is.na(id_col) && !is.na(eco_col)) {
              feow_map <- feow_raw[, c(id_col, eco_col), drop = FALSE]
              names(feow_map) <- c("Ecoregion_ID", "Ecoregion_Name")
              feow_map$Ecoregion_ID <- as.integer(feow_map$Ecoregion_ID)
              log_info("Loaded %d FEOW ecoregions from Excel.", nrow(feow_map), module = module)
            } else {
              log_warn("FEOW Excel missing expected columns (ID, ECOREGION)", module = module)
            }
          } else {
            log_warn("readxl package not available. Cannot read Excel file.", module = module)
          }
        } else {
          log_warn("Unsupported FEOW file format: %s. Use .tsv, .csv, or .xlsx", 
                   ext, module = module)
        }
      }, error = function(e) {
        log_warn("FEOW lookup failed: %s", conditionMessage(e), module = module)
      })
    }
    
    # Provenance
    prov <- provenance %||% infer_provenance(output_dir)
    
    if (!is.null(result$taxonomy_map) && nrow(result$taxonomy_map) > 0) {
      prov$taxonomy_method <- "WoRMS via worrms package (fuzzy match)"
    }
    if (!is.null(vernacular_result) && !is.null(vernacular_result$wide) && 
        nrow(vernacular_result$wide) > 0) {
      prov$vernacular_method <- "ITIS API (ritis) or Manual Dictionary"
    }
    
    # Ensure columns exist
    cols_needed <- c("status", "is_type_locality", "year", "country", "continents",
                     "hydrobasin", "ecoregion", "freshwater_ecoregion", "protected_area",
                     "accuracy", "distribution_category")
    for (c in cols_needed) if (!c %in% names(cd)) cd[[c]] <- NA_character_
    
    sp_metrics <- compute_species_metrics(cd)
    
    # Aggregation
    detailed_reports <- cd %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(
        total_records = dplyr::n(),
        native_records = sum(tolower(status) == "native", na.rm = TRUE),
        alien_records = sum(tolower(status) == "alien", na.rm = TRUE),
        has_type_locality = any(is_type_locality, na.rm = TRUE),
        
        year_min = .safe_min(year),
        year_max = .safe_max(year),
        temporal_span = ifelse(is.na(year_min) | is.na(year_max), NA_real_, 
                               year_max - year_min + 1),
        
        countries_list = paste(sort(unique(na_chr(country)[nzchar(na_chr(country))])), 
                               collapse = " | "),
        continents_list = paste(sort(unique(na_chr(continents)[nzchar(na_chr(continents))])), 
                                collapse = " | "),
        ecoregions_list = paste(sort(unique(na_chr(ecoregion)[nzchar(na_chr(ecoregion))])), 
                                collapse = " | "),
        
        feow_list = {
          ids <- unique(na_chr(freshwater_ecoregion))
          ids <- ids[nzchar(ids) & !is.na(ids)]
          if (!is.null(feow_map) && length(ids) > 0) {
            mapped <- feow_map$Ecoregion_Name[match(as.integer(ids), feow_map$Ecoregion_ID)]
            vals <- ifelse(!is.na(mapped), mapped, ids)
            paste(sort(unique(vals)), collapse = " | ")
          } else {
            paste(sort(ids), collapse = " | ")
          }
        },
        
        protected_areas_list = {
          pas <- na_chr(protected_area)
          pas <- unlist(strsplit(pas, " \\| "))
          pas <- pas[nzchar(pas) & !is.na(pas)]
          paste(sort(unique(pas)), collapse = " | ")
        },
        
        hydrobasins_list = {
          codes <- unique(na_chr(hydrobasin))
          codes <- codes[nzchar(codes) & !is.na(codes)]
          if (length(codes) > 0 && !is.null(hydrobasin_names)) {
            named <- resolve_basin_names(codes, hydrobasin_names)
            paste(sort(unique(named)), collapse = " | ")
          } else {
            paste(sort(codes), collapse = " | ")
          }
        },
        
        countries_count = dplyr::n_distinct(country, na.rm = TRUE),
        basins_count = dplyr::n_distinct(hydrobasin, na.rm = TRUE),
        teow_count = dplyr::n_distinct(ecoregion, na.rm = TRUE),
        feow_count = dplyr::n_distinct(freshwater_ecoregion, na.rm = TRUE),
        
        protected_records = sum(!is.na(protected_area) & nzchar(protected_area), na.rm = TRUE),
        protection_pct = round((protected_records / total_records) * 100, 1),
        high_accuracy_rec = sum(tolower(accuracy) == "exact", na.rm = TRUE),
        recent_records = sum(year >= 2000, na.rm = TRUE),
        data_quality_score = round((high_accuracy_rec + recent_records) / (total_records * 2) * 100, 1),
        
        eoo_historical_km2 = dplyr::first(eoo_historical),
        aoo_historical_km2 = dplyr::first(aoo_historical),
        eoo_current_km2 = dplyr::first(eoo_current),
        aoo_current_km2 = dplyr::first(aoo_current),
        extirpation_signature = dplyr::first(extirpation_signature),
        distribution_category = dplyr::first(distribution_category),
        
        .groups = "drop"
      )
    
    write_tsv(detailed_reports, file.path(reports_dir, "species_detailed_reports.tsv"))
    
    # Dataset summary statistics
    summary_stats <- list(
      run_info = list(
        script_run_utc = format(as.POSIXct(script_run_time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
        script_version = format(as.Date(script_run_time), "%Y-%m-%d")
      ),
      methods = list(
        taxonomy = na_chr(prov$taxonomy_method),
        vernacular = na_chr(prov$vernacular_method),
        teow = na_chr(prov$teow_method),
        feow = na_chr(prov$feow_method),
        wdpa = na_chr(prov$wdpa_method),
        hydrobasins = na_chr(prov$hydrobasins_method)
      ),
      totals = list(
        total_species = nrow(detailed_reports),
        total_records = sum(detailed_reports$total_records, na.rm = TRUE)
      ),
      categories = list(
        micro_endemic = sum(detailed_reports$distribution_category == "micro-endemic", na.rm = TRUE),
        endemic = sum(detailed_reports$distribution_category == "endemic", na.rm = TRUE),
        regional = sum(detailed_reports$distribution_category == "regional", na.rm = TRUE),
        cosmopolitan = sum(detailed_reports$distribution_category == "cosmopolitan", na.rm = TRUE)
      ),
      conservation = list(
        species_in_protected_areas = sum(detailed_reports$protected_records > 0, na.rm = TRUE),
        avg_protection_percentage = round(mean(detailed_reports$protection_pct, na.rm = TRUE), 1)
      ),
      data_quality = list(
        avg_quality_score = round(mean(detailed_reports$data_quality_score, na.rm = TRUE), 1),
        species_with_type_locality = sum(detailed_reports$has_type_locality, na.rm = TRUE)
      ),
      temporal = list(
        oldest_record = suppressWarnings(min(detailed_reports$year_min, na.rm = TRUE)),
        newest_record = suppressWarnings(max(detailed_reports$year_max, na.rm = TRUE)),
        avg_temporal_span = round(mean(detailed_reports$temporal_span, na.rm = TRUE), 1)
      )
    )
    
    summary_json_file <- file.path(reports_dir, "dataset_summary_statistics.json")
    jsonlite::write_json(summary_stats, summary_json_file, pretty = TRUE, auto_unbox = TRUE)
    log_info("Wrote dataset summary JSON.", module = module)
    
    # Per-species JSON
    vern_map <- vernacular_result
    frag_df <- if (!is.null(result$fragmentation)) result$fragmentation else NULL
    
    log_info("Writing per-species JSON reports...", module = module)
    pb <- create_progress_bar(nrow(detailed_reports))
    
    for (i in seq_len(nrow(detailed_reports))) {
      pb$tick()
      
      row <- detailed_reports[i, ]
      sp <- row$species
      sp_clean <- gsub("[^A-Za-z0-9_\\.]+", "_", sp)
      
      # Extract vernacular
      v_str <- NA_character_
      if (!is.null(vern_map)) {
        if (is.list(vern_map) && "wide" %in% names(vern_map)) {
          vern_df <- vern_map$wide
        } else if (is.data.frame(vern_map)) {
          vern_df <- vern_map
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
      
      frag_block <- list(computed = FALSE, status = "not_computed")
      if (!is.null(frag_df)) {
        fr <- frag_df[frag_df$species == sp, ]
        if (nrow(fr) > 0) {
          frag_block <- list(
            computed = as.logical(fr$computed),
            scope = na_chr(fr$scope),
            status = na_chr(fr$status),
            n_clusters = na_num(fr$n_clusters),
            cluster_sizes_n = na_chr(fr$cluster_sizes_n),
            mean_threshold_km = na_num(fr$mean_distance_km)
          )
        }
      }
      
      sp_json <- list(
        species = sp,
        vernaculars = v_str,
        metrics = list(
          total_records = na_num(row$total_records),
          native_records = na_num(row$native_records),
          alien_records = na_num(row$alien_records),
          eoo_km2 = na_num(row$eoo_current_km2),
          aoo_km2 = na_num(row$aoo_current_km2),
          eoo_historical_km2 = na_num(row$eoo_historical_km2),
          aoo_historical_km2 = na_num(row$aoo_historical_km2),
          eoo_current_km2 = na_num(row$eoo_current_km2),
          aoo_current_km2 = na_num(row$aoo_current_km2),
          extirpation_signature = na_chr(row$extirpation_signature),
          has_type_locality = isTRUE(as.logical(row$has_type_locality)),
          year_range = list(min = na_num(row$year_min), max = na_num(row$year_max)),
          countries = na_chr(row$countries_list),
          continents = na_chr(row$continents_list),
          ecoregions = na_chr(row$ecoregions_list),
          freshwater_ecoregions = na_chr(row$feow_list),
          hydrobasins = na_chr(row$hydrobasins_list),
          protected_areas = na_chr(row$protected_areas_list),
          count_countries = na_num(row$countries_count),
          count_basins = na_num(row$basins_count),
          protection_percentage = na_num(row$protection_pct),
          category = na_chr(row$distribution_category)
        ),
        fragmentation_signal = frag_block
      )
      
      jsonlite::write_json(sp_json, file.path(species_dir, paste0(sp_clean, ".json")),
                           pretty = TRUE, auto_unbox = TRUE)
    }
    
    pb$terminate()
    
    html_report_file <- file.path(reports_dir, "dataset_summary_report.html")
    writeLines(paste0("<html><body><h1>cheCkOVER Report</h1><p>Generated: ", 
                      Sys.time(), "</p></body></html>"), html_report_file)
    
    log_info("Report generation complete.", module = module)
    
    detailed_file <- file.path(reports_dir, "species_detailed_reports.tsv")
    
    return(list(
      detailed_reports = detailed_reports,
      summary_stats = summary_stats,
      files_created = c(detailed_file, summary_json_file, html_report_file)
    ))
  })
}