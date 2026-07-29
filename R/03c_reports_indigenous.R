#### MODULE 3C: INDIGENOUS REPORTS ####

#' Generate detailed JSON reports for indigenous species
#' @param result_indigenous   Enriched indigenous result (list with $clean_data,
#'                            $fragmentation; clean_data must contain taxonomy
#'                            columns joined by Module 1: order, superfamily, family)
#' @param vernacular_lookup   Vernacular names lookup (list with $wide, or data frame)
#' @param output_dir          Output directory
#' @param feow_lookup_path    Path to FEOW TSV lookup (ID | Realm | Major Habitat Type | Ecoregion)
#' @param hydrobasin_names    HydroBASINS name lookup data frame (Basin_level | HYBAS_ID |
#'                            Basin_name | Subbasin_name), as loaded by load_hydrobasin_names()
#' @return List of generated reports
generate_indigenous_reports <- function(result_indigenous,
                                        vernacular_lookup  = NULL,
                                        output_dir         = "checkover_output",
                                        feow_lookup_path   = NULL,
                                        hydrobasin_names   = NULL) {
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
    
    # ---------- Shared lookups ----------
    vern_map <- .extract_vern_map_3c(vernacular_lookup)
    feow_map <- .load_feow_map_3c(feow_lookup_path, module)
    frag_df  <- result_indigenous$fragmentation %||% NULL
    
    reports_generated <- list()
    
    for (sp in species_list) {
      sp_data_all <- cd[cd$species == sp, ]
      # Filter to active records for metrics; keep sp_data_all for extinction counts
      sp_data <- if ("temporal_status" %in% names(sp_data_all)) {
        sp_data_all[sp_data_all$temporal_status == "active", , drop = FALSE]
      } else sp_data_all
      sp_clean <- make_package_id(sp)
      
      # --- Vernacular ---
      vern_str <- .get_vern_str_3c(sp, vern_map)
      
      # --- Higher taxonomy ---
      # Reads columns joined from WoRMS resolution in Module 1 (01_ingest.R).
      # Template format: "Order > Superfamily > Family"
      tax_order      <- .first_val(sp_data$order)
      tax_superfam   <- .first_val(sp_data$superfamily)
      tax_family     <- .first_val(sp_data$family)
      higher_taxonomy <- .build_higher_taxonomy(tax_order, tax_superfam, tax_family)

      # ── ZERO-ACTIVE (TOTAL EXTINCTION) TERMINAL STATE ──────────────────────
      # All indigenous occurrences extirpated (0 active, >=1 known). This is a
      # VALID terminal state, not an error. Do NOT attempt classification or
      # spatial enumeration (they assume >=1 point and otherwise crash silently,
      # leaving a partial folder). Close everything on zero, mark status Extinct,
      # count the extirpated localities, and attach the mandatory data-state
      # disclaimer (Lucian, 2026-06). Metrics/counts computed on sp_data below
      # would otherwise return Inf/NaN on the empty active slice.
      if (nrow(sp_data) == 0L && nrow(sp_data_all) > 0L) {
        # Taxonomy is population-status-independent — take it from the full slice
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
          population_type  = "indigenous",
          vernacular_names = vern_str,
          status           = "Extinct",
          terminal_state   = "zero_active",
          taxonomy = list(
            order       = if (!is.na(tax_order))    tax_order    else NULL,
            superfamily = if (!is.na(tax_superfam)) tax_superfam else NULL,
            family      = if (!is.na(tax_family))   tax_family   else NULL,
            higher_taxonomy = if (!is.na(higher_taxonomy)) higher_taxonomy else NULL
          ),
          # Close on zero: AOO = 0, EOO = NA (undefined), no category attempted.
          metrics = list(
            n_records         = 0L,
            eoo_km2           = NA_real_,
            aoo_km2           = 0,
            iucn_category     = "Extinct",
            hydrobasins_level = NA_integer_
          ),
          temporal = list(
            year_min      = first_ext_y,
            year_max      = last_ext_y,
            year_range    = if (!is.na(first_ext_y) && !is.na(last_ext_y)) last_ext_y - first_ext_y + 1 else NA_integer_,
            pct_post_2000 = NA_real_
          ),
          # No active occurrences -> no basins, no ecoregions, no PAs.
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
          spatial_clustering = list(computed = FALSE, status = "not_applicable_extinct"),
          type_locality_present = isTRUE(any(sp_data_all$is_type_locality, na.rm = TRUE)),
          extinction_year = last_ext_y,
          integrity_flag  = "MAX",
          disclaimer      = CHECKOVER_EXTINCTION_DISCLAIMER
        )

        json_file <- file.path(reports_dir, paste0(sp_clean, ".json"))
        jsonlite::write_json(report, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
        reports_generated[[sp]] <- json_file
        log_info("  %s: zero-active terminal state (Extinct) — %d extirpated localit%s",
                 sp, n_ext_loc, if (n_ext_loc != 1L) "ies" else "y", module = module)
        next
      }

      # --- Resolve FEOW IDs -> names ---
      feow_raw      <- sp_data$freshwater_ecoregion[!is.na(sp_data$freshwater_ecoregion)]
      feow_resolved <- .resolve_feow_3c(feow_raw, feow_map)
      feow_unique   <- sort(unique(feow_resolved[nzchar(feow_resolved)]))
      
      # --- Hydrographic basins: units vs named basins (Lucian, 2026-07) ---
      # Two DISTINCT quantities, kept under distinct names to avoid the
      # "basins means two things" collision:
      #   * basin UNITS  = distinct HydroBASINS polygon codes (HYBAS_ID). This
      #     is the fine count that feeds package_metadata basins_count. A record
      #     may span >1 unit (pipe-joined), so we split first.
      #   * NAMED basins = distinct human-readable river/basin names (river-aware
      #     resolution). Fewer than units; used for the narrative enumeration.
      basin_cells    <- sp_data$hydrobasin[!is.na(sp_data$hydrobasin) & nzchar(sp_data$hydrobasin)]
      basin_codes    <- unique(unlist(strsplit(basin_cells, "\\s*\\|\\s*")))
      basin_codes    <- basin_codes[nzchar(basin_codes)]
      n_basin_units  <- length(basin_codes)
      basin_resolved <- .resolve_basin_3c(basin_codes, hydrobasin_names)
      basin_unique   <- sort(unique(basin_resolved[nzchar(basin_resolved)]))
      n_named_basins <- length(basin_unique)
      
      # --- Fragmentation block ---
      frag_block <- .build_frag_block_3c(sp, frag_df)
      
      # --- v3 metadata derivations: distinct PAs, extinction localities, type locality ---
      # PAs derived from ACTIVE records (extinct/suppressed records' PA membership shouldn't count)
      pa_split <- unlist(strsplit(
        sp_data$protected_area[!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)],
        "\\s*\\|\\s*"
      ))
      n_distinct_pas <- length(unique(pa_split[nzchar(pa_split)]))
      
      # Extinctions: use the FULL slice (sp_data_all), not the active-filtered one,
      # because extinct records ARE the thing we're counting here.
      ext_idx <- which(!is.na(sp_data_all$is_extinct) & sp_data_all$is_extinct == TRUE)
      n_extinctions <- if (length(ext_idx) > 0L) {
        length(unique(paste(sp_data_all$longitude[ext_idx], sp_data_all$latitude[ext_idx])))
      } else 0L
      
      type_locality_present <- isTRUE(any(sp_data$is_type_locality, na.rm = TRUE))
      
      # --- Build report ---
      report <- list(
        species         = sp,
        population_type = "indigenous",
        vernacular_names = vern_str,
        
        taxonomy = list(
          order       = if (!is.na(tax_order))    tax_order    else NULL,
          superfamily = if (!is.na(tax_superfam)) tax_superfam else NULL,
          family      = if (!is.na(tax_family))   tax_family   else NULL,
          higher_taxonomy = if (!is.na(higher_taxonomy)) higher_taxonomy else NULL
        ),
        
        metrics = list(
          n_records        = nrow(sp_data),
          eoo_km2          = sp_data$eoo_km2[1],
          aoo_km2          = sp_data$aoo_km2[1],
          iucn_category    = sp_data$iucn_category[1],
          hydrobasins_level = sp_data$hydrobasins_level[1]
        ),
        
        temporal = list(
          year_min  = min(sp_data$year, na.rm = TRUE),
          year_max  = max(sp_data$year, na.rm = TRUE),
          year_range = max(sp_data$year, na.rm = TRUE) - min(sp_data$year, na.rm = TRUE) + 1,
          # post-2000 means strictly year > 2000 (a record dated 2000 is NOT
          # post-2000). Boundary agreed with the narrative generator so the two
          # never diverge (Lucian, 2026-06, bug 3a).
          pct_post_2000 = if (nrow(sp_data) > 0L) round(100 * sum(sp_data$year > 2000, na.rm = TRUE) / nrow(sp_data), 1) else NA_real_
        ),
        
        spatial_context = list(
          # geo_usable() strips NA/blank AND the `unresolved` sentinel, so an
          # unresolvable record never appears in an exposed geography list.
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
          # basins_count in package_metadata binds to n_hydrobasins = fine UNIT
          # count (distinct HYBAS_ID polygons). n_named_basins is the distinct
          # river/basin NAME count used for the narrative enumeration — kept
          # separate so the two are never confused (Lucian 2026-07).
          n_hydrobasins     = n_basin_units,
          n_named_basins    = n_named_basins,
          n_distinct_protected_areas = n_distinct_pas,
          n_extinctions     = n_extinctions
        ),
        
        conservation = list(
          n_protected_records   = sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)),
          protection_percentage = round(
            sum(!is.na(sp_data$protected_area) & nzchar(sp_data$protected_area)) / nrow(sp_data) * 100, 1)
        ),
        
        # Key renamed from `fragmentation` (Lucian, 2026-07): that term is
        # reserved for checKOLOGY; here it is a descriptive spatial signal only.
        spatial_clustering = frag_block,
        type_locality_present = type_locality_present
      )
      
      json_file <- file.path(reports_dir, paste0(sp_clean, ".json"))
      jsonlite::write_json(report, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
      reports_generated[[sp]] <- json_file
    }
    
    log_info("Generated %d per-species reports.", length(reports_generated), module = module)
    
    # Group summary report (unchanged)
    summary_report <- list(
      population_type = "indigenous",
      total_species   = length(species_list),
      total_records   = nrow(cd),
      
      categorization = list(
        endemic      = sum(cd$iucn_category == "endemic",      na.rm = TRUE) / nrow(cd) * 100,
        regional     = sum(cd$iucn_category == "regional",     na.rm = TRUE) / nrow(cd) * 100,
        cosmopolitan = sum(cd$iucn_category == "cosmopolitan", na.rm = TRUE) / nrow(cd) * 100
      ),
      
      temporal = list(
        oldest_record = min(cd$year, na.rm = TRUE),
        newest_record = max(cd$year, na.rm = TRUE)
      ),
      
      conservation = list(
        avg_protection_pct = round(mean(
          tapply(!is.na(cd$protected_area) & nzchar(cd$protected_area),
                 cd$species, mean, na.rm = TRUE) * 100), 1)
      )
    )
    
    summary_file <- file.path(reports_dir, "group_summary.json")
    jsonlite::write_json(summary_report, summary_file, pretty = TRUE, auto_unbox = TRUE)
    log_info("Saved group summary to: %s", summary_file, module = module)
    log_info("Indigenous reports complete.", module = module)
    
    return(list(
      per_species   = reports_generated,
      group_summary = summary_file
    ))
  })
}


# ===========================================================================
# MODULE-PRIVATE HELPERS
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

# Return first non-NA value from a vector (for per-species taxonomy columns)
.first_val <- function(x) {
  v <- x[!is.na(x) & nzchar(x)]
  if (length(v) == 0) return(NA_character_)
  v[1]
}

# Build "Order > Superfamily > Family" string; omit NA components
.build_higher_taxonomy <- function(order, superfamily, family) {
  parts <- c(order, superfamily, family)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = " > ")
}

# Load FEOW lookup from a plain TSV file
# Expected columns: ID, Realm, Major Habitat Type, Ecoregion
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

# Resolve a vector of raw FEOW IDs to ecoregion names
.resolve_feow_3c <- function(x, feow_map) {
  if (is.null(feow_map) || length(x) == 0) return(x)
  id_col   <- intersect(c("ID", "FEOW_ID"),                             names(feow_map))[1]
  name_col <- intersect(c("ECOREGION", "NAME", "ECOREGION_NAME"),       names(feow_map))[1]
  if (is.na(id_col) || is.na(name_col)) return(x)
  ids       <- suppressWarnings(as.integer(x))
  match_idx <- match(ids, feow_map[[id_col]])
  resolved  <- feow_map[[name_col]][match_idx]
  ok        <- !is.na(resolved) & nzchar(resolved)
  x[ok]     <- resolved[ok]
  x
}

# Resolve a vector of raw hydrobasin codes to basin names
# Input format: "L10:2100522290"
# Table columns: Basin_level | HYBAS_ID | Basin_name | Subbasin_name
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
    # Finest available name (river-aware): Basin > Subbasin > river_name.
    basin_display_name(
      hb_lookup$Basin_name[idx],
      hb_lookup$Subbasin_name[idx],
      if ("river_name" %in% names(hb_lookup)) hb_lookup$river_name[idx] else NA_character_,
      fallback = code
    )
  }, character(1), USE.NAMES = FALSE)
}

.build_frag_block_3c <- function(sp, frag_df) {
  if (is.null(frag_df)) return(list(computed = FALSE, status = "not_computed"))
  fr <- frag_df[frag_df$species == sp, ]
  if (nrow(fr) == 0) return(list(computed = FALSE, status = "not_computed"))
  list(
    computed          = as.logical(fr$computed),
    scope             = as.character(fr$scope),
    status            = as.character(fr$status),
    n_clusters        = if (!is.na(fr$n_clusters))     as.integer(fr$n_clusters)    else NA,
    cluster_sizes     = if (!is.na(fr$cluster_sizes_n)) as.character(fr$cluster_sizes_n) else NA,
    mean_threshold_km = if (!is.na(fr$mean_distance_km)) as.numeric(fr$mean_distance_km) else NA
  )
}

# NULL coalescing (safe re-definition)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}