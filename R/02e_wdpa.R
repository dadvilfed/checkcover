#### MODULE 2E: WDPA ENRICHMENT ####

enrich_with_wdpa <- function(result, output_dir = "checkover_output",
                             cache_dir = file.path(output_dir, "cache"),
                             use_point_pas = FALSE, POINT_MATCH_MAX_KM = 2,
                             clean_wdpa_geometry = TRUE) {
  module <- "MODULE2E_WDPA"
  
  with_log_section(module, {
    log_info("=== MODULE 2E: WDPA ENRICHMENT (protected_area) ===", module = module)
    
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Missing 'clean_data' or 'clean_sf' in result.")
    }
    if (!inherits(result$clean_sf, "sf")) {
      stop("'result$clean_sf' must be an sf POINT object.")
    }
    if (!requireNamespace("wdpar", quietly = TRUE)) {
      stop("Package 'wdpar' required. Install with install.packages('wdpar').")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created cache directory: %s", cache_dir, module = module)
    }
    
    # Helper: pick PA name column
    .pick_name <- function(x) {
      cands <- c("NAME", "ORIG_NAME", "REC_SIT_N", "WDPA_NAME", "SITE_NAME")
      hits <- cands[cands %in% names(x)]
      if (!length(hits)) return(NULL)
      for (h in hits) {
        v <- x[[h]]
        if (is.factor(v)) v <- as.character(v)
        if (is.character(v) && any(nzchar(v))) return(h)
      }
      hits[1]
    }
    
    # Helper: cache path
    .wdpa_cache <- function(iso) file.path(cache_dir, paste0("wdpa_", iso, ".rds"))
    
    # Helper: fix ISO3
    .fix_iso3 <- function(iso3) {
      m <- c("ROM" = "ROU", "KSV" = NA_character_, "XKX" = NA_character_)
      out <- ifelse(iso3 %in% names(m), m[iso3], iso3)
      if (is.na(out)) {
        log_warn("ISO '%s' not supported by WDPA. Will skip.", iso3, module = module)
      }
      out
    }
    
    .empty_sf <- function(crs) sf::st_sf(geometry = sf::st_sfc(), crs = crs)
    
    # Helper: fetch WDPA
    .fetch_wdpa_one <- function(iso, target_crs) {
      cp <- .wdpa_cache(iso)
      if (is.na(iso) || !nzchar(iso)) {
        log_warn("Invalid ISO. Skipping fetch.", module = module)
        return(.empty_sf(target_crs))
      }
      if (file.exists(cp)) {
        log_info("Using cached WDPA for %s", iso, module = module)
        x <- readRDS(cp)
      } else {
        log_info("Downloading WDPA for %s...", iso, module = module)
        x <- try(wdpar::wdpa_fetch(iso, wait = TRUE, download_dir = cache_dir), silent = TRUE)
        if (inherits(x, "try-error")) {
          log_warn("WDPA fetch rejected ISO '%s'. Skipping.", iso, module = module)
          return(.empty_sf(target_crs))
        }
        if (clean_wdpa_geometry && "wdpa_clean" %in% getNamespaceExports("wdpar")) {
          log_info("Cleaning WDPA geometries for %s...", iso, module = module)
          x_try <- try(wdpar::wdpa_clean(x), silent = TRUE)
          if (!inherits(x_try, "try-error")) {
            x <- x_try
          } else {
            log_warn("wdpa_clean() failed for %s. Using raw.", iso, module = module)
          }
        }
        x <- sf::st_as_sf(x)
        x <- sf::st_make_valid(x)
        x <- sf::st_zm(x, drop = TRUE, what = "ZM")
        .save_with_lock(x, cp)
        log_info("Cached WDPA for %s", iso, module = module)
      }
      if (is.na(sf::st_crs(x))) sf::st_crs(x) <- target_crs
      if (sf::st_crs(x) != target_crs) x <- sf::st_transform(x, target_crs)
      .std_geom(x)
    }
    
    # Helper: extract polygons
    .extract_polygons_any <- function(x, name_col) {
      poly_sf <- try(suppressWarnings(sf::st_collection_extract(x, "POLYGON")), silent = TRUE)
      if (inherits(poly_sf, "try-error")) {
        gt <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
        keep <- gt %in% c("POLYGON", "MULTIPOLYGON")
        poly_sf <- x[keep & !sf::st_is_empty(x), , drop = FALSE]
      }
      poly_sf <- .std_geom(poly_sf)
      keep_cols <- intersect(c(name_col, "geometry"), names(poly_sf))
      poly_sf <- poly_sf[, keep_cols, drop = FALSE]
      casted <- try(suppressWarnings(sf::st_cast(poly_sf, "MULTIPOLYGON", warn = FALSE)), silent = TRUE)
      if (!inherits(casted, "try-error")) poly_sf <- casted
      names(poly_sf)[names(poly_sf) == name_col] <- "PA_NAME"
      poly_sf[!sf::st_is_empty(poly_sf), , drop = FALSE]
    }
    
    # Detect countries
    log_info("Detecting target countries for WDPA download...", module = module)
    ne <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      dplyr::select(iso_a3, iso_a3_eh, wb_a3, adm0_a3, name, name_long, geometry) %>%
      sf::st_make_valid()
    ne$iso3_best <- dplyr::coalesce(
      dplyr::na_if(ne$iso_a3, "-99"),
      dplyr::na_if(ne$iso_a3_eh, "-99"),
      dplyr::na_if(ne$wb_a3, "-99"),
      dplyr::na_if(ne$adm0_a3, "-99")
    )
    
    pts_ne <- suppressWarnings(sf::st_join(
      result$clean_sf,
      ne[, c("iso3_best", "name", "name_long", "geometry")],
      join = sf::st_within,
      left = TRUE
    ))
    
    iso_vec <- pts_ne$iso3_best
    iso_list <- unique(iso_vec[!is.na(iso_vec) & grepl("^[A-Z]{3}$", iso_vec)])
    iso_list <- setdiff(iso_list, c("ATA"))
    
    log_info("Countries detected (ISO3): %s", paste(iso_list, collapse = ", "), module = module)
    
    target_crs <- sf::st_crs(result$clean_sf)
    result$clean_data$protected_area <- NA_character_
    result$clean_sf$protected_area <- NA_character_
    idx_by_iso <- split(seq_len(nrow(result$clean_sf)), iso_vec)
    dist_m <- as.numeric(units::set_units(POINT_MATCH_MAX_KM, "km")) * 1000
    
    pb <- create_progress_bar(length(iso_list))
    
    for (iso_raw in iso_list) {
      pb$tick()
      log_info("Processing WDPA for: %s", iso_raw, module = module)
      
      idx <- idx_by_iso[[iso_raw]]
      if (length(idx) == 0L) {
        log_debug("No points for ISO %s, skipping.", iso_raw, module = module)
        next
      }
      
      pts_iso <- result$clean_sf[idx, , drop = FALSE]
      if (sf::st_crs(pts_iso) != target_crs) {
        pts_iso <- sf::st_transform(pts_iso, target_crs)
      }
      
      iso <- .fix_iso3(iso_raw)
      wdpa_sf <- .fetch_wdpa_one(iso, target_crs)
      if (nrow(wdpa_sf) == 0) {
        log_info("No WDPA features for %s. Skipping.", iso, module = module)
        next
      }
      
      name_col <- .pick_name(wdpa_sf)
      if (is.null(name_col)) {
        log_warn("No PA name column for ISO '%s'. Skipping.", iso, module = module)
        next
      }
      
      wdpa_poly <- .extract_polygons_any(wdpa_sf, name_col)
      log_debug("WDPA polygons for %s: %d features.", iso, nrow(wdpa_poly), module = module)
      
      poly_ix <- if (nrow(wdpa_poly) > 0) {
        .safe_intersects(pts_iso, wdpa_poly)
      } else {
        rep(list(integer(0)), nrow(pts_iso))
      }
      
      if (use_point_pas) {
        pts_only <- try(suppressWarnings(sf::st_collection_extract(wdpa_sf, "POINT")), silent = TRUE)
        if (inherits(pts_only, "try-error")) {
          gt2 <- as.character(sf::st_geometry_type(wdpa_sf, by_geometry = TRUE))
          pt_mask <- gt2 %in% c("POINT", "MULTIPOINT")
          pts_only <- wdpa_sf[pt_mask & !sf::st_is_empty(wdpa_sf), , drop = FALSE]
        }
        pts_only <- .std_geom(pts_only)
        keep_cols <- intersect(c(name_col, "geometry"), names(pts_only))
        pts_only <- if (length(keep_cols)) {
          pts_only[, keep_cols, drop = FALSE]
        } else {
          pts_only[, "geometry", drop = FALSE]
        }
        names(pts_only)[names(pts_only) == name_col] <- "PA_NAME"
        near_ix <- if (nrow(pts_only) > 0) {
          .safe_within_distance(pts_iso, pts_only, dist_m)
        } else {
          rep(list(integer(0)), nrow(pts_iso))
        }
      } else {
        pts_only <- NULL
        near_ix <- rep(list(integer(0)), nrow(pts_iso))
      }
      
      out_vec <- character(length(idx))
      for (i in seq_along(idx)) {
        n_poly <- if (length(poly_ix[[i]]) > 0) {
          wdpa_poly$PA_NAME[poly_ix[[i]]]
        } else {
          character(0)
        }
        n_pts <- if (!is.null(pts_only) && length(near_ix[[i]]) > 0) {
          pts_only$PA_NAME[near_ix[[i]]]
        } else {
          character(0)
        }
        nms <- unique(c(n_poly, n_pts))
        nms <- nms[!is.na(nms) & nzchar(nms)]
        out_vec[i] <- if (length(nms) == 0) NA_character_ else paste(nms, collapse = " | ")
      }
      
      result$clean_sf$protected_area[idx] <- out_vec
      result$clean_data$protected_area[idx] <- out_vec
      
      rm(wdpa_sf, wdpa_poly, pts_only, poly_ix, near_ix, out_vec)
      gc()
    }
    
    pb$terminate()
    
    out_tsv <- file.path(output_dir, "clean_occurrences_with_wdpa.tsv")
    out_rds <- file.path(output_dir, "clean_occurrences_sf_with_wdpa.rds")
    
    write_tsv(result$clean_data, out_tsv)
    saveRDS(result$clean_sf, out_rds)
    
    log_info("Added column: protected_area.", module = module)
    
    result$files_created <- unique(c(result$files_created, out_tsv, out_rds))
    
    result
  })
}