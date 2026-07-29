#### MODULE 2B: GADM ENRICHMENT ####

enrich_with_gadm <- function(result, output_dir = "checkover_output",
                             cache_dir = file.path(output_dir, "cache"),
                             version = "4.1", res = 1) {
  module <- "MODULE2B_GADM"
  
  with_log_section(module, {
    log_info("=== MODULE 2B: GADM ENRICHMENT (country + admin_1) ===", module = module)
    
    # Preconditions
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Expected result with clean_data and clean_sf.")
    }
    if (!inherits(result$clean_sf, "sf")) {
      stop("result$clean_sf must be sf.")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created cache directory: %s", cache_dir, module = module)
    }
    
    # Helper: standardize geometry column
    .std_geom <- function(x) {
      x <- sf::st_as_sf(x)
      g <- attr(x, "sf_column")
      if (!identical(g, "geometry")) {
        names(x)[names(x) == g] <- "geometry"
        attr(x, "sf_column") <- "geometry"
      }
      x
    }
    
    # Helper: pick name column
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
    
    # Helper: cache file path
    .wdpa_cache <- function(iso) file.path(cache_dir, paste0("wdpa_", iso, ".rds"))
    
    # Helper: fix ISO3
    .fix_iso3 <- function(iso3) {
      m <- c("ROM" = "ROU", "KSV" = NA_character_, "XKX" = NA_character_)
      out <- ifelse(iso3 %in% names(m), m[iso3], iso3)
      if (is.na(out)) {
        log_warn("ISO '%s' not supported by GADM. Will skip.", iso3, module = module)
      }
      out
    }
    
    .empty_sf <- function(crs) sf::st_sf(geometry = sf::st_sfc(), crs = crs)
    
    # Helper: fetch GADM level
    .fetch_gadm_level <- function(country_code_or_name, level) {
      log_debug("Fetching GADM level %d for '%s' (version=%s, res=%s)...",
                level, country_code_or_name, version, as.character(res), module = module)
      vv <- try(geodata::gadm(
        country = country_code_or_name,
        level = level,
        path = cache_dir,
        version = version,
        resolution = res
      ), silent = TRUE)
      if (inherits(vv, "try-error") || is.null(vv)) {
        log_warn("Failed to fetch GADM level %d for '%s'.",
                 level, country_code_or_name, module = module)
        return(NULL)
      }
      vv
    }
    
    # ── WoC-native first, GADM overlay only as fallback ──────────────────────
    # country / admin_1 are curated in the WoC database (canonical names with
    # diacritics; coastal points already snapped to the nearest shore). Those
    # values are authoritative: we only compute the ones WoC left empty and we
    # NEVER overwrite a populated native value (Lucian, 2026-07).
    for (cc in c("country", "admin_1")) {
      if (!cc %in% names(result$clean_data)) result$clean_data[[cc]] <- NA_character_
      if (!cc %in% names(result$clean_sf))   result$clean_sf[[cc]]   <- NA_character_
      result$clean_data[[cc]] <- nz_or_na(as.character(result$clean_data[[cc]]))
      result$clean_sf[[cc]]   <- nz_or_na(as.character(result$clean_sf[[cc]]))
    }
    # Country names are normalised to the canonical WoC vocabulary (USA ->
    # United States, Czechia -> Czech Republic, ...) so cheCkOVER never emits a
    # variant spelling. canon_country() returns the sentinel for blanks; convert
    # back to NA here so the gap detection below still finds them.
    result$clean_data$country <- canon_country(result$clean_data$country)
    result$clean_sf$country   <- canon_country(result$clean_sf$country)
    result$clean_data$country[result$clean_data$country == GEO_UNRESOLVED] <- NA_character_
    result$clean_sf$country[result$clean_sf$country     == GEO_UNRESOLVED] <- NA_character_
    for (sc in c("country_source", "admin1_source")) {
      if (!sc %in% names(result$clean_data)) result$clean_data[[sc]] <- NA_character_
      if (!sc %in% names(result$clean_sf))   result$clean_sf[[sc]]   <- NA_character_
    }

    need_mask <- is.na(result$clean_data$country) | is.na(result$clean_data$admin_1)
    need_idx  <- which(need_mask)
    n_need    <- length(need_idx)
    log_info("GADM: %d/%d records already carry WoC-native country/admin_1; %d need computing.",
             nrow(result$clean_data) - n_need, nrow(result$clean_data), n_need, module = module)

    if (n_need == 0L) {
      log_info("All records carry native country and admin_1 — skipping the GADM overlay entirely.",
               module = module)
    } else {

    # Tag points with ISO3 using Natural Earth
    log_info("Tagging points with admin-0 (Natural Earth) to find target countries...",
             module = module)
    
    ne_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      dplyr::select(iso_a3, iso_a3_eh, wb_a3, adm0_a3, name, name_long = name_long, geometry)
    ne_countries <- sanitize_spatial_layer(ne_countries, layer_name = "NE_countries_GADM")
    
    log_debug("Loaded Natural Earth admin-0 with %d features.", nrow(ne_countries), module = module)
    
    ne_countries$iso3_best <- dplyr::coalesce(
      dplyr::na_if(ne_countries$iso_a3, "-99"),
      dplyr::na_if(ne_countries$iso_a3_eh, "-99"),
      dplyr::na_if(ne_countries$wb_a3, "-99"),
      dplyr::na_if(ne_countries$adm0_a3, "-99")
    )
    
    # Only the gap rows are overlaid — everything else keeps its native value.
    pts_ne <- suppressWarnings(sf::st_join(
      result$clean_sf[need_idx, , drop = FALSE],
      ne_countries[, c("iso3_best", "name", "name_long", "geometry")],
      join = sf::st_within,
      left = TRUE
    ))

    iso_per_point <- pts_ne$iso3_best
    name_per_point <- ifelse(is.na(pts_ne$name_long), pts_ne$name, pts_ne$name_long)

    good_iso_mask <- !is.na(iso_per_point) & grepl("^[A-Z]{3}$", iso_per_point)
    iso_list <- unique(iso_per_point[good_iso_mask])
    iso_list <- setdiff(iso_list, "ATA")

    log_info("Countries detected in the gap records (ISO3): %s",
             paste(iso_list, collapse = ", "), module = module)

    # Map ISO3 -> ORIGINAL row indices (need_idx), so write-back targets the
    # right rows of the full table.
    pts_idx_by_iso <- split(need_idx, iso_per_point)
    
    # Map ISO3 to NE country name
    iso_to_name <- ne_countries %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(iso3_best, ne_name = dplyr::coalesce(name_long, name)) %>%
      dplyr::distinct()
    
    target_crs <- sf::st_crs(result$clean_sf)
    
    # Process per ISO3
    pb <- create_progress_bar(length(iso_list))
    
    for (iso_raw in iso_list) {
      pb$tick()
      log_info("Processing GADM for: %s", iso_raw, module = module)
      
      idx <- pts_idx_by_iso[[iso_raw]]
      if (length(idx) == 0L) {
        log_debug("No points for ISO %s, skipping.", iso_raw, module = module)
        next
      }
      
      pts_iso <- result$clean_sf[idx, , drop = FALSE]
      if (sf::st_crs(pts_iso) != target_crs) {
        pts_iso <- sf::st_transform(pts_iso, target_crs)
      }
      
      iso <- .fix_iso3(iso_raw)
      
      ne_name_fallback <- iso_to_name$ne_name[match(iso, iso_to_name$iso3_best)]
      if (is.na(ne_name_fallback)) ne_name_fallback <- iso
      
      # Try Level-1 first
      vv <- .fetch_gadm_level(iso, level = 1)
      used_level <- 1
      if (is.null(vv)) {
        vv <- .fetch_gadm_level(ne_name_fallback, level = 1)
      }
      if (is.null(vv)) {
        log_info("Level 1 unavailable. Falling back to Level 0 for %s.", iso, module = module)
        vv <- .fetch_gadm_level(iso, level = 0)
        if (is.null(vv)) vv <- .fetch_gadm_level(ne_name_fallback, level = 0)
        used_level <- 0
        if (is.null(vv)) {
          log_warn("Could not retrieve any GADM layer for %s. Skipping.", iso, module = module)
          next
        }
      }
      
      # Convert to sf
      gadm_sf <- sf::st_as_sf(vv)
      gadm_sf <- sf::st_zm(gadm_sf, drop = TRUE, what = "ZM")
      gadm_sf <- sanitize_spatial_layer(gadm_sf,
                                        layer_name = sprintf("GADM_%s_L%d", iso, level))
      
      has_NAME_0 <- "NAME_0" %in% names(gadm_sf)
      has_NAME_1 <- "NAME_1" %in% names(gadm_sf)
      
      keep_cols <- c(intersect(names(gadm_sf), c("NAME_0", "NAME_1")), "geometry")
      gadm_sf <- gadm_sf[, keep_cols, drop = FALSE]
      
      log_debug("GADM layer for %s: level=%d, NAME_0=%s, NAME_1=%s.",
                iso, used_level, 
                if (has_NAME_0) "yes" else "no",
                if (has_NAME_1) "yes" else "no", module = module)
      
      # Spatial join
      joined <- suppressWarnings(sf::st_join(
        pts_iso, gadm_sf, join = sf::st_within, left = TRUE
      ))
      
      # Optional nearest fallback for admin-1, WITH a distance sanity limit.
      # The point-in-polygon join above is the clean resolution; this snap is a
      # guess, and per Lucian (2026-07) an admin-1 that cannot be resolved
      # cleanly must be left unresolved rather than guessed at. Same guard as
      # the continent snap in Module 2A, so a point far outside every admin unit
      # is never assigned to whichever one happens to be closest.
      # NOTE: admin-1 names are emitted from the spatial layer VERBATIM — they
      # are deliberately NOT canonicalised, because there is no trustworthy
      # authority list for subnational names.
      if (used_level == 1 && has_NAME_1) {
        na_mask <- is.na(joined$NAME_1)
        if (any(na_mask)) {
          nearest_idx <- sf::st_nearest_feature(joined[na_mask, ], gadm_sf)
          d_km <- as.numeric(sf::st_distance(joined[na_mask, ], gadm_sf[nearest_idx, ],
                                             by_element = TRUE)) / 1000
          within_limit <- !is.na(d_km) & d_km <= GEO_MAX_SNAP_KM
          log_info("Admin-1 snap for %s: %d candidate(s), %d accepted (<=%d km), %d left unresolved.",
                   iso, sum(na_mask), sum(within_limit), GEO_MAX_SNAP_KM,
                   sum(!within_limit), module = module)
          fill <- rep(NA_character_, length(nearest_idx))
          fill[within_limit] <- gadm_sf$NAME_1[nearest_idx[within_limit]]
          joined$NAME_1[na_mask] <- fill
          if (has_NAME_0 && "NAME_0" %in% names(gadm_sf)) {
            fill0 <- rep(NA_character_, length(nearest_idx))
            fill0[within_limit] <- gadm_sf$NAME_0[nearest_idx[within_limit]]
            joined$NAME_0[na_mask] <- fill0
          }
        }
      }
      
      n_out <- nrow(joined)
      
      # COUNTRY: prefer GADM NAME_0; else NE fallback.
      # canon_country() is applied HERE, at the point the derived value is
      # produced. The earlier canonicalisation pass runs before this overlay, so
      # without this call a fallback-derived name would ship exactly as GADM or
      # Natural Earth spells it ("United States of America", "Czechia", ...) and
      # diverge from the WoC-native strings. Requirement: a country name must be
      # byte-identical whether it came from WoC or from the fallback.
      if ("NAME_0" %in% names(joined)) {
        vec_country <- joined$NAME_0
        if (any(is.na(vec_country))) {
          vec_country[is.na(vec_country)] <- ne_name_fallback
        }
      } else {
        vec_country <- rep(ne_name_fallback, n_out)
      }
      vec_country <- canon_country(vec_country)
      # canon_country() maps blanks to the sentinel; keep NA here so the
      # fill-only write-back below still recognises them as gaps.
      vec_country[vec_country == GEO_UNRESOLVED] <- NA_character_
      
      # ADMIN_1
      if ("NAME_1" %in% names(joined)) {
        vec_admin1 <- joined$NAME_1
      } else {
        vec_admin1 <- rep(NA_character_, n_out)
      }
      
      # Safety checks
      if (length(vec_country) != length(idx)) {
        log_warn("Length mismatch for 'country'. Using fallback.", module = module)
        vec_country <- rep(ne_name_fallback, length(idx))
      }
      if (length(vec_admin1) != length(idx)) {
        log_warn("Length mismatch for 'admin_1'. Setting to NA.", module = module)
        vec_admin1 <- rep(NA_character_, length(idx))
      }
      
      # Write back — FILL ONLY. A populated WoC-native value is never replaced;
      # we only write into positions that are still NA.
      pos_c <- is.na(result$clean_data$country[idx])
      if (any(pos_c)) {
        tgt <- idx[pos_c]
        result$clean_data$country[tgt]        <- vec_country[pos_c]
        result$clean_sf$country[tgt]          <- vec_country[pos_c]
        result$clean_data$country_source[tgt] <- "computed"
        result$clean_sf$country_source[tgt]   <- "computed"
      }
      pos_a <- is.na(result$clean_data$admin_1[idx])
      if (any(pos_a)) {
        tgt <- idx[pos_a]
        result$clean_data$admin_1[tgt]       <- vec_admin1[pos_a]
        result$clean_sf$admin_1[tgt]         <- vec_admin1[pos_a]
        result$clean_data$admin1_source[tgt] <- ifelse(is.na(vec_admin1[pos_a]),
                                                       NA_character_, "computed")
        result$clean_sf$admin1_source[tgt]   <- result$clean_data$admin1_source[tgt]
      }
    }

    pb$terminate()

    log_info("Country provenance: %d WoC-native, %d computed, %d still unassigned.",
             sum(result$clean_data$country_source == "WoC", na.rm = TRUE),
             sum(result$clean_data$country_source == "computed", na.rm = TRUE),
             sum(is.na(result$clean_data$country)), module = module)

    }  # end if (n_need > 0L)

    # ── Explicit failure state ───────────────────────────────────────────────
    # Anything the native data did not supply and the overlay could not resolve
    # becomes the GEO_UNRESOLVED sentinel — never a placeholder, never a guess.
    # Downstream counting and classification treat it exactly like NA, so a data
    # gap can never alter a species' category (Lucian, 2026-07).
    for (cc in c("country", "admin_1")) {
      miss <- is.na(result$clean_data[[cc]]) | !nzchar(result$clean_data[[cc]])
      result$clean_data[[cc]][miss] <- GEO_UNRESOLVED
      miss_sf <- is.na(result$clean_sf[[cc]]) | !nzchar(result$clean_sf[[cc]])
      result$clean_sf[[cc]][miss_sf] <- GEO_UNRESOLVED
    }
    for (sc in c("country_source", "admin1_source")) {
      result$clean_data[[sc]][is.na(result$clean_data[[sc]])] <- GEO_UNRESOLVED
      result$clean_sf[[sc]][is.na(result$clean_sf[[sc]])]     <- GEO_UNRESOLVED
    }
    n_unres_c <- sum(result$clean_data$country == GEO_UNRESOLVED)
    if (n_unres_c > 0)
      log_warn("%d record(s) have country '%s' — EXCLUDED from country counts and classification.",
               n_unres_c, GEO_UNRESOLVED, module = module)

    # Persist
    out_tsv <- file.path(output_dir, "clean_occurrences_with_gadm.tsv")
    out_rds <- file.path(output_dir, "clean_occurrences_sf_with_gadm.rds")
    
    write_tsv(result$clean_data, out_tsv)
    saveRDS(result$clean_sf, out_rds)
    
    log_info("Added columns: country, admin_1.", module = module)
    log_info("Saved enriched table to: %s", out_tsv, module = module)
    
    result$files_created <- unique(c(result$files_created, out_tsv, out_rds))
    
    result
  })
}