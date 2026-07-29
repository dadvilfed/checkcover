#### MODULE 2A: CONTINENTS ENRICHMENT ####

load_ne_continents_medium <- function(cache_dir, module = "MODULE2A_CONTINENTS") {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_path <- file.path(cache_dir, "ne_continents_50m.rds")
  
  if (file.exists(cache_path)) {
    log_info("Loading continents from cache: %s", cache_path, module = module)
    continents <- readRDS(cache_path)
    # drop_corrupt_bbox = FALSE on purpose. This layer is the DISSOLVED union of
    # an already-sanitized country layer, so the extent audit can only produce
    # false positives here: Europe legitimately spans ~216 deg of longitude once
    # Russia (which reaches -169) is unioned in, and the audit was silently
    # dropping the entire Europe polygon — which would push every European
    # fallback record into nearest-land snapping. Validity repair still runs.
    continents <- sanitize_spatial_layer(continents,
                                         layer_name = "NE_continents_cached",
                                         drop_corrupt_bbox = FALSE)
    log_debug("Loaded %d continent polygons from cache.", nrow(continents), module = module)
    return(continents)
  }
  
  log_info("Loading Natural Earth admin-0 countries (scale = 'medium')...", module = module)
  
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  countries <- sanitize_spatial_layer(countries, layer_name = "NE_countries")
  # Match older sf-name convention used downstream (geom, not geometry)
  countries <- sf::st_set_geometry(countries, "geom")
  
  log_info("Cleaning and dissolving countries into continents...", module = module)
  
  countries <- sf::st_zm(countries, drop = TRUE, what = "ZM")
  
  continents <- countries %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(geom = sf::st_union(geom), .groups = "drop") %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(4326)
  
  saveRDS(continents, cache_path)
  log_info("Cached continents to: %s", cache_path, module = module)
  
  continents
}

enrich_with_continents <- function(result, output_dir = "checkover_output",
                                   use_nearest_for_na = TRUE) {
  module <- "MODULE2A_CONTINENTS"
  
  with_log_section(module, {
    log_info("=== MODULE 2A: CONTINENTS ENRICHMENT ===", module = module)
    
    if (!all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Expected list from ingest_clean(): missing 'clean_data' or 'clean_sf'.")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    
    # ── WoC-native first, computed overlay only as fallback ─────────────────
    # The WoC database curates continent per occurrence. Those values are
    # authoritative and must never be overwritten by the Natural Earth overlay;
    # we only compute the ones WoC left empty. Where WoC covers everything the
    # spatial step is skipped outright (Lucian, 2026-07).
    n_rows <- nrow(result$clean_data)
    # Native values are normalised to the canonical six-value vocabulary. Any
    # blank / unknown / non-canonical label (including "Australia", which is a
    # country, and corrupted labels from a shifted column) becomes the explicit
    # GEO_UNRESOLVED sentinel rather than being carried through.
    native <- if ("continents" %in% names(result$clean_data))
      canon_continent(result$clean_data$continents) else rep(GEO_UNRESOLVED, n_rows)

    need   <- native == GEO_UNRESOLVED
    n_need <- sum(need)
    log_info("Continent: %d/%d records carry a WoC-native value; %d need computing.",
             n_rows - n_need, n_rows, n_need, module = module)

    final <- native
    src   <- ifelse(native == GEO_UNRESOLVED, NA_character_, "WoC")
    # Per-record snap diagnostics for the integrity report.
    snap_km   <- rep(NA_real_, n_rows)
    reject    <- rep(NA_character_, n_rows)

    if (n_need == 0L) {
      log_info("All records already have a native continent — skipping the overlay.",
               module = module)
    } else {
      continents_sf <- load_ne_continents_medium(
        cache_dir = file.path(output_dir, "cache"),
        module = module
      )
      continents_min <- continents_sf %>% dplyr::select(continents = continent)

      pts_need <- result$clean_sf[need, , drop = FALSE]
      # Drop any pre-existing column of the same name so st_join doesn't produce
      # continents.x / continents.y.
      if ("continents" %in% names(pts_need)) pts_need$continents <- NULL

      log_info("Intersecting %d point(s) with continent polygons...", n_need, module = module)
      joined <- suppressWarnings(
        sf::st_join(pts_need, continents_min, join = sf::st_within, left = TRUE)
      )
      # A point INSIDE a continent polygon is an exact answer and is trusted.
      got      <- canon_continent(joined$continents)
      got_km   <- rep(NA_real_, length(got))
      got_rej  <- rep(NA_character_, length(got))

      # ── Nearest-land snapping, with sanity limits ────────────────────────
      # A snap is meant to rescue a point sitting just offshore, NOT to relocate
      # it to another ocean. Five E. suttoni records with a dropped longitude
      # digit were snapped ~8,000 km onto Africa, which alone flipped the
      # species into the cosmopolitan class. Two guards now apply, and a record
      # failing either is marked unresolved and reported rather than assigned.
      na_mask <- got == GEO_UNRESOLVED
      if (any(na_mask) && use_nearest_for_na) {
        log_info("Attempting nearest-continent snap for %d coastal/offshore point(s)...",
                 sum(na_mask), module = module)
        sub <- joined[na_mask, , drop = FALSE]
        idx <- sf::st_nearest_feature(sub, continents_min)
        d_km <- as.numeric(sf::st_distance(sub, continents_min[idx, ],
                                           by_element = TRUE)) / 1000
        cand <- canon_continent(continents_min$continents[idx])

        # Guard 1 — distance sanity limit.
        too_far <- is.na(d_km) | d_km > GEO_MAX_SNAP_KM

        # Guard 2 — the snapped continent must be consistent with what the rest
        # of the species already resolves to. If the species has other resolved
        # records and none of them sit on the candidate continent, the snap is
        # almost certainly rescuing a bad coordinate, not a coastal one.
        sp_all   <- as.character(result$clean_data$species)
        sp_sub   <- sp_all[need][na_mask]
        resolved_by_sp <- split(final[final != GEO_UNRESOLVED],
                                sp_all[final != GEO_UNRESOLVED])
        inconsistent <- vapply(seq_along(cand), function(k) {
          others <- resolved_by_sp[[ sp_sub[k] ]]
          length(others) > 0L && !(cand[k] %in% others)
        }, logical(1))

        accept <- !too_far & !inconsistent
        cand_out <- ifelse(accept, cand, GEO_UNRESOLVED)
        rej <- ifelse(too_far & inconsistent, "snap_too_far+species_inconsistent",
               ifelse(too_far, sprintf("snap_%.0fkm_exceeds_%dkm", d_km, GEO_MAX_SNAP_KM),
               ifelse(inconsistent, "continent_absent_from_species", NA_character_)))

        got[na_mask]     <- cand_out
        got_km[na_mask]  <- d_km
        got_rej[na_mask] <- rej

        n_ok <- sum(accept); n_far <- sum(too_far); n_inc <- sum(inconsistent & !too_far)
        log_info("  Snap accepted for %d point(s) (<=%d km and species-consistent).",
                 n_ok, GEO_MAX_SNAP_KM, module = module)
        if (n_far > 0)
          log_warn("  REJECTED %d snap(s) exceeding %d km (max %.0f km) — marked '%s'.",
                   n_far, GEO_MAX_SNAP_KM, max(d_km, na.rm = TRUE), GEO_UNRESOLVED,
                   module = module)
        if (n_inc > 0)
          log_warn("  REJECTED %d snap(s) landing on a continent absent from the rest of the species — marked '%s'.",
                   n_inc, GEO_UNRESOLVED, module = module)
      }

      final[need]   <- got
      src[need]     <- ifelse(got == GEO_UNRESOLVED, NA_character_, "computed")
      snap_km[need] <- got_km
      reject[need]  <- got_rej

      n_unres <- sum(final == GEO_UNRESOLVED)
      if (n_unres > 0) {
        log_warn("After fallback, %d point(s) remain '%s' and are EXCLUDED from all counts and classification.",
                 n_unres, GEO_UNRESOLVED, module = module)
      } else {
        log_info("All points now have a continent (%d native, %d computed).",
                 sum(src == "WoC", na.rm = TRUE), sum(src == "computed", na.rm = TRUE),
                 module = module)
      }
    }

    # Explicit failure state: `unresolved`, never a placeholder or a guess.
    src[is.na(src)] <- GEO_UNRESOLVED

    result$clean_data$continents          <- final
    result$clean_data$continent_source    <- src
    result$clean_data$continent_snap_km   <- snap_km
    result$clean_data$continent_reject    <- reject
    result$clean_sf$continents            <- final
    result$clean_sf$continent_source      <- src
    
    # Debug summary
    dbg <- result$clean_data %>%
      dplyr::count(continents, name = "n") %>%
      dplyr::arrange(dplyr::desc(n))
    
    if (nrow(dbg) > 0L) {
      dbg_str <- paste(capture.output(print(dbg)), collapse = "\n")
      log_info("Per-continent record counts:\n%s", dbg_str, module = module)
    }
    
    out_tsv <- file.path(output_dir, "clean_occurrences_with_continents.tsv")
    out_rds <- file.path(output_dir, "clean_occurrences_sf_with_continents.rds")
    
    write_tsv(result$clean_data, out_tsv)
    saveRDS(result$clean_sf, out_rds)
    
    log_info("Saved enriched occurrences with continents.", module = module)
    
    result$files_created <- c(result$files_created, out_tsv, out_rds)
    
    result
  })
}