#### MODULE 8: SCENARIO-AWARE MAP GENERATION (PARALLEL VERSION) ####

#' Generate maps for all scenarios with proper styling
#' Now supports parallel processing for species-level map generation
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @param cache_dir Cache directory for HydroBASINS
#' @param formats Vector of output formats (geojson, kml)
#' @param bbox_expand_km Bounding box expansion in km
#' @param parallel_maps Enable parallel map generation
#' @return List of generated maps
generate_all_maps_par <- function(scenario_table,
                              result_indigenous,
                              result_non_indigenous,
                              output_dir = "checkover_output",
                              cache_dir = file.path(output_dir, "cache"),
                              formats = c("geojson", "kml"),
                              bbox_expand_km = 200,
                              parallel_maps = TRUE) {
  module <- "MODULE8_MAPS"

  with_log_section(module, {
    log_info("=== MODULE 8: SCENARIO-AWARE MAP GENERATION ===", module = module)

    # Check parallel status
    par_status <- parallel_status()
    use_parallel <- parallel_maps && par_status$is_parallel

    if (use_parallel) {
      log_info("Parallel map generation ENABLED (%d workers)", par_status$workers, module = module)
    } else {
      log_info("Parallel map generation DISABLED (sequential mode)", module = module)
    }

    # Create maps directories
    maps_dir <- file.path(output_dir, "maps")
    if (!dir.exists(maps_dir)) dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)

    eoo_dir <- file.path(maps_dir, "EOO")
    aoo_dir <- file.path(maps_dir, "AOO")
    basins_dir <- file.path(maps_dir, "basins")

    for (d in c(eoo_dir, aoo_dir, basins_dir)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }

    # Load all species
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]

    log_info("Generating maps for %d species across all scenarios...", length(all_species), module = module)

    start_time <- Sys.time()

    # Define per-species map generation function
    generate_species_maps <- function(sp) {
      sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]

      # Get species data from appropriate branch(es)
      sp_data <- NULL
      sp_sf <- NULL

      if (scenario == 1) {
        # Indigenous only
        sp_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
        sp_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
        population_types <- "indigenous"

      } else if (scenario == 2) {
        # Non-indigenous only
        sp_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
        sp_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
        population_types <- "non-indigenous"

      } else if (scenario == 3) {
        # Both - combine
        ind_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
        non_ind_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
        sp_data <- rbind(ind_data, non_ind_data)

        ind_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
        non_ind_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
        sp_sf <- rbind(ind_sf, non_ind_sf)

        population_types <- "both"
      }

      if (is.null(sp_data) || nrow(sp_data) == 0) {
        return(NULL)
      }

      maps_generated <- list()

      # --- 1. EOO (Extent of Occurrence) ---
      eoo_poly <- NULL
      if (nrow(sp_sf) >= 3) {
        eoo_poly <- tryCatch({
          hull <- sf::st_convex_hull(sf::st_union(sp_sf))
          if (!sf::st_is_valid(hull)) hull <- sf::st_make_valid(hull)
          hull
        }, error = function(e) NULL)
      }

      if (!is.null(eoo_poly)) {
        eoo_sf <- sf::st_sf(geometry = sf::st_sfc(eoo_poly), crs = sf::st_crs(sp_sf))
        eoo_sf$Name <- paste(sp, "EOO")
        eoo_sf$population_type <- population_types

        # GeoJSON
        eoo_geo <- eoo_sf
        eoo_geo$fill <- "#FFFF00"
        eoo_geo$`fill-opacity` <- 0.80
        eoo_geo$stroke <- "#FFFF00"
        eoo_geo$`stroke-width` <- 2

        eoo_file <- file.path(eoo_dir, paste0(sp_clean, "_EOO.geojson"))
        try(sf::st_write(eoo_geo, eoo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
        maps_generated$eoo_geojson <- eoo_file

        # KML
        if ("kml" %in% formats) {
          eoo_kml <- file.path(eoo_dir, paste0(sp_clean, "_EOO.kml"))
          .write_styled_kml(eoo_sf, eoo_kml, "EOO", "#FFFF00", 0.80)
          maps_generated$eoo_kml <- eoo_kml
        }
      }

      # --- 2. AOO (Area of Occupancy) ---
      aoo_poly <- NULL
      tryCatch({
        grid <- sf::st_make_grid(sp_sf, cellsize = 0.018, square = TRUE)
        inter <- sf::st_intersects(grid, sp_sf)
        has_pts <- lengths(inter) > 0
        aoo_cells <- grid[has_pts]

        if (length(aoo_cells) > 0) {
          u <- sf::st_union(aoo_cells)
          if (!sf::st_is_valid(u)) u <- sf::st_make_valid(u)
          aoo_poly <- u
        }
      }, error = function(e) NULL)

      if (!is.null(aoo_poly)) {
        aoo_sf <- sf::st_sf(geometry = sf::st_sfc(aoo_poly), crs = sf::st_crs(sp_sf))
        aoo_sf$Name <- paste(sp, "AOO")
        aoo_sf$population_type <- population_types

        aoo_geo <- aoo_sf
        aoo_geo$fill <- "#FFFF00"
        aoo_geo$`fill-opacity` <- 0.80
        aoo_geo$stroke <- "#FFFF00"
        aoo_geo$`stroke-width` <- 2

        aoo_file <- file.path(aoo_dir, paste0(sp_clean, "_AOO.geojson"))
        try(sf::st_write(aoo_geo, aoo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
        maps_generated$aoo_geojson <- aoo_file

        if ("kml" %in% formats) {
          aoo_kml <- file.path(aoo_dir, paste0(sp_clean, "_AOO.kml"))
          .write_styled_kml(aoo_sf, aoo_kml, "AOO", "#FFFF00", 0.80)
          maps_generated$aoo_kml <- aoo_kml
        }
      }

      # --- 3. HydroBASINS (with population-based styling) ---
      basins_map <- .generate_hydrobasins_map(
        sp, sp_data, sp_sf, scenario, cache_dir, bbox_expand_km, module
      )

      if (!is.null(basins_map)) {
        # GeoJSON
        if ("geojson" %in% formats) {
          basins_geo_file <- file.path(basins_dir, paste0(sp_clean, "_basins.geojson"))
          try(sf::st_write(basins_map$styled, basins_geo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
          maps_generated$basins_geojson <- basins_geo_file
        }

        # KML
        if ("kml" %in% formats) {
          basins_kml_file <- file.path(basins_dir, paste0(sp_clean, "_basins.kml"))
          .write_basins_kml(basins_map$raw, basins_kml_file)
          maps_generated$basins_kml <- basins_kml_file
        }
      }

      return(maps_generated)
    }

    # Execute map generation (parallel or sequential)
    if (use_parallel) {
      # Parallel execution
      generated_maps <- parallel_lapply(
        all_species,
        generate_species_maps,
        .parallel = TRUE,
        .progress = TRUE,
        .error_value = list(),
        .module = module
      )
      names(generated_maps) <- all_species

    } else {
      # Sequential with progress
      generated_maps <- list()

      if (requireNamespace("progress", quietly = TRUE)) {
        pb <- progress::progress_bar$new(
          format = "  Maps [:bar] :current/:total (:percent) :eta",
          total = length(all_species),
          clear = FALSE
        )
      }

      for (sp in all_species) {
        if (exists("pb")) pb$tick()
        log_info("Processing maps for: %s", sp, module = module)

        result <- tryCatch({
          generate_species_maps(sp)
        }, error = function(e) {
          log_warn("Error generating maps for %s: %s", sp, conditionMessage(e), module = module)
          list()
        })

        generated_maps[[sp]] <- result

        if (!is.null(result) && length(result) > 0) {
          log_info("  Generated %d map files for %s", length(result), sp, module = module)
        }
      }
    }

    # Remove NULL entries
    generated_maps <- generated_maps[!sapply(generated_maps, is.null)]

    # Calculate elapsed time
    elapsed <- difftime(Sys.time(), start_time, units = "secs")

    # Summary
    total_files <- sum(sapply(generated_maps, length))
    log_info("Map generation complete: %d species, %d total files in %.1f seconds",
             length(generated_maps), total_files, as.numeric(elapsed), module = module)

    if (use_parallel && length(all_species) > 1) {
      avg_time <- as.numeric(elapsed) / length(all_species)
      log_info("Average time per species: %.2f seconds", avg_time, module = module)
    }

    return(list(
      maps = generated_maps,
      summary = list(
        species_count = length(generated_maps),
        total_files = total_files,
        elapsed_seconds = as.numeric(elapsed),
        parallel_enabled = use_parallel
      )
    ))
  })
}


# --- HELPER: GENERATE HYDROBASINS MAP ---
.generate_hydrobasins_map <- function(sp, sp_data, sp_sf, scenario, cache_dir, bbox_expand_km, module) {

  # Get HydroBASINS assignments
  basins_raw <- unique(sp_data$hydrobasin)
  basins_raw <- basins_raw[!is.na(basins_raw) & nzchar(basins_raw)]

  if (length(basins_raw) == 0) {
    return(NULL)
  }

  # Parse basin codes: "L8:2080008490 | L8:2080008491"
  all_basin_codes <- unlist(strsplit(basins_raw, " \\| "))

  # Extract level and IDs
  basin_info <- data.frame(
    code = all_basin_codes,
    level = as.integer(sub("^L(\\d+):.*", "\\1", all_basin_codes)),
    id = sub("^L\\d+:", "", all_basin_codes),
    stringsAsFactors = FALSE
  )

  # Determine which levels we need
  levels_needed <- unique(basin_info$level)

  # Load HydroBASINS layers
  basin_geometries <- list()

  for (lvl in levels_needed) {
    cache_file <- file.path(cache_dir, sprintf("hydro_lev%02d_merged.rds", lvl))

    if (!file.exists(cache_file)) {
      next
    }

    lyr <- readRDS(cache_file)

    # Get basin IDs for this level
    ids_needed <- basin_info$id[basin_info$level == lvl]

    # Filter to needed basins
    basins_subset <- lyr[lyr$HB_LABEL %in% ids_needed, ]

    if (nrow(basins_subset) > 0) {
      basin_geometries[[as.character(lvl)]] <- basins_subset
    }
  }

  if (length(basin_geometries) == 0) {
    return(NULL)
  }

  # Combine all basin levels
  all_basins <- do.call(rbind, basin_geometries)

  # Assign population status to each basin
  if (scenario == 3) {
    # Scenario 3: Need to determine which basins are native vs invaded
    basin_status <- data.frame(
      HB_LABEL = all_basins$HB_LABEL,
      status = NA_character_,
      stringsAsFactors = FALSE
    )

    for (i in seq_len(nrow(all_basins))) {
      basin_geom <- all_basins[i, ]

      # Find points in this basin
      pts_in_basin <- suppressWarnings(sf::st_filter(sp_sf, basin_geom))

      if (nrow(pts_in_basin) > 0) {
        pop_types <- unique(pts_in_basin$population_type)

        if ("indigenous" %in% pop_types && "non-indigenous" %in% pop_types) {
          basin_status$status[i] <- "Mixed"
        } else if ("indigenous" %in% pop_types) {
          basin_status$status[i] <- "Native"
        } else if ("non-indigenous" %in% pop_types) {
          basin_status$status[i] <- "Introduced"
        }
      }
    }

    all_basins$status <- basin_status$status

  } else if (scenario == 1) {
    # All native
    all_basins$status <- "Native"
  } else {
    # All introduced
    all_basins$status <- "Introduced"
  }

  # Apply styling
  styled_basins <- all_basins
  styled_basins$fill <- ifelse(all_basins$status == "Native", "#D48D00",
                               ifelse(all_basins$status == "Introduced", "#4D0073", "#FF6600"))
  styled_basins$`fill-opacity` <- 0.35
  styled_basins$stroke <- styled_basins$fill
  styled_basins$`stroke-width` <- 1.5

  return(list(
    raw = all_basins,
    styled = styled_basins
  ))
}


# --- HELPER: WRITE STYLED KML (EOO/AOO) ---
.write_styled_kml <- function(sf_obj, file_path, layer_name, color, opacity) {
  tmp <- tempfile(fileext = ".kml")
  sf_obj$kml_id <- seq_len(nrow(sf_obj))
  sf::st_write(sf_obj, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
  kml_txt <- paste(readLines(tmp), collapse = "\n")

  # Convert color to KML format (AABBGGRR)
  hex_color <- sub("^#", "", color)
  r <- substr(hex_color, 1, 2)
  g <- substr(hex_color, 3, 4)
  b <- substr(hex_color, 5, 6)
  opacity_hex <- sprintf("%02x", as.integer(opacity * 255))
  kml_color <- paste0(opacity_hex, b, g, r)

  style_def <- sprintf('
  <Style id="%sStyle">
    <LineStyle>
      <color>ff%s%s%s</color>
      <width>2</width>
    </LineStyle>
    <PolyStyle>
      <color>%s</color>
      <fill>1</fill>
      <outline>1</outline>
    </PolyStyle>
  </Style>', layer_name, b, g, r, kml_color)

  kml_txt <- sub("<Document>", paste0("<Document>", style_def), kml_txt)
  kml_txt <- gsub("(<Placemark[^>]*>)", paste0("\\1<styleUrl>#", layer_name, "Style</styleUrl>"), kml_txt)

  writeLines(kml_txt, file_path)
  unlink(tmp)
}


# --- HELPER: WRITE BASINS KML (with population-based styling) ---
.write_basins_kml <- function(basins_sf, file_path) {
  tmp <- tempfile(fileext = ".kml")
  sf::st_write(basins_sf, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
  kml_txt <- paste(readLines(tmp), collapse = "\n")

  # Define styles for each status
  style_defs <- '
  <Style id="nativeStyle">
    <LineStyle><color>ff008dd4</color><width>1.5</width></LineStyle>
    <PolyStyle><color>59008dd4</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>
  <Style id="introducedStyle">
    <LineStyle><color>ff73004d</color><width>1.5</width></LineStyle>
    <PolyStyle><color>5973004d</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>
  <Style id="mixedStyle">
    <LineStyle><color>ff0066ff</color><width>1.5</width></LineStyle>
    <PolyStyle><color>800066ff</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>'

  kml_txt <- sub("<Document>", paste0("<Document>", style_defs), kml_txt)

  # Apply styles based on status
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Native</n>)",
    "\\1<styleUrl>#nativeStyle</styleUrl>\\2",
    kml_txt
  )
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Introduced</n>)",
    "\\1<styleUrl>#introducedStyle</styleUrl>\\2",
    kml_txt
  )
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Mixed</n>)",
    "\\1<styleUrl>#mixedStyle</styleUrl>\\2",
    kml_txt
  )

  writeLines(kml_txt, file_path)
  unlink(tmp)
}