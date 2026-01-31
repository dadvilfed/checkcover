#### MODULE 8: SCENARIO-AWARE MAP GENERATION ####
# Includes type locality markers on HydroBASINS maps

#' Generate maps for all scenarios with proper styling
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @param cache_dir Cache directory for HydroBASINS
#' @param formats Vector of output formats (geojson, kml)
#' @param bbox_expand_km Bounding box expansion in km
#' @return List of generated maps
generate_all_maps_seq <- function(scenario_table,
                              result_indigenous,
                              result_non_indigenous,
                              output_dir = "checkover_output",
                              cache_dir = file.path(output_dir, "cache"),
                              formats = c("geojson", "kml"),
                              bbox_expand_km = 200) {
  module <- "MODULE8_MAPS"

  with_log_section(module, {
    log_info("=== MODULE 8: SCENARIO-AWARE MAP GENERATION ===", module = module)

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

    generated_maps <- list()

    for (sp in all_species) {
      log_info("Processing maps for: %s", sp, module = module)

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
        #sp_data <- rbind(ind_data, non_ind_data)
        sp_data <- dplyr::bind_rows(ind_data, non_ind_data)

        ind_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
        non_ind_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
        #sp_sf <- rbind(ind_sf, non_ind_sf)
        sp_sf <- dplyr::bind_rows(ind_sf, non_ind_sf)

        population_types <- "both"
      }

      if (is.null(sp_data) || nrow(sp_data) == 0) {
        log_warn("  No data for %s. Skipping.", sp, module = module)
        next
      }

      maps_generated <- list()

      # --- Extract Type Locality and create 2km x 2km square polygon ---
      type_loc_sf <- NULL
      if ("is_type_locality" %in% names(sp_data)) {
        type_loc_idx <- which(sp_data$is_type_locality == TRUE)
        if (length(type_loc_idx) > 0) {
          tl_pts <- sp_sf[type_loc_idx, , drop = FALSE]
          log_info("  Found %d type locality record(s) for %s", nrow(tl_pts), sp, module = module)

          # Convert points to 2km x 2km squares (polygons)
          tryCatch({
            ea_crs <- 6933  # Equal-area projection for accurate buffering
            pts_ea <- sf::st_transform(tl_pts, ea_crs)

            # Create square from each point: buffer 1km then take bounding box
            sq_list <- lapply(sf::st_geometry(pts_ea), function(geo) {
              circle <- sf::st_buffer(geo, dist = 1000)  # 1km radius
              sf::st_as_sfc(sf::st_bbox(circle))         # Bounding box = 2km square
            })

            sq_sfc <- do.call(c, sq_list)
            sf::st_crs(sq_sfc) <- ea_crs
            tl_poly_final <- sf::st_transform(sq_sfc, sf::st_crs(sp_sf))

            type_loc_sf <- sf::st_sf(geometry = tl_poly_final)
            type_loc_sf$Name <- "Type Locality"
            type_loc_sf$status <- "Type Locality"
            type_loc_sf$layer_type <- "Type_Locality"
          }, error = function(e) {
            log_warn("  Failed to create type locality polygon: %s", conditionMessage(e), module = module)
          })
        }
      }

      # --- 1. EOO (Extent of Occurrence) ---
      eoo_poly <- NULL
      if (nrow(sp_sf) >= 3) {
        eoo_poly <- tryCatch({
          hull <- sf::st_convex_hull(sf::st_union(sp_sf))
          if (!sf::st_is_valid(hull)) hull <- sf::st_make_valid(hull)
          hull
        }, error = function(e) {
          log_warn("  EOO calculation failed: %s", conditionMessage(e), module = module)
          NULL
        })
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
      }, error = function(e) {
        log_warn("  AOO calculation failed: %s", conditionMessage(e), module = module)
      })

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

      # --- 3. HydroBASINS (with population-based styling + type locality) ---
      basins_map <- .generate_hydrobasins_map(
        sp, sp_data, sp_sf, scenario, cache_dir, bbox_expand_km, module
      )

      if (!is.null(basins_map)) {
        # GeoJSON - basins + type locality combined
        if ("geojson" %in% formats) {
          basins_geo_file <- file.path(basins_dir, paste0(sp_clean, "_basins.geojson"))
          .write_basins_geojson_with_type_locality(basins_map$styled, type_loc_sf, sp, basins_geo_file)
          maps_generated$basins_geojson <- basins_geo_file
        }

        # KML - basins + type locality combined
        if ("kml" %in% formats) {
          basins_kml_file <- file.path(basins_dir, paste0(sp_clean, "_basins.kml"))
          .write_basins_kml_with_type_locality(basins_map$raw, type_loc_sf, sp, basins_kml_file)
          maps_generated$basins_kml <- basins_kml_file
        }
      }

      generated_maps[[sp]] <- maps_generated
      log_info("  Generated %d map files for %s", length(maps_generated), sp, module = module)
    }

    # Summary
    total_files <- sum(sapply(generated_maps, length))
    log_info("Map generation complete: %d species, %d total files",
             length(generated_maps), total_files, module = module)

    return(list(
      maps = generated_maps,
      summary = list(
        species_count = length(generated_maps),
        total_files = total_files
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
    log_warn("  No HydroBASINS data for %s", sp, module = module)
    return(NULL)
  }

  # Parse basin codes: "L8:2080008490 | L8:2080008491"
  all_basin_codes <- unlist(strsplit(basins_raw, " \\| "))

  log_info("  [DEBUG] Parsed %d basin codes", length(all_basin_codes), module = module)

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
      log_warn("  HydroBASINS L%d cache not found", lvl, module = module)
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
    log_warn("  No HydroBASINS geometries found for %s", sp, module = module)
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


# --- HELPER: WRITE BASINS GEOJSON WITH TYPE LOCALITY ---
.write_basins_geojson_with_type_locality <- function(basins_sf, type_loc_sf, species_name, file_path) {

  # Style definitions
  style_type_loc <- list(fill = "#FF0000", `fill-opacity` = 0.50, stroke = "#FF0000", `stroke-width` = 2)

  # Add feature_type to basins
  basins_sf$feature_type <- "basin"

  # Add type locality polygon if present
  if (!is.null(type_loc_sf) && nrow(type_loc_sf) > 0) {
    # Style the type locality polygon
    tl_styled <- type_loc_sf
    tl_styled$HB_LABEL <- "Type Locality"
    tl_styled$status <- "Type Locality"
    tl_styled$feature_type <- "type_locality"
    tl_styled$fill <- style_type_loc$fill
    tl_styled$`fill-opacity` <- style_type_loc$`fill-opacity`
    tl_styled$stroke <- style_type_loc$stroke
    tl_styled$`stroke-width` <- style_type_loc$`stroke-width`

    # Match columns between basins and type locality
    basin_cols <- names(basins_sf)
    tl_cols <- names(tl_styled)

    for (col in setdiff(basin_cols, tl_cols)) {
      tl_styled[[col]] <- NA
    }
    for (col in setdiff(tl_cols, basin_cols)) {
      basins_sf[[col]] <- NA
    }

    # Combine - type locality on top
    all_cols <- union(basin_cols, tl_cols)
    combined_sf <- rbind(
      basins_sf[, all_cols],
      tl_styled[, all_cols]
    )
  } else {
    combined_sf <- basins_sf
  }

  # Write to file
  try(sf::st_write(combined_sf, file_path, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
}



# --- HELPER: WRITE BASINS KML WITH TYPE LOCALITY ---
.write_basins_kml_with_type_locality <- function(basins_sf, type_loc_sf, species_name, file_path) {

  # Add layer_type column for styling
  basins_sf$layer_type <- basins_sf$status

  # Combine with type locality polygon if present
  if (!is.null(type_loc_sf) && nrow(type_loc_sf) > 0) {
    type_loc_sf$layer_type <- "Type_Locality"
    type_loc_sf$status <- "Type Locality"
    type_loc_sf$HB_LABEL <- "Type Locality"

    # Match columns
    common_cols <- intersect(names(basins_sf), names(type_loc_sf))
    basins_sf <- basins_sf[, common_cols]
    type_loc_sf <- type_loc_sf[, common_cols]

    combined_sf <- rbind(basins_sf, type_loc_sf)
  } else {
    combined_sf <- basins_sf
  }

  # Write combined to temp KML
  tmp <- tempfile(fileext = ".kml")
  sf::st_write(combined_sf, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
  kml_txt <- paste(readLines(tmp), collapse = "\n")

  # KML colors are in AABBGGRR format
  # Native: #D48D00 -> 59008dd4 (35% opacity)
  # Introduced: #4D0073 -> 5973004d (35% opacity)
  # Mixed: #FF6600 -> 800066ff (50% opacity)
  # Type Locality: #FF0000 -> 800000ff (50% opacity)
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
  </Style>
  <Style id="typeLocalityStyle">
    <LineStyle><color>ff0000ff</color><width>2</width></LineStyle>
    <PolyStyle><color>800000ff</color><fill>1</fill><outline>1</outline></PolyStyle>
  </Style>'

  kml_txt <- sub("<Document>", paste0("<Document>", style_defs), kml_txt)

  # Apply styles based on status/name in KML
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
  kml_txt <- gsub(
    "(<Placemark[^>]*>)([\\s\\S]*?<n>Type Locality</n>)",
    "\\1<styleUrl>#typeLocalityStyle</styleUrl>\\2",
    kml_txt
  )

  writeLines(kml_txt, file_path)
  unlink(tmp)
}


# --- HELPER: WRITE BASINS KML (legacy - without type locality) ---
.write_basins_kml <- function(basins_sf, file_path) {
  .write_basins_kml_with_type_locality(basins_sf, NULL, "", file_path)
}

#### MODULE 8: SCENARIO-AWARE MAP GENERATION ####

#' Safe bind for data frames with potentially different columns
#' Uses dplyr::bind_rows which handles mismatched columns gracefully
#' @param df1 First data frame
#' @param df2 Second data frame
#' @return Combined data frame
# .safe_bind_rows <- function(df1, df2) {
#   if (is.null(df1) || nrow(df1) == 0) return(df2)
#   if (is.null(df2) || nrow(df2) == 0) return(df1)
#   
#   # Use dplyr::bind_rows which handles column mismatches gracefully
#   tryCatch({
#     dplyr::bind_rows(df1, df2)
#   }, error = function(e) {
#     # Fallback: align columns manually
#     all_cols <- union(names(df1), names(df2))
#     
#     # Add missing columns to df1
#     for (col in setdiff(all_cols, names(df1))) {
#       df1[[col]] <- NA
#     }
#     # Add missing columns to df2
#     for (col in setdiff(all_cols, names(df2))) {
#       df2[[col]] <- NA
#     }
#     
#     # Reorder to match
#     df1 <- df1[, all_cols, drop = FALSE]
#     df2 <- df2[, all_cols, drop = FALSE]
#     
#     rbind(df1, df2)
#   })
# }


#' Safe bind for sf objects with potentially different columns
#' @param sf1 First sf object
#' @param sf2 Second sf object
#' @return Combined sf object
# .safe_bind_sf <- function(sf1, sf2) {
#   if (is.null(sf1) || nrow(sf1) == 0) return(sf2)
#   if (is.null(sf2) || nrow(sf2) == 0) return(sf1)
#   
#   tryCatch({
#     # Get all column names (excluding geometry)
#     geom_col <- attr(sf1, "sf_column")
#     cols1 <- setdiff(names(sf1), geom_col)
#     cols2 <- setdiff(names(sf2), geom_col)
#     all_cols <- union(cols1, cols2)
#     
#     # Add missing columns to sf1
#     for (col in setdiff(all_cols, cols1)) {
#       sf1[[col]] <- NA
#     }
#     # Add missing columns to sf2
#     for (col in setdiff(all_cols, cols2)) {
#       sf2[[col]] <- NA
#     }
#     
#     # Reorder non-geometry columns to match, keeping geometry at end
#     sf1 <- sf1[, c(all_cols, geom_col)]
#     sf2 <- sf2[, c(all_cols, geom_col)]
#     
#     rbind(sf1, sf2)
#   }, error = function(e) {
#     # Last resort fallback - just use first dataset
#     warning(sprintf("Failed to combine sf objects: %s. Using first dataset only.", conditionMessage(e)))
#     sf1
#   })
# }


#' Generate maps for all scenarios with proper styling
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @param formats Vector of output formats (geojson, kml)
#' @param bbox_expand_km Bounding box expansion in km
#' @return List of generated maps
# generate_all_maps_seq <- function(scenario_table,
#                               result_indigenous,
#                               result_non_indigenous,
#                               output_dir = "checkover_output",
#                               cache_dir = file.path(output_dir, "cache"),
#                               formats = c("geojson", "kml"),
#                               bbox_expand_km = 200) {
#   module <- "MODULE8_MAPS"
#   
#   with_log_section(module, {
#     log_info("=== MODULE 8: SCENARIO-AWARE MAP GENERATION ===", module = module)
#     
#     # Create maps directories
#     maps_dir <- file.path(output_dir, "maps")
#     if (!dir.exists(maps_dir)) dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
#     
#     eoo_dir <- file.path(maps_dir, "EOO")
#     aoo_dir <- file.path(maps_dir, "AOO")
#     basins_dir <- file.path(maps_dir, "basins")
#     
#     for (d in c(eoo_dir, aoo_dir, basins_dir)) {
#       if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
#     }
#     
#     # Load all species
#     all_species <- unique(scenario_table$species)
#     all_species <- all_species[!is.na(all_species)]
#     
#     log_info("Generating maps for %d species across all scenarios...", length(all_species), module = module)
#     
#     generated_maps <- list()
#     failed_species <- character(0)
#     
#     for (sp in all_species) {
#       log_info("Processing maps for: %s", sp, module = module)
#       
#       # Wrap each species in tryCatch for robustness
#       tryCatch({
#         sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
#         scenario <- scenario_table$scenario[scenario_table$species == sp][1]
#         
#         # Get species data from appropriate branch(es)
#         sp_data <- NULL
#         sp_sf <- NULL
#         
#         if (scenario == 1) {
#           # Indigenous only
#           sp_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
#           sp_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
#           population_types <- "indigenous"
#           
#         } else if (scenario == 2) {
#           # Non-indigenous only
#           sp_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
#           sp_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
#           population_types <- "non-indigenous"
#           
#         } else if (scenario == 3) {
#           # Both - combine using safe bind functions
#           ind_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]
#           non_ind_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
#           sp_data <- .safe_bind_rows(ind_data, non_ind_data)
#           
#           ind_sf <- result_indigenous$clean_sf[result_indigenous$clean_sf$species == sp, ]
#           non_ind_sf <- result_non_indigenous$clean_sf[result_non_indigenous$clean_sf$species == sp, ]
#           sp_sf <- .safe_bind_sf(ind_sf, non_ind_sf)
#           
#           population_types <- "both"
#         }
#         
#         if (is.null(sp_data) || nrow(sp_data) == 0) {
#           log_warn("  No data for %s. Skipping.", sp, module = module)
#           next
#         }
#         
#         maps_generated <- list()
#         
#         # --- 1. EOO (Extent of Occurrence) ---
#         eoo_poly <- NULL
#         if (nrow(sp_sf) >= 3) {
#           eoo_poly <- tryCatch({
#             hull <- sf::st_convex_hull(sf::st_union(sp_sf))
#             if (!sf::st_is_valid(hull)) hull <- sf::st_make_valid(hull)
#             hull
#           }, error = function(e) {
#             log_warn("  EOO calculation failed: %s", conditionMessage(e), module = module)
#             NULL
#           })
#         }
#         
#         if (!is.null(eoo_poly)) {
#           eoo_sf <- sf::st_sf(geometry = sf::st_sfc(eoo_poly), crs = sf::st_crs(sp_sf))
#           eoo_sf$Name <- paste(sp, "EOO")
#           eoo_sf$population_type <- population_types
#           
#           # GeoJSON
#           eoo_geo <- eoo_sf
#           eoo_geo$fill <- "#FFFF00"
#           eoo_geo$`fill-opacity` <- 0.80
#           eoo_geo$stroke <- "#FFFF00"
#           eoo_geo$`stroke-width` <- 2
#           
#           eoo_file <- file.path(eoo_dir, paste0(sp_clean, "_EOO.geojson"))
#           try(sf::st_write(eoo_geo, eoo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
#           maps_generated$eoo_geojson <- eoo_file
#           
#           # KML
#           if ("kml" %in% formats) {
#             eoo_kml <- file.path(eoo_dir, paste0(sp_clean, "_EOO.kml"))
#             .write_styled_kml(eoo_sf, eoo_kml, "EOO", "#FFFF00", 0.80)
#             maps_generated$eoo_kml <- eoo_kml
#           }
#         }
#         
#         # --- 2. AOO (Area of Occupancy) ---
#         aoo_poly <- NULL
#         tryCatch({
#           grid <- sf::st_make_grid(sp_sf, cellsize = 0.018, square = TRUE)
#           inter <- sf::st_intersects(grid, sp_sf)
#           has_pts <- lengths(inter) > 0
#           aoo_cells <- grid[has_pts]
#           
#           if (length(aoo_cells) > 0) {
#             u <- sf::st_union(aoo_cells)
#             if (!sf::st_is_valid(u)) u <- sf::st_make_valid(u)
#             aoo_poly <- u
#           }
#         }, error = function(e) {
#           log_warn("  AOO calculation failed: %s", conditionMessage(e), module = module)
#         })
#         
#         if (!is.null(aoo_poly)) {
#           aoo_sf <- sf::st_sf(geometry = sf::st_sfc(aoo_poly), crs = sf::st_crs(sp_sf))
#           aoo_sf$Name <- paste(sp, "AOO")
#           aoo_sf$population_type <- population_types
#           
#           aoo_geo <- aoo_sf
#           aoo_geo$fill <- "#FFFF00"
#           aoo_geo$`fill-opacity` <- 0.80
#           aoo_geo$stroke <- "#FFFF00"
#           aoo_geo$`stroke-width` <- 2
#           
#           aoo_file <- file.path(aoo_dir, paste0(sp_clean, "_AOO.geojson"))
#           try(sf::st_write(aoo_geo, aoo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
#           maps_generated$aoo_geojson <- aoo_file
#           
#           if ("kml" %in% formats) {
#             aoo_kml <- file.path(aoo_dir, paste0(sp_clean, "_AOO.kml"))
#             .write_styled_kml(aoo_sf, aoo_kml, "AOO", "#FFFF00", 0.80)
#             maps_generated$aoo_kml <- aoo_kml
#           }
#         }
#         
#         # --- 3. HydroBASINS (with population-based styling) ---
#         basins_map <- .generate_hydrobasins_map(
#           sp, sp_data, sp_sf, scenario, cache_dir, bbox_expand_km, module
#         )
#         
#         if (!is.null(basins_map)) {
#           # GeoJSON
#           if ("geojson" %in% formats) {
#             basins_geo_file <- file.path(basins_dir, paste0(sp_clean, "_basins.geojson"))
#             try(sf::st_write(basins_map$styled, basins_geo_file, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
#             maps_generated$basins_geojson <- basins_geo_file
#           }
#           
#           # KML
#           if ("kml" %in% formats) {
#             basins_kml_file <- file.path(basins_dir, paste0(sp_clean, "_basins.kml"))
#             .write_basins_kml(basins_map$raw, basins_kml_file)
#             maps_generated$basins_kml <- basins_kml_file
#           }
#         }
#         
#         generated_maps[[sp]] <- maps_generated
#         log_info("  Generated %d map files for %s", length(maps_generated), sp, module = module)
#         
#       }, error = function(e) {
#         log_warn("  Failed to generate maps for %s: %s", sp, conditionMessage(e), module = module)
#         failed_species <<- c(failed_species, sp)
#       })
#     }
#     
#     # Summary
#     total_files <- sum(sapply(generated_maps, length))
#     log_info("Map generation complete: %d species, %d total files", 
#              length(generated_maps), total_files, module = module)
#     
#     if (length(failed_species) > 0) {
#       log_warn("Failed to generate maps for %d species: %s", 
#                length(failed_species), paste(failed_species, collapse = ", "), module = module)
#     }
#     
#     return(list(
#       maps = generated_maps,
#       failed_species = failed_species,
#       summary = list(
#         species_count = length(generated_maps),
#         total_files = total_files,
#         failed_count = length(failed_species)
#       )
#     ))
#   })
# }
# 
# 
# # --- HELPER: GENERATE HYDROBASINS MAP (WITH DEBUG LOGGING) ---
# .generate_hydrobasins_map <- function(sp, sp_data, sp_sf, scenario, cache_dir, bbox_expand_km, module) {
#   
#   # DEBUG: Log cache directory
#   log_info("  [DEBUG] Cache directory: %s", cache_dir, module = module)
#   log_info("  [DEBUG] Cache dir exists: %s", dir.exists(cache_dir), module = module)
#   
#   # List files in cache
#   if (dir.exists(cache_dir)) {
#     cache_files <- list.files(cache_dir, pattern = "^hydro_lev.*\\.rds$")
#     log_info("  [DEBUG] HydroBASINS files in cache: %s", 
#              if(length(cache_files) > 0) paste(cache_files, collapse = ", ") else "NONE", 
#              module = module)
#   }
#   
#   # Get HydroBASINS assignments
#   basins_raw <- unique(sp_data$hydrobasin)
#   basins_raw <- basins_raw[!is.na(basins_raw) & nzchar(basins_raw)]
#   
#   log_info("  [DEBUG] Raw basin codes from data: %s", 
#            if(length(basins_raw) > 0) paste(head(basins_raw, 3), collapse = " | ") else "NONE", 
#            module = module)
#   
#   if (length(basins_raw) == 0) {
#     log_warn("  No HydroBASINS data for %s", sp, module = module)
#     return(NULL)
#   }
#   
#   # Parse basin codes: "L8:2080008490 | L8:2080008491"
#   all_basin_codes <- unlist(strsplit(basins_raw, " \\| "))
#   
#   log_info("  [DEBUG] Parsed %d basin codes", length(all_basin_codes), module = module)
#   log_info("  [DEBUG] Sample codes: %s", paste(head(all_basin_codes, 5), collapse = ", "), module = module)
#   
#   # Extract level and IDs
#   basin_info <- data.frame(
#     code = all_basin_codes,
#     level = as.integer(sub("^L(\\d+):.*", "\\1", all_basin_codes)),
#     id = sub("^L\\d+:", "", all_basin_codes),
#     stringsAsFactors = FALSE
#   )
#   
#   # DEBUG: Show what we extracted
#   log_info("  [DEBUG] Extracted levels: %s", paste(unique(basin_info$level), collapse = ", "), module = module)
#   log_info("  [DEBUG] Sample basin info:", module = module)
#   log_info("  [DEBUG]   First code: %s -> Level: %d, ID: %s", 
#            basin_info$code[1], basin_info$level[1], basin_info$id[1], module = module)
#   
#   # Determine which levels we need
#   levels_needed <- unique(basin_info$level)
#   
#   log_info("  [DEBUG] Levels needed: %s", paste(levels_needed, collapse = ", "), module = module)
#   
#   # Load HydroBASINS layers
#   basin_geometries <- list()
#   
#   for (lvl in levels_needed) {
#     cache_file <- file.path(cache_dir, sprintf("hydro_lev%02d_merged.rds", lvl))
#     
#     log_info("  [DEBUG] Looking for: %s", basename(cache_file), module = module)
#     log_info("  [DEBUG] Full path: %s", cache_file, module = module)
#     log_info("  [DEBUG] File exists: %s", file.exists(cache_file), module = module)
#     
#     if (!file.exists(cache_file)) {
#       log_warn("  HydroBASINS L%d cache not found", lvl, module = module)
#       next
#     }
#     
#     log_info("  [DEBUG] Loading HydroBASINS L%d...", lvl, module = module)
#     lyr <- readRDS(cache_file)
#     log_info("  [DEBUG] Loaded %d features for L%d", nrow(lyr), lvl, module = module)
#     
#     # Get basin IDs for this level
#     ids_needed <- basin_info$id[basin_info$level == lvl]
#     log_info("  [DEBUG] Need %d basin IDs for L%d", length(ids_needed), lvl, module = module)
#     log_info("  [DEBUG] Sample IDs needed: %s", paste(head(ids_needed, 3), collapse = ", "), module = module)
#     
#     # Check what column name is in the layer
#     log_info("  [DEBUG] Layer columns: %s", paste(names(lyr), collapse = ", "), module = module)
#     
#     # Filter to needed basins
#     basins_subset <- lyr[lyr$HB_LABEL %in% ids_needed, ]
#     
#     log_info("  [DEBUG] Matched %d basins for L%d", nrow(basins_subset), lvl, module = module)
#     
#     if (nrow(basins_subset) > 0) {
#       basin_geometries[[as.character(lvl)]] <- basins_subset
#     }
#   }
#   
#   if (length(basin_geometries) == 0) {
#     log_warn("  No HydroBASINS geometries found for %s", sp, module = module)
#     return(NULL)
#   }
#   
#   log_info("  [DEBUG] Successfully loaded %d levels of HydroBASINS", length(basin_geometries), module = module)
#   
#   # Combine all basin levels - use safe binding
#   all_basins <- tryCatch({
#     # Try standard rbind first
#     do.call(rbind, basin_geometries)
#   }, error = function(e) {
#     # Fallback: align columns then bind
#     log_warn("  [DEBUG] Standard rbind failed, using safe bind: %s", conditionMessage(e), module = module)
#     
#     # Get all column names across all sf objects
#     all_cols <- unique(unlist(lapply(basin_geometries, function(x) {
#       setdiff(names(x), attr(x, "sf_column"))
#     })))
#     
#     # Align each sf object
#     aligned <- lapply(basin_geometries, function(sf_obj) {
#       geom_col <- attr(sf_obj, "sf_column")
#       for (col in setdiff(all_cols, names(sf_obj))) {
#         sf_obj[[col]] <- NA
#       }
#       sf_obj[, c(all_cols, geom_col)]
#     })
#     
#     do.call(rbind, aligned)
#   })
#   
#   log_info("  [DEBUG] Combined %d total basin features", nrow(all_basins), module = module)
#   
#   # Assign population status to each basin
#   if (scenario == 3) {
#     # Scenario 3: Need to determine which basins are native vs invaded
#     basin_status <- data.frame(
#       HB_LABEL = all_basins$HB_LABEL,
#       status = NA_character_,
#       stringsAsFactors = FALSE
#     )
#     
#     for (i in seq_len(nrow(all_basins))) {
#       basin_geom <- all_basins[i, ]
#       
#       # Find points in this basin
#       pts_in_basin <- suppressWarnings(sf::st_filter(sp_sf, basin_geom))
#       
#       if (nrow(pts_in_basin) > 0) {
#         pop_types <- unique(pts_in_basin$population_type)
#         
#         if ("indigenous" %in% pop_types && "non-indigenous" %in% pop_types) {
#           basin_status$status[i] <- "Mixed"
#         } else if ("indigenous" %in% pop_types) {
#           basin_status$status[i] <- "Native"
#         } else if ("non-indigenous" %in% pop_types) {
#           basin_status$status[i] <- "Introduced"
#         }
#       }
#     }
#     
#     all_basins$status <- basin_status$status
#     
#   } else if (scenario == 1) {
#     # All native
#     all_basins$status <- "Native"
#   } else {
#     # All introduced
#     all_basins$status <- "Introduced"
#   }
#   
#   # Apply styling
#   styled_basins <- all_basins
#   styled_basins$fill <- ifelse(all_basins$status == "Native", "#D48D00", 
#                                ifelse(all_basins$status == "Introduced", "#4D0073", "#FF6600"))
#   styled_basins$`fill-opacity` <- 0.35
#   styled_basins$stroke <- styled_basins$fill
#   styled_basins$`stroke-width` <- 1.5
#   
#   log_info("  [DEBUG] Successfully styled %d basins", nrow(styled_basins), module = module)
#   
#   return(list(
#     raw = all_basins,
#     styled = styled_basins
#   ))
# }
# 
# 
# # --- HELPER: WRITE STYLED KML (EOO/AOO) ---
# .write_styled_kml <- function(sf_obj, file_path, layer_name, color, opacity) {
#   tmp <- tempfile(fileext = ".kml")
#   sf_obj$kml_id <- seq_len(nrow(sf_obj))
#   sf::st_write(sf_obj, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
#   kml_txt <- paste(readLines(tmp), collapse = "\n")
#   
#   # Convert color to KML format (AABBGGRR)
#   # Yellow #FFFF00 -> opacity=80% (cc) -> cc00ffff
#   hex_color <- sub("^#", "", color)
#   r <- substr(hex_color, 1, 2)
#   g <- substr(hex_color, 3, 4)
#   b <- substr(hex_color, 5, 6)
#   opacity_hex <- sprintf("%02x", as.integer(opacity * 255))
#   kml_color <- paste0(opacity_hex, b, g, r)
#   
#   style_def <- sprintf('
#   <Style id="%sStyle">
#     <LineStyle>
#       <color>ff%s%s%s</color>
#       <width>2</width>
#     </LineStyle>
#     <PolyStyle>
#       <color>%s</color>
#       <fill>1</fill>
#       <outline>1</outline>
#     </PolyStyle>
#   </Style>', layer_name, b, g, r, kml_color)
#   
#   kml_txt <- sub("<Document>", paste0("<Document>", style_def), kml_txt)
#   kml_txt <- gsub("(<Placemark[^>]*>)", paste0("\\1<styleUrl>#", layer_name, "Style</styleUrl>"), kml_txt)
#   
#   writeLines(kml_txt, file_path)
#   unlink(tmp)
# }
# 
# 
# # --- HELPER: WRITE BASINS KML (with population-based styling) ---
# .write_basins_kml <- function(basins_sf, file_path) {
#   tmp <- tempfile(fileext = ".kml")
#   sf::st_write(basins_sf, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
#   kml_txt <- paste(readLines(tmp), collapse = "\n")
#   
#   # Define styles for each status
#   # Native: #D48D00 (orange)
#   # Introduced: #4D0073 (purple)
#   # Mixed: #FF6600 (red-orange)
#   
#   style_defs <- '
#   <Style id="nativeStyle">
#     <LineStyle><color>ff008dd4</color><width>1.5</width></LineStyle>
#     <PolyStyle><color>59008dd4</color><fill>1</fill><outline>1</outline></PolyStyle>
#   </Style>
#   <Style id="introducedStyle">
#     <LineStyle><color>ff73004d</color><width>1.5</width></LineStyle>
#     <PolyStyle><color>5973004d</color><fill>1</fill><outline>1</outline></PolyStyle>
#   </Style>
#   <Style id="mixedStyle">
#     <LineStyle><color>ff0066ff</color><width>1.5</width></LineStyle>
#     <PolyStyle><color>800066ff</color><fill>1</fill><outline>1</outline></PolyStyle>
#   </Style>'
#   
#   kml_txt <- sub("<Document>", paste0("<Document>", style_defs), kml_txt)
#   
#   # Apply styles based on status
#   kml_txt <- gsub(
#     "(<Placemark[^>]*>)([\\s\\S]*?<n>Native</n>)",
#     "\\1<styleUrl>#nativeStyle</styleUrl>\\2",
#     kml_txt
#   )
#   kml_txt <- gsub(
#     "(<Placemark[^>]*>)([\\s\\S]*?<n>Introduced</n>)",
#     "\\1<styleUrl>#introducedStyle</styleUrl>\\2",
#     kml_txt
#   )
#   kml_txt <- gsub(
#     "(<Placemark[^>]*>)([\\s\\S]*?<n>Mixed</n>)",
#     "\\1<styleUrl>#mixedStyle</styleUrl>\\2",
#     kml_txt
#   )
#   
#   writeLines(kml_txt, file_path)
#   unlink(tmp)
# }