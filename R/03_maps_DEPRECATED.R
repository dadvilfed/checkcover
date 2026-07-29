#### MODULE 3: GENERATE MAPS ####

generate_maps <- function(result, hydro_dir = "spatial_data/hydrobasins",
                          output_dir = "checkover_output", species_list = NULL,
                          formats = c("geojson", "kml"), bbox_expand_km = 200,
                          reuse_cached = FALSE, force_regenerate = TRUE,
                          parallel = FALSE) {
  module <- "MODULE3_MAPS"
  
  with_log_section(module, {
    log_info("=== MODULE 3: GENERATE MAPS ===", module = module)
    log_info("Parallel processing: %s", as.character(parallel), module = module)
    log_info("Force regenerate: %s", as.character(force_regenerate), module = module)
    
    if (!is.list(result) || !all(c("clean_data", "clean_sf") %in% names(result))) {
      stop("Invalid result")
    }
    
    cd <- result$clean_data
    sf_pts <- result$clean_sf
    tryCatch({ sf::st_geometry(sf_pts) <- "geometry" }, error = function(e) {})
    
    maps_dir <- file.path(output_dir, "maps")
    if (!dir.exists(maps_dir)) dir.create(maps_dir, recursive = TRUE)
    
    eoo_dir <- file.path(maps_dir, "EOO")
    aoo_dir <- file.path(maps_dir, "AOO")
    if (!dir.exists(eoo_dir)) dir.create(eoo_dir)
    if (!dir.exists(aoo_dir)) dir.create(aoo_dir)
    
    cache_dir <- file.path(output_dir, "cache")
    if (!dir.exists(cache_dir) && dir.exists(file.path("checkover_output", "cache"))) {
      cache_dir <- file.path("checkover_output", "cache")
    }
    
    # Helper: Write styled KML
    .write_styled_kml <- function(sf_obj, file_path, layer_name) {
      tmp <- tempfile(fileext = ".kml")
      sf_obj$kml_id <- seq_len(nrow(sf_obj))
      sf::st_write(sf_obj, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
      kml_txt <- paste(readLines(tmp), collapse = "\n")
      
      style_def <- "
      <Style id=\"yellowPoly\">
        <LineStyle>
          <color>ff00ffff</color>
          <width>2</width>
        </LineStyle>
        <PolyStyle>
          <color>cc00ffff</color>
          <fill>1</fill>
          <outline>1</outline>
        </PolyStyle>
      </Style>"
      
      kml_txt <- sub("<Document>", paste0("<Document>", style_def), kml_txt)
      kml_txt <- gsub("(<Placemark[^>]*>)", "\\1<styleUrl>#yellowPoly</styleUrl>", kml_txt)
      writeLines(kml_txt, file_path)
      unlink(tmp)
    }
    
    # Helper: Write combined KML (basins + type locality)
    .write_combined_kml <- function(basins_sf, type_loc_sf, file_path) {
      tmp <- tempfile(fileext = ".kml")
      
      basins_sf$layer_type <- basins_sf$status
      
      if (!is.null(type_loc_sf) && nrow(type_loc_sf) > 0) {
        type_loc_sf$layer_type <- "Type_Locality"
        type_loc_sf$status <- "Type Locality"
        type_loc_sf$HB_LABEL <- "Type Locality"
        
        common_cols <- intersect(names(basins_sf), names(type_loc_sf))
        basins_sf <- basins_sf[, common_cols]
        type_loc_sf <- type_loc_sf[, common_cols]
        
        combined_sf <- rbind(basins_sf, type_loc_sf)
      } else {
        combined_sf <- basins_sf
      }
      
      sf::st_write(combined_sf, tmp, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
      kml_txt <- paste(readLines(tmp), collapse = "\n")
      
      style_defs <- "
      <Style id=\"nativeStyle\">
        <LineStyle><color>ff008dd4</color><width>1.5</width></LineStyle>
        <PolyStyle><color>59008dd4</color><fill>1</fill><outline>1</outline></PolyStyle>
      </Style>
      <Style id=\"introducedStyle\">
        <LineStyle><color>ff73004d</color><width>1.5</width></LineStyle>
        <PolyStyle><color>5973004d</color><fill>1</fill><outline>1</outline></PolyStyle>
      </Style>
      <Style id=\"typeLocalityStyle\">
        <LineStyle><color>ff0000ff</color><width>2</width></LineStyle>
        <PolyStyle><color>800000ff</color><fill>1</fill><outline>1</outline></PolyStyle>
      </Style>"
      
      kml_txt <- sub("<Document>", paste0("<Document>", style_defs), kml_txt)
      
      kml_txt <- gsub("(<Placemark[^>]*>)([\\s\\S]*?<name>Native</name>)",
                      "\\1<styleUrl>#nativeStyle</styleUrl>\\2", kml_txt)
      kml_txt <- gsub("(<Placemark[^>]*>)([\\s\\S]*?<name>Introduced</name>)",
                      "\\1<styleUrl>#introducedStyle</styleUrl>\\2", kml_txt)
      kml_txt <- gsub("(<Placemark[^>]*>)([\\s\\S]*?<name>Type Locality</name>)",
                      "\\1<styleUrl>#typeLocalityStyle</styleUrl>\\2", kml_txt)
      
      writeLines(kml_txt, file_path)
      unlink(tmp)
    }
    
    if (is.null(species_list)) species_list <- unique(cd$species)
    species_list <- species_list[!is.na(species_list) & nzchar(species_list)]
    
    log_info("Processing maps for %d species...", length(species_list), module = module)
    
    # Processing function
    process_species_map <- function(sp) {
      sp_id <- gsub("[^A-Za-z0-9_]+", "_", sp)
      
      # Resume check
      if (!force_regenerate) {
        check_file <- file.path(maps_dir, sprintf("%s_basins.geojson", sp_id))
        check_eoo <- file.path(eoo_dir, sprintf("%s_EOO.geojson", sp_id))
        if (file.exists(check_file) || file.exists(check_eoo)) {
          return(list(status = "skipped"))
        }
      }
      
      library(sf)
      library(dplyr)
      
      sp_rows <- which(cd$species == sp)
      if (!length(sp_rows)) return(NULL)
      sp_pts <- sf_pts[sp_rows, , drop = FALSE]
      
      # 1. EOO
      eoo_poly <- NULL
      if (nrow(sp_pts) >= 3) {
        eoo_poly <- tryCatch({
          hull <- sf::st_convex_hull(sf::st_union(sp_pts))
          if (!sf::st_is_valid(hull)) hull <- sf::st_make_valid(hull)
          hull
        }, error = function(e) NULL)
      }
      
      # 2. AOO
      aoo_poly <- NULL
      tryCatch({
        grid <- sf::st_make_grid(sp_pts, cellsize = 0.018, square = TRUE)
        inter <- sf::st_intersects(grid, sp_pts)
        has_pts <- lengths(inter) > 0
        aoo_cells <- grid[has_pts]
        if (length(aoo_cells) > 0) {
          u <- sf::st_union(aoo_cells)
          if (!sf::st_is_valid(u)) u <- sf::st_make_valid(u)
          aoo_poly <- u
        }
      }, error = function(e) {})
      
      # 3. Type Locality
      tl_poly_sf <- NULL
      is_tl <- cd$is_type_locality[sp_rows]
      
      if (any(is_tl, na.rm = TRUE)) {
        tl_raw_pts <- sp_pts[which(is_tl), , drop = FALSE]
        ea_crs <- 6933
        pts_ea <- sf::st_transform(tl_raw_pts, ea_crs)
        
        sq_list <- lapply(sf::st_geometry(pts_ea), function(geo) {
          circle <- sf::st_buffer(geo, dist = 1000)
          sf::st_as_sfc(sf::st_bbox(circle))
        })
        
        sq_sfc <- do.call(c, sq_list)
        sf::st_crs(sq_sfc) <- ea_crs
        tl_poly_final <- sf::st_transform(sq_sfc, sf::st_crs(sp_pts))
        
        tl_poly_sf <- sf::st_sf(geometry = tl_poly_final)
        tl_poly_sf$Name <- "Type Locality"
        tl_poly_sf$layer_type <- "Type_Locality"
      }
      
      # 4. HydroBASINS
      cat_val <- if ("distribution_category" %in% names(cd)) {
        cd$distribution_category[sp_rows][1]
      } else {
        "regional"
      }
      if (is.na(cat_val)) cat_val <- "regional"
      lvl <- .level_for_cat(cat_val)
      
      has_basins <- FALSE
      basins_final <- NULL
      
      cpath <- file.path(cache_dir, sprintf("hydro_lev%02d_merged.rds", as.integer(lvl)))
      if (!file.exists(cpath)) {
        cpath_shared <- file.path("checkover_output", "cache", basename(cpath))
        if (file.exists(cpath_shared)) cpath <- cpath_shared
      }
      
      if (file.exists(cpath)) {
        lyr <- try(readRDS(cpath), silent = TRUE)
        if (!inherits(lyr, "try-error")) {
          if (sf::st_crs(lyr) != sf::st_crs(sp_pts)) {
            lyr <- sf::st_transform(lyr, sf::st_crs(sp_pts))
          }
          
          bb_search <- sf::st_as_sfc(sf::st_bbox(sp_pts))
          bb_search <- .expand_bbox_km(bb_search, sf::st_crs(sp_pts), bbox_expand_km)
          
          lyr_neighborhood <- try(suppressWarnings(sf::st_crop(lyr, bb_search)), silent = TRUE)
          if (inherits(lyr_neighborhood, "try-error") || nrow(lyr_neighborhood) == 0) {
            lyr_neighborhood <- lyr
          }
          
          basins_final <- try(sf::st_filter(lyr_neighborhood, sp_pts), silent = TRUE)
          
          if (!inherits(basins_final, "try-error") && nrow(basins_final) > 0) {
            has_basins <- TRUE
            is_alien_global <- any(tolower(cd$status[sp_rows]) == "alien", na.rm = TRUE)
            basins_final$status <- if (is_alien_global) "Introduced" else "Native"
            basins_final$layer_type <- basins_final$status
          }
        }
      }
      
      # Export
      out <- list()
      
      # A. EOO
      if (!is.null(eoo_poly)) {
        eoo_sf <- sf::st_sf(geometry = sf::st_sfc(eoo_poly), crs = sf::st_crs(sp_pts))
        eoo_sf$Name <- paste(sp, "EOO")
        
        eoo_geo <- eoo_sf
        eoo_geo$fill <- "#FFFF00"
        eoo_geo$`fill-opacity` <- 0.80
        eoo_geo$stroke <- "#FFFF00"
        eoo_geo$`stroke-width` <- 2
        eoo_geo$`stroke-opacity` <- 1.0
        
        f <- file.path(eoo_dir, sprintf("%s_EOO.geojson", sp_id))
        try(sf::st_write(eoo_geo, f, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
        out$eoo_geojson <- f
        
        if ("kml" %in% formats) {
          f_kml <- file.path(eoo_dir, sprintf("%s_EOO.kml", sp_id))
          .write_styled_kml(eoo_sf, f_kml, "EOO")
          out$eoo_kml <- f_kml
        }
      }
      
      # B. AOO
      if (!is.null(aoo_poly)) {
        aoo_sf <- sf::st_sf(geometry = sf::st_sfc(aoo_poly), crs = sf::st_crs(sp_pts))
        aoo_sf$Name <- paste(sp, "AOO")
        
        aoo_geo <- aoo_sf
        aoo_geo$fill <- "#FFFF00"
        aoo_geo$`fill-opacity` <- 0.80
        aoo_geo$stroke <- "#FFFF00"
        aoo_geo$`stroke-width` <- 2
        aoo_geo$`stroke-opacity` <- 1.0
        
        f <- file.path(aoo_dir, sprintf("%s_AOO.geojson", sp_id))
        try(sf::st_write(aoo_geo, f, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
        out$aoo_geojson <- f
        
        if ("kml" %in% formats) {
          f_kml <- file.path(aoo_dir, sprintf("%s_AOO.kml", sp_id))
          .write_styled_kml(aoo_sf, f_kml, "AOO")
          out$aoo_kml <- f_kml
        }
      }
      
      # C. HydroBASINS + Type Locality
      if (has_basins) {
        style_to_apply <- if (all(basins_final$status == "Native")) {
          list(fill = "#D48D00", `fill-opacity` = 0.35, stroke = "#D48D00", `stroke-width` = 1.5)
        } else {
          list(fill = "#4D0073", `fill-opacity` = 0.35, stroke = "#4D0073", `stroke-width` = 1.5)
        }
        
        basins_styled <- basins_final
        basins_styled$fill <- style_to_apply$fill
        basins_styled$`fill-opacity` <- style_to_apply$`fill-opacity`
        basins_styled$stroke <- style_to_apply$stroke
        basins_styled$`stroke-width` <- style_to_apply$`stroke-width`
        
        if (!is.null(tl_poly_sf) && nrow(tl_poly_sf) > 0) {
          tl_styled <- tl_poly_sf
          tl_styled$HB_LABEL <- "Type Locality"
          tl_styled$status <- "Type Locality"
          tl_styled$fill <- "#FF0000"
          tl_styled$`fill-opacity` <- 0.50
          tl_styled$stroke <- "#FF0000"
          tl_styled$`stroke-width` <- 2
          
          basin_cols <- names(basins_styled)
          tl_cols <- names(tl_styled)
          
          for (col in setdiff(basin_cols, tl_cols)) {
            tl_styled[[col]] <- NA
          }
          for (col in setdiff(tl_cols, basin_cols)) {
            basins_styled[[col]] <- NA
          }
          
          combined_sf <- rbind(
            basins_styled[, union(basin_cols, tl_cols)],
            tl_styled[, union(basin_cols, tl_cols)]
          )
        } else {
          combined_sf <- basins_styled
        }
        
        if ("geojson" %in% formats) {
          f <- file.path(maps_dir, sprintf("%s_basins.geojson", sp_id))
          try(sf::st_write(combined_sf, f, delete_dsn = TRUE, quiet = TRUE), silent = TRUE)
          out$basins_geojson <- f
        }
        
        if ("kml" %in% formats) {
          f <- file.path(maps_dir, sprintf("%s_basins.kml", sp_id))
          .write_combined_kml(basins_final, tl_poly_sf, f)
          out$basins_kml <- f
        }
      }
      
      return(out)
    }
    
    # Execute
    if (parallel) {
      log_info("Executing maps in PARALLEL mode...", module = module)
      generated_list <- future.apply::future_lapply(
        species_list, process_species_map,
        future.seed = TRUE,
        future.packages = c("sf", "dplyr")
      )
    } else {
      log_info("Executing maps in SEQUENTIAL mode...", module = module)
      pb <- create_progress_bar(length(species_list))
      generated_list <- lapply(species_list, function(sp) {
        pb$tick()
        process_species_map(sp)
      })
      pb$terminate()
    }
    
    names(generated_list) <- species_list
    generated_list <- Filter(Negate(is.null), generated_list)
    actual_gen <- Filter(function(x) is.null(x$status) || x$status != "skipped", generated_list)
    
    log_info("Map generation complete. Processed %d species.", length(actual_gen), module = module)
    return(generated_list)
  })
}