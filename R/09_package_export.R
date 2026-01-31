#### MODULE 9: SPECIES PACKAGE EXPORT ####

#' Export individual species packages with maps, narratives, and citations
#' @param scenario_table Scenario detection table
#' @param all_maps Maps result object
#' @param all_narratives Narratives result object
#' @param all_citations Citations result object
#' @param vernacular_lookup Vernacular names lookup
#' @param output_dir Output directory
#' @return List of packaged species
export_species_packages <- function(scenario_table,
                                    all_maps,
                                    canonical_narratives,  # ✅ CHANGED
                                    all_citations,
                                    vernacular_lookup = NULL,
                                    output_dir = "checkover_output") {
  module <- "MODULE9_PACKAGING"
  
  with_log_section(module, {
    log_info("=== MODULE 9: SPECIES PACKAGE EXPORT ===", module = module)
    
    # Create packages directory
    packages_dir <- file.path(output_dir, "species_packages")
    if (!dir.exists(packages_dir)) dir.create(packages_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Get all species
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]
    
    log_info("Packaging %d species...", length(all_species), module = module)
    
    # Extract vernacular lookup
    vern_map <- NULL
    if (!is.null(vernacular_lookup)) {
      if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup)) {
        vern_map <- vernacular_lookup$wide
      } else if (is.data.frame(vernacular_lookup)) {
        vern_map <- vernacular_lookup
      }
    }
    
    packaged_species <- list()
    
    for (sp in all_species) {
      log_info("Packaging: %s", sp, module = module)
      
      sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]
      
      # Create species directory
      species_dir <- file.path(packages_dir, sp_clean)
      if (!dir.exists(species_dir)) dir.create(species_dir, recursive = TRUE, showWarnings = FALSE)
      
      package_files <- list()
      files_count <- 0
      
      # --- 1. MAPS ---
      if (!is.null(all_maps) && sp %in% names(all_maps$maps)) {
        maps_subdir <- file.path(species_dir, "maps")
        if (!dir.exists(maps_subdir)) dir.create(maps_subdir, showWarnings = FALSE)
        
        sp_maps <- all_maps$maps[[sp]]
        
        for (map_type in names(sp_maps)) {
          src_file <- sp_maps[[map_type]]
          
          if (file.exists(src_file)) {
            dst_file <- file.path(maps_subdir, basename(src_file))
            file.copy(src_file, dst_file, overwrite = TRUE)
            package_files[[paste0("map_", map_type)]] <- dst_file
            files_count <- files_count + 1
          }
        }
        
        log_info("  Copied %d map files", length(sp_maps), module = module)
      }
      
      # --- 2. NARRATIVES (FROM CANONICAL) ---
      if (!is.null(canonical_narratives) && sp %in% names(canonical_narratives$narratives)) {
        narratives_subdir <- file.path(species_dir, "narratives")
        if (!dir.exists(narratives_subdir)) dir.create(narratives_subdir, showWarnings = FALSE)
        
        sp_narratives <- canonical_narratives$narratives[[sp]]
        
        # Copy canonical markdown (full template)
        if (!is.null(sp_narratives$canonical_markdown) && file.exists(sp_narratives$canonical_markdown)) {
          dst_file <- file.path(narratives_subdir, basename(sp_narratives$canonical_markdown))
          file.copy(sp_narratives$canonical_markdown, dst_file, overwrite = TRUE)
          package_files$narrative_canonical <- dst_file
          files_count <- files_count + 1
        }
        
        # Copy formal narrative TXT (Section 5)
        if (!is.null(sp_narratives$formal_text) && file.exists(sp_narratives$formal_text)) {
          dst_file <- file.path(narratives_subdir, basename(sp_narratives$formal_text))
          file.copy(sp_narratives$formal_text, dst_file, overwrite = TRUE)
          package_files$narrative_text <- dst_file
          files_count <- files_count + 1
        }
        
        # Copy formal narrative JSON (Section 5)
        if (!is.null(sp_narratives$formal_json) && file.exists(sp_narratives$formal_json)) {
          dst_file <- file.path(narratives_subdir, basename(sp_narratives$formal_json))
          file.copy(sp_narratives$formal_json, dst_file, overwrite = TRUE)
          package_files$narrative_json <- dst_file
          files_count <- files_count + 1
        }
        
        log_info("  Copied %d narrative files", 3, module = module)
      }
      
      # --- 3. CITATIONS ---
      if (!is.null(all_citations) && sp %in% names(all_citations$citations)) {
        citations_subdir <- file.path(species_dir, "citations")
        if (!dir.exists(citations_subdir)) dir.create(citations_subdir, showWarnings = FALSE)
        
        sp_citations <- all_citations$citations[[sp]]
        
        if (!is.null(sp_citations$files)) {
          for (src_file in sp_citations$files) {
            if (file.exists(src_file)) {
              dst_file <- file.path(citations_subdir, basename(src_file))
              file.copy(src_file, dst_file, overwrite = TRUE)
              
              ext <- tools::file_ext(src_file)
              package_files[[paste0("citation_", ext)]] <- dst_file
              files_count <- files_count + 1
            }
          }
          
          log_info("  Copied %d citation files", length(sp_citations$files), module = module)
        }
      }
      
      # --- 4. METADATA ---
      # Extract vernacular
      vernacular_string <- NA_character_
      if (!is.null(vern_map) && "species" %in% names(vern_map)) {
        idx <- match(sp, vern_map$species)
        if (!is.na(idx) && "vernacular_string" %in% names(vern_map)) {
          raw_v <- vern_map$vernacular_string[idx]
          if (!is.na(raw_v) && nzchar(raw_v)) {
            vernacular_string <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
          }
        }
      }
      
      # Scenario description
      scenario_desc <- switch(
        as.character(scenario),
        "1" = "Indigenous population only",
        "2" = "Non-indigenous population only",
        "3" = "Both indigenous and non-indigenous populations",
        "Unknown scenario"
      )
      
      package_metadata <- list(
        species = sp,
        vernacular_names = vernacular_string,
        package_id = sp_clean,
        scenario = scenario,
        scenario_description = scenario_desc,
        generated_date = as.character(Sys.Date()),
        total_files = files_count,
        provenance = list(
          source = "World of Crayfish",
          license = "CC-BY-4.0",
          framework = "cheCkOVER"
        ),
        contents = list(
          maps = sum(grepl("^map_", names(package_files))),
          narratives = sum(grepl("^narrative_", names(package_files))),
          citations = sum(grepl("^citation_", names(package_files)))
        )
      )
      
      metadata_file <- file.path(species_dir, "package_metadata.json")
      jsonlite::write_json(package_metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
      package_files$metadata <- metadata_file
      
      # --- 5. MANIFEST ---
      all_files <- unlist(package_files)
      
      manifest <- data.frame(
        filename = basename(all_files),
        filepath = all_files,
        size_bytes = file.size(all_files),
        file_type = tools::file_ext(all_files),
        stringsAsFactors = FALSE
      )
      
      # Add checksums if digest available
      if (requireNamespace("digest", quietly = TRUE)) {
        manifest$md5 <- vapply(all_files, function(f) {
          digest::digest(file = f, algo = "md5")
        }, character(1))
      }
      
      manifest_file <- file.path(species_dir, "file_manifest.csv")
      write.csv(manifest, manifest_file, row.names = FALSE)
      
      # --- 6. README ---
      readme_text <- sprintf(
        "# %s Species Package\n\n",
        sp
      )
      
      if (!is.na(vernacular_string) && nzchar(vernacular_string)) {
        readme_text <- paste0(readme_text, sprintf("**Common names:** %s\n\n", vernacular_string))
      }
      
      readme_text <- paste0(
        readme_text,
        sprintf("**Scenario:** %s\n", scenario_desc),
        sprintf("**Generated:** %s\n\n", Sys.Date()),
        "## Package Contents\n\n",
        sprintf("- **Maps:** %d files (EOO, AOO, HydroBASINS)\n", package_metadata$contents$maps),
        sprintf("- **Narratives:** %d files (text, JSON)\n", package_metadata$contents$narratives),
        sprintf("- **Citations:** %d files (JSON, BibTeX, CSV, CFF)\n", package_metadata$contents$citations),
        "\n## File Structure\n\n",
        "```\n",
        paste0(sp_clean, "/\n"),
        "├── maps/\n",
        "│   ├── *_EOO.geojson\n",
        "│   ├── *_EOO.kml\n",
        "│   ├── *_AOO.geojson\n",
        "│   ├── *_AOO.kml\n",
        "│   ├── *_basins.geojson\n",
        "│   └── *_basins.kml\n",
        "├── narratives/\n",
        "│   ├── *_narrative.txt\n",
        "│   └── *_narrative.json\n",
        "├── citations/\n",
        "│   ├── *_bibliography.json\n",
        "│   ├── *_bibliography.bib\n",
        "│   ├── *_bibliography.csv\n",
        "│   └── *_CITATION.cff\n",
        "├── package_metadata.json\n",
        "├── file_manifest.csv\n",
        "└── README.md\n",
        "```\n\n",
        "## License\n\n",
        "CC-BY-4.0\n\n",
        "## Citation\n\n",
        "See `citations/` folder for detailed references.\n"
      )
      
      readme_file <- file.path(species_dir, "README.md")
      writeLines(readme_text, readme_file)
      
      log_info("  Package complete: %d files total", files_count, module = module)
      
      packaged_species[[sp]] <- list(
        directory = species_dir,
        files_count = files_count,
        metadata = package_metadata
      )
    }
    
    # --- OVERALL SUMMARY ---
    summary_data <- list(
      total_species = length(packaged_species),
      generated_date = as.character(Sys.Date()),
      scenarios = list(
        scenario_1 = sum(scenario_table$scenario == 1, na.rm = TRUE),
        scenario_2 = sum(scenario_table$scenario == 2, na.rm = TRUE),
        scenario_3 = sum(scenario_table$scenario == 3, na.rm = TRUE)
      ),
      total_files = sum(sapply(packaged_species, function(x) x$files_count)),
      package_structure = list(
        maps = "EOO, AOO, HydroBASINS (GeoJSON, KML)",
        narratives = "Human-readable text + structured JSON",
        citations = "JSON, BibTeX, CSV, CFF formats",
        metadata = "package_metadata.json per species",
        manifest = "file_manifest.csv per species"
      )
    )
    
    summary_file <- file.path(packages_dir, "packaging_summary.json")
    jsonlite::write_json(summary_data, summary_file, pretty = TRUE, auto_unbox = TRUE)
    
    # --- INDEX FILE (for easy browsing) ---
    index_lines <- c(
      "# cheCkOVER Species Packages Index",
      "",
      sprintf("Generated: %s", Sys.Date()),
      sprintf("Total species: %d", length(packaged_species)),
      "",
      "## Species List",
      ""
    )
    
    for (sp in names(packaged_species)) {
      sp_info <- packaged_species[[sp]]
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]
      scenario_badge <- switch(
        as.character(scenario),
        "1" = "[Indigenous]",
        "2" = "[Non-indigenous]",
        "3" = "[Combined]",
        "[Unknown]"
      )
      
      index_lines <- c(
        index_lines,
        sprintf("- **%s** %s - %d files - `%s/`", 
                sp, scenario_badge, sp_info$files_count, sp_info$metadata$package_id)
      )
    }
    
    index_file <- file.path(packages_dir, "INDEX.md")
    writeLines(index_lines, index_file)
    
    log_info("Packaging complete: %d species, %d total files", 
             length(packaged_species), summary_data$total_files, module = module)
    log_info("Saved summary to: %s", summary_file, module = module)
    log_info("Saved index to: %s", index_file, module = module)
    
    return(list(
      packages = packaged_species,
      summary = summary_data
    ))
  })
}