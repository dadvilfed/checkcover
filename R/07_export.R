#### MODULE 7: PACKAGE EXPORT ####

package_export <- function(species_list, output_dir = "checkover_output",
                           maps_list = NULL, narratives_list = NULL,
                           citations_list = NULL, vernacular_result = NULL,
                           include_checksums = TRUE) {
  module <- "MODULE7_PACKAGE_EXPORT"
  
  with_log_section(module, {
    log_info("=== MODULE 7: PACKAGE EXPORT ===", module = module)
    
    .avail <- function(pkg) requireNamespace(pkg, quietly = TRUE)
    if (include_checksums && !.avail("digest")) {
      log_warn("digest not installed; disabling checksums.", module = module)
      include_checksums <- FALSE
    }
    
    # Input normalization
    if (!is.null(maps_list) && is.list(maps_list) && "species_maps" %in% names(maps_list)) {
      maps_list <- maps_list$species_maps
    }
    
    if (missing(species_list) || is.null(species_list)) {
      species_from <- unique(na.omit(unlist(list(
        if (!is.null(maps_list)) names(maps_list) else NULL,
        if (!is.null(narratives_list)) names(narratives_list) else NULL,
        if (!is.null(citations_list)) names(citations_list) else NULL
      ))))
      if (length(species_from) == 0) stop("species_list is empty.")
      species_list <- species_from
    }
    
    packages_dir <- file.path(output_dir, "species_packages")
    if (!dir.exists(packages_dir)) dir.create(packages_dir, recursive = TRUE)
    
    # Data loading
    .pick_best_occ_tsv <- function(odir) {
      prefs <- c("clean_occurrences_with_hydrobasin.tsv",
                 "clean_occurrences_with_metrics.tsv",
                 "clean_occurrences.tsv")
      cand <- file.path(odir, prefs)
      hit <- cand[file.exists(cand)]
      if (length(hit)) return(hit[1])
      
      any_tsv <- list.files(odir, pattern = "^clean_occurrences.*\\.tsv$", full.names = TRUE)
      if (length(any_tsv)) return(any_tsv[1])
      NA_character_
    }
    
    occ_tsv <- .pick_best_occ_tsv(output_dir)
    occurrences_df <- NULL
    if (isTRUE(nzchar(occ_tsv))) {
      occurrences_df <- tryCatch(read_tsv(occ_tsv), error = function(e) NULL)
    }
    
    # Load provenance
    .load_provenance <- function(odir) {
      p <- file.path(odir, "reports", "dataset_summary_statistics.json")
      if (file.exists(p)) {
        tryCatch(jsonlite::read_json(p, simplifyVector = TRUE), error = function(e) NULL)
      } else {
        NULL
      }
    }
    provenance <- .load_provenance(output_dir)
    method_block <- if (!is.null(provenance) && "methods" %in% names(provenance)) {
      provenance$methods
    } else {
      list()
    }
    
    # Vernacular lookup
    vern_lookup <- NULL
    if (!is.null(vernacular_result)) {
      if (is.list(vernacular_result) && "wide" %in% names(vernacular_result)) {
        vern_lookup <- vernacular_result$wide
      } else if (is.data.frame(vernacular_result)) {
        vern_lookup <- vernacular_result
      }
    }
    
    # Copy global files
    global_files_to_copy <- c(
      file.path(output_dir, "fragmentation_analysis.tsv"),
      file.path(output_dir, "reports", "dataset_summary_statistics.json"),
      file.path(output_dir, "reports", "dataset_summary_report.html"),
      file.path(output_dir, "reports", "species_detailed_reports.tsv")
    )
    
    log_info("Copying global summary files...", module = module)
    for (f in global_files_to_copy) {
      if (file.exists(f)) {
        file.copy(f, file.path(packages_dir, basename(f)), overwrite = TRUE)
      }
    }
    
    # Packaging loop
    packaged_species <- list()
    pb <- create_progress_bar(length(species_list))
    
    for (species_name in species_list) {
      pb$tick()
      
      species_clean <- gsub("[^A-Za-z0-9_]", "_", species_name)
      species_dir <- file.path(packages_dir, species_clean)
      if (!dir.exists(species_dir)) dir.create(species_dir, recursive = TRUE)
      
      package_files <- list()
      
      # Maps
      if (!is.null(maps_list) && species_name %in% names(maps_list)) {
        maps_subdir <- file.path(species_dir, "maps")
        dir.create(maps_subdir, showWarnings = FALSE)
        
        m_files <- maps_list[[species_name]]
        for (k in names(m_files)) {
          src <- m_files[[k]]
          if (is.character(src) && length(src) == 1 && file.exists(src)) {
            dst <- file.path(maps_subdir, basename(src))
            file.copy(src, dst, overwrite = TRUE)
            package_files[[paste0("map_", k)]] <- dst
          }
        }
      }
      
      # Narratives
      if (!is.null(narratives_list) && species_name %in% names(narratives_list)) {
        narr_subdir <- file.path(species_dir, "narratives")
        dir.create(narr_subdir, showWarnings = FALSE)
        nf <- narratives_list[[species_name]]
        
        if (is.list(nf)) {
          if ("text_file" %in% names(nf) && file.exists(nf$text_file)) {
            dst <- file.path(narr_subdir, basename(nf$text_file))
            file.copy(nf$text_file, dst, overwrite = TRUE)
            package_files$narrative_txt <- dst
          }
          if ("json_file" %in% names(nf) && file.exists(nf$json_file)) {
            dst <- file.path(narr_subdir, basename(nf$json_file))
            file.copy(nf$json_file, dst, overwrite = TRUE)
            package_files$narrative_json <- dst
          }
        }
      }
      
      # Citations
      if (!is.null(citations_list) && species_name %in% names(citations_list)) {
        cit_subdir <- file.path(species_dir, "citations")
        dir.create(cit_subdir, showWarnings = FALSE)
        cf <- citations_list[[species_name]]
        
        if (is.list(cf) && "files_created" %in% names(cf)) {
          for (src in cf$files_created) {
            if (file.exists(src)) {
              dst <- file.path(cit_subdir, basename(src))
              file.copy(src, dst, overwrite = TRUE)
              package_files[[paste0("citation_", tools::file_ext(src))]] <- dst
            }
          }
        }
      }
      
      # Data (Privacy Filtered)
      data_subdir <- file.path(species_dir, "data")
      if (!dir.exists(data_subdir)) dir.create(data_subdir)
      
      if (!is.null(occurrences_df) && "species" %in% names(occurrences_df)) {
        sp_rows <- occurrences_df[occurrences_df$species == species_name, , drop = FALSE]
        if (nrow(sp_rows) > 0) {
          cols_remove <- c("longitude", "latitude", "lat", "lon", "Y", "X",
                           "confidentiality_level", "is_sensitive")
          cols_keep <- setdiff(names(sp_rows), cols_remove)
          sp_rows_safe <- sp_rows[, cols_keep, drop = FALSE]
          
          sp_tsv <- file.path(data_subdir, paste0(species_clean, "_occurrences.tsv"))
          write_tsv(sp_rows_safe, sp_tsv)
          package_files$occurrence_data <- sp_tsv
        }
      }
      
      # Extract vernacular
      vernacular_string <- NA_character_
      if (!is.null(vern_lookup) && "species" %in% names(vern_lookup)) {
        idx <- match(species_name, vern_lookup$species)
        if (!is.na(idx) && "vernacular_string" %in% names(vern_lookup)) {
          raw_v <- vern_lookup$vernacular_string[idx]
          if (!is.na(raw_v) && nzchar(raw_v)) {
            vernacular_string <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", 
                                      "", raw_v)
          }
        }
      }
      
      # Metadata
      package_metadata <- list(
        species = species_name,
        vernacular_names = vernacular_string,
        package_id = species_clean,
        generated_date = as.character(Sys.Date()),
        provenance = list(
          source = "World of Crayfish",
          license = "CC-BY-4.0"
        ),
        methods = method_block
      )
      
      meta_file <- file.path(species_dir, "package_metadata.json")
      jsonlite::write_json(package_metadata, meta_file, pretty = TRUE, auto_unbox = TRUE)
      package_files$metadata <- meta_file
      
      # Manifest
      all_files <- unlist(package_files)
      manifest <- data.frame(
        filename = basename(all_files),
        filepath = all_files,
        size_bytes = file.size(all_files),
        stringsAsFactors = FALSE
      )
      if (include_checksums) {
        manifest$md5 <- vapply(all_files, function(f) digest::digest(f, algo = "md5"), character(1))
      }
      write_tsv(manifest, file.path(species_dir, "file_manifest.tsv"))
      
      packaged_species[[species_name]] <- list(dir = species_dir, files = nrow(manifest))
    }
    
    pb$terminate()
    
    # Summary
    pkg_summary <- list(
      total_species = length(packaged_species),
      date = as.character(Sys.Date())
    )
    jsonlite::write_json(pkg_summary, file.path(packages_dir, "packaging_summary.json"),
                         pretty = TRUE, auto_unbox = TRUE)
    
    log_info("Packaging complete. Packages at: %s", packages_dir, module = module)
    return(list(packaged = packaged_species))
  })
}