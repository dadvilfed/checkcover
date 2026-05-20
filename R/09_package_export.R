#### MODULE 9: SPECIES PACKAGE EXPORT ####

#' Export individual species packages with maps, narratives, and citations
#' @param scenario_table Scenario detection table
#' @param all_maps Maps result object
#' @param all_narratives Narratives result object
#' @param all_citations Citations result object
#' @param vernacular_lookup Vernacular names lookup
#' @param output_dir Output directory
#' @return List of packaged species
export_species_packages <- function(ctx,
                                    scenario_table,
                                    all_maps,
                                    canonical_narratives,  # ✅ CHANGED
                                    all_citations,
                                    vernacular_lookup = NULL,
                                    reports_dir = NULL) {
  # ctx          : RunContext from Session 1 (carries version, paths, outcomes)
  # reports_dir  : Where the per-species report JSONs from 03c/04c live.
  #                Typically run_env$run_dir from checkcover_main.R.
  
  if (!inherits(ctx, "RunContext")) {
    stop("export_species_packages: ctx must be a RunContext (got class '",
         paste(class(ctx), collapse = "/"), "')")
  }
  validate_RunContext(ctx, expected_phase = "post_change_detection")
  if (is.null(reports_dir)) {
    stop("export_species_packages: reports_dir is required (path where 03c/04c wrote per-species report JSONs)")
  }
  
  framework_version <- ctx$framework_version
  module <- "MODULE9_PACKAGING"
  
  
  with_log_section(module, {
    log_info("=== MODULE 9: SPECIES PACKAGE EXPORT ===", module = module)
    
    # ── v3 metadata helpers (Phase 4B) ──────────────────────────────────────
    # Read a per-species report JSON from disk; NULL if absent (e.g. scenario 1
    # species has no non-indigenous report).
    .read_species_report_v3 <- function(scope, sp_clean) {
      f <- file.path(reports_dir, "reports", scope, paste0(sp_clean, ".json"))
      if (!file.exists(f)) return(NULL)
      tryCatch(jsonlite::read_json(f, simplifyVector = TRUE), error = function(e) NULL)
    }
    
    # Build the per-population metrics block per Lucian's v3 spec. Returns the
    # zero/null block when report is NULL (population type absent for species).
    .build_pop_metrics_v3 <- function(report) {
      if (is.null(report)) {
        return(list(
          AOO_km2 = 0L, EOO_km2 = 0L, records = 0L,
          basins_count = 0L, countries_count = 0L, countries = list(),
          protected_areas_count = 0L, fragmentation_clusters = 0L,
          records_post_2000_pct = NA_real_,
          records_in_protected_areas_pct = NA_real_,
          extinctions_count = 0L,
          trend_vs_previous = NA_character_
        ))
      }
      # Parse pipe-separated countries string into a JSON array
      cstr <- report$spatial_context$countries %||% ""
      carr <- if (is.character(cstr) && nzchar(cstr)) {
        parts <- unlist(strsplit(cstr, "\\s*\\|\\s*"))
        as.list(parts[nzchar(parts)])
      } else list()
      
      list(
        AOO_km2 = if (!is.null(report$metrics$aoo_km2) && is.finite(report$metrics$aoo_km2))
          as.integer(round(report$metrics$aoo_km2)) else 0L,
        EOO_km2 = if (!is.null(report$metrics$eoo_km2) && is.finite(report$metrics$eoo_km2))
          as.integer(round(report$metrics$eoo_km2)) else 0L,
        records = report$metrics$n_records %||% 0L,
        basins_count = report$counts$n_hydrobasins %||% 0L,
        countries_count = report$counts$n_countries %||% 0L,
        countries = carr,
        protected_areas_count = report$counts$n_distinct_protected_areas %||% 0L,
        fragmentation_clusters = report$fragmentation$n_clusters %||% 0L,
        records_post_2000_pct = report$temporal$pct_post_2000 %||% NA_real_,
        records_in_protected_areas_pct = report$conservation$protection_percentage %||% NA_real_,
        extinctions_count = report$counts$n_extinctions %||% 0L,
        trend_vs_previous = NA_character_  # null in JSON; v1.0 baseline has no comparison
      )
    }
    
    # Walk prior version manifests to find:
    #   - is_baseline      : TRUE iff no prior version has an entry for this species
    #   - previous_version : the source_version of the prior manifest entry
    #   - prior_aoo_*      : AOO at source for trend computation
    # Replaces the old temporal_delta lookup (Phase 5). Uses ctx + the version
    # manifests written by sparse-versioning runs.
    .read_prior_aux <- function(sp_clean) {
      empty <- list(is_baseline = TRUE, previous_version = NULL,
                    prior_aoo_indigenous = NULL, prior_aoo_non_indigenous = NULL)
      
      # Walk priors (numeric-desc) looking for an entry for this species
      for (v in ctx$prior_versions) {
        m <- read_version_manifest(ctx, v)
        if (is.null(m) || is.null(m$species)) next
        entry <- m$species[[sp_clean]]
        if (is.null(entry)) next
        
        # Found. source_version is where artifacts live; read THAT version's
        # package_metadata.json for prior AOO values.
        source_v <- entry$source_version %||% v
        prior_pkg_path <- file.path(ctx$root_output_dir, source_v, sp_clean,
                                    "package_metadata.json")
        prior_pkg <- if (file.exists(prior_pkg_path)) {
          tryCatch(jsonlite::read_json(prior_pkg_path, simplifyVector = TRUE),
                   error = function(e) NULL)
        } else NULL
        
        return(list(
          is_baseline              = FALSE,
          previous_version         = as.character(source_v),
          prior_aoo_indigenous     = prior_pkg$metrics$indigenous$AOO_km2     %||% NULL,
          prior_aoo_non_indigenous = prior_pkg$metrics$non_indigenous$AOO_km2 %||% NULL
        ))
      }
      
      empty
    }
    
    # spec: ±5% AOO change is the threshold.
    # NA when either side is missing (population absent in current or prior).
    .compute_trend_v3 <- function(curr_aoo, prev_aoo) {
      if (is.null(curr_aoo) || is.null(prev_aoo)) return(NA_character_)
      if (!is.finite(curr_aoo) || !is.finite(prev_aoo)) return(NA_character_)
      if (prev_aoo == 0) {
        if (curr_aoo == 0) return("stable")
        return("increase")
      }
      pct <- (curr_aoo - prev_aoo) / prev_aoo * 100
      if (pct >=  5) return("increase")
      if (pct <= -5) return("decrease")
      "stable"
    }
    # ────────────────────────────────────────────────────────────────────────
    
    # Per-species output now lives at <root>/<v>/<sp>/ (no run_dir/ middle layer).
    # Lucian's WoC-drop-in invariant: <root>/<v>/<sp>/<maps,narratives,citations,...>
    # Scaffolding (manifest, summary, INDEX) lives at <root>/<v>/checkover/.
    packages_dir <- ctx$current_version_dir
    if (!dir.exists(packages_dir)) dir.create(packages_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(ctx$current_scaffolding_dir)) {
      dir.create(ctx$current_scaffolding_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Active species: only reprocessed + new actually get packaged at this version.
    # Unchanged species are referenced by manifest, not duplicated on disk.
    all_species <- intersect(unique(scenario_table$species[!is.na(scenario_table$species)]),
                             ctx$active_species)
    n_unchanged <- length(ctx$all_species) - length(ctx$active_species)
    log_info("Sparse packaging: %d active species (skipping %d unchanged) at v%s",
             length(all_species), n_unchanged, framework_version, module = module)
    
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
      
      sp_clean <- make_package_id(sp)
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
      
      # Read per-population reports from disk (one or both may be present per scenario)
      ind_rep <- .read_species_report_v3("indigenous",     sp_clean)
      non_rep <- .read_species_report_v3("non_indigenous", sp_clean)
      
      # Phase 5: prior-version manifest lookup (real is_baseline / previous_version / trend)
      tax <- .read_prior_aux(sp_clean)
      
      # Build per-population metric blocks, then patch trends from the manifest data
      ind_block <- .build_pop_metrics_v3(ind_rep)
      non_block <- .build_pop_metrics_v3(non_rep)
      ind_block$trend_vs_previous <- .compute_trend_v3(ind_block$AOO_km2, tax$prior_aoo_indigenous)
      non_block$trend_vs_previous <- .compute_trend_v3(non_block$AOO_km2, tax$prior_aoo_non_indigenous)
      
      # Temporal coverage = union of years across both populations
      year_mins <- c(ind_rep$temporal$year_min, non_rep$temporal$year_min)
      year_mins <- year_mins[is.finite(year_mins)]
      year_maxs <- c(ind_rep$temporal$year_max, non_rep$temporal$year_max)
      year_maxs <- year_maxs[is.finite(year_maxs)]
      first_year <- if (length(year_mins) > 0L) as.integer(min(year_mins)) else NA_integer_
      last_year  <- if (length(year_maxs) > 0L) as.integer(max(year_maxs)) else NA_integer_
      
      # Type locality presence (sourced from indigenous report only — type
      # localities are by definition native)
      tlp <- if (!is.null(ind_rep) && !is.null(ind_rep$type_locality_present))
        isTRUE(ind_rep$type_locality_present) else FALSE
      
      # Snapshot block — add previous_version only when this is not a baseline
      snapshot_block <- list(
        version     = framework_version,
        date        = as.character(Sys.Date()),
        is_baseline = tax$is_baseline
      )
      if (!is.null(tax$previous_version)) {
        snapshot_block$previous_version <- tax$previous_version
      }
      
      package_metadata <- list(
        species = sp,
        package_id = sp_clean,
        vernacular_names = vernacular_string,
        
        snapshot = snapshot_block,
        
        scenario = scenario,
        scenario_description = scenario_desc,
        
        metrics = list(
          indigenous     = ind_block,
          non_indigenous = non_block,
          type_locality_present = tlp,
          temporal_coverage = list(
            first_year = first_year,
            last_year  = last_year
          )
        ),
        
        provenance = list(
          source = "World of Crayfish",
          license = "CC-BY-4.0",
          framework = "cheCkOVER",
          framework_version = framework_version,
          generated_date = as.character(Sys.Date())
        ),
        
        contents = list(
          maps       = list.files(file.path(species_dir, "maps")),
          narratives = list.files(file.path(species_dir, "narratives")),
          citations  = list.files(file.path(species_dir, "citations"))
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
        sprintf("- **Maps:** %d files (EOO, AOO, HydroBASINS)\n", length(package_metadata$contents$maps)),
        sprintf("- **Narratives:** %d files (text, JSON)\n", length(package_metadata$contents$narratives)),
        sprintf("- **Citations:** %d files (JSON, BibTeX, CSV, CFF)\n", length(package_metadata$contents$citations)),
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
    
    # Scaffolding artifacts live under <v>/checkover/ — invisible to the WoC frontend
    # (anything not matching the species regex ^[A-Z][a-zA-Z]+_... is ignored).
    summary_file <- file.path(ctx$current_scaffolding_dir, "packaging_summary.json")
    jsonlite::write_json(summary_data, summary_file, pretty = TRUE, auto_unbox = TRUE)
    
    # --- INDEX FILE (for easy browsing) ---
    # Lists only species physically packaged at this version. For the full cohort
    # with outcomes (including unchanged species), readers consult manifest.json.
    index_lines <- c(
      "# cheCkOVER Species Packages Index",
      "",
      sprintf("Version: %s", framework_version),
      sprintf("Generated: %s", Sys.Date()),
      sprintf("Species packaged at this version: %d", length(packaged_species)),
      sprintf("Species in full cohort: %d (see manifest.json for unchanged ones)",
              length(ctx$all_species)),
      "",
      "## Species packaged at this version",
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
      outcome <- ctx$species_outcomes[[sp]]$outcome %||% "unknown"
      outcome_badge <- sprintf("(%s)", outcome)
      
      index_lines <- c(
        index_lines,
        sprintf("- **%s** %s %s - %d files - `%s/`", 
                sp, scenario_badge, outcome_badge,
                sp_info$files_count, sp_info$metadata$package_id)
      )
    }
    
    index_file <- file.path(ctx$current_scaffolding_dir, "INDEX.md")
    writeLines(index_lines, index_file)
    
    # --- VERSION MANIFEST (this is the consumer-facing source of truth) ---
    # Schema: framework_version + totals + per-species entries (outcome,
    # source_version, fingerprint). Unchanged species get a source_version
    # pointing to a prior version; reprocessed/new point to the current version.
    
    outcome_count <- function(name) {
      sum(vapply(ctx$species_outcomes, \(o) identical(o$outcome, name), logical(1)))
    }
    
    # Determine prior_version: the most recent prior framework version we've seen.
    # NA_character_ (not NULL) so jsonlite serializes as JSON null, not {}.
    prior_v <- if (length(ctx$prior_versions) > 0L) ctx$prior_versions[1] else NA_character_
    
    # Build per-species manifest entries from ctx$species_outcomes
    species_entries <- list()
    for (sp in names(ctx$species_outcomes)) {
      o <- ctx$species_outcomes[[sp]]
      entry <- list(
        outcome              = o$outcome,
        source_version       = o$source_version,
        # NA_character_ (not NULL) for new species so jsonlite serializes as null, not {}
        prior_source_version = o$prior_source_version %||% NA_character_,
        fingerprint          = o$fingerprint,
        n_records            = o$n_records,
        change_summary       = o$change_summary
      )
      # fingerprint_at_source: same as fingerprint, EXCEPT for unchanged species
      # (where the source's fingerprint may differ from ours conceptually — but
      # since unchanged means identical fps, they coincide. Stored explicitly
      # for forward compatibility / audit.)
      entry$fingerprint_at_source <- o$fingerprint
      species_entries[[o$species_clean]] <- entry
    }
    
    version_manifest <- list(
      framework         = "cheCkOVER",
      framework_version = framework_version,
      run_id            = ctx$run_id,
      generated_date    = format(ctx$generated_date, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      prior_version     = prior_v,
      totals = list(
        total_species_in_cohort = length(ctx$all_species),
        unchanged               = outcome_count("unchanged"),
        reprocessed             = outcome_count("reprocessed"),
        new                     = outcome_count("new"),
        active_runtime_species  = length(ctx$active_species)
      ),
      species = species_entries
    )
    
    manifest_path <- file.path(ctx$current_scaffolding_dir, "manifest.json")
    jsonlite::write_json(version_manifest, manifest_path,
                         pretty = TRUE, auto_unbox = TRUE, na = "null")
    
    log_info("Packaging complete: %d species packaged, %d total files",
             length(packaged_species), summary_data$total_files, module = module)
    log_info("Saved summary to: %s", summary_file, module = module)
    log_info("Saved index to: %s", index_file, module = module)
    log_info("Saved manifest to: %s (%d total cohort entries)",
             manifest_path, length(species_entries), module = module) 
    
    return(list(
      packages = packaged_species,
      summary = summary_data
    ))
  })
}