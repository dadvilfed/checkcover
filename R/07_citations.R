#### MODULE 7: CITATION MANAGEMENT (ALL SCENARIOS) ####

#' Generate citation files for all species across all scenarios
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @param script_run_time Script execution timestamp
#' @param methods Provenance methods list
#' @return List of generated citations
generate_all_citations <- function(scenario_table,
                                   result_indigenous,
                                   result_non_indigenous,
                                   output_dir = "checkover_output",
                                   script_run_time = Sys.time(),
                                   methods = NULL) {
  module <- "MODULE7_CITATIONS"
  
  with_log_section(module, {
    log_info("=== MODULE 7: CITATION MANAGEMENT (ALL SCENARIOS) ===", module = module)
    
    # Create citations directory
    citations_dir <- file.path(output_dir, "citations")
    if (!dir.exists(citations_dir)) dir.create(citations_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Helper functions
    .str_clean <- function(x) {
      x <- as.character(x)
      x <- gsub("[\u00A0\t\r\n]+", " ", x)
      x <- gsub("\\s+", " ", x)
      trimws(x)
    }
    
    .norm_doi <- function(x) {
      x <- as.character(x)
      out <- rep(NA_character_, length(x))
      mask <- !is.na(x) & nzchar(x)
      if (any(mask)) {
        d <- tolower(x[mask])
        d <- sub("^https?://(dx\\.)?doi\\.org/", "", d)
        d <- sub("^doi:\\s*", "", d)
        d <- trimws(d)
        d[!grepl("^10\\.", d)] <- NA_character_
        out[mask] <- d
      }
      out
    }
    
    .detect_doi <- function(txt) {
      txt <- as.character(txt)
      out <- rep(NA_character_, length(txt))
      mask <- !is.na(txt) & nzchar(txt)
      if (any(mask)) {
        m <- stringr::str_extract(txt[mask], "(?i)10\\.\\d{4,9}/[-._;()/:A-Z0-9]+")
        m <- tolower(m)
        out[mask] <- m
      }
      out
    }
    
    .extract_url <- function(txt) {
      txt <- as.character(txt)
      out <- rep(NA_character_, length(txt))
      mask <- !is.na(txt) & nzchar(txt)
      if (any(mask)) {
        u <- stringr::str_extract(txt[mask], "(?i)https?://[^\\s]+")
        out[mask] <- u
      }
      out
    }
    
    .title_from_citation <- function(cit) {
      if (is.na(cit) || !nzchar(cit)) return(NA_character_)
      t <- stringr::str_extract(cit, "(?<=\\)\\.|\\]\\.|\\d\\.)\\s*[^.]{6,}?\\.")
      if (is.na(t)) t <- stringr::str_extract(cit, "(?<=\\. ).+?\\.")
      t <- .str_clean(t)
      if (is.na(t) || !nzchar(t)) return(NA_character_)
      sub("\\.$", "", t)
    }
    
    .year_from_citation <- function(cit) {
      y <- stringr::str_extract(cit, "(19|20)\\d{2}")
      suppressWarnings(as.integer(y))
    }
    
    # Dataset signature
    all_data <- rbind(
      result_indigenous$clean_data[, c("species", "citation", "doi", "url")],
      result_non_indigenous$clean_data[, c("species", "citation", "doi", "url")]
    )
    
    unique_cits <- unique(.str_clean(all_data$citation[!is.na(all_data$citation) & nzchar(all_data$citation)]))
    dataset_signature <- digest::digest(paste(
      "records=", nrow(all_data),
      "species=", length(unique(all_data$species)),
      "cits=", length(unique_cits),
      sep = "|"
    ), algo = "xxhash64")
    
    log_info("Dataset signature: %s", dataset_signature, module = module)
    
    all_citations <- list()
    
    # --- PROCESS EACH SPECIES ---
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]
    
    log_info("Processing citations for %d species...", length(all_species), module = module)
    
    for (sp in all_species) {
      log_info("Processing: %s", sp, module = module)
      
      sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]
      
      # Collect data from appropriate branch(es)
      sp_data <- NULL
      
      if (scenario == 1) {
        # Indigenous only
        sp_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, 
                                                c("citation", "doi", "url")]
      } else if (scenario == 2) {
        # Non-indigenous only
        sp_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, 
                                                    c("citation", "doi", "url")]
      } else if (scenario == 3) {
        # Both - combine
        ind_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, 
                                                 c("citation", "doi", "url")]
        non_ind_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, 
                                                         c("citation", "doi", "url")]
        sp_data <- rbind(ind_data, non_ind_data)
      }
      
      if (is.null(sp_data) || nrow(sp_data) == 0) {
        log_warn("  No citation data for %s", sp, module = module)
        next
      }
      
      # Clean and normalize
      sp_data$citation <- .str_clean(sp_data$citation)
      sp_data$doi <- .str_clean(sp_data$doi)
      sp_data$url <- .str_clean(sp_data$url)
      
      # Keep rows with citation, DOI, or URL
      keep <- (!is.na(sp_data$citation) & nzchar(sp_data$citation)) |
        (!is.na(sp_data$doi) & nzchar(sp_data$doi)) |
        (!is.na(sp_data$url) & nzchar(sp_data$url))
      sp_data <- sp_data[keep, , drop = FALSE]
      
      if (nrow(sp_data) == 0) {
        log_info("  No usable citations for %s", sp, module = module)
        next
      }
      
      # Normalize DOI
      doi_id <- .norm_doi(sp_data$doi)
      doi_guess <- .detect_doi(sp_data$citation)
      fill_mask <- is.na(doi_id) | !nzchar(doi_id)
      doi_id[fill_mask] <- .norm_doi(doi_guess[fill_mask])
      
      doi_link <- ifelse(
        !is.na(doi_id) & nzchar(doi_id),
        paste0("https://doi.org/", doi_id),
        NA_character_
      )
      
      # Normalize URL
      url_clean <- .str_clean(sp_data$url)
      miss_u <- is.na(url_clean) | !nzchar(url_clean)
      url_guess <- .extract_url(sp_data$citation)
      url_clean[miss_u] <- url_guess[miss_u]
      
      # Build output dataframe
      out_df <- data.frame(
        citation_clean = .str_clean(sp_data$citation),
        doi_id = doi_id,
        doi_link = doi_link,
        url_clean = url_clean,
        stringsAsFactors = FALSE
      )
      
      # Deduplicate
      out_df <- out_df[!duplicated(out_df), , drop = FALSE]
      
      if (nrow(out_df) == 0) {
        log_info("  No citations after deduplication for %s", sp, module = module)
        next
      }
      
      # Count unpublished
      unpublished_count <- sum(grepl("(?i)^\\s*unpubl", out_df$citation_clean), na.rm = TRUE)
      
      # Format references
      out_df$apa_citation <- out_df$citation_clean
      out_df$full_reference <- ifelse(
        !is.na(out_df$doi_link),
        paste0(out_df$apa_citation, " DOI: ", out_df$doi_link),
        ifelse(
          !is.na(out_df$url_clean),
          paste0(out_df$apa_citation, " Available at: ", out_df$url_clean),
          out_df$apa_citation
        )
      )
      
      # Assign IDs
      out_df$citation_id <- sprintf("REF_%03d", seq_len(nrow(out_df)))
      
      # JSON references
      refs_list <- lapply(seq_len(nrow(out_df)), function(i) {
        list(
          id = out_df$citation_id[i],
          citation = out_df$apa_citation[i] %||% NA_character_,
          doi = out_df$doi_id[i] %||% NA_character_,
          doi_link = out_df$doi_link[i] %||% NA_character_,
          url = out_df$url_clean[i] %||% NA_character_,
          full_reference = out_df$full_reference[i] %||% NA_character_
        )
      })
      
      # BibTeX entries
      bib_entries <- vapply(seq_len(nrow(out_df)), function(i) {
        key <- paste0(gsub("[^A-Za-z0-9_]", "_", sp), "_", i)
        title <- .title_from_citation(out_df$apa_citation[i]) %||% paste("Reference for", sp)
        year <- .year_from_citation(out_df$apa_citation[i]) %||% ""
        
        lines <- c(
          paste0("@misc{", key, ","),
          paste0("  title={", title, "},"),
          if (nzchar(year)) paste0("  year={", year, "},") else NULL,
          if (!is.na(out_df$doi_id[i]) && nzchar(out_df$doi_id[i]))
            paste0("  doi={", out_df$doi_id[i], "},") else NULL,
          if (!is.na(out_df$url_clean[i]) && nzchar(out_df$url_clean[i]))
            paste0("  url={", out_df$url_clean[i], "},") else NULL,
          paste0("  note={", .str_clean(out_df$apa_citation[i]), "}"),
          "}"
        )
        paste(lines, collapse = "\n")
      }, character(1))
      
      # CSV export
      csv_df <- out_df[, c("citation_id", "citation_clean", "doi_link", "url_clean"), drop = FALSE]
      names(csv_df) <- c("ID", "Citation", "DOI", "URL")
      
      # File paths
      json_file <- file.path(citations_dir, paste0(sp_clean, "_bibliography.json"))
      bib_file <- file.path(citations_dir, paste0(sp_clean, "_bibliography.bib"))
      csv_file <- file.path(citations_dir, paste0(sp_clean, "_bibliography.csv"))
      cff_file <- file.path(citations_dir, paste0(sp_clean, "_CITATION.cff"))
      
      # JSON payload
      biblio_json <- list(
        species = sp,
        scenario = scenario,
        generated_utc = format(as.POSIXct(script_run_time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
        script_version = format(as.Date(script_run_time), "%Y-%m-%d"),
        dataset_signature = dataset_signature,
        methods = methods,
        totals = list(
          total_references = nrow(out_df),
          unpublished_records = unpublished_count
        ),
        references = refs_list
      )
      
      # Write files
      jsonlite::write_json(biblio_json, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
      writeLines(bib_entries, bib_file, useBytes = TRUE)
      utils::write.csv(csv_df, csv_file, row.names = FALSE)
      
      # CFF
      cff_txt <- glue::glue(
        'cff-version: 1.2.0
message: "If you use this species data package, please cite it as below."
type: dataset
title: "cheCkOVER species package for {sp}"
authors:
  - family-names: "cheCkOVER"
    given-names: "Framework"
version: "{format(as.Date(script_run_time), "%Y-%m-%d")}"
date-released: "{format(as.Date(script_run_time), "%Y-%m-%d")}"
abstract: "Biodiversity occurrence data and eco-narrative for {sp} processed through the cheCkOVER pipeline."
keywords:
  - biodiversity
  - occurrence-data
  - crayfish
license: CC-BY-4.0
'
      )
      writeLines(cff_txt, cff_file, useBytes = TRUE)
      
      log_info("  Generated %d references (%d formats)", nrow(out_df), 4, module = module)
      
      all_citations[[sp]] <- list(
        files = c(json_file, bib_file, csv_file, cff_file),
        total_references = nrow(out_df),
        unpublished_records = unpublished_count
      )
    }
    
    # --- OVERALL SUMMARY ---
    overall_summary <- list(
      generated_utc = format(as.POSIXct(script_run_time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      script_version = format(as.Date(script_run_time), "%Y-%m-%d"),
      dataset_signature = dataset_signature,
      methods = methods,
      total_species = length(all_citations),
      total_unique_references = length(unique_cits),
      species_with_citations = length(all_citations),
      unpublished_records_total = sum(vapply(all_citations, function(x) x$unpublished_records %||% 0, numeric(1)))
    )
    
    summary_file <- file.path(citations_dir, "citation_summary.json")
    jsonlite::write_json(overall_summary, summary_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
    
    log_info("Saved overall citation summary: %s", summary_file, module = module)
    
    # Run registry
    registry_file <- file.path(citations_dir, "_runs.json")
    reg <- if (file.exists(registry_file)) {
      try(jsonlite::read_json(registry_file, simplifyVector = TRUE), silent = TRUE)
    } else {
      NULL
    }
    if (inherits(reg, "try-error") || is.null(reg)) reg <- list(runs = list())
    
    reg$runs <- append(reg$runs, list(list(
      generated_utc = overall_summary$generated_utc,
      script_version = overall_summary$script_version,
      dataset_signature = overall_summary$dataset_signature,
      total_unique_references = overall_summary$total_unique_references
    )))
    
    jsonlite::write_json(reg, registry_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
    log_info("Updated citation run registry: %s", registry_file, module = module)
    
    log_info("Processed citations for %d species.", length(all_citations), module = module)
    
    return(list(
      citations = all_citations,
      summary = overall_summary
    ))
  })
}