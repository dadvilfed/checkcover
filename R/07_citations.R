#' Generate citation files for all species across all scenarios
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param output_dir Output directory
#' @param script_run_time Script execution timestamp
#' @param methods Provenance methods list
#' @param temporal_record_tags Optional data.frame with columns `record_id` and
#'   `source_type` ("baseline"|"new_presence"|"extinction_evidence"). When NULL,
#'   all references are tagged "baseline".
#' @return List of generated citations
generate_all_citations <- function(scenario_table,
                                   result_indigenous,
                                   result_non_indigenous,
                                   output_dir = "checkover_output",
                                   script_run_time = Sys.time(),
                                   methods = NULL,
                                   temporal_record_tags = NULL) {
  module <- "MODULE7_CITATIONS"
  
  with_log_section(module, {
    log_info("=== MODULE 7: CITATION MANAGEMENT (ALL SCENARIOS) ===", module = module)
    
    # Create citations directory
    citations_dir <- file.path(output_dir, "citations")
    if (!dir.exists(citations_dir)) dir.create(citations_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ── Helper functions (unchanged from original) ──
    
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
    
    # ── Dataset signature ──
    
    cols_needed <- c("species", "record_id", "citation", "doi", "url", "is_extinct")
    .safe_cols <- function(df, cols) {
      present <- intersect(cols, names(df))
      df[, present, drop = FALSE]
    }
    
    all_data <- dplyr::bind_rows(
      .safe_cols(result_indigenous$clean_data, cols_needed),
      .safe_cols(result_non_indigenous$clean_data, cols_needed)
    )
    
    if (!"record_id"  %in% names(all_data)) all_data$record_id  <- NA_character_
    if (!"is_extinct" %in% names(all_data)) all_data$is_extinct <- FALSE
    
    unique_cits <- unique(.str_clean(
      all_data$citation[!is.na(all_data$citation) & nzchar(all_data$citation)]
    ))
    dataset_signature <- digest::digest(paste(
      "records=", nrow(all_data),
      "species=", length(unique(all_data$species)),
      "cits=", length(unique_cits),
      sep = "|"
    ), algo = "xxhash64")
    
    log_info("Dataset signature: %s", dataset_signature, module = module)
    
    # ── Source-type lookup ──
    
    .get_source_type <- function(record_id_val, is_extinct_val) {
      if (!is.null(temporal_record_tags) &&
          !is.null(record_id_val) && !is.na(record_id_val) &&
          record_id_val %in% temporal_record_tags$record_id) {
        return(temporal_record_tags$source_type[
          temporal_record_tags$record_id == record_id_val][1])
      }
      if (!is.na(is_extinct_val) && isTRUE(is_extinct_val)) return("extinction_evidence")
      "baseline"
    }
    
    all_citations <- list()
    
    # ── PROCESS EACH SPECIES ──
    
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]
    
    log_info("Processing citations for %d species...", length(all_species), module = module)
    
    for (sp in all_species) {
      log_info("Processing: %s", sp, module = module)
      
      sp_clean <- make_package_id(sp)
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]
      
      # Collect data
      sp_data <- NULL
      if (scenario == 1) {
        sp_data <- .safe_cols(
          result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ],
          cols_needed)
      } else if (scenario == 2) {
        sp_data <- .safe_cols(
          result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ],
          cols_needed)
      } else if (scenario == 3) {
        sp_data <- dplyr::bind_rows(
          .safe_cols(result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ],
                     cols_needed),
          .safe_cols(result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ],
                     cols_needed))
      }
      
      if (is.null(sp_data) || nrow(sp_data) == 0) {
        log_warn("  No citation data for %s", sp, module = module)
        next
      }
      
      if (!"record_id"  %in% names(sp_data)) sp_data$record_id  <- NA_character_
      if (!"is_extinct" %in% names(sp_data)) sp_data$is_extinct <- FALSE
      
      sp_data$citation <- .str_clean(sp_data$citation)
      sp_data$doi      <- .str_clean(sp_data$doi)
      sp_data$url      <- .str_clean(sp_data$url)
      
      keep <- (!is.na(sp_data$citation) & nzchar(sp_data$citation)) |
        (!is.na(sp_data$doi) & nzchar(sp_data$doi)) |
        (!is.na(sp_data$url) & nzchar(sp_data$url))
      sp_data <- sp_data[keep, , drop = FALSE]
      
      if (nrow(sp_data) == 0) {
        log_info("  No usable citations for %s", sp, module = module)
        next
      }
      
      # ── Source type per record ──
      sp_data$source_type <- mapply(
        .get_source_type, sp_data$record_id, sp_data$is_extinct,
        USE.NAMES = FALSE
      )
      
      # ── Normalize DOI ──
      doi_id <- .norm_doi(sp_data$doi)
      doi_guess <- .detect_doi(sp_data$citation)
      fill_mask <- is.na(doi_id) | !nzchar(doi_id)
      doi_id[fill_mask] <- .norm_doi(doi_guess[fill_mask])
      doi_link <- ifelse(!is.na(doi_id) & nzchar(doi_id),
                         paste0("https://doi.org/", doi_id), NA_character_)
      
      # ── Normalize URL ──
      url_clean <- .str_clean(sp_data$url)
      miss_u <- is.na(url_clean) | !nzchar(url_clean)
      url_guess <- .extract_url(sp_data$citation)
      url_clean[miss_u] <- url_guess[miss_u]
      
      # ── Work frame (one row per record, BEFORE dedup) ──
      work_df <- data.frame(
        citation_clean = .str_clean(sp_data$citation),
        doi_id = doi_id, doi_link = doi_link, url_clean = url_clean,
        source_type = sp_data$source_type,
        stringsAsFactors = FALSE
      )
      
      # ── Split: published vs Unpublished ──
      is_unpub <- grepl("(?i)^\\s*unpubl", work_df$citation_clean)
      unpub_rows <- work_df[is_unpub,  , drop = FALSE]
      pub_rows   <- work_df[!is_unpub, , drop = FALSE]
      unpublished_count <- nrow(unpub_rows)
      
      # ── Count + dedup for published ──
      if (nrow(pub_rows) > 0) {
        pub_rows$dedup_key <- pub_rows$citation_clean
        
        count_tbl <- as.data.frame(table(pub_rows$dedup_key), stringsAsFactors = FALSE)
        names(count_tbl) <- c("dedup_key", "count")
        
        source_types_map <- tapply(
          pub_rows$source_type, pub_rows$dedup_key,
          function(x) sort(unique(x)), simplify = FALSE
        )
        
        pub_dedup <- pub_rows[!duplicated(pub_rows$dedup_key), , drop = FALSE]
        pub_dedup <- merge(pub_dedup, count_tbl, by = "dedup_key", all.x = TRUE)
        pub_dedup$source_types <- source_types_map[pub_dedup$dedup_key]
        pub_dedup$dedup_key    <- NULL
        pub_dedup$source_type  <- NULL
      } else {
        pub_dedup <- data.frame(
          citation_clean = character(0), doi_id = character(0),
          doi_link = character(0), url_clean = character(0),
          count = integer(0), stringsAsFactors = FALSE
        )
        pub_dedup$source_types <- list()
      }
      
      # ── Consolidated Unpublished entry ──
      if (unpublished_count > 0) {
        unpub_st <- sort(unique(unpub_rows$source_type))
        unpub_entry <- data.frame(
          citation_clean = "Unpublished",
          doi_id = NA_character_, doi_link = NA_character_,
          url_clean = NA_character_, count = unpublished_count,
          stringsAsFactors = FALSE
        )
        unpub_entry$source_types <- list(unpub_st)
        pub_dedup <- dplyr::bind_rows(pub_dedup, unpub_entry)
      }
      
      # ── Sort descending by count ──
      pub_dedup <- pub_dedup[order(-pub_dedup$count), , drop = FALSE]
      rownames(pub_dedup) <- NULL
      
      if (nrow(pub_dedup) == 0) {
        log_info("  No citations after processing for %s", sp, module = module)
        next
      }
      
      # ── APA format with DOI ──
      pub_dedup$full_reference_APA <- ifelse(
        !is.na(pub_dedup$doi_link),
        paste0(pub_dedup$citation_clean, " DOI: ", pub_dedup$doi_link),
        ifelse(
          !is.na(pub_dedup$url_clean),
          paste0(pub_dedup$citation_clean, " Available at: ", pub_dedup$url_clean),
          pub_dedup$citation_clean
        )
      )
      
      # ── IDs (after sort: REF_001 = most cited) ──
      pub_dedup$citation_id <- sprintf("REF_%03d", seq_len(nrow(pub_dedup)))
      
      # ── JSON references ──
      refs_list <- lapply(seq_len(nrow(pub_dedup)), function(i) {
        st <- pub_dedup$source_types[[i]]
        if (is.null(st) || length(st) == 0) st <- "baseline"
        list(
          id                 = pub_dedup$citation_id[i],
          count              = pub_dedup$count[i],
          full_reference_APA = pub_dedup$full_reference_APA[i],
          source_types       = as.list(st)
        )
      })
      
      # ── BibTeX ──
      bib_entries <- vapply(seq_len(nrow(pub_dedup)), function(i) {
        key   <- paste0(make_package_id(sp), "_", i)
        title <- .title_from_citation(pub_dedup$citation_clean[i]) %||%
          paste("Reference for", sp)
        year  <- .year_from_citation(pub_dedup$citation_clean[i]) %||% ""
        lines <- c(
          paste0("@misc{", key, ","),
          paste0("  title={", title, "},"),
          if (nzchar(year)) paste0("  year={", year, "},") else NULL,
          if (!is.na(pub_dedup$doi_id[i]) && nzchar(pub_dedup$doi_id[i]))
            paste0("  doi={", pub_dedup$doi_id[i], "},") else NULL,
          if (!is.na(pub_dedup$url_clean[i]) && nzchar(pub_dedup$url_clean[i]))
            paste0("  url={", pub_dedup$url_clean[i], "},") else NULL,
          paste0("  note={", .str_clean(pub_dedup$citation_clean[i]), "}"),
          "}"
        )
        paste(lines, collapse = "\n")
      }, character(1))
      
      # ── CSV ──
      csv_df <- data.frame(
        ID = pub_dedup$citation_id,
        Count = pub_dedup$count,
        Citation_APA = pub_dedup$full_reference_APA,
        DOI = pub_dedup$doi_link,
        URL = pub_dedup$url_clean,
        Source_Types = vapply(pub_dedup$source_types,
                              function(x) paste(x, collapse = "; "), character(1)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      
      # ── File paths ──
      json_file <- file.path(citations_dir, paste0(sp_clean, "_bibliography.json"))
      bib_file  <- file.path(citations_dir, paste0(sp_clean, "_bibliography.bib"))
      csv_file  <- file.path(citations_dir, paste0(sp_clean, "_bibliography.csv"))
      cff_file  <- file.path(citations_dir, paste0(sp_clean, "_CITATION.cff"))
      
      # ── JSON payload ──
      biblio_json <- list(
        species = sp, scenario = scenario,
        generated_utc = format(as.POSIXct(script_run_time, tz = "UTC"),
                               "%Y-%m-%dT%H:%M:%SZ"),
        script_version = format(as.Date(script_run_time), "%Y-%m-%d"),
        dataset_signature = dataset_signature,
        methods = methods,
        totals = list(
          total_references = nrow(pub_dedup),
          total_records_with_citations = nrow(work_df),
          unpublished_records = unpublished_count,
          source_type_summary = list(
            baseline = sum(vapply(pub_dedup$source_types,
                                  function(x) "baseline" %in% x, logical(1))),
            new_presence = sum(vapply(pub_dedup$source_types,
                                      function(x) "new_presence" %in% x, logical(1))),
            extinction_evidence = sum(vapply(pub_dedup$source_types,
                                             function(x) "extinction_evidence" %in% x, logical(1)))
          )
        ),
        references = refs_list
      )
      
      # ── Write files ──
      jsonlite::write_json(biblio_json, json_file, pretty = TRUE,
                           auto_unbox = TRUE, na = "null")
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
      
      log_info("  Generated %d references (%d formats), top ref count=%d",
               nrow(pub_dedup), 4, pub_dedup$count[1], module = module)
      
      all_citations[[sp]] <- list(
        files = c(json_file, bib_file, csv_file, cff_file),
        total_references = nrow(pub_dedup),
        unpublished_records = unpublished_count
      )
    }
    
    # ── OVERALL SUMMARY ──
    overall_summary <- list(
      generated_utc = format(as.POSIXct(script_run_time, tz = "UTC"),
                             "%Y-%m-%dT%H:%M:%SZ"),
      script_version = format(as.Date(script_run_time), "%Y-%m-%d"),
      dataset_signature = dataset_signature,
      methods = methods,
      total_species = length(all_citations),
      total_unique_references = length(unique_cits),
      species_with_citations = length(all_citations),
      unpublished_records_total = sum(vapply(
        all_citations, function(x) x$unpublished_records %||% 0, numeric(1)
      ))
    )
    
    summary_file <- file.path(citations_dir, "citation_summary.json")
    jsonlite::write_json(overall_summary, summary_file, pretty = TRUE,
                         auto_unbox = TRUE, na = "null")
    log_info("Saved overall citation summary: %s", summary_file, module = module)
    
    # Run registry
    registry_file <- file.path(citations_dir, "_runs.json")
    reg <- if (file.exists(registry_file)) {
      try(jsonlite::read_json(registry_file, simplifyVector = TRUE), silent = TRUE)
    } else NULL
    if (inherits(reg, "try-error") || is.null(reg)) reg <- list(runs = list())
    reg$runs <- append(reg$runs, list(list(
      generated_utc = overall_summary$generated_utc,
      script_version = overall_summary$script_version,
      dataset_signature = overall_summary$dataset_signature,
      total_unique_references = overall_summary$total_unique_references
    )))
    jsonlite::write_json(reg, registry_file, pretty = TRUE, auto_unbox = TRUE,
                         na = "null")
    log_info("Updated citation run registry: %s", registry_file, module = module)
    
    log_info("Processed citations for %d species.", length(all_citations),
             module = module)
    
    return(list(citations = all_citations, summary = overall_summary))
  })
}