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
    
    .norm_doi <- function(x) normalize_doi(x)

    .detect_doi <- function(txt) {
      txt <- as.character(txt)
      out <- rep(NA_character_, length(txt))
      mask <- !is.na(txt) & nzchar(txt)
      if (any(mask)) {
        m <- stringr::str_extract(txt[mask], "(?i)10\\.\\d{4,9}/[-._;()/:A-Z0-9]+")
        out[mask] <- normalize_doi(m)
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
    
    # Most frequent non-NA value, ties to the first in sort order; NA if none.
    .modal <- function(x) {
      x <- x[!is.na(x) & nzchar(x)]
      if (!length(x)) return(NA_character_)
      tab <- table(x)
      names(tab)[tab == max(tab)][1]
    }

    # Rows of one citation text that lack a DOI take the text's DOI (its most
    # frequent one on the other rows), so a work is not split by the records
    # that omit it. Rows that carry a DOI keep their own.
    .fill_doi_by_citation <- function(cit, doi) {
      miss <- is.na(doi) & !is.na(cit) & nzchar(cit)
      if (!any(miss)) return(doi)
      known <- !is.na(doi) & !is.na(cit)
      if (!any(known)) return(doi)
      by_cit <- vapply(split(doi[known], cit[known]), .modal, "")
      doi[miss] <- unname(by_cit[cit[miss]])
      doi
    }

    .year_from_citation <- function(cit) {
      y <- stringr::str_extract(cit, "(19|20)\\d{2}")
      suppressWarnings(as.integer(y))
    }
    
    # ── Dataset signature ──
    
    # citation_all carries every source that reported a consolidated occurrence
    # (see 01_ingest.R). It must be in this list or .safe_cols() drops it before
    # the expansion below can see it, and the retained citations are lost again.
    cols_needed <- c("species", "record_id", "citation", "doi", "url", "is_extinct",
                     "citation_all", "doi_all", "url_all", "sources_all")
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

      # ── Expand consolidated citations ───────────────────────────────────
      # De-duplication collapses records of one occurrence reported by several
      # publications, carrying every citation onto the survivor in the *_all
      # columns (see 01_ingest.R). Expanding them here, one row per distinct
      # source, means the bibliography counts each publication that reported an
      # occurrence rather than only whichever row happened to survive -- without
      # changing any record count, because the expansion is local to this module
      # (Lucian, 2026-09).
      #
      # Each source keeps its OWN DOI and URL (sources_all, 01_ingest.R). The
      # earlier expansion replaced only the citation and left every row with
      # the survivor's DOI, which is how Satmari et al. 2023 got Pârvulescu
      # 2019's DOI (Lucian, 2026-10). Records from before sources_all existed
      # expand citation_all as before, with no DOI or URL on the sources that
      # are not the survivor's: none is better than another work's.
      n_before <- nrow(sp_data)
      if ("sources_all" %in% names(sp_data)) {
        parts <- lapply(sp_data$sources_all, decode_sources)
        reps  <- vapply(parts, nrow, integer(1))
        sp_data <- sp_data[rep(seq_len(nrow(sp_data)), reps), , drop = FALSE]
        flat <- do.call(rbind, parts)
        sp_data$citation <- flat$citation
        sp_data$doi      <- flat$doi
        sp_data$url      <- flat$url
      } else if ("citation_all" %in% names(sp_data) &&
                 any(grepl("|", sp_data$citation_all, fixed = TRUE), na.rm = TRUE)) {
        parts <- strsplit(ifelse(is.na(sp_data$citation_all), "",
                                 sp_data$citation_all), "\\s*\\|\\s*")
        reps  <- pmax(lengths(parts), 1L)
        own   <- .str_clean(sp_data$citation)
        sp_data <- sp_data[rep(seq_len(nrow(sp_data)), reps), , drop = FALSE]
        flat <- unlist(lapply(parts, function(p) if (length(p)) p else NA_character_))
        own_rep <- own[rep(seq_along(reps), reps)]
        not_own <- !is.na(flat) & (is.na(own_rep) | .str_clean(flat) != own_rep)
        sp_data$citation <- flat
        sp_data$doi[not_own] <- NA_character_
        sp_data$url[not_own] <- NA_character_
      }
      if (nrow(sp_data) != n_before) {
        log_info("  %s: expanded %d consolidated record(s) to %d citation row(s)",
                 sp, n_before, nrow(sp_data), module = module)
      }

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
      
      # ── Normalize URL ──
      url_clean <- .str_clean(sp_data$url)
      miss_u <- is.na(url_clean) | !nzchar(url_clean)
      url_guess <- .extract_url(sp_data$citation)
      url_clean[miss_u] <- url_guess[miss_u]
      
      # ── Work frame (one row per record, BEFORE dedup) ──
      work_df <- data.frame(
        citation_clean = .str_clean(sp_data$citation),
        doi_id = doi_id, url_clean = url_clean,
        source_type = sp_data$source_type,
        stringsAsFactors = FALSE
      )
      
      # ── Split: published vs Unpublished ──
      is_unpub <- grepl("(?i)^\\s*unpubl", work_df$citation_clean)
      unpub_rows <- work_df[is_unpub,  , drop = FALSE]
      pub_rows   <- work_df[!is_unpub, , drop = FALSE]
      unpublished_count <- nrow(unpub_rows)
      
      # ── Count + dedup for published ──
      # One reference per WORK. A work is its normalised DOI when it has one,
      # else its citation text (else its URL). Keyed on the text alone, one
      # paper written in several citation styles became several references:
      # 219 DOIs sat under two or more REF ids across 1.0, five of them for
      # A. astacus' 10.2478/v10191-010-0007-6 (Lucian, 2026-10). A row whose
      # text has no DOI first takes the DOI that the same text carries on its
      # other rows, so records that omit the DOI do not split a work off.
      # Counts add up, source types are united, and the work is shown under
      # its most frequent citation text and URL.
      #
      # Every row has a key, so a count is never NA: keying on the text alone
      # dropped rows with no citation from table() and emitted count = NA (the
      # Cherax cartalacoolah crash of 2026-08 came from that path).
      if (nrow(pub_rows) > 0) {
        pub_rows$doi_id <- .fill_doi_by_citation(pub_rows$citation_clean, pub_rows$doi_id)
        has_cit <- !is.na(pub_rows$citation_clean) & nzchar(pub_rows$citation_clean)
        pub_rows$dedup_key <- ifelse(!is.na(pub_rows$doi_id), paste0("doi:", pub_rows$doi_id),
                              ifelse(has_cit, paste0("cit:", pub_rows$citation_clean),
                                     paste0("url:", pub_rows$url_clean)))
        groups <- split(seq_len(nrow(pub_rows)), pub_rows$dedup_key)
        pub_dedup <- data.frame(
          citation_clean = vapply(groups, function(ix) .modal(pub_rows$citation_clean[ix]), ""),
          doi_id         = vapply(groups, function(ix) pub_rows$doi_id[ix[1]], ""),
          url_clean      = vapply(groups, function(ix) .modal(pub_rows$url_clean[ix]), ""),
          count          = vapply(groups, length, integer(1)),
          stringsAsFactors = FALSE
        )
        pub_dedup$source_types <- lapply(groups, function(ix) sort(unique(pub_rows$source_type[ix])))
        # Text order before the count sort below, so tied works keep the
        # alphabetical order they always had.
        pub_dedup <- pub_dedup[order(pub_dedup$citation_clean, names(groups), na.last = TRUE), , drop = FALSE]
      } else {
        pub_dedup <- data.frame(
          citation_clean = character(0), doi_id = character(0),
          url_clean = character(0),
          count = integer(0), stringsAsFactors = FALSE
        )
        pub_dedup$source_types <- list()
      }
      
      # ── Consolidated Unpublished entry ──
      if (unpublished_count > 0) {
        unpub_st <- sort(unique(unpub_rows$source_type))
        unpub_entry <- data.frame(
          citation_clean = "Unpublished",
          doi_id = NA_character_,
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
      # A work known only by its DOI or URL is listed by that alone, not as
      # "NA DOI: ...".
      pub_dedup$doi_link <- ifelse(!is.na(pub_dedup$doi_id),
                                   paste0("https://doi.org/", pub_dedup$doi_id), NA_character_)
      cit_txt <- ifelse(is.na(pub_dedup$citation_clean), "", pub_dedup$citation_clean)
      pub_dedup$full_reference_APA <- trimws(paste(cit_txt, ifelse(
        !is.na(pub_dedup$doi_link),
        paste0("DOI: ", pub_dedup$doi_link),
        ifelse(!is.na(pub_dedup$url_clean), paste0("Available at: ", pub_dedup$url_clean), "")
      )))
      
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
          # The work's normalised DOI, null without one. Section 4.4 of the
          # narrative lists these.
          doi                = pub_dedup$doi_id[i],
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
      # preferred-citation: the cheCkOVER reference, identical in every package
      # and on the World of Crayfish site (CHECKOVER_REFERENCE in config.R).
      writeLines(enc2utf8(c(as.character(cff_txt), checkover_reference_cff())),
                 cff_file, useBytes = TRUE)
      
      log_info("  Generated %d references (%d formats), top ref count=%d",
               nrow(pub_dedup), 4, pub_dedup$count[1], module = module)
      
      all_citations[[sp]] <- list(
        files = c(json_file, bib_file, csv_file, cff_file),
        bibliography_json = json_file,
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