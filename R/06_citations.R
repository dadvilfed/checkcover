#### MODULE 6: CITATION MANAGER ####

# DOI helpers
.norm_doi_id <- function(x) {
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
    m[is.na(m)] <- NA_character_
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
    u[is.na(u)] <- NA_character_
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

# Load methods from summary
.load_methods_from_summary <- function(output_dir) {
  p <- file.path(output_dir, "reports", "dataset_summary_statistics.json")
  if (file.exists(p)) {
    js <- try(jsonlite::read_json(p, simplifyVector = TRUE), silent = TRUE)
    if (!inherits(js, "try-error") && !is.null(js$methods)) return(js$methods)
  }
  NULL
}

citation_manager <- function(result, output_dir = "checkover_output",
                             species_list = NULL, methods = NULL,
                             script_run_time = Sys.time()) {
  module <- "MODULE6_CITATIONS"
  
  with_log_section(module, {
    log_info("=== MODULE 6: CITATION MANAGER ===", module = module)
    
    stopifnot(is.list(result), "clean_data" %in% names(result))
    cd <- result$clean_data
    if (!nrow(cd)) stop("clean_data is empty.")
    
    # Ensure required columns
    need <- c("species", "citation", "doi", "url")
    for (nm in need) {
      if (!(nm %in% names(cd))) {
        cd[[nm]] <- NA_character_
        log_debug("Added missing column '%s'.", nm, module = module)
      }
    }
    
    if (is.null(species_list)) {
      species_list <- unique(cd$species)
      species_list <- species_list[!is.na(species_list)]
      log_info("Using all %d species in clean_data.", length(species_list), module = module)
    } else {
      log_info("Using user-provided species_list with %d species.", 
               length(species_list), module = module)
    }
    
    citations_dir <- file.path(output_dir, "citations")
    if (!dir.exists(citations_dir)) {
      dir.create(citations_dir, recursive = TRUE, showWarnings = FALSE)
      log_info("Created citations directory: %s", citations_dir, module = module)
    }
    
    # Provenance methods
    if (is.null(methods)) {
      log_debug("Trying to load methods from dataset summary.", module = module)
      methods <- .load_methods_from_summary(output_dir)
    }
    if (is.null(methods)) {
      methods <- list(
        taxonomy = NA_character_,
        vernacular = NA_character_,
        teow = NA_character_,
        feow = NA_character_,
        wdpa = NA_character_,
        hydrobasins = NA_character_
      )
      log_debug("No methods info found; using NA placeholders.", module = module)
    }
    
    # Dataset signature
    unique_cits <- unique(.str_clean(cd$citation[!is.na(cd$citation) & nzchar(cd$citation)]))
    sig_payload <- paste("records=", nrow(cd), "species=", length(unique(cd$species)),
                         "unique_citations=", length(unique_cits),
                         "cit_hash=", .safe_digest(paste(sort(unique_cits), collapse = "||")),
                         sep = "|")
    dataset_signature <- .safe_digest(sig_payload)
    log_info("Dataset signature: %s", dataset_signature, module = module)
    
    species_citations <- vector("list", length(species_list))
    names(species_citations) <- species_list
    
    pb <- create_progress_bar(length(species_list))
    
    for (sp in species_list) {
      pb$tick()
      
      sp_df <- cd[cd$species == sp, c("citation", "doi", "url"), drop = FALSE]
      if (!nrow(sp_df)) {
        log_warn("No records for species %s. Skipping.", sp, module = module)
        next
      }
      
      sp_df$citation <- .str_clean(sp_df$citation)
      sp_df$doi <- .str_clean(sp_df$doi)
      sp_df$url <- .str_clean(sp_df$url)
      
      keep <- (!is.na(sp_df$citation) & nzchar(sp_df$citation)) |
        (!is.na(sp_df$doi) & nzchar(sp_df$doi)) |
        (!is.na(sp_df$url) & nzchar(sp_df$url))
      sp_df <- sp_df[keep, , drop = FALSE]
      
      if (!nrow(sp_df)) {
        log_debug("No usable citations for species %s.", sp, module = module)
        next
      }
      
      # Normalize DOI and URL
      doi_id <- .norm_doi_id(sp_df$doi)
      doi_guess <- .detect_doi(sp_df$citation)
      fill_mask <- is.na(doi_id) | !nzchar(doi_id)
      doi_id[fill_mask] <- .norm_doi_id(doi_guess[fill_mask])
      
      doi_link <- ifelse(!is.na(doi_id) & nzchar(doi_id),
                         paste0("https://doi.org/", doi_id),
                         NA_character_)
      
      url_clean <- .str_clean(sp_df$url)
      miss_u <- is.na(url_clean) | !nzchar(url_clean)
      url_guess <- .extract_url(sp_df$citation)
      url_clean[miss_u] <- url_guess[miss_u]
      
      # Deduplicate
      out_df <- data.frame(
        citation_clean = .str_clean(sp_df$citation),
        doi_id = doi_id,
        doi_link = doi_link,
        url_clean = url_clean,
        stringsAsFactors = FALSE
      )
      out_df <- out_df[!duplicated(out_df), , drop = FALSE]
      
      if (!nrow(out_df)) {
        log_debug("After deduplication, no citations remain for %s.", sp, module = module)
        next
      }
      
      unpublished_count <- sum(grepl("(?i)^\\s*unpubl", out_df$citation_clean), na.rm = TRUE)
      
      # Format reference
      out_df$apa_citation <- out_df$citation_clean
      out_df$full_reference <- ifelse(
        !is.na(out_df$doi_link),
        paste0(out_df$apa_citation, " DOI: ", out_df$doi_link),
        ifelse(!is.na(out_df$url_clean),
               paste0(out_df$apa_citation, " Available at: ", out_df$url_clean),
               out_df$apa_citation)
      )
      
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
      
      # TSV export
      tsv_df <- out_df[, c("citation_id", "citation_clean", "doi_link", "url_clean"), drop = FALSE]
      names(tsv_df) <- c("ID", "Citation", "DOI", "URL")
      
      # File paths
      sp_file_stub <- gsub("[^A-Za-z0-9_]", "_", sp)
      json_file <- file.path(citations_dir, paste0(sp_file_stub, "_bibliography.json"))
      bib_file <- file.path(citations_dir, paste0(sp_file_stub, "_bibliography.bib"))
      tsv_file <- file.path(citations_dir, paste0(sp_file_stub, "_bibliography.tsv"))
      cff_file <- file.path(citations_dir, paste0(sp_file_stub, "_CITATION.cff"))
      
      # JSON payload
      biblio_json <- list(
        species = sp,
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
      
      jsonlite::write_json(biblio_json, json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
      writeLines(bib_entries, bib_file, useBytes = TRUE)
      write_tsv(tsv_df, tsv_file)
      
      # Minimal CFF
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
      
      species_citations[[sp]] <- list(
        bibliography_json = biblio_json,
        files_created = c(json_file, bib_file, tsv_file, cff_file),
        total_references = nrow(out_df),
        unpublished_records = unpublished_count
      )
    }
    
    pb$terminate()
    
    # Overall summary
    all_unique_refs <- unique(.str_clean(cd$citation[!is.na(cd$citation) & nzchar(cd$citation)]))
    overall_summary <- list(
      generated_utc = format(as.POSIXct(script_run_time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      script_version = format(as.Date(script_run_time), "%Y-%m-%d"),
      dataset_signature = dataset_signature,
      methods = methods,
      total_species = length(Filter(Negate(is.null), species_citations)),
      total_unique_references = length(all_unique_refs),
      species_with_citations = sum(vapply(species_citations, function(x) !is.null(x), logical(1))),
      unpublished_records_total = sum(vapply(species_citations, 
                                             function(x) x$unpublished_records %||% 0, numeric(1)))
    )
    
    summary_file <- file.path(citations_dir, "citation_summary.json")
    jsonlite::write_json(overall_summary, summary_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
    log_info("Wrote overall citation summary: %s", summary_file, module = module)
    
    # Registry
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
    
    n_sp <- length(Filter(Negate(is.null), species_citations))
    log_info("Processed citations for %d species (total unique refs: %d).",
             n_sp, overall_summary$total_unique_references, module = module)
    
    species_citations
  })
}