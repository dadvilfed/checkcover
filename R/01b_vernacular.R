#### MODULE 1B: VERNACULAR NAMES ####

.load_vernaculars_from_file <- function(file_path, module) {
  log_info("Reading vernacular dictionary: %s", basename(file_path), module = module)
  
  file_ext <- tolower(tools::file_ext(file_path))
  
  if (file_ext == "tsv") {
    # TSV format (preferred)
    log_info("Reading TSV vernacular dictionary...", module = module)
    vern_raw <- read_tsv(file_path)
    
    # Expected format: Species | Vernacular_Names (multiple columns with lang codes)
    # Example: "idle crayfish(eng)" "racul bihorean(ron)"
    
    if (!"Species" %in% names(vern_raw)) {
      log_error("TSV must have 'Species' column", module = module)
      stop("Invalid TSV format. Expected column: Species")
    }
    
    # Get all vernacular columns (everything except Species)
    vern_cols <- setdiff(names(vern_raw), "Species")
    
    vernacular_list <- list()
    
    for (i in seq_len(nrow(vern_raw))) {
      sp_name <- stringr::str_trim(vern_raw$Species[i])
      if (is.na(sp_name) || !nzchar(sp_name)) next
      
      # Collect all non-empty vernacular names from this row
      vern_values <- character()
      for (col in vern_cols) {
        val <- vern_raw[[col]][i]
        if (!is.na(val) && nzchar(stringr::str_trim(val))) {
          # Clean the value (already has language code in parentheses)
          vern_values <- c(vern_values, stringr::str_trim(val))
        }
      }
      
      if (length(vern_values) > 0) {
        # Join all vernacular names with " | " separator
        vernacular_list[[sp_name]] <- paste(vern_values, collapse = " | ")
      }
    }
    
    if (length(vernacular_list) == 0) {
      log_warn("No vernacular names extracted from TSV", module = module)
      return(data.frame(
        species = character(),
        vernacular_string = character(),
        source = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    return(data.frame(
      species = names(vernacular_list),
      vernacular_string = unlist(vernacular_list),
      source = "TSV_Manual",
      stringsAsFactors = FALSE
    ))
    
  } else if (file_ext == "docx") {
    # DOCX format (legacy support)
    log_info("Reading DOCX vernacular dictionary...", module = module)
    
    if (!requireNamespace("readtext", quietly = TRUE)) {
      stop("Package 'readtext' required. Install with install.packages('readtext')")
    }
    
    doc_text <- readtext::readtext(file_path)$text
    lines <- stringr::str_split(doc_text, "\n")[[1]]
    lines <- lines[nzchar(lines)]
    
    vernacular_list <- list()
    
    for (line in lines) {
      match <- stringr::str_match(line, "^\\s*([^,]+),\\s*[\"']?(.*?)[\"']?\\s*$")
      
      if (!is.na(match[1,1])) {
        sp_name <- stringr::str_trim(match[1, 2])
        v_str <- stringr::str_trim(match[1, 3])
        v_str <- gsub("^\"|\"$", "", v_str)
        
        if (nzchar(sp_name) && nzchar(v_str)) {
          vernacular_list[[sp_name]] <- v_str
        }
      }
    }
    
    return(data.frame(
      species = names(vernacular_list),
      vernacular_string = unlist(vernacular_list),
      source = "DOCX_Manual",
      stringsAsFactors = FALSE
    ))
    
  } else {
    log_error("Unsupported vernacular file format: %s. Use .tsv or .docx", 
              file_ext, module = module)
    stop("Unsupported vernacular file format: ", file_ext)
  }
}

generate_vernacular_file <- function(result, output_dir = "checkover_output",
                                     source = c("itis", "file"), docx_path = NULL) {
  module <- "MODULE1B_VERNACULAR"
  source <- match.arg(source)
  
  with_log_section(module, {
    log_info("=== MODULE 1B: GENERATING VERNACULAR LOOKUP ===", module = module)
    
    species_list <- unique(result$clean_data$species)
    log_info("Targeting %d species.", length(species_list), module = module)
    
    vernacular_df <- NULL
    
    if (source == "file") {
      if (is.null(docx_path) || !file.exists(docx_path)) {
        stop("Vernacular file path is invalid")
      }
      
      full_df <- .load_vernaculars_from_file(docx_path, module)
      vernacular_df <- full_df %>% dplyr::filter(species %in% species_list)
      log_info("Matched %d species from file.", nrow(vernacular_df), module = module)
      
    } else {
      # ITIS mode
      if (!requireNamespace("ritis", quietly = TRUE)) {
        stop("ritis package required")
      }
      log_info("Querying ITIS API...", module = module)
      
      found_names <- list()
      pb <- create_progress_bar(length(species_list))
      
      for (sp in species_list) {
        pb$tick()
        tryCatch({
          search_res <- ritis::search_scientific(sp)
          if (!is.null(search_res) && nrow(search_res) > 0) {
            tsn <- search_res$tsn[1]
            cnames <- ritis::common_names(tsn)
            if (!is.null(cnames) && nrow(cnames) > 0) {
              eng <- cnames[cnames$language == "English", ]
              if (nrow(eng) > 0) {
                v_str <- paste(unique(eng$commonName), collapse = " | ")
                found_names[[sp]] <- v_str
              }
            }
          }
        }, error = function(e) {})
        Sys.sleep(0.2)
      }
      
      pb$terminate()
      
      if (length(found_names) > 0) {
        vernacular_df <- data.frame(
          species = names(found_names),
          vernacular_string = unlist(found_names),
          source = "ITIS_API",
          stringsAsFactors = FALSE
        )
        log_info("Found vernaculars for %d species via ITIS.", 
                 nrow(vernacular_df), module = module)
      }
    }
    
    if (is.null(vernacular_df)) {
      vernacular_df <- data.frame(
        species = character(),
        vernacular_string = character(),
        source = character()
      )
    }
    
    # Add missing species as NA
    missing_sp <- setdiff(species_list, vernacular_df$species)
    if (length(missing_sp) > 0) {
      missing_df <- data.frame(
        species = missing_sp,
        vernacular_string = NA,
        source = "Not Found"
      )
      vernacular_df <- rbind(vernacular_df, missing_df)
    }
    
    out_file <- file.path(output_dir, "vernacular_names_lookup.tsv")
    write_tsv(vernacular_df, out_file)
    log_info("Saved vernacular lookup to: %s", out_file, module = module)
    
    return(vernacular_df)
  })
}