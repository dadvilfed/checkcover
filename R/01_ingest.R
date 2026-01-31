#### MODULE 1: INGEST & CLEAN ####

#' Load World of Crayfish Excel file
load_woc_data <- function(file_path, sheet_name = NULL, module = "MODULE1_INGEST") {
  if (!file.exists(file_path)) {
    log_error("File not found: %s", file_path, module = module)
    stop("File not found: ", file_path)
  }
  
  log_info("Loading World of Crayfish data from: %s", file_path, module = module)
  
  # Detect file format
  file_ext <- tolower(tools::file_ext(file_path))
  
  if (file_ext %in% c("xlsx", "xls")) {
    # Excel format (legacy support)
    log_info("Reading Excel file...", module = module)
    if (is.null(sheet_name)) {
      woc_raw <- openxlsx::read.xlsx(file_path)
      log_debug("Read first sheet by default.", module = module)
    } else {
      woc_raw <- openxlsx::read.xlsx(file_path, sheet = sheet_name)
      log_debug("Read specified sheet: %s.", sheet_name, module = module)
    }
  } else if (file_ext == "tsv") {
    # TSV format (preferred)
    log_info("Reading TSV file...", module = module)
    woc_raw <- read.delim(file_path, sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE, quote = "", 
                          na.strings = c("", "NA", "N/A"))
                          #fileEncoding = "UTF-8")
    log_debug("Read TSV with tab delimiter.", module = module)
  } else if (file_ext == "csv") {
    # CSV format (fallback)
    log_info("Reading CSV file...", module = module)
    woc_raw <- read.csv(file_path, stringsAsFactors = FALSE, 
                        na.strings = c("", "NA", "N/A"))
    log_debug("Read CSV with comma delimiter.", module = module)
  } else {
    log_error("Unsupported file format: %s. Use .tsv, .csv, or .xlsx", file_ext, module = module)
    stop("Unsupported file format: ", file_ext)
  }
  
  # Fix any UTF-8 encoding issues
  woc_raw <- fix_utf8_encoding(woc_raw, module = module)
  
  log_info("Loaded %d raw records and %d columns.", nrow(woc_raw), ncol(woc_raw), module = module)
  woc_raw
}

#' Helper function to fix UTF-8 encoding issues
fix_utf8_encoding <- function(data, module = "MODULE1_INGEST") {
  log_info("Checking and fixing UTF-8 encoding in text columns...", module = module)
  
  # Get character columns
  char_cols <- sapply(data, is.character)
  
  if (sum(char_cols) == 0) {
    log_debug("No character columns found.", module = module)
    return(data)
  }
  
  fixed_count <- 0
  
  # Fix encoding in each character column
  for (col_name in names(data)[char_cols]) {
    original_col <- data[[col_name]]
    
    # Strategy: Try to ensure valid UTF-8 by converting from Latin1 if needed
    # This handles Windows-1252 and other common encodings
    fixed_col <- tryCatch({
      # First check if already valid UTF-8
      test <- stringi::stri_enc_mark(original_col)
      if (all(test %in% c("ASCII", "UTF-8", "unknown", NA))) {
        # Already good or ASCII
        original_col
      } else {
        # Convert from Latin1 to UTF-8 (this handles most Windows encodings)
        stringi::stri_encode(original_col, from = "latin1", to = "UTF-8")
      }
    }, error = function(e) {
      # If encoding detection fails, try direct UTF-8 validation
      tryCatch({
        # Use stringi to fix invalid sequences
        stringi::stri_enc_toutf8(original_col, is_unknown_8bit = TRUE)
      }, error = function(e2) {
        log_warn("Could not fix encoding in column '%s': %s", 
                 col_name, e2$message, module = module)
        original_col
      })
    })
    
    # Check if conversion produced NAs where there weren't any before
    new_nas <- sum(is.na(fixed_col)) - sum(is.na(original_col))
    if (new_nas > 0) {
      log_warn("Column '%s': encoding fix created %d new NAs, keeping original", 
               col_name, new_nas, module = module)
      data[[col_name]] <- original_col
    } else {
      data[[col_name]] <- fixed_col
      if (!identical(original_col, fixed_col)) {
        fixed_count <- fixed_count + 1
      }
    }
  }
  
  if (fixed_count > 0) {
    log_info("Fixed UTF-8 encoding issues in %d character columns.", fixed_count, module = module)
  } else {
    log_info("No encoding issues detected in character columns.", module = module)
  }
  
  return(data)
}
map_woc_to_checkover <- function(woc_data, module = "MODULE1_INGEST") {
  log_info(
    "Mapping WoC data to cheCkOVER format (input records: %d, columns: %d).",
    nrow(woc_data),
    ncol(woc_data),
    module = module
  )

  # Drop duplicated columns
  dup_cols <- colnames(woc_data)[duplicated(colnames(woc_data))]
  if (length(dup_cols) > 0L) {
    log_warn(
      "Detected duplicated column names: %s. Keeping first occurrence of each.",
      paste(unique(dup_cols), collapse = ", "),
      module = module
    )
  }
  woc_data <- woc_data[, !duplicated(colnames(woc_data)), drop = FALSE]

  # Column mapping from WoC to cheCkOVER standard
  checkover_data <- woc_data %>%
    mutate(
      # Core identification
      record_id = WoCID,
      species   = str_trim(Crayfish_scientific_name),

      # Coordinates (Lat/Long to latitude/longitude)
      # Handle both comma and period decimal separators
      longitude = as.numeric(gsub(",", ".", as.character(Long), fixed = TRUE)),
      latitude  = as.numeric(gsub(",", ".", as.character(Lat), fixed = TRUE)),
      # Temporal data
      year      = as.numeric(Year_of_record),

      # Status mapping - Using Occurrence_origin for native/alien classification
      status = case_when(
        str_to_lower(str_trim(Occurrence_origin)) %in% c("native", "indigenous", "endemic") ~ "native",
        str_to_lower(str_trim(Occurrence_origin)) == "type locality" ~ "native",
        str_to_lower(str_trim(Occurrence_origin)) %in% c("alien", "invasive", "introduced", "non-native") ~ "alien",
        TRUE ~ "unknown"
      ),

      # NEW: Population status (indigenous vs non-indigenous)
      population_status = str_trim(Population_status),  # ✅ NEW COLUMN

      # Track type locality separately for narratives
      is_type_locality = str_to_lower(str_trim(Occurrence_origin)) == "type locality",

      # Extinction logic
      is_extinct = !is.na(Claim_extinction) & str_to_lower(str_trim(Claim_extinction)) == "yes",

      # Location accuracy
      accuracy = case_when(
        str_to_lower(str_trim(Accuracy)) == "high"   ~ "exact",
        str_to_lower(str_trim(Accuracy)) == "medium" ~ "approximate",
        str_to_lower(str_trim(Accuracy)) == "low"    ~ "locality",
        TRUE ~ "unknown"
      ),

      # Bibliographic information
      doi      = str_trim(DOI),
      url      = str_trim(URL),
      citation = str_trim(Citation),

      # Privacy/confidentiality
      confidentiality_level = as.numeric(Confidentiality_level),
      is_sensitive = !is.na(Confidentiality_level) &
        Confidentiality_level > 0,

      # Contributors and additional metadata
      contributor = str_trim(Contributor),
      comments    = str_trim(Comments)
    ) %>%

    # Select only relevant columns for cheCkOVER
    select(
      record_id,
      species,
      longitude,
      latitude,
      year,
      status,
      population_status,  # ✅ NEW: Added to output
      is_type_locality,
      is_extinct,
      accuracy,
      doi,
      url,
      citation,
      confidentiality_level,
      is_sensitive,
      contributor,
      comments
    ) %>%

    # Filter out invalid records
    filter(
      !is.na(species) & species != "",
      !is.na(longitude) & !is.na(latitude),
      between(longitude, -180, 180),
      between(latitude, -90, 90),
      !is.na(year)
    )

  log_info(
    "Mapped %d valid records from %d input records.",
    nrow(checkover_data),
    nrow(woc_data),
    module = module
  )

  checkover_data
}

#' Resolve full taxonomic hierarchy via WoRMS
resolve_taxonomy <- function(species_list, module = "MODULE1_INGEST") {
  n_species <- length(species_list)
  log_info("Resolving taxonomy for %d species via WoRMS...", n_species, module = module)
  
  taxonomy_map <- data.frame(
    original_name = character(),
    accepted_name = character(),
    aphia_id = integer(),
    status = character(),
    rank = character(),
    authority = character(),
    superdomain = character(),
    kingdom = character(),
    phylum = character(),
    subphylum = character(),
    superclass = character(),
    class = character(),
    subclass = character(),
    superorder = character(),
    order = character(),
    suborder = character(),
    infraorder = character(),
    superfamily = character(),
    family = character(),
    genus = character(),
    species_epithet = character(),
    subspecies = character(),
    stringsAsFactors = FALSE
  )
  
  pb <- create_progress_bar(n_species)
  
  for (sp in species_list) {
    pb$tick()
    
    tryCatch({
      recs <- worrms::wm_records_name(sp, fuzzy = TRUE, marine_only = FALSE)
      
      if (!is.null(recs) && nrow(recs) > 0) {
        accepted <- recs[recs$status == "accepted", ]
        if (nrow(accepted) == 0) accepted <- recs[1, , drop = FALSE]
        
        aphia_id <- accepted$AphiaID[1]
        classification <- worrms::wm_classification(aphia_id)
        
        tax_row <- list(
          original_name = sp,
          accepted_name = accepted$valid_name[1],
          aphia_id = aphia_id,
          status = accepted$status[1],
          rank = accepted$rank[1],
          authority = accepted$authority[1],
          superdomain = NA_character_,
          kingdom = NA_character_,
          phylum = NA_character_,
          subphylum = NA_character_,
          superclass = NA_character_,
          class = NA_character_,
          subclass = NA_character_,
          superorder = NA_character_,
          order = NA_character_,
          suborder = NA_character_,
          infraorder = NA_character_,
          superfamily = NA_character_,
          family = NA_character_,
          genus = NA_character_,
          species_epithet = NA_character_,
          subspecies = NA_character_
        )
        
        if (!is.null(classification) && nrow(classification) > 0) {
          for (i in seq_len(nrow(classification))) {
            rank_name <- tolower(classification$rank[i])
            taxon_name <- classification$scientificname[i]
            
            if (rank_name == "superdomain") tax_row$superdomain <- taxon_name
            else if (rank_name == "kingdom") tax_row$kingdom <- taxon_name
            else if (rank_name == "phylum") tax_row$phylum <- taxon_name
            else if (rank_name == "subphylum") tax_row$subphylum <- taxon_name
            else if (rank_name == "superclass") tax_row$superclass <- taxon_name
            else if (rank_name == "class") tax_row$class <- taxon_name
            else if (rank_name == "subclass") tax_row$subclass <- taxon_name
            else if (rank_name == "superorder") tax_row$superorder <- taxon_name
            else if (rank_name == "order") tax_row$order <- taxon_name
            else if (rank_name == "suborder") tax_row$suborder <- taxon_name
            else if (rank_name == "infraorder") tax_row$infraorder <- taxon_name
            else if (rank_name == "superfamily") tax_row$superfamily <- taxon_name
            else if (rank_name == "family") tax_row$family <- taxon_name
            else if (rank_name == "genus") tax_row$genus <- taxon_name
            else if (rank_name == "species") {
              species_parts <- strsplit(taxon_name, " ")[[1]]
              if (length(species_parts) >= 2) {
                tax_row$species_epithet <- species_parts[2]
              }
            } else if (rank_name == "subspecies") {
              species_parts <- strsplit(taxon_name, " ")[[1]]
              if (length(species_parts) >= 3) {
                tax_row$subspecies <- species_parts[3]
              }
            }
          }
        }
        
        taxonomy_map <- rbind(taxonomy_map, as.data.frame(tax_row, stringsAsFactors = FALSE))
        
      } else {
        taxonomy_map <- rbind(taxonomy_map, data.frame(
          original_name = sp,
          accepted_name = NA,
          aphia_id = NA,
          status = "not found",
          rank = NA,
          authority = NA,
          superdomain = NA, kingdom = NA, phylum = NA, subphylum = NA,
          superclass = NA, class = NA, subclass = NA, superorder = NA,
          order = NA, suborder = NA, infraorder = NA, superfamily = NA,
          family = NA, genus = NA, species_epithet = NA, subspecies = NA,
          stringsAsFactors = FALSE
        ))
      }
      
    }, error = function(e) {
      log_error("Error resolving '%s': %s", sp, e$message, module = module)
      taxonomy_map <<- rbind(taxonomy_map, data.frame(
        original_name = sp,
        accepted_name = NA,
        aphia_id = NA,
        status = paste("error:", e$message),
        rank = NA,
        authority = NA,
        superdomain = NA, kingdom = NA, phylum = NA, subphylum = NA,
        superclass = NA, class = NA, subclass = NA, superorder = NA,
        order = NA, suborder = NA, infraorder = NA, superfamily = NA,
        family = NA, genus = NA, species_epithet = NA, subspecies = NA,
        stringsAsFactors = FALSE
      ))
    })
    
    Sys.sleep(0.5)  # Be polite to API
  }
  
  pb$terminate()
  taxonomy_map
}

#' Main ingest and clean function
ingest_clean <- function(file_path, output_dir = "checkover_output", 
                         resolve_taxonomy = TRUE) {
  module <- "MODULE1_INGEST"
  
  with_log_section(module, {
    log_info("Starting INGEST & CLEAN.", module = module)
    log_info("Input file: %s", file_path, module = module)
    log_info("Output directory: %s", output_dir, module = module)
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      log_info("Created output directory: %s", output_dir, module = module)
    }
    
    # Load raw data
    woc_raw <- load_woc_data(file_path, module = module)
    
    # Map to standard format
    clean_data <- map_woc_to_checkover(woc_raw, module = module)
    
    log_info("After mapping: %d records, %d species.",
             nrow(clean_data), length(unique(clean_data$species)), module = module)
    
    # Additional cleaning
    year_now <- as.numeric(format(Sys.Date(), "%Y"))
    
    clean_data <- clean_data %>%
      dplyr::distinct(species, longitude, latitude, year, .keep_all = TRUE) %>%
      dplyr::mutate(
        species = stringr::str_replace_all(species, "\\s+", " "),
        species = stringr::str_to_sentence(species)
      ) %>%
      dplyr::filter(dplyr::between(year, 1500, year_now))
    
    log_info("After cleaning: %d records (year range 1500-%d).",
             nrow(clean_data), year_now, module = module)
    
    # Identify type localities
    type_localities <- clean_data %>%
      dplyr::filter(is_type_locality == TRUE) %>%
      dplyr::group_by(species) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
    log_info("Identified %d species with type locality records.",
             nrow(type_localities), module = module)
    
    # Create spatial object
    clean_sf <- sf::st_as_sf(clean_data, coords = c("longitude", "latitude"), crs = 4326)
    log_debug("Converted to sf object with CRS EPSG:4326.", module = module)
    
    # Summary statistics
    summary_stats <- list(
      total_records = nrow(clean_data),
      unique_species = length(unique(clean_data$species)),
      native_records = sum(clean_data$status == "native"),
      alien_records = sum(clean_data$status == "alien"),
      type_localities = nrow(type_localities),
      sensitive_records = sum(clean_data$is_sensitive, na.rm = TRUE),
      year_range = range(clean_data$year, na.rm = TRUE),
      geographic_extent = list(
        lon_range = range(clean_data$longitude),
        lat_range = range(clean_data$latitude)
      ),
      contributors = length(unique(clean_data$contributor[!is.na(clean_data$contributor)]))
    )
    
    log_info("Summary: records=%d, species=%d, native=%d, alien=%d, type_loc=%d",
             summary_stats$total_records, summary_stats$unique_species,
             summary_stats$native_records, summary_stats$alien_records,
             summary_stats$type_localities, module = module)
    
    # Taxonomy resolution
    taxonomy_map <- NULL
    
    if (resolve_taxonomy) {
      taxonomy_map <- resolve_taxonomy(unique(clean_data$species), module = module)
      
      # Join taxonomy
      clean_data <- clean_data %>%
        dplyr::left_join(
          taxonomy_map %>% dplyr::select(-rank, -authority),
          by = c("species" = "original_name")
        ) %>%
        dplyr::mutate(
          species_resolved = ifelse(!is.na(accepted_name), accepted_name, species),
          taxonomic_path = paste(kingdom, phylum, class, order, family, genus, 
                                 species_epithet, sep = " > ")
        )
      
      # Export taxonomy
      taxon_map_file <- file.path(output_dir, "taxonomy_mapping_full.tsv")
      write_tsv(taxonomy_map, taxon_map_file)
      log_info("Saved taxonomy mapping to: %s", taxon_map_file, module = module)
      
      taxonomy_summary <- list(
        total_species_queried = nrow(taxonomy_map),
        successfully_resolved = sum(!is.na(taxonomy_map$aphia_id)),
        not_found = sum(taxonomy_map$status == "not found", na.rm = TRUE),
        errors = sum(grepl("error", taxonomy_map$status), na.rm = TRUE),
        unique_families = length(unique(taxonomy_map$family[!is.na(taxonomy_map$family)])),
        unique_orders = length(unique(taxonomy_map$order[!is.na(taxonomy_map$order)]))
      )
      
      summary_stats$taxonomy_resolution <- taxonomy_summary
    }
    
    # Export cleaned data
    clean_file <- file.path(output_dir, "clean_occurrences.tsv")
    write_tsv(clean_data, clean_file)
    log_info("Saved cleaned occurrences to: %s", clean_file, module = module)
    
    clean_rds <- file.path(output_dir, "clean_occurrences.rds")
    saveRDS(clean_data, clean_rds)
    
    clean_sf_rds <- file.path(output_dir, "clean_occurrences_sf.rds")
    saveRDS(clean_sf, clean_sf_rds)
    
    type_localities_file <- file.path(output_dir, "type_localities.tsv")
    write_tsv(type_localities, type_localities_file)
    
    log_info("Module 1 completed.", module = module)
    
    list(
      clean_data = clean_data,
      clean_sf = clean_sf,
      type_localities = type_localities,
      summary_stats = summary_stats,
      taxonomy_map = taxonomy_map,
      files_created = c(clean_file, clean_rds, clean_sf_rds, type_localities_file)
    )
  })
}