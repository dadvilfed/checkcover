#### MODULE 1C: DISTRIBUTION METRICS ####

compute_distribution_context <- function(result, output_dir) {
  module <- "MODULE1C_METRICS"
  
  with_log_section(module, {
    log_info("=== COMPUTING DISTRIBUTION METRICS (HISTORICAL vs CURRENT) ===", 
             module = module)
    
    df <- result$clean_data
    
    # Clean status columns
    if ("status.x" %in% names(df)) {
      df$status <- df$status.x
      df$status.x <- NULL
    }
    if ("status.y" %in% names(df)) df$status.y <- NULL
    
    df$longitude <- as.numeric(df$longitude)
    df$latitude <- as.numeric(df$latitude)
    
    # Ensure is_extinct exists
    if (!"is_extinct" %in% names(df)) df$is_extinct <- FALSE
    
    # Calculate metrics per species
    stats <- df %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(
        n_continents = dplyr::n_distinct(continents, na.rm = TRUE),
        n_countries = dplyr::n_distinct(country, na.rm = TRUE),
        has_alien = any(tolower(status) == "alien", na.rm = TRUE),
        
        # Historical metrics (ALL points)
        eoo_historical = .calc_eoo_val(longitude, latitude),
        aoo_historical = .calc_aoo_val(longitude, latitude),
        
        # CRITICAL FIX: Current metrics (exclude extinct, handle edge cases)
        eoo_current = {
          living_mask <- !is_extinct
          living_count <- sum(living_mask, na.rm = TRUE)
          if (living_count >= 3) {
            .calc_eoo_val(longitude[living_mask], latitude[living_mask])
          } else {
            NA_real_
          }
        },
        aoo_current = {
          living_mask <- !is_extinct
          living_count <- sum(living_mask, na.rm = TRUE)
          if (living_count > 0) {
            .calc_aoo_val(longitude[living_mask], latitude[living_mask])
          } else {
            NA_real_
          }
        },
        
        # Extirpation signature
        extirpation_signature = {
          ext_rows <- which(is_extinct)
          if (length(ext_rows) > 0) {
            sig <- paste0(
              round(latitude[ext_rows], 3), ",",
              round(longitude[ext_rows], 3),
              " (", year[ext_rows], ")"
            )
            paste(sig, collapse = " | ")
          } else {
            NA_character_
          }
        },
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        dist_cat = dplyr::case_when(
          has_alien ~ "cosmopolitan",
          n_continents > 1 ~ "cosmopolitan",
          !is.na(eoo_current) & eoo_current < 2000 ~ "micro-endemic",
          !is.na(eoo_current) & eoo_current < 5000 & n_countries <= 2 ~ "endemic",
          TRUE ~ "regional"
        )
      )
    
    # Join back
    if ("distribution_category" %in% names(df)) df$distribution_category <- NULL
    
    df <- df %>%
      dplyr::left_join(stats, by = "species") %>%
      dplyr::rename(distribution_category = dist_cat)
    
    df$distribution_category[is.na(df$distribution_category)] <- "regional"
    
    # Update result
    result$clean_data <- df
    if (!is.null(result$clean_sf)) {
      result$clean_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), 
                                      crs = 4326, remove = FALSE)
    }
    
    # Save
    out_tsv <- file.path(output_dir, "clean_occurrences_with_metrics.tsv")
    write_tsv(df, out_tsv)
    log_info("Saved data with Historical/Current metrics.", module = module)
    
    return(result)
  })
}