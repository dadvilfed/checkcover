#### HELPER FUNCTIONS ####
# Shared utilities used across modules

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Safe type conversions
na_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
na_lgl <- function(x) if (is.null(x) || length(x) == 0) NA else as.logical(x)
na_num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)
one_chr <- function(x) if (length(x) >= 1) as.character(x[[1]]) else NA_character_
one_num <- function(x) if (length(x) >= 1) suppressWarnings(as.numeric(x[[1]])) else NA_real_

# Safe min/max
.safe_min <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

.safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

# String cleaning
nz_or_na <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

.str_clean <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u00A0\t\r\n]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Standardize sf geometry column name
.std_geom <- function(x) {
  x <- sf::st_as_sf(x)
  g <- attr(x, "sf_column")
  if (!identical(g, "geometry")) {
    names(x)[names(x) == g] <- "geometry"
    attr(x, "sf_column") <- "geometry"
  }
  x
}

# Expand bbox by kilometers (Equal Area projection)
.expand_bbox_km <- function(bbox_sfc, target_crs, km) {
  if (km <= 0) return(bbox_sfc)
  ea <- 6933
  bb_ea <- try(sf::st_transform(bbox_sfc, ea), silent = TRUE)
  if (inherits(bb_ea, "try-error")) return(bbox_sfc)
  bb_poly <- sf::st_as_sfc(sf::st_bbox(bb_ea))
  buf <- sf::st_buffer(bb_poly, dist = km * 1000)
  tryCatch(sf::st_transform(buf, target_crs), error = function(e) bbox_sfc)
}

# Safe digest (hashing)
.safe_digest <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(x, algo = "xxhash64"))
  }
  NA_character_
}

# Format latitude/longitude
fmt_latlon <- function(lat, lon, digits = 2) {
  if (!is.finite(lat) || !is.finite(lon)) return(NA_character_)
  ns <- if (lat >= 0) "N" else "S"
  ew <- if (lon >= 0) "E" else "W"
  paste0(abs(round(lat, digits)), "°", ns, ", ", abs(round(lon, digits)), "°", ew)
}

# EOO/AOO calculation helpers
.calc_eoo_val <- function(lon, lat) {
  coords <- unique(data.frame(lon, lat))
  coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
  if (nrow(coords) >= 3) {
    pts <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
    return(as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(pts)))) / 1e6)
  }
  return(NA_real_)
}

.calc_aoo_val <- function(lon, lat) {
  coords <- data.frame(lon, lat)
  coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
  if (nrow(coords) == 0) return(NA_real_)
  grid_size <- 0.018
  n_cells <- length(unique(paste(floor(coords$lon/grid_size), floor(coords$lat/grid_size))))
  return(n_cells * 4)
}

# Distribution category from EOO
.level_for_cat <- function(cat) {
  switch(cat, 
         "micro-endemic" = 10L, 
         "endemic" = 10L, 
         "regional" = 8L, 
         "cosmopolitan" = 6L, 
         8L)
}

# CRITICAL FIX: Safe intersects with multiple fallbacks
.safe_intersects <- function(pts_sf, polys_sf) {
  out <- try(sf::st_intersects(pts_sf, polys_sf, sparse = TRUE), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  
  # Fallback 1: Validate geometries
  polys_ok <- try(suppressWarnings(lwgeom::st_make_valid(polys_sf)), silent = TRUE)
  if (inherits(polys_ok, "try-error")) polys_ok <- polys_sf
  polys_ok <- polys_ok[!sf::st_is_empty(polys_ok), , drop = FALSE]
  
  out <- try(sf::st_intersects(pts_sf, polys_ok, sparse = TRUE), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  
  # Fallback 2: Disable s2 and use Equal Area
  s2_old <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(s2_old), add = TRUE)
  sf::sf_use_s2(FALSE)
  
  ea <- 6933
  pts_ea <- try(sf::st_transform(pts_sf, ea), silent = TRUE)
  pol_ea <- try(sf::st_transform(polys_ok, ea), silent = TRUE)
  
  if (inherits(pts_ea, "try-error") || inherits(pol_ea, "try-error")) {
    return(sf::st_intersects(sf::st_geometry(pts_sf), sf::st_geometry(polys_ok), sparse = TRUE))
  }
  
  sf::st_intersects(pts_ea, pol_ea, sparse = TRUE)
}

# CRITICAL FIX: Safe distance calculation (always uses Equal Area)
.safe_within_distance <- function(pts_sf, pts_wdpa_sf, dist_m) {
  ea <- 6933
  tryCatch({
    pts_ea <- sf::st_transform(pts_sf, ea)
    wdpa_ea <- sf::st_transform(pts_wdpa_sf, ea)
    sf::st_is_within_distance(pts_ea, wdpa_ea, dist = dist_m)
  }, error = function(e) {
    rep(list(integer(0)), nrow(pts_sf))
  })
}

# CRITICAL FIX: File locking for cache writes
.save_with_lock <- function(obj, path, max_wait = 30) {
  lock_file <- paste0(path, ".lock")
  start_time <- Sys.time()
  
  # Try to create lock (atomic operation)
  while (!file.create(lock_file, showWarnings = FALSE)) {
    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > max_wait) {
      warning("Cache lock timeout. Proceeding anyway.")
      break
    }
    # Random wait to avoid thundering herd
    Sys.sleep(runif(1, 0.1, 0.5))
    # If file exists and lock is gone, we're good
    if (file.exists(path) && !file.exists(lock_file)) {
      return(invisible(NULL))
    }
  }
  
  tryCatch({
    saveRDS(obj, path)
    log_info("Saved cache: %s", basename(path), module = "CACHE")
  }, error = function(e) {
    log_error("Failed to save cache: %s", conditionMessage(e), module = "CACHE")
  }, finally = {
    unlink(lock_file)
  })
  
  invisible(NULL)
}

# Progress bar creator
create_progress_bar <- function(total, format = "[:bar] :percent ETA: :eta") {
  if (requireNamespace("progress", quietly = TRUE)) {
    progress::progress_bar$new(format = format, total = total, clear = FALSE)
  } else {
    # Fallback: simple counter
    list(
      tick = function() cat("."),
      terminate = function() cat("\n")
    )
  }
}

# Memory-safe batch processor
process_in_batches <- function(items, batch_size, process_fn, ...) {
  n_items <- length(items)
  n_batches <- ceiling(n_items / batch_size)
  
  log_info("Processing %d items in %d batches", n_items, n_batches, module = "BATCH")
  
  results <- vector("list", n_items)
  
  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_items)
    batch_items <- items[start_idx:end_idx]
    
    log_info("Batch %d/%d: Processing items %d-%d", i, n_batches, start_idx, end_idx, module = "BATCH")
    
    batch_results <- lapply(batch_items, process_fn, ...)
    results[start_idx:end_idx] <- batch_results
    
    # Aggressive garbage collection
    gc(verbose = FALSE)
    log_memory(sprintf("after_batch_%d", i), module = "BATCH")
  }
  
  results
}
# TSV write helper (replaces write.csv throughout)
write_tsv <- function(x, file, row.names = FALSE, quote = FALSE, na = "", ...) {
  write.table(x, file = file, sep = "\t", row.names = row.names, 
              quote = quote, na = na, ...)
  invisible(file)
}

# TSV read helper
read_tsv <- function(file, header = TRUE, stringsAsFactors = FALSE, 
                     quote = "", na.strings = c("", "NA"), ...) {
  read.delim(file, sep = "\t", header = header, 
             stringsAsFactors = stringsAsFactors, 
             quote = quote, na.strings = na.strings, ...)
}

#### SCENARIO DETECTION HELPERS ####

#' Detect population status for records
#' @param data Data frame with population_status and status columns
#' @return Character vector: "indigenous" or "non-indigenous"
detect_population_type <- function(data) {
  # Primary: Use population_status column if available
  if ("population_status" %in% names(data)) {
    pop_status <- tolower(trimws(as.character(data$population_status)))
    
    # Map to standard values
    result <- case_when(
      pop_status %in% c("indigenous", "native") ~ "indigenous",
      pop_status %in% c("non-indigenous", "non indigenous", "alien", "introduced") ~ "non-indigenous",
      TRUE ~ NA_character_
    )
    
    # Fallback: Use status (occurrence_origin) if population_status is NA
    if (any(is.na(result)) && "status" %in% names(data)) {
      origin_status <- tolower(trimws(as.character(data$status)))
      result[is.na(result)] <- case_when(
        origin_status[is.na(result)] %in% c("native") ~ "indigenous",
        origin_status[is.na(result)] %in% c("alien", "introduced") ~ "non-indigenous",
        TRUE ~ NA_character_
      )
    }
    
    return(result)
  }
  
  # Fallback: Use status column only
  if ("status" %in% names(data)) {
    origin_status <- tolower(trimws(as.character(data$status)))
    return(case_when(
      origin_status %in% c("native") ~ "indigenous",
      origin_status %in% c("alien", "introduced") ~ "non-indigenous",
      TRUE ~ NA_character_
    ))
  }
  
  # No valid columns
  return(rep(NA_character_, nrow(data)))
}

#' Create species scenario lookup table
#' @param data Data frame with species and population_type columns
#' @return Data frame with species, scenario, counts
create_scenario_table <- function(data) {
  if (!"population_type" %in% names(data)) {
    stop("Data must have 'population_type' column. Run detect_population_type() first.")
  }
  
  scenario_summary <- data %>%
    group_by(species, population_type) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = population_type,
      values_from = n,
      values_fill = 0
    )
  
  # Ensure both columns exist
  if (!"indigenous" %in% names(scenario_summary)) {
    scenario_summary$indigenous <- 0
  }
  if (!"non-indigenous" %in% names(scenario_summary)) {
    scenario_summary$`non-indigenous` <- 0
  }
  
  scenario_summary <- scenario_summary %>%
    mutate(
      scenario = case_when(
        indigenous > 0 & `non-indigenous` == 0 ~ 1L,
        indigenous == 0 & `non-indigenous` > 0 ~ 2L,
        indigenous > 0 & `non-indigenous` > 0 ~ 3L,
        TRUE ~ NA_integer_
      ),
      total_records = indigenous + `non-indigenous`
    ) %>%
    select(species, scenario, indigenous, `non-indigenous`, total_records) %>%
    arrange(scenario, species)
  
  return(scenario_summary)
}

#' Get species list by scenario
#' @param scenario_table Output from create_scenario_table()
#' @param scenario_num Scenario number (1, 2, or 3)
#' @return Character vector of species names
get_species_by_scenario <- function(scenario_table, scenario_num) {
  scenario_table %>%
    filter(scenario == scenario_num) %>%
    pull(species)
}