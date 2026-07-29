#### HELPER FUNCTIONS ####
# Shared utilities used across modules

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# Total-extinction / zero-active terminal state
# ---------------------------------------------------------------------------
# Mandatory disclaimer wherever a species is reported with zero remaining
# occurrences. This is a DATA-STATE signal, never a biological-extinction
# verdict (Lucian, 2026-06; ties to the Ecography Forum "local extinction as
# information"). Keep this wording in one place so output + narrative agree.
CHECKOVER_EXTINCTION_DISCLAIMER <- paste0(
  "This status reflects the STATE OF THE WORLD OF CRAYFISH DATA (zero remaining ",
  "occurrences on record), not a field-verified biological extinction. A species ",
  "flagged extinct on the basis of data warrants intensive, thorough field ",
  "verification before any real-world conclusion is drawn. Treat this as a signal ",
  "that the data indicate zero remaining occurrences, never as a determination of ",
  "biological extinction."
)

#' Count distinct usable geographic values (countries, admin units, ...).
#'
#' Excludes NA, blanks and the explicit `unresolved` sentinel, so a record whose
#' geography could not be resolved never inflates a count. Use this instead of
#' `n_distinct(x, na.rm = TRUE)` for any geographic field.
n_distinct_geo <- function(x) {
  v <- trimws(as.character(x))
  if (exists("is_geo_unresolved", mode = "function")) {
    v <- v[!is_geo_unresolved(v)]
  } else {
    v <- v[!is.na(v) & nzchar(v) & tolower(v) != "unresolved"]
  }
  length(unique(v))
}

#' Count the continents a species genuinely occupies.
#'
#' A plain `n_distinct(continents)` makes one stray record enough to call a
#' species cosmopolitan: *A. torrentium* qualified on 4 Asian records out of
#' 3,484 (0.1%), *O. pellucidus* (a Kentucky cave endemic) on a single European
#' record, and *E. spinifer/suttoni* purely because a BLANK label counted as a
#' second continent. Rule agreed with Lucian (2026-07):
#'
#'   * blank / NA labels are excluded outright — they are missing data, not a
#'     continent;
#'   * a continent counts only if it holds at least `min_records` records AND at
#'     least `min_share` of the species' labelled records.
#'
#' If no continent clears the bar the species is still somewhere, so the
#' dominant one is kept and the count is 1 — never 0, which would fall through
#' the classifier's `n_continents == 1` branch into "regional".
#'
#' @param cont        Character vector of per-record continent labels.
#' @param min_records Minimum records for a continent to count (default 5).
#' @param min_share   Minimum share of labelled records (default 0.05 = 5%).
#' @return integer(1)
count_continents <- function(cont, min_records = 5L, min_share = 0.05) {
  v <- trimws(as.character(cont))
  # Drop blanks AND the explicit `unresolved` sentinel: a value the fallback
  # could not resolve must never contribute to a continent count, because that
  # would let a data gap change a species' biogeographic category — exactly what
  # happened to E. suttoni (Lucian, 2026-07).
  if (exists("is_geo_unresolved", mode = "function")) {
    v <- v[!is_geo_unresolved(v)]
  } else {
    v <- v[!is.na(v) & nzchar(v) & tolower(v) != "unresolved"]
  }
  if (length(v) == 0L) return(0L)
  tb <- table(v)
  keep <- (as.integer(tb) >= min_records) & ((as.integer(tb) / length(v)) >= min_share)
  n <- as.integer(sum(keep))
  if (n == 0L) 1L else n
}

#' Build the finest human-readable basin name from Table_S3 components.
#'
#' Table_S3 carries three name levels: Basin_name (e.g. Danube) >
#' Subbasin_name (e.g. Tisza) > river_name (e.g. Crișul Alb, the level-10
#' river). Endemics assigned at level 10 must show the RIVER, not collapse to
#' the coarse basin (Lucian, 2026-07). Uses the two finest non-empty, distinct
#' components for a specific-but-readable label; falls back to `fallback`
#' (the raw code) when nothing resolves.
#'
#' @param basin,subbasin,river Scalar name components (any may be NA/empty).
#' @param fallback Value to return when no component is available.
#' @return Character(1).
basin_display_name <- function(basin, subbasin, river, fallback = NA_character_) {
  comps <- c(basin, subbasin, river)
  comps <- comps[!is.na(comps) & nzchar(comps)]
  comps <- comps[!duplicated(comps)]
  if (length(comps) == 0L) return(fallback)
  if (length(comps) >= 2L) paste(utils::tail(comps, 2L), collapse = " - ") else comps[1]
}

#' Is a species in the zero-active (total-extinction) terminal state?
#'
#' TRUE when the active (post-suppression, non-extinct) record count is 0 while
#' the species is still known to the dataset (>=1 extinct/suppressed record).
#' Distinguishes a genuinely extirpated species from one simply absent in a
#' population branch (which has 0 records of ANY kind).
#'
#' @param active_count Number of active records.
#' @param known_count  Total records of any temporal_status for the species.
#' @return logical(1)
is_zero_active_terminal <- function(active_count, known_count) {
  isTRUE(active_count == 0L) && isTRUE(known_count > 0L)
}

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

# Build filesystem-safe package ID from a species display name.
# Per cheCkOVER package spec v1.0 (Lucian / WoC integration):
#   1. Remove parentheses (subgenus survives as a bare word)
#   2. Replace whitespace with underscore
#   3. Collapse consecutive underscores
# Casing is preserved (subgenus capitalisation is part of the name).
#
#   make_package_id("Astacus astacus")
#     -> "Astacus_astacus"
#   make_package_id("Cambarellus (Cambarellus) chapalanus")
#     -> "Cambarellus_Cambarellus_chapalanus"
#   make_package_id("Procambarus hagenianus vesticeps")
#     -> "Procambarus_hagenianus_vesticeps"
make_package_id <- function(sp) {
  out <- trimws(as.character(sp))
  out <- gsub("[()]", "", out, perl = TRUE)
  out <- gsub("\\s+", "_", out, perl = TRUE)
  out <- gsub("_+", "_", out, perl = TRUE)
  out <- gsub("^_+|_+$", "", out, perl = TRUE)
  out
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

# ──────────────────────────────────────────────────────────────────────────────
# EXTINCTION MASKING ORCHESTRATOR (post-Phase-1.5, pre-Phase-2)
# ──────────────────────────────────────────────────────────────────────────────

#' Apply per-species extinction masking to both indigenous and non-indigenous
#' branches.
#'
#' Walks each active species, computes its extinction events, and applies the
#' 500m geodesic mask to both branches' clean_data. Adds two columns:
#'   temporal_status:           "active" | "suppressed" | "extinct"
#'   suppressed_by_extinction:  NA or extinction record_id
#'
#' Modules downstream (3A/3C/4A/4C reports + metrics) filter on
#' temporal_status == "active" so extinct + suppressed records don't pollute
#' metrics. Existing modules without temporal_status awareness will still see
#' all records (the column simply doesn't exist), so this is forward-compatible.
#'
#' Requires apply_spatial_temporal_mask() + parse_extinction_causes() from
#' Module 11 (R/11_temporal_delta.R) to be sourced.
#'
#' @param result_indigenous       List with $clean_data and $clean_sf.
#' @param result_non_indigenous   List with $clean_data and $clean_sf.
#' @param active_species          Character vector of species names to process.
#' @return Named list with the same two objects, with `temporal_status` +
#'         `suppressed_by_extinction` columns added.
apply_extinction_masking_to_branches <- function(result_indigenous,
                                                 result_non_indigenous,
                                                 active_species) {
  
  module <- "EXTINCTION_MASKING"
  if (exists("log_info", mode = "function")) {
    log_info("=== Applying extinction masking (500m geodesic) ===", module = module)
  }
  
  # Process one branch (indigenous or non_indigenous). For each species:
  #   1. Slice the branch to that species' records
  #   2. Parse extinctions (records with is_extinct == TRUE)
  #   3. Apply 500m mask (per-species, scoped)
  #   4. Stitch tagged slice back into the branch's clean_data
  process_branch <- function(result_branch, branch_label) {
    if (is.null(result_branch) || is.null(result_branch$clean_data) ||
        nrow(result_branch$clean_data) == 0L) {
      return(result_branch)
    }
    cd <- result_branch$clean_data
    
    # Initialize columns (default everything active)
    cd$temporal_status          <- "active"
    cd$suppressed_by_extinction <- NA_character_
    
    n_extinct_total    <- 0L
    n_suppressed_total <- 0L
    
    for (sp in active_species) {
      idx <- which(cd$species == sp)
      if (length(idx) == 0L) next
      sp_slice <- cd[idx, , drop = FALSE]
      # Need is_extinct present
      if (!"is_extinct" %in% names(sp_slice)) next
      if (!any(!is.na(sp_slice$is_extinct) & sp_slice$is_extinct == TRUE)) next
      
      ext <- tryCatch(parse_extinction_causes(sp_slice),
                      error = function(e) NULL,
                      warning = function(w) parse_extinction_causes(sp_slice))
      if (is.null(ext) || nrow(ext) == 0L) next
      
      masked <- tryCatch(
        suppressWarnings(apply_spatial_temporal_mask(sp_slice, ext, log_unlinked = FALSE)),
        error = function(e) {
          if (exists("log_warn", mode = "function")) {
            log_warn("Masking failed for %s: %s", sp, conditionMessage(e), module = module)
          }
          sp_slice  # fall through with unmasked slice
        }
      )
      
      # Stitch back
      cd$temporal_status[idx]          <- masked$temporal_status
      cd$suppressed_by_extinction[idx] <- masked$suppressed_by_extinction
      
      n_extinct_total    <- n_extinct_total    + sum(masked$temporal_status == "extinct",    na.rm = TRUE)
      n_suppressed_total <- n_suppressed_total + sum(masked$temporal_status == "suppressed", na.rm = TRUE)
    }
    
    if (exists("log_info", mode = "function")) {
      log_info("Branch '%s': %d extinct + %d suppressed (of %d records, %d will count as 'active')",
               branch_label, n_extinct_total, n_suppressed_total, nrow(cd),
               nrow(cd) - n_extinct_total - n_suppressed_total, module = module)
    }
    
    # Also tag clean_sf if present
    result_branch$clean_data <- cd
    if (!is.null(result_branch$clean_sf) && nrow(result_branch$clean_sf) == nrow(cd)) {
      result_branch$clean_sf$temporal_status          <- cd$temporal_status
      result_branch$clean_sf$suppressed_by_extinction <- cd$suppressed_by_extinction
    }
    result_branch
  }
  
  result_indigenous     <- process_branch(result_indigenous,     "indigenous")
  result_non_indigenous <- process_branch(result_non_indigenous, "non_indigenous")
  
  list(
    result_indigenous     = result_indigenous,
    result_non_indigenous = result_non_indigenous
  )
}

# ---------------------------------------------------------------------------
# Dependency self-heal: canonical geographic vocabulary
# ---------------------------------------------------------------------------
# R/00_geo_canon.R supplies canon_continent(), canon_country(), geo_usable(),
# is_geo_unresolved() and the GEO_* constants. Module 1 (ingest), 2A, 2B, 2C and
# the report/narrative writers all call into it, so a run where it has not been
# sourced dies mid-pipeline with "could not find function canon_continent"
# rather than at load time.
#
# checkcover_main.R sources it explicitly, but the modules are also documented as
# individually runnable (see README) and are sourced directly by the test suite,
# so load order must not be able to break them. 00_helpers.R is sourced before
# everything else in every entry point, which makes this the one place that
# guarantees availability. Mirrors the defensive `%||%` definitions used
# elsewhere in the codebase.
if (!exists("canon_continent", mode = "function")) {
  for (.geo_canon_path in c("R/00_geo_canon.R", "00_geo_canon.R",
                            file.path("..", "R", "00_geo_canon.R"))) {
    if (file.exists(.geo_canon_path)) {
      source(.geo_canon_path)
      break
    }
  }
  rm(.geo_canon_path)
}
