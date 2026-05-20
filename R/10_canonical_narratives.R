#### MODULE 10: CANONICAL NARRATIVE TEMPLATE GENERATOR ####
# Implements the Canonical cheCkOVER Geo-Narrative Template (File_S1, v1.0, March 2025)
# Generates Section 1–5 structured markdown + Section 5 plain-text / JSON outputs.

#' Generate canonical geo-narrative template for all species
#'
#' @param scenario_table         Scenario detection table (columns: species, scenario)
#' @param result_indigenous      Indigenous result object (list with $clean_data)
#' @param result_non_indigenous  Non-indigenous result object (list with $clean_data)
#' @param indigenous_reports     Indigenous reports list ($per_species named by species)
#' @param non_indigenous_reports Non-indigenous reports list ($per_species named by species)
#' @param scenario3_merged       Scenario 3 merged reports
#' @param vernacular_lookup      Vernacular names lookup (list with $wide data frame, or data frame)
#' @param output_dir             Output directory
#' @param feow_lookup_path       FEOW name lookup path (optional, for ID-to-name mapping)
#' @param hydrobasin_names       HydroBASINS name lookup data frame (optional)
#' @param checkover_version      cheCkOVER software version string (e.g. "1.0")
#' @param output_version         Output version label (e.g. "v1.0" or "v1.1")
#' @param snapshot_date          Data snapshot date string "YYYY-MM-DD" (defaults to today)
#' @param prev_reports_dir       Path to previous-version per-species JSON reports (enables
#'                               version delta; NULL = v1.0 behaviour, delta omitted)
#' @return List of generated canonical narratives
generate_canonical_narratives <- function(scenario_table,
                                          result_indigenous,
                                          result_non_indigenous,
                                          indigenous_reports,
                                          non_indigenous_reports,
                                          scenario3_merged,
                                          vernacular_lookup  = NULL,
                                          output_dir         = "checkover_output",
                                          feow_lookup_path   = NULL,
                                          hydrobasin_names   = NULL,
                                          checkover_version  = "1.0",
                                          output_version     = "v1.0",
                                          snapshot_date      = NULL,
                                          prev_reports_dir   = NULL) {
  
  module <- "MODULE10_CANONICAL"
  
  with_log_section(module, {
    log_info("=== MODULE 10: CANONICAL NARRATIVE GENERATION ===", module = module)
    
    if (is.null(snapshot_date)) snapshot_date <- as.character(Sys.Date())
    
    # ---------- Output directories ----------
    canonical_dir <- file.path(output_dir, "narratives_canonical")
    formal_dir    <- file.path(output_dir, "narratives_formal")
    for (d in c(canonical_dir, formal_dir)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
    
    # ---------- Shared lookups ----------
    vern_map <- .extract_vernacular_map(vernacular_lookup)
    feow_map <- .load_feow_map(feow_lookup_path, module)
    iso_lang <- .iso639_language_map()
    
    # ---------- Raw data pools ----------
    ind_data  <- if (!is.null(result_indigenous))     result_indigenous$clean_data     else NULL
    nind_data <- if (!is.null(result_non_indigenous)) result_non_indigenous$clean_data else NULL
    
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]
    log_info("Generating canonical narratives for %d species...", length(all_species), module = module)
    
    canonical_narratives <- list()
    
    for (sp in all_species) {
      log_info("Processing: %s", sp, module = module)
      
      sp_clean <- make_package_id(sp)
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]
      
      # --- Extract vernacular string ---
      vern_str <- .get_vernacular_string(sp, vern_map)
      
      # --- Fetch per-species raw subsets ---
      sp_ind  <- if (!is.null(ind_data)  && nrow(ind_data)  > 0) ind_data[ind_data$species   == sp, ] else NULL
      sp_nind <- if (!is.null(nind_data) && nrow(nind_data) > 0) nind_data[nind_data$species == sp, ] else NULL
      
      # --- Fetch compiled JSON reports (for pre-computed metrics) ---
      ind_report  <- .load_species_report(sp, sp_clean, "indigenous",     indigenous_reports)
      nind_report <- .load_species_report(sp, sp_clean, "non_indigenous",  non_indigenous_reports)
      
      # --- Build canonical sections ---
      canonical <- .build_canonical_narrative(
        sp            = sp,
        scenario      = scenario,
        vern_str      = vern_str,
        sp_ind        = sp_ind,
        sp_nind       = sp_nind,
        ind_report    = ind_report,
        nind_report   = nind_report,
        iso_lang      = iso_lang,
        feow_map      = feow_map,
        hydrobasin_names = hydrobasin_names,
        checkover_version = checkover_version,
        output_version    = output_version,
        snapshot_date     = snapshot_date,
        prev_reports_dir  = prev_reports_dir,
        sp_clean          = sp_clean,
        module            = module
      )
      
      if (!is.null(canonical)) {
        # Save full canonical markdown
        canonical_file <- file.path(canonical_dir, paste0(sp_clean, "_canonical.md"))
        writeLines(canonical$full_markdown, canonical_file, useBytes = TRUE)
        
        # Save formal narrative (Section 5) as TXT
        formal_txt_file <- file.path(formal_dir, paste0(sp_clean, "_narrative.txt"))
        writeLines(canonical$formal_narrative_text, formal_txt_file, useBytes = TRUE)
        
        # Save formal narrative (Section 5) as JSON
        formal_json <- list(
          species        = sp,
          scenario       = scenario,
          output_version = output_version,
          narrative      = canonical$formal_narrative_text,
          word_count     = canonical$formal_narrative_word_count,
          metadata       = canonical$metadata
        )
        formal_json_file <- file.path(formal_dir, paste0(sp_clean, "_narrative.json"))
        jsonlite::write_json(formal_json, formal_json_file,
                             pretty = TRUE, auto_unbox = TRUE, na = "null")
        
        canonical_narratives[[sp]] <- list(
          canonical_markdown = canonical_file,
          formal_text        = formal_txt_file,
          formal_json        = formal_json_file
        )
      }
    }
    
    log_info("Generated %d canonical narratives.", length(canonical_narratives), module = module)
    return(list(narratives = canonical_narratives))
  })
}


# ===========================================================================
# HELPER: ISO 639-3 code -> full language name map
# ===========================================================================
.iso639_language_map <- function() {
  c(
    # ---- Core ISO 639-3 codes ----
    eng = "English",    ron = "Romanian",    rum = "Romanian",
    deu = "German",     fra = "French",      ita = "Italian",
    spa = "Spanish",    por = "Portuguese",  hun = "Hungarian",
    pol = "Polish",     ces = "Czech",       slk = "Slovak",
    slv = "Slovenian",  hrv = "Croatian",    bul = "Bulgarian",
    srp = "Serbian",    rus = "Russian",     ukr = "Ukrainian",
    nld = "Dutch",      swe = "Swedish",     nor = "Norwegian",
    dan = "Danish",     fin = "Finnish",     tur = "Turkish",
    jpn = "Japanese",   zho = "Chinese",     kor = "Korean",
    ara = "Arabic",     heb = "Hebrew",      ell = "Greek",
    lat = "Latin",      cat = "Catalan",     eus = "Basque",
    glg = "Galician",   oci = "Occitan",
    # ---- Additional European ----
    afr = "Afrikaans",  sqi = "Albanian",    bel = "Belarusian",
    est = "Estonian",   lav = "Latvian",     lit = "Lithuanian",
    mkd = "Macedonian", mlt = "Maltese",     isl = "Icelandic",
    fao = "Faroese",    cym = "Welsh",       bre = "Breton",
    gle = "Irish",      gla = "Scottish Gaelic",
    # ---- Asian / African / Other ----
    tha = "Thai",       vie = "Vietnamese",  ind = "Indonesian",
    msa = "Malay",      tgl = "Filipino",    khm = "Khmer",
    ben = "Bengali",    hin = "Hindi",       urd = "Urdu",
    fas = "Persian",    swa = "Swahili",     zul = "Zulu",
    xho = "Xhosa",      hau = "Hausa",       amh = "Amharic",
    # ---- Legacy / GBIF / non-standard aliases ----
    esp = "Spanish",    por_br = "Portuguese",
    slo = "Slovenian",  alb = "Albanian",    mac = "Macedonian",
    chi = "Chinese",    ger = "German",      fre = "French",
    dut = "Dutch",      cze = "Czech",       wel = "Welsh"
  )
}


# ===========================================================================
# HELPER: Extract vernacular map from lookup object
# ===========================================================================
.extract_vernacular_map <- function(vernacular_lookup) {
  if (is.null(vernacular_lookup)) return(NULL)
  if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup))
    return(vernacular_lookup$wide)
  if (is.data.frame(vernacular_lookup))
    return(vernacular_lookup)
  NULL
}


# ===========================================================================
# HELPER: Load FEOW lookup table
# Expects a plain TSV file with columns: ID, Realm, Major Habitat Type, Ecoregion
# After uppercasing: ID (integer), ECOREGION (name string).
# ===========================================================================
.load_feow_map <- function(feow_lookup_path, module) {
  if (is.null(feow_lookup_path) || !file.exists(feow_lookup_path)) return(NULL)
  
  feow_map <- tryCatch(
    read.delim(feow_lookup_path, stringsAsFactors = FALSE,
               encoding = "UTF-8", quote = ""),
    error = function(e) {
      log_warn("Failed to load FEOW lookup: %s", conditionMessage(e), module = module)
      NULL
    }
  )
  
  if (is.null(feow_map)) return(NULL)
  
  names(feow_map) <- toupper(names(feow_map))
  if ("ID" %in% names(feow_map)) feow_map$ID <- as.integer(feow_map$ID)
  
  log_info("FEOW lookup loaded: %d rows, columns: %s",
           nrow(feow_map), paste(names(feow_map), collapse = ", "),
           module = module)
  feow_map
}


# ===========================================================================
# HELPER: Get vernacular string for a species
# ===========================================================================
.get_vernacular_string <- function(sp, vern_map) {
  if (is.null(vern_map) || !"species" %in% names(vern_map)) return(NA_character_)
  idx <- match(sp, vern_map$species)
  if (is.na(idx) || !"vernacular_string" %in% names(vern_map)) return(NA_character_)
  raw_v <- vern_map$vernacular_string[idx]
  if (is.na(raw_v) || !nzchar(raw_v)) return(NA_character_)
  gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
}


# ===========================================================================
# HELPER: Parse vernacular string -> list by full language name
# Format of raw entries: "name(iso639_3)"  (multiple entries separated by " | ")
# Output: named list, e.g. list(English = c("idle crayfish"), Romanian = c("racul bihorean"))
# ===========================================================================
.parse_vernacular_by_language <- function(vern_str, iso_lang) {
  if (is.na(vern_str) || !nzchar(vern_str)) return(NULL)
  entries <- strsplit(vern_str, "\\s*\\|\\s*")[[1]]
  entries <- trimws(entries)
  result  <- list()
  for (e in entries) {
    m <- regmatches(e, regexpr("\\(([a-z]{3})\\)$", e))
    if (length(m) == 0 || !nzchar(m)) {
      # No language code — treat as English
      lang     <- "English"
      name_str <- trimws(e)
    } else {
      code     <- sub(".*\\(([a-z]{3})\\)$", "\\1", e)
      lang     <- if (!is.na(iso_lang[code])) iso_lang[code] else code
      name_str <- trimws(sub("\\s*\\([a-z]{3}\\)$", "", e))
    }
    if (!nzchar(name_str)) next
    result[[lang]] <- unique(c(result[[lang]], name_str))
  }
  if (length(result) == 0) return(NULL)
  result
}


# ===========================================================================
# HELPER: Format vernacular names block for Section 1
# ===========================================================================
.format_vernacular_block <- function(vern_by_lang) {
  if (is.null(vern_by_lang) || length(vern_by_lang) == 0) return(character(0))
  vapply(names(vern_by_lang), function(lang) {
    paste0("  - ", lang, ": ", paste(vern_by_lang[[lang]], collapse = "; "))
  }, character(1))
}


# ===========================================================================
# HELPER: Load per-species JSON report safely
# ===========================================================================
.load_species_report <- function(sp, sp_clean, pop_type, reports_list) {
  if (is.null(reports_list)) return(NULL)
  
  # Try named lookup first (fastest)
  if (!is.null(reports_list$per_species)) {
    report_path <- reports_list$per_species[[sp]]
    if (!is.null(report_path) && file.exists(report_path)) {
      return(tryCatch(jsonlite::read_json(report_path, simplifyVector = TRUE),
                      error = function(e) NULL))
    }
    # Fallback: reconstruct path from first entry's directory
    if (length(reports_list$per_species) > 0) {
      first_path <- reports_list$per_species[[1]]
      candidate  <- file.path(dirname(first_path), paste0(sp_clean, ".json"))
      if (file.exists(candidate)) {
        return(tryCatch(jsonlite::read_json(candidate, simplifyVector = TRUE),
                        error = function(e) NULL))
      }
    }
  }
  NULL
}


# ===========================================================================
# HELPER: Frequency-ordered unique values from a data column
# Returns character vector of unique values ordered by frequency (descending)
# ===========================================================================
.freq_order <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(character(0))
  tb  <- sort(table(x), decreasing = TRUE)
  names(tb)
}

# Returns a data.frame with columns: value, n
.freq_table <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(data.frame(value = character(0), n = integer(0)))
  tb <- sort(table(x), decreasing = TRUE)
  data.frame(value = names(tb), n = as.integer(tb), stringsAsFactors = FALSE)
}


# ===========================================================================
# HELPER: Safe numeric formatting
# ===========================================================================
.fmt_n <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("N/A")
  format(round(as.numeric(x[[1]]), 0), big.mark = ",", scientific = FALSE)
}

.fmt_pct <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("N/A")
  paste0(round(as.numeric(x[[1]]), 1), "%")
}


# ===========================================================================
# HELPER: Compute recent-records stats (post-2000)
# ===========================================================================
.recent_stats <- function(sp_data) {
  if (is.null(sp_data) || nrow(sp_data) == 0 || !"year" %in% names(sp_data))
    return(list(n_recent = NA_integer_, pct_recent = NA_real_))
  yrs <- suppressWarnings(as.integer(sp_data$year))
  n_recent  <- sum(!is.na(yrs) & yrs > 2000)
  pct_recent <- if (nrow(sp_data) > 0) round(n_recent / nrow(sp_data) * 100, 1) else NA_real_
  list(n_recent = n_recent, pct_recent = pct_recent)
}


# ===========================================================================
# HELPER: Resolve raw hydrobasin codes to human-readable names.
#
# Input format:  "L10:2100522290"  (Basin_level:HYBAS_ID)
# Table_S3 cols: Basin_level | HYBAS_ID | Basin_name | Subbasin_name
#
# Output: "Basin_name" or "Basin_name - Subbasin_name" (no ID suffix).
# Falls back to the raw code if the lookup fails.
# ===========================================================================
.resolve_basin_col <- function(x, hb_lookup) {
  if (is.null(hb_lookup) || length(x) == 0) return(x)
  
  # Ensure required columns exist
  req_cols <- c("Basin_level", "HYBAS_ID", "Basin_name", "Subbasin_name")
  if (!all(req_cols %in% names(hb_lookup))) {
    # Try case-insensitive match
    names(hb_lookup) <- sub("^basin_level$",  "Basin_level",  names(hb_lookup), ignore.case = TRUE)
    names(hb_lookup) <- sub("^hybas_id$",     "HYBAS_ID",     names(hb_lookup), ignore.case = TRUE)
    names(hb_lookup) <- sub("^basin_name$",   "Basin_name",   names(hb_lookup), ignore.case = TRUE)
    names(hb_lookup) <- sub("^subbasin_name$","Subbasin_name",names(hb_lookup), ignore.case = TRUE)
    if (!all(req_cols %in% names(hb_lookup))) return(x)
  }
  
  # Build a composite lookup key: "L10:2100522290"
  hb_lookup$lookup_key <- paste0(hb_lookup$Basin_level, ":", hb_lookup$HYBAS_ID)
  
  vapply(x, function(code) {
    if (is.na(code) || !nzchar(code)) return(code)
    
    idx <- match(code, hb_lookup$lookup_key)
    
    if (is.na(idx)) {
      # Fallback: try matching on HYBAS_ID alone (strip "Lxx:" prefix)
      id_only <- sub("^L\\d+:", "", code)
      idx     <- match(id_only, as.character(hb_lookup$HYBAS_ID))
    }
    
    if (is.na(idx)) return(code)   # Give up, return raw code
    
    basin    <- hb_lookup$Basin_name[idx]
    subbasin <- hb_lookup$Subbasin_name[idx]
    
    if (is.na(basin) || !nzchar(basin)) return(code)
    
    if (is.na(subbasin) || !nzchar(subbasin) || basin == subbasin) {
      basin
    } else {
      paste0(basin, " - ", subbasin)
    }
  }, character(1), USE.NAMES = FALSE)
}


# ===========================================================================
# HELPER: Resolve raw FEOW IDs to human-readable ecoregion names.
# feow_map columns (after uppercasing in .load_feow_map):
#   ID (integer)  |  REALM  |  MAJOR HABITAT TYPE  |  ECOREGION (name)
# ===========================================================================
.resolve_feow_col <- function(x, feow_map) {
  if (is.null(feow_map) || length(x) == 0) return(x)
  
  # Locate the ID column and the name column using the known column names
  id_col   <- intersect(c("ID", "FEOW_ID"), names(feow_map))[1]
  name_col <- intersect(c("ECOREGION", "NAME", "ECOREGION_NAME", "FEOW_NAME"), names(feow_map))[1]
  
  if (is.na(id_col) || is.na(name_col)) {
    # Log available columns for diagnostics and return unchanged
    message("[FEOW resolver] Could not find ID/name columns. Available: ",
            paste(names(feow_map), collapse = ", "))
    return(x)
  }
  
  ids       <- suppressWarnings(as.integer(x))
  match_idx <- match(ids, feow_map[[id_col]])
  resolved  <- feow_map[[name_col]][match_idx]
  
  # Replace only where resolution succeeded
  ok   <- !is.na(resolved) & nzchar(resolved)
  x[ok] <- resolved[ok]
  x
}


# ===========================================================================
# MAIN BUILDER: Assemble all sections into one canonical object
# ===========================================================================
.build_canonical_narrative <- function(sp, scenario, vern_str,
                                       sp_ind, sp_nind,
                                       ind_report, nind_report,
                                       iso_lang, feow_map, hydrobasin_names,
                                       checkover_version, output_version,
                                       snapshot_date, prev_reports_dir,
                                       sp_clean, module) {
  
  # We need at least one report to proceed
  if (is.null(ind_report) && is.null(nind_report)) {
    log_warn("  No report found for %s — skipping.", sp, module = module)
    return(NULL)
  }
  
  # Parse vernacular names
  vern_by_lang <- .parse_vernacular_by_language(vern_str, iso_lang)
  
  # --- Load version delta (if prev_reports_dir provided) ---
  delta_ind  <- .load_version_delta(sp_clean, "indigenous",    prev_reports_dir, ind_report)
  delta_nind <- .load_version_delta(sp_clean, "non_indigenous", prev_reports_dir, nind_report)
  
  # --- Build sections ---
  sec1 <- .build_section1(sp, vern_by_lang, ind_report, nind_report)
  sec2 <- .build_section2(sp, sp_ind, ind_report, scenario, feow_map, hydrobasin_names)
  sec3 <- .build_section3(sp, sp_nind, nind_report, scenario, feow_map, hydrobasin_names)
  sec4 <- .build_section4(sp, sp_ind, sp_nind, ind_report, nind_report,
                          checkover_version, output_version, snapshot_date,
                          delta_ind, delta_nind, output_version)
  sec5 <- .build_section5(sp, scenario, vern_by_lang, sp_ind, sp_nind,
                          ind_report, nind_report, delta_ind, delta_nind,
                          output_version, checkover_version, feow_map, hydrobasin_names)
  
  # Assemble YAML front-matter + all sections
  is_versioned <- !grepl("^v?1\\.0$", output_version, ignore.case = TRUE)
  full_markdown <- paste(
    "---",
    "title: \"Canonical Geo-Narrative\"",
    paste0("species: \"", sp, "\""),
    paste0("output_version: \"", output_version, "\""),
    paste0("checkover_version: \"", checkover_version, "\""),
    paste0("generated: \"", Sys.Date(), "\""),
    paste0("snapshot_date: \"", snapshot_date, "\""),
    "---",
    "",
    sec1, "", sec2, "", sec3, "", sec4, "", sec5$markdown,
    sep = "\n"
  )
  
  list(
    full_markdown             = full_markdown,
    formal_narrative_text     = sec5$text,
    formal_narrative_word_count = sec5$word_count,
    metadata = list(
      species           = sp,
      scenario          = scenario,
      vernacular        = vern_str,
      output_version    = output_version,
      checkover_version = checkover_version,
      generated_date    = as.character(Sys.Date()),
      snapshot_date     = snapshot_date
    )
  )
}


# ===========================================================================
# HELPER: Load version delta for one population type
# ===========================================================================
.load_version_delta <- function(sp_clean, pop_type, prev_reports_dir, current_report) {
  if (is.null(prev_reports_dir) || is.null(current_report)) return(NULL)
  
  prev_file <- file.path(prev_reports_dir, pop_type, paste0(sp_clean, ".json"))
  if (!file.exists(prev_file)) return(NULL)
  
  prev_report <- tryCatch(jsonlite::read_json(prev_file, simplifyVector = TRUE),
                          error = function(e) NULL)
  if (is.null(prev_report)) return(NULL)
  
  curr_n   <- as.integer(current_report$metrics$n_records %||% NA)
  prev_n   <- as.integer(prev_report$metrics$n_records    %||% NA)
  curr_eoo <- as.numeric(current_report$metrics$eoo_km2   %||% NA)
  prev_eoo <- as.numeric(prev_report$metrics$eoo_km2      %||% NA)
  curr_aoo <- as.numeric(current_report$metrics$aoo_km2   %||% NA)
  prev_aoo <- as.numeric(prev_report$metrics$aoo_km2      %||% NA)
  
  aoo_change_pct <- if (!is.na(curr_aoo) && !is.na(prev_aoo) && prev_aoo > 0)
    round((curr_aoo - prev_aoo) / prev_aoo * 100, 1) else NA_real_
  
  trend <- if (is.na(aoo_change_pct)) "unknown" else
    if (aoo_change_pct > 5) "increasing" else
      if (aoo_change_pct < -5) "decline" else "stable"
  
  list(
    prev_n          = prev_n,
    curr_n          = curr_n,
    new_records     = if (!is.na(curr_n) && !is.na(prev_n)) curr_n - prev_n else NA_integer_,
    prev_eoo        = prev_eoo,
    curr_eoo        = curr_eoo,
    prev_aoo        = prev_aoo,
    curr_aoo        = curr_aoo,
    aoo_change_pct  = aoo_change_pct,
    trend           = trend
  )
}


# ===========================================================================
# SECTION 1: TAXONOMIC IDENTITY
# ===========================================================================
.build_section1 <- function(sp, vern_by_lang, ind_report, nind_report) {
  report <- ind_report %||% nind_report
  
  lines <- c(
    "## 1. TAXONOMIC IDENTITY",
    "",
    paste0("- **Scientific name:** *", sp, "*")
  )
  
  # Higher taxonomy — now stored under report$taxonomy$higher_taxonomy
  higher_tax <- report$taxonomy$higher_taxonomy %||%
    report$higher_taxonomy          %||%   # legacy fallback
    report$taxonomy                %||% NULL
  # Ensure we have a scalar string, not a list
  if (is.list(higher_tax)) higher_tax <- higher_tax$higher_taxonomy %||% NULL
  higher_tax_str <- if (!is.null(higher_tax) && length(higher_tax) == 1 &&
                        !is.na(higher_tax) && nzchar(as.character(higher_tax)))
    as.character(higher_tax) else NULL
  
  if (!is.null(higher_tax_str)) {
    lines <- c(lines, paste0("- **Higher taxonomy:** ", higher_tax_str))
  } else {
    lines <- c(lines, "- **Higher taxonomy:** N/A")
  }
  
  lines <- c(lines, "")
  
  # Vernacular names (grouped by language)
  if (!is.null(vern_by_lang) && length(vern_by_lang) > 0) {
    lines <- c(lines, "### Common names", "")
    lines <- c(lines, .format_vernacular_block(vern_by_lang))
    lines <- c(lines, "")
  }
  
  # Type locality (if available)
  type_loc <- report$type_locality %||% NULL
  if (!is.null(type_loc)) {
    lines <- c(lines, "### Type locality", "")
    if (is.list(type_loc)) {
      if (!is.null(type_loc$geographic_unit))
        lines <- c(lines, paste0("- **Geographic unit:** ", type_loc$geographic_unit))
      if (!is.null(type_loc$protected_area) && nzchar(type_loc$protected_area))
        lines <- c(lines, paste0("- **Protected area:** ", type_loc$protected_area))
      if (!is.null(type_loc$reference))
        lines <- c(lines, paste0("- **Reference:** ", type_loc$reference))
    } else {
      lines <- c(lines, paste0("- **Locality:** ", as.character(type_loc)))
    }
    lines <- c(lines, "")
  }
  
  paste(lines, collapse = "\n")
}


# ===========================================================================
# SECTION 2: INDIGENOUS RANGE OVERVIEW
# ===========================================================================
.build_section2 <- function(sp, sp_ind, ind_report, scenario,
                            feow_map = NULL, hydrobasin_names = NULL) {
  
  if (scenario == 2 || is.null(ind_report) || is.null(sp_ind) || nrow(sp_ind) == 0) {
    return(paste(
      "## 2. INDIGENOUS RANGE OVERVIEW",
      "",
      "*Not applicable — no indigenous populations detected for this species.*",
      sep = "\n"
    ))
  }
  
  lines <- c("## 2. INDIGENOUS RANGE OVERVIEW", "")
  
  # --- 2.1 Distribution Summary ---
  lines <- c(lines, "### 2.1 Distribution Summary", "")
  
  category <- ind_report$metrics$iucn_category %||% "N/A"
  eoo_val  <- ind_report$metrics$eoo_km2       %||% NA
  aoo_val  <- ind_report$metrics$aoo_km2       %||% NA
  
  lines <- c(lines,
             paste0("- **Distribution category:** ", category),
             paste0("- **Area of occupancy (AOO):** ",
                    if (!is.na(aoo_val)) paste0(format(round(aoo_val, 0), big.mark = ","), " km\u00b2") else "N/A"),
             paste0("- **Extent of occurrence (EOO):** ",
                    if (!is.na(eoo_val)) paste0(format(round(eoo_val, 0), big.mark = ","), " km\u00b2") else "N/A"),
             ""
  )
  
  # --- Geographic distribution ---
  lines <- c(lines, "#### Geographic distribution", "")
  
  # Resolve basin and FEOW codes to human-readable names before tabulation
  basin_col <- if (!is.null(sp_ind)) .resolve_basin_col(sp_ind$hydrobasin, hydrobasin_names) else character(0)
  feow_col  <- if (!is.null(sp_ind)) .resolve_feow_col(sp_ind$freshwater_ecoregion, feow_map) else character(0)
  
  # Countries — frequency ordered
  cntry_ft <- .freq_table(sp_ind$country)
  n_countries <- nrow(cntry_ft)
  if (n_countries > 0) {
    cntry_str <- paste(paste0(cntry_ft$value, " (n=", cntry_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Countries (native):** ", cntry_str),
               paste0("  - Count: ", n_countries),
               "")
  }
  
  # Subnational admin units — frequency ordered
  admin_ft <- .freq_table(sp_ind$admin_1)
  n_admin  <- nrow(admin_ft)
  if (n_admin > 0) {
    admin_str <- paste(paste0(admin_ft$value, " (n=", admin_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Subnational administrative units (native):** ", admin_str),
               paste0("  - Count: ", n_admin),
               "")
  }
  
  # Hydrographic basins — frequency ordered, names resolved
  basin_ft <- .freq_table(basin_col)
  n_basins <- nrow(basin_ft)
  if (n_basins > 0) {
    basin_str <- paste(paste0(basin_ft$value, " (n=", basin_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Hydrographic basins (native):** ", basin_str),
               paste0("  - Count: ", n_basins),
               "")
  }
  
  # --- Conservation context ---
  lines <- c(lines, "#### Conservation context", "")
  
  n_prot      <- ind_report$conservation$n_protected_records %||% NA
  pct_prot    <- ind_report$conservation$protection_percentage %||% NA
  pa_ft       <- .freq_table(sp_ind$protected_area[!is.na(sp_ind$protected_area) & nzchar(sp_ind$protected_area)])
  n_pa_total  <- nrow(sp_ind)  # denominator for PA % per ecoregion uses total records
  
  if (!is.na(n_prot)) {
    pct_str <- if (!is.na(pct_prot)) paste0(" (", round(pct_prot, 1), "%)") else ""
    lines <- c(lines,
               paste0("- **Native records within protected areas:** ", n_prot, pct_str))
  }
  if (nrow(pa_ft) > 0) {
    pa_str <- paste(
      paste0(pa_ft$value, " (n=", pa_ft$n, ", ",
             round(pa_ft$n / nrow(sp_ind) * 100, 1), "%)"),
      collapse = "; ")
    lines <- c(lines,
               paste0("- **Protected areas represented:** ", pa_str))
  }
  lines <- c(lines, "")
  
  # --- Biogeographic context ---
  lines <- c(lines, "#### Biogeographic context", "")
  
  teow_ft   <- .freq_table(sp_ind$ecoregion)
  feow_ft   <- .freq_table(feow_col)
  n_rec_ind <- nrow(sp_ind)
  
  if (nrow(teow_ft) > 0) {
    teow_str <- paste(
      paste0(teow_ft$value, " (n=", teow_ft$n, ", ",
             round(teow_ft$n / n_rec_ind * 100, 1), "%)"),
      collapse = "; ")
    lines <- c(lines, paste0("- **Terrestrial ecoregions (TEOW):** ", teow_str))
  } else {
    lines <- c(lines, "- **Terrestrial ecoregions (TEOW):** N/A")
  }
  if (nrow(feow_ft) > 0) {
    feow_str <- paste(
      paste0(feow_ft$value, " (n=", feow_ft$n, ", ",
             round(feow_ft$n / n_rec_ind * 100, 1), "%)"),
      collapse = "; ")
    lines <- c(lines, paste0("- **Freshwater ecoregions (FEOW):** ", feow_str))
  } else {
    lines <- c(lines, "- **Freshwater ecoregions (FEOW):** N/A")
  }
  lines <- c(lines, "")
  
  # --- Temporal context ---
  lines <- c(lines, "#### Temporal context", "")
  
  yr_min <- ind_report$temporal$year_min %||% NA
  yr_max <- ind_report$temporal$year_max %||% NA
  rs     <- .recent_stats(sp_ind)
  
  lines <- c(lines,
             paste0("- **Temporal coverage:** ",
                    if (!is.na(yr_min) && !is.na(yr_max)) paste0(yr_min, "\u2013", yr_max) else "N/A"),
             paste0("- **Recent records (post-2000):** ",
                    if (!is.na(rs$n_recent))
                      paste0(rs$n_recent, " records (", rs$pct_recent, "%)")
                    else "N/A"),
             ""
  )
  
  # --- 2.2 Auto-Generated Contextual Statements ---
  lines <- c(lines, "### 2.2 Auto-Generated Contextual Statements", "")
  
  # Distribution category statement
  cat_stmt <- switch(tolower(as.character(category)),
                     endemic      = "The species is restricted to a limited geographic range.",
                     regional     = "The species is native across a broader regional extent.",
                     cosmopolitan = "The species\u2019 native range spans multiple continents.",
                     paste0("Distribution category: ", category, ".")
  )
  lines <- c(lines, paste0("- ", cat_stmt))
  
  # Protected area coverage statement
  if (!is.na(pct_prot)) {
    pa_stmt <- if (pct_prot < 25)
      "Only a minority of native occurrences fall within protected areas."
    else if (pct_prot >= 75)
      "Most native occurrences fall within protected areas."
    else NULL
    if (!is.null(pa_stmt)) lines <- c(lines, paste0("- ", pa_stmt))
  }
  
  # Temporal coverage statement
  yr_range <- if (!is.na(yr_min) && !is.na(yr_max)) yr_max - yr_min + 1 else NA_integer_
  if (!is.na(yr_range) && yr_range <= 20) {
    lines <- c(lines,
               "- Occurrence data span a limited temporal window.")
  }
  
  # Recent records statement
  if (!is.na(rs$pct_recent) && rs$pct_recent < 25) {
    lines <- c(lines,
               "- Most records predate 2000, suggesting outdated distributional knowledge.")
  }
  
  lines <- c(lines, "")
  
  # --- 2.3 Fragmentation Assessment ---
  frag <- ind_report$fragmentation
  
  if (!is.null(frag) && isTRUE(frag$computed)) {
    is_applicable <- !identical(tolower(as.character(category)), "cosmopolitan")
    if (is_applicable) {
      lines <- c(lines,
                 "### 2.3 Fragmentation Assessment (Indigenous Range Only)",
                 "",
                 "**Method summary:** Adaptive spatial clustering of indigenous localities in an equal-area projection.",
                 ""
      )
      
      if (identical(as.character(frag$status), "detected")) {
        n_cl   <- frag$n_clusters %||% "N/A"
        cl_sz  <- frag$cluster_sizes %||% frag$cluster_sizes_n %||% "N/A"
        lines  <- c(lines,
                    paste0("- **Fragmentation signal:** detected"),
                    paste0("- **Number of clusters:** ", n_cl),
                    paste0("- **Cluster composition:** ", cl_sz),
                    "- **Interpretation:** Native occurrences are distributed across multiple spatially disjunct clusters.",
                    "",
                    "> *This is a conservative descriptive signal and does not represent a formal connectivity or Red List assessment.*",
                    ""
        )
      } else {
        lines <- c(lines,
                   "- **Fragmentation signal:** not detected",
                   ""
        )
      }
    }
  }
  
  paste(lines, collapse = "\n")
}


# ===========================================================================
# SECTION 3: NON-INDIGENOUS RANGE OVERVIEW
# ===========================================================================
.build_section3 <- function(sp, sp_nind, nind_report, scenario,
                            feow_map = NULL, hydrobasin_names = NULL) {
  
  if (scenario == 1 || is.null(sp_nind) || nrow(sp_nind) == 0 || is.null(nind_report)) {
    return(paste(
      "## 3. NON-INDIGENOUS RANGE OVERVIEW",
      "",
      "*Not applicable \u2014 no non-indigenous populations detected for this species.*",
      "",
      "> Metrics in this section do not contribute to conservation status assessment.",
      sep = "\n"
    ))
  }
  
  lines <- c(
    "## 3. NON-INDIGENOUS RANGE OVERVIEW",
    "",
    "> Metrics in this section do not contribute to conservation status assessment.",
    ""
  )
  
  # --- 3.1 Status ---
  lines <- c(lines, "### 3.1 Status", "")
  
  ni_category <- nind_report$metrics$category %||% "N/A"
  
  orig_col <- intersect(c("origin", "occurrence_origin", "status", "population_type"), names(sp_nind))
  origins  <- if (length(orig_col) > 0) {
    vals <- unique(sp_nind[[orig_col[1]]])
    vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
    if (length(vals) > 0) paste(sort(vals), collapse = "; ") else "N/A"
  } else "N/A"
  
  lines <- c(lines,
             "- **Non-indigenous populations:** present",
             paste0("- **Distribution category:** ", ni_category),
             paste0("- **Occurrence origin(s):** ", origins),
             ""
  )
  
  # --- 3.2 Metrics ---
  lines <- c(lines, "### 3.2 Metrics", "")
  
  eoo_val <- nind_report$metrics$eoo_km2 %||% NA
  aoo_val <- nind_report$metrics$aoo_km2 %||% NA
  
  lines <- c(lines,
             paste0("- **Area of occupancy (AOO):** ",
                    if (!is.na(aoo_val)) paste0(format(round(aoo_val, 0), big.mark = ","), " km\u00b2") else "N/A"),
             paste0("- **Extent of occurrence (EOO):** ",
                    if (!is.na(eoo_val)) paste0(format(round(eoo_val, 0), big.mark = ","), " km\u00b2") else "N/A"),
             ""
  )
  
  # Resolve basin and FEOW codes to human-readable names before tabulation
  basin_col <- .resolve_basin_col(sp_nind$hydrobasin,           hydrobasin_names)
  feow_col  <- .resolve_feow_col(sp_nind$freshwater_ecoregion,  feow_map)
  
  # Countries — frequency ordered
  cntry_ft <- .freq_table(sp_nind$country)
  if (nrow(cntry_ft) > 0) {
    cntry_str <- paste(paste0(cntry_ft$value, " (n=", cntry_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Countries:** ", cntry_str),
               paste0("  - Count: ", nrow(cntry_ft)),
               "")
  }
  
  # Admin units
  admin_ft <- .freq_table(sp_nind$admin_1)
  if (nrow(admin_ft) > 0) {
    admin_str <- paste(paste0(admin_ft$value, " (n=", admin_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Subnational administrative units:** ", admin_str),
               paste0("  - Count: ", nrow(admin_ft)),
               "")
  }
  
  # Hydrographic basins — resolved, deduplicated count
  basin_ft <- .freq_table(basin_col)
  if (nrow(basin_ft) > 0) {
    basin_str <- paste(paste0(basin_ft$value, " (n=", basin_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Hydrographic basins:** ", basin_str),
               paste0("  - Count: ", nrow(basin_ft)),
               "")
  }
  
  # Protected areas
  pa_ft_nind    <- .freq_table(sp_nind$protected_area[!is.na(sp_nind$protected_area) & nzchar(sp_nind$protected_area)])
  n_rec_nind    <- nrow(sp_nind)
  if (nrow(pa_ft_nind) > 0) {
    pa_str_nind <- paste(
      paste0(pa_ft_nind$value, " (n=", pa_ft_nind$n, ", ",
             round(pa_ft_nind$n / n_rec_nind * 100, 1), "%)"),
      collapse = "; ")
    lines <- c(lines,
               paste0("- **Protected areas represented:** ", pa_str_nind),
               "")
  }
  
  # Biogeographic context
  teow_ft_nind <- .freq_table(sp_nind$ecoregion)
  feow_ft_nind <- .freq_table(feow_col)
  
  if (nrow(teow_ft_nind) > 0 || nrow(feow_ft_nind) > 0) {
    if (nrow(teow_ft_nind) > 0) {
      teow_str <- paste(
        paste0(teow_ft_nind$value, " (n=", teow_ft_nind$n, ", ",
               round(teow_ft_nind$n / n_rec_nind * 100, 1), "%)"),
        collapse = "; ")
      lines <- c(lines, paste0("- **Terrestrial ecoregions (TEOW):** ", teow_str))
    } else {
      lines <- c(lines, "- **Terrestrial ecoregions (TEOW):** N/A")
    }
    if (nrow(feow_ft_nind) > 0) {
      feow_str <- paste(
        paste0(feow_ft_nind$value, " (n=", feow_ft_nind$n, ", ",
               round(feow_ft_nind$n / n_rec_nind * 100, 1), "%)"),
        collapse = "; ")
      lines <- c(lines, paste0("- **Freshwater ecoregions (FEOW):** ", feow_str))
    } else {
      lines <- c(lines, "- **Freshwater ecoregions (FEOW):** N/A")
    }
    lines <- c(lines, "")
  }
  
  # Temporal context
  yr_min <- nind_report$temporal$year_min %||% NA
  yr_max <- nind_report$temporal$year_max %||% NA
  
  lines <- c(lines,
             paste0("- **Temporal coverage:** ",
                    if (!is.na(yr_min) && !is.na(yr_max)) paste0(yr_min, "\u2013", yr_max) else "N/A"))
  
  if (!is.null(sp_nind) && "year" %in% names(sp_nind)) {
    yrs      <- suppressWarnings(as.integer(sp_nind$year))
    first_yr <- min(yrs, na.rm = TRUE)
    if (is.finite(first_yr))
      lines <- c(lines, paste0("- **First record year:** ", first_yr))
  }
  
  lines <- c(lines, "")
  
  paste(lines, collapse = "\n")
}


# ===========================================================================
# SECTION 4: DATA QUALITY, TRACEABILITY, AND PROVENANCE
# ===========================================================================
.build_section4 <- function(sp, sp_ind, sp_nind, ind_report, nind_report,
                            checkover_version, output_version, snapshot_date,
                            delta_ind, delta_nind, curr_version_label) {
  
  is_versioned <- !grepl("^v?1\\.0$", output_version, ignore.case = TRUE)
  
  n_ind  <- if (!is.null(sp_ind))  nrow(sp_ind)  else
    if (!is.null(ind_report))  as.integer(ind_report$metrics$n_records  %||% 0) else 0L
  n_nind <- if (!is.null(sp_nind)) nrow(sp_nind) else
    if (!is.null(nind_report)) as.integer(nind_report$metrics$n_records %||% 0) else 0L
  n_total <- n_ind + n_nind
  
  lines <- c(
    "## 4. DATA QUALITY, TRACEABILITY, AND PROVENANCE",
    ""
  )
  
  # --- 4.1 Data Summary ---
  # Accuracy and data quality — computed from raw data
  all_acc <- c(if (!is.null(sp_ind))  sp_ind$accuracy  else character(0),
               if (!is.null(sp_nind)) sp_nind$accuracy else character(0))
  n_high_acc  <- sum(!is.na(all_acc) & tolower(all_acc) == "exact",  na.rm = TRUE)
  n_low_acc   <- sum(!is.na(all_acc) & tolower(all_acc) == "locality", na.rm = TRUE)
  n_known_acc <- sum(!is.na(all_acc) & tolower(all_acc) != "unknown", na.rm = TRUE)
  dq_score    <- if (n_total > 0) round(n_high_acc / n_total * 100, 1) else NA_real_
  dq_str      <- if (!is.na(dq_score))
    paste0(dq_score, "% high-accuracy records (", n_high_acc, "/", n_total,
           "; low-accuracy: ", n_low_acc, ")")
  else "N/A"
  
  # Bibliography — DOIs and citations from raw data
  all_doi  <- c(if (!is.null(sp_ind))  sp_ind$doi      else character(0),
                if (!is.null(sp_nind)) sp_nind$doi      else character(0))
  all_cite <- c(if (!is.null(sp_ind))  sp_ind$citation  else character(0),
                if (!is.null(sp_nind)) sp_nind$citation else character(0))
  valid_doi  <- sort(unique(all_doi [!is.na(all_doi)  & nzchar(all_doi)]))
  valid_cite <- sort(unique(all_cite[!is.na(all_cite) & nzchar(all_cite)]))
  n_refs     <- length(unique(c(valid_doi, valid_cite)))
  doi_str    <- if (length(valid_doi) > 0) paste(valid_doi, collapse = "; ") else "none"
  
  lines <- c(lines,
             "### 4.1 Data summary", "",
             paste0("- **Total records analyzed:** ", format(n_total, big.mark = ",")),
             paste0("  - **Indigenous records:** ",     format(n_ind,   big.mark = ",")),
             paste0("  - **Non-indigenous records:** ", format(n_nind,  big.mark = ",")),
             paste0("- **High-accuracy records:** ", n_high_acc, " (", round(n_high_acc/max(n_total,1)*100,1), "%)"),
             paste0("- **Data quality score:** ", dq_str),
             ""
  )
  
  # --- 4.2 Version delta: indigenous range (v1.1+) ---
  if (is_versioned && !is.null(delta_ind)) {
    prev_version <- "previous version"
    lines <- c(lines,
               "### 4.2 Version delta: indigenous range", "",
               paste0("- **Current version:** ", output_version, ", ", Sys.Date()),
               paste0("- **Previous version:** ", prev_version),
               paste0("- **New presence records added:** ",
                      if (!is.na(delta_ind$new_records)) delta_ind$new_records else "N/A"),
               paste0("- **Record count change:** ",
                      if (!is.na(delta_ind$prev_n) && !is.na(delta_ind$curr_n))
                        paste0(delta_ind$prev_n, " \u2192 ", delta_ind$curr_n,
                               " (net: ", ifelse(delta_ind$curr_n >= delta_ind$prev_n, "+", ""),
                               delta_ind$curr_n - delta_ind$prev_n, ")")
                      else "N/A"),
               paste0("- **EOO:** ",
                      if (!is.na(delta_ind$prev_eoo) && !is.na(delta_ind$curr_eoo))
                        paste0(format(round(delta_ind$prev_eoo, 0), big.mark = ","), " \u2192 ",
                               format(round(delta_ind$curr_eoo, 0), big.mark = ","), " km\u00b2")
                      else "N/A"),
               paste0("- **AOO:** ",
                      if (!is.na(delta_ind$prev_aoo) && !is.na(delta_ind$curr_aoo))
                        paste0(format(round(delta_ind$prev_aoo, 0), big.mark = ","), " \u2192 ",
                               format(round(delta_ind$curr_aoo, 0), big.mark = ","), " km\u00b2",
                               if (!is.na(delta_ind$aoo_change_pct))
                                 paste0(" (", ifelse(delta_ind$aoo_change_pct >= 0, "+", ""),
                                        delta_ind$aoo_change_pct, "%)")
                               else "")
                      else "N/A"),
               paste0("- **Current trend:** ", delta_ind$trend),
               "",
               "> *Trend reflects data updates (new records, refined coordinates, documented extinctions)*",
               "> *and may not indicate biological range change.*",
               ""
    )
  } else if (is_versioned) {
    lines <- c(lines,
               "### 4.2 Version delta: indigenous range", "",
               "*No previous version available for comparison.*",
               ""
    )
  }
  
  # --- 4.3 Version delta: non-indigenous range (v1.1+) ---
  if (is_versioned && !is.null(nind_report)) {
    if (!is.null(delta_nind)) {
      lines <- c(lines,
                 "### 4.3 Version delta: non-indigenous range", "",
                 paste0("- **New presence records added:** ",
                        if (!is.na(delta_nind$new_records)) delta_nind$new_records else "N/A"),
                 paste0("- **Record count change:** ",
                        if (!is.na(delta_nind$prev_n) && !is.na(delta_nind$curr_n))
                          paste0(delta_nind$prev_n, " \u2192 ", delta_nind$curr_n,
                                 " (net: ", ifelse(delta_nind$curr_n >= delta_nind$prev_n, "+", ""),
                                 delta_nind$curr_n - delta_nind$prev_n, ")")
                        else "N/A"),
                 paste0("- **EOO:** ",
                        if (!is.na(delta_nind$prev_eoo) && !is.na(delta_nind$curr_eoo))
                          paste0(format(round(delta_nind$prev_eoo, 0), big.mark = ","), " \u2192 ",
                                 format(round(delta_nind$curr_eoo, 0), big.mark = ","), " km\u00b2")
                        else "N/A"),
                 paste0("- **AOO:** ",
                        if (!is.na(delta_nind$prev_aoo) && !is.na(delta_nind$curr_aoo))
                          paste0(format(round(delta_nind$prev_aoo, 0), big.mark = ","), " \u2192 ",
                                 format(round(delta_nind$curr_aoo, 0), big.mark = ","), " km\u00b2",
                                 if (!is.na(delta_nind$aoo_change_pct))
                                   paste0(" (", ifelse(delta_nind$aoo_change_pct >= 0, "+", ""),
                                          delta_nind$aoo_change_pct, "%)")
                                 else "")
                        else "N/A"),
                 paste0("- **Current trend:** ", delta_nind$trend),
                 "",
                 "> *Trend reflects data updates and may not indicate biological invasion dynamics.*",
                 ""
      )
    } else {
      lines <- c(lines,
                 "### 4.3 Version delta: non-indigenous range", "",
                 "*No previous non-indigenous version available for comparison.*",
                 ""
      )
    }
  }
  
  # --- 4.4 Bibliographic coverage ---
  lines <- c(lines,
             "### 4.4 Bibliographic coverage", "",
             paste0("- **Total references:** ", n_refs),
             paste0("- **DOI-linked references:** ", doi_str),
             "- **Bibliography handling:** Full, de-duplicated bibliography of raw data sources",
             "  provided as a separate artifact and linked to this geo-narrative output.",
             ""
  )
  
  # --- 4.5 Raw data provenance ---
  lines <- c(lines,
             "### 4.5 Raw data provenance", "",
             "- **Raw data repository:** World of Crayfish\u00ae",
             "- **Raw data citation:**",
             "  Ion et al. (2024)",
             "  *World of Crayfish\u2122: A web platform towards real-time global mapping of freshwater crayfish and their pathogens*",
             "  *PeerJ 12. e18229*",
             "  https://doi.org/10.7717/peerj.18229",
             ""
  )
  
  # --- 4.6 Processing framework provenance ---
  lines <- c(lines,
             "### 4.6 Processing framework provenance", "",
             paste0("- **Processing date:** ", Sys.Date()),
             paste0("- **Data snapshot date:** ", snapshot_date),
             ""
  )
  
  # --- 4.7 Interpretation note (numbered as 4.6 in template; kept here as final sub) ---
  lines <- c(lines,
             "### 4.7 Interpretation note", "",
             "This geo-narrative is a **descriptive synthesis** derived from curated occurrence data and",
             "standardized spatial analyses. It does **not** constitute a formal conservation status",
             "assessment and should be interpreted in conjunction with primary sources and expert evaluation.",
             ""
  )
  
  paste(lines, collapse = "\n")
}


# ===========================================================================
# SECTION 5: FORMAL NARRATIVE SUMMARY
# ===========================================================================
.build_section5 <- function(sp, scenario, vern_by_lang, sp_ind, sp_nind,
                            ind_report, nind_report, delta_ind, delta_nind,
                            output_version, checkover_version = "1.0",
                            feow_map = NULL, hydrobasin_names = NULL) {
  
  is_versioned <- !grepl("^v?1\\.0$", output_version, ignore.case = TRUE)
  
  paragraphs <- character(0)
  
  # --- Paragraph 1: Taxonomic identity and distributional category (50-80 words) ---
  paragraphs <- c(paragraphs, .p1_taxonomy(sp, vern_by_lang, ind_report, nind_report))
  
  # --- Paragraph 2: Indigenous range — geography and biogeography (100-120 words) ---
  if (!is.null(ind_report) && (scenario == 1 || scenario == 3)) {
    paragraphs <- c(paragraphs,
                    .p2_indigenous_range(sp, sp_ind, ind_report, feow_map, hydrobasin_names))
  }
  
  # --- Paragraph 3: Conservation context (80-120 words) ---
  if (!is.null(ind_report) && (scenario == 1 || scenario == 3)) {
    paragraphs <- c(paragraphs, .p3_conservation(sp, sp_ind, ind_report, delta_ind, is_versioned, output_version))
  }
  
  # --- Type locality note (if applicable) ---
  if (!is.null(sp_ind) && "is_type_locality" %in% names(sp_ind) &&
      any(sp_ind$is_type_locality == TRUE, na.rm = TRUE)) {
    paragraphs <- c(paragraphs,
                    paste0("The type locality of *", sp, "* is documented within the World of Crayfish\u00ae database ",
                           "and has been included in the spatial analyses presented in this geo-narrative."))
  } else if (!is.null(sp_nind) && "is_type_locality" %in% names(sp_nind) &&
             any(sp_nind$is_type_locality == TRUE, na.rm = TRUE)) {
    paragraphs <- c(paragraphs,
                    paste0("The type locality of *", sp, "* is documented within the World of Crayfish\u00ae database ",
                           "and has been included in the spatial analyses presented in this geo-narrative."))
  }
  
  # --- Paragraph 4: Non-indigenous populations (50-70 words, if present) ---
  if (!is.null(nind_report) && (scenario == 2 || scenario == 3)) {
    paragraphs <- c(paragraphs, .p4_non_indigenous(sp, sp_nind, nind_report, delta_nind, is_versioned, output_version))
  }
  
  # --- Paragraph 5: Data quality, provenance, and interpretation (80-100 words) ---
  paragraphs <- c(paragraphs, .p5_provenance(sp, sp_ind, sp_nind, ind_report, nind_report,
                                             output_version, checkover_version, is_versioned))
  
  full_text  <- paste(paragraphs, collapse = "\n\n")
  word_count <- length(strsplit(gsub("\\*\\*|\\*", "", full_text), "\\s+")[[1]])
  
  list(
    markdown   = paste("## 5. FORMAL NARRATIVE SUMMARY", "",
                       full_text, sep = "\n"),
    text       = full_text,
    word_count = word_count
  )
}


# ---------------------------------------------------------------------------
# Paragraph 1: Taxonomic identity and distributional category
# ---------------------------------------------------------------------------
.p1_taxonomy <- function(sp, vern_by_lang, ind_report, nind_report) {
  report   <- ind_report %||% nind_report
  
  category <- if (!is.null(ind_report))
    ind_report$metrics$iucn_category %||% "unresolved"
  else
    nind_report$metrics$category     %||% "unresolved"
  
  aoo_val  <- if (!is.null(ind_report)) ind_report$metrics$aoo_km2 %||% NA else NA
  
  higher   <- report$taxonomy$higher_taxonomy %||%
    report$higher_taxonomy          %||% NULL
  if (is.list(higher)) higher <- higher$higher_taxonomy %||% NULL
  tax_str  <- if (!is.null(higher) && nzchar(as.character(higher)))
    paste0(" (", as.character(higher), ")")
  else ""
  
  type_cat  <- switch(tolower(as.character(category)),
                      endemic      = "**endemic**",
                      regional     = "**regional**",
                      cosmopolitan = "**cosmopolitan**",
                      paste0("**", category, "**"))
  
  # Vernacular — render as "name (Language)" pairs joined by " and "
  # Up to 2 languages; within each language, up to 2 names separated by ", "
  vern_phrase <- ""
  if (!is.null(vern_by_lang) && length(vern_by_lang) > 0) {
    v_items <- head(names(vern_by_lang), 2)
    v_parts <- vapply(v_items, function(lang) {
      names_str <- paste(head(vern_by_lang[[lang]], 2), collapse = ", ")
      paste0(names_str, " (", lang, ")")
    }, character(1))
    vern_phrase <- paste0(", commonly known as the ",
                          paste(v_parts, collapse = " and "))
  }
  
  # AOO phrase — wording depends on distributional category
  aoo_phrase <- if (!is.na(aoo_val)) {
    aoo_str <- paste0(format(round(aoo_val, 0), big.mark = ","), " km\u00b2")
    switch(tolower(as.character(category)),
           endemic = paste0(" With a native area of occupancy of ", aoo_str, ",",
                            " its limited geographic footprint places it among",
                            " restricted-range taxa of conservation concern."),
           regional = paste0(" With a native area of occupancy of ", aoo_str, ",",
                             " the species occupies a moderately restricted regional range."),
           cosmopolitan = paste0(" Its native area of occupancy spans ", aoo_str,
                                 " across a broad, multi-continental range."),
           paste0(" With a native area of occupancy of ", aoo_str, ".")
    )
  } else ""
  
  # Use "an" before vowel-starting categories (endemic)
  article <- if (tolower(substr(as.character(category), 1, 1)) %in% c("a","e","i","o","u"))
    "an" else "a"
  
  paste0(
    "*", sp, "*", tax_str, vern_phrase, ", is ", article, " ", type_cat,
    " freshwater species.", aoo_phrase
  )
}


# ---------------------------------------------------------------------------
# Paragraph 2: Indigenous range — geography and biogeography
# ---------------------------------------------------------------------------
.p2_indigenous_range <- function(sp, sp_ind, ind_report,
                                 feow_map = NULL, hydrobasin_names = NULL) {
  n_countries <- ind_report$counts$n_countries  %||% NA
  
  # Resolve codes to human-readable names before tabulation
  basin_col <- if (!is.null(sp_ind)) .resolve_basin_col(sp_ind$hydrobasin,           hydrobasin_names) else character(0)
  feow_col  <- if (!is.null(sp_ind)) .resolve_feow_col(sp_ind$freshwater_ecoregion,  feow_map)         else character(0)
  
  # Top basins (up to 3 by frequency) — use resolved, deduplicated count as the headline number
  basin_ft   <- if (!is.null(sp_ind)) .freq_table(basin_col) else data.frame(value = character(0), n = integer(0))
  n_basins   <- nrow(basin_ft)   # post-resolution unique count (replaces ind_report$counts$n_hydrobasins)
  top_basins <- head(basin_ft$value, 3)
  
  # Top countries (up to 3 by frequency)
  cntry_ft    <- if (!is.null(sp_ind)) .freq_table(sp_ind$country) else data.frame(value = character(0), n = integer(0))
  top_countries <- head(cntry_ft$value, 3)
  
  # Ecoregions (resolved)
  teow_vals <- if (!is.null(sp_ind)) .freq_order(sp_ind$ecoregion) else character(0)
  feow_vals <- if (!is.null(sp_ind)) .freq_order(feow_col)          else character(0)
  
  eoo_val   <- ind_report$metrics$eoo_km2 %||% NA
  aoo_val   <- ind_report$metrics$aoo_km2 %||% NA
  
  yr_min    <- ind_report$temporal$year_min %||% NA
  yr_max    <- ind_report$temporal$year_max %||% NA
  rs        <- .recent_stats(sp_ind)
  
  # --- Build sentence components ---
  # Basin phrase: "20 hydrographic basins, including Glenelg River, ..."
  # Only list top-3 by name; the headline count already conveys the total.
  basin_phrase <- if (length(top_basins) > 0) {
    incl_suffix <- if (!is.na(n_basins) && n_basins > length(top_basins))
      paste0(", including ", paste(top_basins, collapse = ", "))
    else ""   # All basins fit in top_basins (n_basins <= 3) — no need for "including"
    paste0("**", n_basins, " hydrographic basin",
           if (!is.na(n_basins) && n_basins != 1) "s" else "",
           "**", incl_suffix)
  } else if (!is.na(n_basins)) {
    paste0(n_basins, " hydrographic basin", if (n_basins != 1) "s" else "")
  } else "an undetermined number of hydrographic basins"
  
  country_phrase <- if (length(top_countries) > 0) {
    more_str <- if (!is.na(n_countries) && n_countries > 3)
      paste0(" and ", n_countries - 3, " other",
             if (n_countries - 3 > 1) "s" else "") else ""
    paste(top_countries, collapse = ", ")
  } else "multiple countries"
  
  teow_phrase <- if (length(teow_vals) > 0) {
    vals <- head(teow_vals, 3)
    if (length(vals) == 1) vals
    else paste0(paste(vals[-length(vals)], collapse = ", "), ", and ", vals[length(vals)])
  } else "undetermined terrestrial ecoregion(s)"
  
  feow_phrase <- if (length(feow_vals) > 0) {
    vals <- head(feow_vals, 2)
    if (length(vals) == 1) vals
    else paste0(vals[1], " and ", vals[2])
  } else "undetermined freshwater ecoregion(s)"
  
  metrics_phrase <- if (!is.na(aoo_val) && !is.na(eoo_val))
    paste0("Spatial metrics yield an AOO of ",
           format(round(aoo_val, 0), big.mark = ","), " km\u00b2 and an EOO of ",
           format(round(eoo_val, 0), big.mark = ","), " km\u00b2.")
  else ""
  
  temporal_phrase <- if (!is.na(yr_min) && !is.na(yr_max)) {
    recent_note <- if (!is.na(rs$pct_recent))
      paste0(", with ", rs$pct_recent, "% of records post-2000, indicating ",
             if (rs$pct_recent >= 25) "adequate" else "limited", " recent survey coverage")
    else ""
    paste0("Validated records span **", yr_min, "\u2013", yr_max, "**", recent_note, ".")
  } else ""
  
  paste0(
    "Native occurrences span ", basin_phrase, ", recorded in ",
    country_phrase, ". The species inhabits the **", teow_phrase,
    "** terrestrial ecoregion(s), and is associated with the **", feow_phrase,
    "** freshwater ecoregion(s). ", metrics_phrase, " ", temporal_phrase
  )
}


# ---------------------------------------------------------------------------
# Paragraph 3: Conservation context
# ---------------------------------------------------------------------------
.p3_conservation <- function(sp, sp_ind, ind_report, delta_ind, is_versioned, output_version) {
  prot_pct <- ind_report$conservation$protection_percentage %||% NA
  prot_n   <- ind_report$conservation$n_protected_records   %||% NA
  
  # Protected area coverage
  pa_phrase <- if (!is.na(prot_pct) && !is.na(prot_n))
    paste0(round(prot_pct, 1), "% of native records (n=", prot_n, ") fall within designated protected areas.")
  else
    "Protected area coverage data are unavailable."
  
  # Fragmentation
  frag     <- ind_report$fragmentation
  frag_str <- ""
  if (!is.null(frag) && isTRUE(frag$computed) && identical(as.character(frag$status), "detected")) {
    n_cl     <- frag$n_clusters %||% "multiple"
    frag_str <- paste0(" Distributional analysis reveals **fragmentation**: native occurrences",
                       " form ", n_cl, " spatially disjunct cluster",
                       if (!is.na(as.integer(n_cl)) && as.integer(n_cl) != 1) "s" else "", ".")
  }
  
  # Version delta trend (v1.1+)
  trend_str <- ""
  if (is_versioned && !is.null(delta_ind)) {
    trend_str <- switch(delta_ind$trend,
                        stable     = paste0(" **Native range extent remains stable** in current version (", output_version, ")."),
                        increasing = paste0(" **Native range shows expansion** in current version (", output_version,
                                            "), driven by ", if (!is.na(delta_ind$new_records)) delta_ind$new_records else "new",
                                            " new presence records."),
                        decline    = paste0(" **Native range exhibits decline** in current version (", output_version,
                                            "), with AOO reduction of ",
                                            if (!is.na(delta_ind$aoo_change_pct)) abs(delta_ind$aoo_change_pct) else "undetermined",
                                            "%."),
                        ""
    )
  }
  
  paste0(pa_phrase, frag_str, trend_str)
}


# ---------------------------------------------------------------------------
# Paragraph 4: Non-indigenous populations
# ---------------------------------------------------------------------------
.p4_non_indigenous <- function(sp, sp_nind, nind_report, delta_nind, is_versioned, output_version) {
  n_countries <- nind_report$counts$n_countries %||% NA
  category    <- nind_report$metrics$category   %||% "unknown"
  
  # Country list (top 3)
  cntry_ft <- if (!is.null(sp_nind)) .freq_table(sp_nind$country) else data.frame(value = character(0), n = integer(0))
  top_countries <- head(cntry_ft$value, 3)
  
  country_phrase <- if (length(top_countries) > 0) {
    more_str <- if (!is.na(n_countries) && n_countries > 3)
      paste0(" and ", n_countries - 3, " other",
             if (n_countries - 3 > 1) "s" else "") else ""
    paste0(paste(top_countries, collapse = ", "), more_str)
  } else if (!is.na(n_countries)) {
    paste0(n_countries, " countr", if (n_countries != 1) "ies" else "y")
  } else "undetermined countries"
  
  # First record year
  first_yr <- NULL
  if (!is.null(sp_nind) && "year" %in% names(sp_nind)) {
    yrs <- suppressWarnings(as.integer(sp_nind$year))
    fy  <- min(yrs, na.rm = TRUE)
    if (is.finite(fy)) first_yr <- fy
  } else if (!is.null(nind_report$temporal$year_min)) {
    first_yr <- as.integer(nind_report$temporal$year_min)
  }
  
  first_yr_phrase <- if (!is.null(first_yr))
    paste0(" (first record: ", first_yr, ")")
  else ""
  
  # Version delta trend
  trend_str <- ""
  if (is_versioned && !is.null(delta_nind)) {
    trend_str <- switch(delta_nind$trend,
                        stable     = paste0(" **Alien range extent remains stable** (", output_version, ")."),
                        increasing = paste0(" **Alien populations continue expanding** (", output_version,
                                            "), with ",
                                            if (!is.na(delta_nind$aoo_change_pct)) paste0(delta_nind$aoo_change_pct, "%") else "undetermined",
                                            " AOO increase driven by ",
                                            if (!is.na(delta_nind$new_records)) delta_nind$new_records else "new",
                                            " new records."),
                        decreasing = paste0(" **Alien range shows contraction** (", output_version,
                                            "), possibly reflecting eradication or survey effort."),
                        ""
    )
  }
  
  paste0(
    "Outside its native range, non-indigenous populations classified as **", category,
    "** have been documented in ", country_phrase, first_yr_phrase, ".", trend_str
  )
}


# ---------------------------------------------------------------------------
# Paragraph 5: Data quality, provenance, and interpretation
# ---------------------------------------------------------------------------
.p5_provenance <- function(sp, sp_ind, sp_nind, ind_report, nind_report,
                           output_version, checkover_version = "1.0", is_versioned) {
  n_ind  <- if (!is.null(sp_ind))  nrow(sp_ind)  else as.integer(ind_report$metrics$n_records  %||% 0)
  n_nind <- if (!is.null(sp_nind)) nrow(sp_nind) else as.integer(nind_report$metrics$n_records %||% 0)
  n_total <- n_ind + n_nind
  
  # Version prefix for v1.1+
  version_prefix <- if (is_versioned)
    paste0("This synthesis represents **version ", sub("^v", "", output_version),
           "** (", Sys.Date(), "), updating the previous assessment. ")
  else ""
  
  paste0(
    version_prefix,
    "This synthesis is based on **",
    format(n_total, big.mark = ","),
    " validated occurrence records** (",
    format(n_ind,  big.mark = ","), " indigenous, ",
    format(n_nind, big.mark = ","), " non-indigenous). ",
    "Bibliographic coverage statistics and high-accuracy record percentages are not yet ",
    "tracked at per-species level in this version. ",
    "Analysis processed on ",
    as.character(Sys.Date()), ". ",
    "Raw data are maintained in the World of Crayfish\u00ae repository (Ion et al. 2024). ",
    "**This geo-narrative is a descriptive synthesis and does not constitute a formal IUCN Red List assessment.**"
  )
}


# ===========================================================================
# NULL coalescing operator (safe to re-define if not already in helpers)
# ===========================================================================
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}