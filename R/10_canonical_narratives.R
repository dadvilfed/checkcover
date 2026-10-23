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
#' @param ctx                    RunContext: the version delta reads the taxon's
#'                               previous package and the input snapshots through it
#'                               (NULL = no delta)
#' @param citations              Module 7's result; section 4.4 states each
#'                               bibliography's own figures (NULL = no bibliography)
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
                                          ctx                = NULL,
                                          citations          = NULL) {
  
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
      
      # --- Fetch per-species subsets, FILTERED TO ACTIVE RECORDS ---
      # SINGLE SOURCE OF TRUTH: the per-species JSON report (Module 3C/4C) is
      # computed over the active set (temporal_status == "active"; extinct +
      # suppressed excluded). The narrative must enumerate over that SAME set, or
      # every recomputed count (records, basins, post-2000 %, PA %) drifts from
      # package_metadata.json. Filtering here is the root-cause fix for the
      # "duplicated path that diverged" bugs (Lucian, 2026-06).
      sp_ind  <- if (!is.null(ind_data)  && nrow(ind_data)  > 0) ind_data[ind_data$species   == sp, ] else NULL
      sp_nind <- if (!is.null(nind_data) && nrow(nind_data) > 0) nind_data[nind_data$species == sp, ] else NULL
      sp_ind  <- .active_only(sp_ind)
      sp_nind <- .active_only(sp_nind)
      
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
        citations         = citations$citations[[sp]],
        sp_clean          = sp_clean,
        ctx               = ctx,
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
  entries <- trimws(strsplit(vern_str, "\\s*\\|\\s*")[[1]])
  entries <- entries[nzchar(entries)]
  if (length(entries) == 0) return(NULL)

  # GBIF trailing-language-code convention: a "(code)" applies to the entire
  # PRECEDING RUN of same-language names, but only the LAST name in the run
  # actually carries the tag. Example:
  #   "... | ウチダザリガニ | タンカイザリガニ | シグナルザリガニ (jpn) | ..."
  # The (jpn) tag covers all three Japanese names; the first two are untagged.
  # The old parser defaulted untagged names to English, which dumped Japanese
  # (and other) names into the English bucket (Lucian, 2026, vernacular bug).
  #
  # Fix: parse each entry into (name, code); then forward-fill so every untagged
  # name inherits the NEXT tagged code (its run's trailing code). Only trailing
  # untagged names with NO following tag fall back to English.
  names_vec <- character(length(entries))
  codes_vec <- rep(NA_character_, length(entries))
  for (i in seq_along(entries)) {
    e <- entries[i]
    if (grepl("\\(([a-z]{2,3})\\)$", e)) {
      codes_vec[i] <- sub(".*\\(([a-z]{2,3})\\)$", "\\1", e)
      names_vec[i] <- trimws(sub("\\s*\\([a-z]{2,3}\\)$", "", e))
    } else {
      names_vec[i] <- e
    }
  }

  # Inherit the run's trailing code (scan right-to-left, carrying the next code
  # seen). Anything still untagged (trailing run with no code) -> English.
  next_code <- NA_character_
  for (i in rev(seq_along(codes_vec))) {
    if (!is.na(codes_vec[i])) next_code <- codes_vec[i] else codes_vec[i] <- next_code
  }
  codes_vec[is.na(codes_vec)] <- "eng"

  result <- list()
  for (i in seq_along(names_vec)) {
    nm <- trimws(names_vec[i])
    if (!nzchar(nm)) next
    code <- codes_vec[i]
    lang <- if (!is.na(iso_lang[code])) iso_lang[code] else code
    result[[lang]] <- unique(c(result[[lang]], nm))
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
# HELPER: Restrict a per-species occurrence slice to the ACTIVE set
# Mirrors the filter in Module 3C/4C so the narrative enumerates the exact
# record set that produced package_metadata.json. Returns NULL unchanged; if
# there is no temporal_status column (pre-temporal data) the slice is returned
# as-is (all records are implicitly active).
# ===========================================================================
.active_only <- function(sp_df) {
  if (is.null(sp_df) || nrow(sp_df) == 0L) return(sp_df)
  if (!"temporal_status" %in% names(sp_df)) return(sp_df)
  sp_df[!is.na(sp_df$temporal_status) & sp_df$temporal_status == "active", , drop = FALSE]
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
# Both helpers drop NA/blank AND the `unresolved` geography sentinel, so a
# record the fallback could not resolve is never named or counted in a
# narrative (Lucian, 2026-07).
.drop_unusable <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (exists("is_geo_unresolved", mode = "function")) x[!is_geo_unresolved(x)] else x
}

.freq_order <- function(x) {
  x <- .drop_unusable(x)
  if (length(x) == 0) return(character(0))
  tb  <- sort(table(x), decreasing = TRUE)
  names(tb)
}

# Returns a data.frame with columns: value, n
.freq_table <- function(x) {
  x <- .drop_unusable(x)
  if (length(x) == 0) return(data.frame(value = character(0), n = integer(0)))
  tb <- sort(table(x), decreasing = TRUE)
  data.frame(value = names(tb), n = as.integer(tb), stringsAsFactors = FALSE)
}

#' EOO as the narrative prints it: the area, or why it is undefined.
#'
#' @param report A per-species report (Module 3C/4C), or NULL.
.eoo_text <- function(report) {
  eoo <- suppressWarnings(as.numeric(report$metrics$eoo_km2 %||% NA))
  if (length(eoo) == 1L && is.finite(eoo))
    return(paste0(format(round(eoo, 0), big.mark = ","), " km²"))
  switch(as.character(report$metrics$eoo_status %||% ""),
         fewer_than_3_localities = "undefined (fewer than 3 distinct localities)",
         exceeds_hemisphere = paste0("undefined (the localities do not fit in one hemisphere, ",
                                     "so no convex hull exists)"),
         "N/A")
}

#' Frequency table over a PIPE-JOINED multi-value column.
#'
#' Several enrichment columns hold every match for a record, joined with " | " —
#' protected_area is the clearest case, where one occurrence can sit inside a
#' national park that is itself inside a larger reserve.
#'
#' .freq_table() would count "Park A | Reserve B" as a category distinct from
#' "Park A" and from "Reserve B", fragmenting the list into compound entries.
#' That was mostly hidden while WDPA geometries were cleaned with
#' erase_overlaps = TRUE, because dissolving overlaps left at most one match per
#' record. Turning that off (CONFIG$spatial$wdpa_erase_overlaps, 2026-09) makes
#' multi-match cells normal, so the split has to happen here.
#'
#' The per-record report module already splits on the same separator for
#' n_distinct_protected_areas; this brings the narrative into line with it.
#'
#' NB a record counted under two areas contributes to both, so percentages over
#' the record total can legitimately sum to more than 100%.
.freq_table_multi <- function(x, sep = "\\s*\\|\\s*") {
  x <- .drop_unusable(x)
  if (length(x) == 0) return(data.frame(value = character(0), n = integer(0)))
  parts <- unlist(strsplit(as.character(x), sep))
  parts <- trimws(parts)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return(data.frame(value = character(0), n = integer(0)))
  tb <- sort(table(parts), decreasing = TRUE)
  data.frame(value = names(tb), n = as.integer(tb), stringsAsFactors = FALSE)
}


# (.fmt_n() and .fmt_pct() used to be defined here as well, but nothing in this
# module called them, and 12_temporal_outputs.R, sourced later, defines its own
# signed-delta versions, which silently replaced these. Removed: one definition
# per name — tests/test_single_definitions.R.)


# ===========================================================================
# HELPER: Compute recent-records stats (post-2000)
# ===========================================================================
.recent_stats <- function(sp_data, report = NULL) {
  if (is.null(sp_data) || nrow(sp_data) == 0 || !"year" %in% names(sp_data))
    return(list(n_recent = NA_integer_, pct_recent = NA_real_))
  yrs <- suppressWarnings(as.integer(sp_data$year))
  # Strictly > 2000 (a 2000 record is not post-2000) — same boundary as the
  # metrics module (03c/04c) so the count matches package_metadata.json.
  n_recent  <- sum(!is.na(yrs) & yrs > 2000)
  # The PERCENTAGE is authoritative from the canonical report when available;
  # sp_data is the active set so the local count matches, but binding the % to
  # the report guarantees exact equality with package_metadata.json.
  pct_recent <- report$temporal$pct_post_2000 %||%
    (if (nrow(sp_data) > 0) round(n_recent / nrow(sp_data) * 100, 1) else NA_real_)
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
# Resolve a column of per-record basin CELLS to names. A record can occupy more
# than one basin, stored pipe-joined ("L10:a | L10:b"); such a cell matches no
# lookup key and would survive as a raw composite string, inflating the distinct
# basin-name count above what the enumeration actually lists. Split first so each
# unit resolves individually (a record in two basins counts toward both) — this
# mirrors the split in Module 3C so narrative and report agree.
.resolve_basin_cells <- function(cells, hb_lookup) {
  cells <- cells[!is.na(cells) & nzchar(cells)]
  if (length(cells) == 0L) return(character(0))
  codes <- unlist(strsplit(cells, "\\s*\\|\\s*"))
  codes <- codes[nzchar(codes)]
  if (length(codes) == 0L) return(character(0))
  .resolve_basin_col(codes, hb_lookup)
}

# Delegates to the ONE canonical resolver in 00_helpers.R (Lucian, 2026-08).
# Previously this and .resolve_basin_3c() in the report module were separate
# implementations that disagreed: this one had a case-insensitive rescue for
# the lookup's column names, that one did not and silently returned raw codes.
# Distinct raw codes == distinct units, which is why the summary sentence
# quoted the unit count as the name count for 483 of 676 species.
.resolve_basin_col <- function(x, hb_lookup) {
  if (length(x) == 0) return(x)
  resolve_basin_names(x, hb_lookup, fallback = "unnamed")$names
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
                                       snapshot_date, citations = NULL,
                                       sp_clean, module, ctx = NULL) {
  
  # We need at least one report to proceed
  if (is.null(ind_report) && is.null(nind_report)) {
    log_warn("  No report found for %s — skipping.", sp, module = module)
    return(NULL)
  }
  
  # Parse vernacular names
  vern_by_lang <- .parse_vernacular_by_language(vern_str, iso_lang)

  # ── ZERO-ACTIVE (TOTAL EXTINCTION) TERMINAL NARRATIVE ─────────────────────
  # If no active occurrences remain across any population (but extinctions are
  # documented), the normal sections assume points that no longer exist. Emit a
  # dedicated terminal narrative instead: extirpation statement + mandatory
  # data-state disclaimer, never phrased as a biological-extinction verdict
  # (Lucian, 2026-06).
  ind_terminal <- identical(ind_report$status,  "Extinct") ||
                  identical(ind_report$terminal_state,  "zero_active")
  nind_terminal<- identical(nind_report$status, "Extinct") ||
                  identical(nind_report$terminal_state, "zero_active")
  n_act_ind  <- as.integer(ind_report$metrics$n_records  %||% 0L)
  n_act_nind <- as.integer(nind_report$metrics$n_records %||% 0L)
  n_ext_tot  <- as.integer(ind_report$counts$n_extinctions %||% 0L) +
                as.integer(nind_report$counts$n_extinctions %||% 0L)
  # Terminal when either population is flagged zero-active, or nothing is active
  # anywhere while extinctions are on record. The terminal_state flag makes this
  # robust even if the extinction count is derived elsewhere.
  if (ind_terminal || nind_terminal || (n_act_ind + n_act_nind == 0L && n_ext_tot > 0L)) {
    return(.build_extinct_narrative(sp, scenario, vern_by_lang,
                                    ind_report, nind_report,
                                    checkover_version, output_version,
                                    snapshot_date))
  }

  # --- Version delta against the taxon's previous package (NULL = first) ---
  delta <- .build_package_delta(ctx, sp_clean, ind_report, nind_report)

  # --- Build sections ---
  sec1 <- .build_section1(sp, vern_by_lang, ind_report, nind_report)
  sec2 <- .build_section2(sp, sp_ind, ind_report, scenario, feow_map, hydrobasin_names)
  sec3 <- .build_section3(sp, sp_nind, nind_report, scenario, feow_map, hydrobasin_names)
  sec4 <- .build_section4(sp, sp_ind, sp_nind, ind_report, nind_report,
                          checkover_version, output_version, snapshot_date,
                          delta, citations)
  sec5 <- .build_section5(sp, scenario, vern_by_lang, sp_ind, sp_nind,
                          ind_report, nind_report, delta,
                          output_version, checkover_version, feow_map, hydrobasin_names)

  # Assemble YAML front-matter + all sections
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
# BUILDER: Zero-active (total extinction) terminal narrative
# Auto-generated, minimal but VALID. States the data-level extirpation and the
# mandatory disclaimer. Never a biological-extinction verdict (Lucian, 2026-06).
# ===========================================================================
.build_extinct_narrative <- function(sp, scenario, vern_by_lang,
                                     ind_report, nind_report,
                                     checkover_version, output_version,
                                     snapshot_date) {
  report <- ind_report %||% nind_report
  last_year <- report$extinction_year %||% report$temporal$year_max %||% NA

  higher <- report$taxonomy$higher_taxonomy %||% NULL
  if (is.list(higher)) higher <- higher$higher_taxonomy %||% NULL
  tax_line <- if (!is.null(higher) && nzchar(as.character(higher)))
    paste0("- **Higher taxonomy:** ", as.character(higher)) else "- **Higher taxonomy:** N/A"

  disclaimer <- report$disclaimer %||%
    (if (exists("CHECKOVER_EXTINCTION_DISCLAIMER")) CHECKOVER_EXTINCTION_DISCLAIMER else
      paste0("This status reflects the state of the World of Crayfish data, not a ",
             "field-verified biological extinction."))

  as_of <- if (!is.na(last_year)) paste0("as of ", last_year) else "on the current data"

  # Formal (Section 5) prose — the "all known occurrences extirpated" statement.
  formal_text <- paste0(
    "As of this data snapshot, all known occurrences of *", sp, "* recorded in the ",
    "World of Crayfish® database are documented as extirpated (", as_of, "). ",
    "No active occurrences remain, so area of occupancy is 0 km² and extent of ",
    "occurrence is undefined (NA); no hydrographic basins or ecoregions are reported. ",
    "**", disclaimer, "**"
  )

  sec_extinct <- paste(
    "## 1. TAXONOMIC IDENTITY", "",
    paste0("- **Scientific name:** *", sp, "*"),
    tax_line,
    paste0("- **Status:** Extinct (zero active occurrences on record)"),
    "",
    if (!is.null(vern_by_lang) && length(vern_by_lang) > 0)
      paste(c("### Common names", "", .format_vernacular_block(vern_by_lang), ""), collapse = "\n")
    else "",
    "## 2. RANGE STATUS", "",
    paste0("- **Area of occupancy (AOO):** 0 km²"),
    paste0("- **Extent of occurrence (EOO):** NA (undefined — no active occurrences)"),
    paste0("- **Hydrographic basins:** none"),
    paste0("- **Documented extirpated localities:** ",
           as.integer((ind_report$counts$n_extinctions %||% 0L) +
                      (nind_report$counts$n_extinctions %||% 0L))),
    paste0("- **Last recorded occurrence:** ", if (!is.na(last_year)) last_year else "unknown"),
    "",
    "## 5. FORMAL NARRATIVE SUMMARY", "",
    formal_text,
    "",
    paste0("> **Data-state disclaimer.** ", disclaimer),
    sep = "\n"
  )

  full_markdown <- paste(
    "---",
    "title: \"Canonical Geo-Narrative (Extinct — zero active occurrences)\"",
    paste0("species: \"", sp, "\""),
    paste0("output_version: \"", output_version, "\""),
    paste0("checkover_version: \"", checkover_version, "\""),
    paste0("generated: \"", Sys.Date(), "\""),
    paste0("snapshot_date: \"", snapshot_date, "\""),
    "status: \"Extinct\"",
    "integrity_flag: \"MAX\"",
    "---",
    "",
    sec_extinct,
    sep = "\n"
  )

  list(
    full_markdown               = full_markdown,
    formal_narrative_text       = formal_text,
    formal_narrative_word_count = length(strsplit(gsub("\\*\\*|\\*", "", formal_text), "\\s+")[[1]]),
    metadata = list(
      species           = sp,
      scenario          = scenario,
      status            = "Extinct",
      output_version    = output_version,
      checkover_version = checkover_version,
      generated_date    = as.character(Sys.Date()),
      snapshot_date     = snapshot_date,
      integrity_flag    = "MAX",
      disclaimer        = disclaimer
    )
  )
}


# ===========================================================================
# HELPER: Version delta against the taxon's previous package
# ===========================================================================
# What a package says about its taxon's previous package (Lucian, 2026-10):
# records, AOO, EOO and documented local extinctions, previous -> current, and
# what changed in the taxon's input since then. Measured values only, no trend
# label. The previous values are those the previous package published (its
# package_metadata.json, read_prior_species_aux()); the current ones are the
# values this package's metadata will carry, converted as Module 9 converts
# them, which the audit checks. The previous package is the taxon's own last
# package, not the previous global release.
#
# @return NULL for the taxon's first package (or without a RunContext), else
#   list(previous_version, previous_date, previous_code, current_code,
#        indigenous, non_indigenous), each population list(prev, curr, changes);
#   changes is NULL when the inputs cannot be compared.
.build_package_delta <- function(ctx, sp_clean, ind_report, nind_report) {
  if (is.null(ctx) || !exists("read_prior_species_aux", mode = "function")) return(NULL)
  aux <- tryCatch(read_prior_species_aux(ctx, sp_clean), error = function(e) NULL)
  if (is.null(aux) || isTRUE(aux$is_baseline)) return(NULL)

  taxon_rows <- function(v) {
    s <- read_input_snapshot(ctx$state_dir, v)
    if (is.null(s)) NULL else s[s$package_id == sp_clean, , drop = FALSE]
  }
  changes <- compare_taxon_inputs(taxon_rows(aux$previous_version),
                                  taxon_rows(ctx$framework_version))

  num <- function(x) { x <- suppressWarnings(as.numeric(x %||% NA)); if (length(x) == 1L) x else NA_real_ }
  # As Module 9's population block: AOO 0 and EOO undefined when absent.
  now <- function(report) {
    aoo <- num(report$metrics$aoo_km2); eoo <- num(report$metrics$eoo_km2)
    list(records     = num(report$metrics$n_records %||% 0),
         aoo         = if (is.finite(aoo)) round(aoo) else 0,
         eoo         = if (is.finite(eoo)) round(eoo) else NA_real_,
         extinctions = num(report$counts$n_extinctions %||% 0))
  }
  before <- function(block) {
    list(records     = num(block$records %||% 0),
         aoo         = num(block$AOO_km2 %||% 0),
         eoo         = num(block$EOO_km2),
         extinctions = num(block$extinctions_count %||% 0))
  }
  list(
    previous_version = aux$previous_version,
    previous_date    = aux$previous_date,
    previous_code    = aux$previous_code_version,
    current_code     = ctx$code_version,
    indigenous     = list(prev = before(aux$prior_metrics$indigenous), curr = now(ind_report),
                          changes = changes$indigenous),
    non_indigenous = list(prev = before(aux$prior_metrics$non_indigenous), curr = now(nind_report),
                          changes = changes$non_indigenous)
  )
}

# "a -> b unit" for one measured value ("208 -> 196 km²"); "undefined" for a
# missing side, the unit then staying with the number.
.delta_pair <- function(a, b, unit = "") {
  f <- function(x) if (is.finite(x)) format(x, big.mark = ",", scientific = FALSE) else "undefined"
  if (is.finite(b)) paste0(f(a), " → ", f(b), unit)
  else if (is.finite(a)) paste0(f(a), unit, " → ", f(b))
  else paste0(f(a), " → ", f(b))
}

# Whether any of the four measured values differs between the two packages.
.delta_changed <- function(pd) {
  any(vapply(c("records", "aoo", "eoo", "extinctions"), function(k) {
    a <- pd$prev[[k]]; b <- pd$curr[[k]]
    !identical(is.finite(a), is.finite(b)) || (is.finite(a) && a != b)
  }, logical(1)))
}

#' One version-delta subsection (4.2 indigenous, 4.3 non-indigenous).
#'
#' The caveat is printed only when the cause of a difference cannot be told
#' from the input changes listed above it: the previous input is not stored,
#' or the input did not change and neither did the code. When the input did not
#' change but the code did, the code is the cause and is named.
.render_delta_block <- function(header, delta, pd, output_version, scope_word) {
  pv <- paste0("v", sub("^v", "", delta$previous_version))
  ch <- pd$changes
  changes_line <- if (is.null(ch)) {
    paste0("not available (the input of ", pv, " is not stored)")
  } else {
    parts <- c(paste0("extinction claims added: ", ch$claims_added),
               if (ch$claims_withdrawn > 0) paste0("extinction claims withdrawn: ", ch$claims_withdrawn),
               paste0("records added: ", ch$records_added),
               paste0("records withdrawn: ", ch$records_withdrawn),
               paste0("records revised: ", ch$records_revised))
    paste(parts, collapse = "; ")
  }

  cause <- character(0)
  if (.delta_changed(pd)) {
    code_changed <- !is.null(delta$previous_code) && !is.null(delta$current_code) &&
      !identical(as.character(delta$previous_code), as.character(delta$current_code))
    if (is.null(ch)) {
      cause <- c("", paste0("> *The input of the previous package is not available for comparison, ",
                            "so the cause of these differences cannot be told from the data.*"))
    } else if (sum(unlist(ch)) == 0) {
      cause <- if (code_changed) {
        c("", paste0("> *The input records of the ", scope_word, " range did not change; the ",
                     "differences come from the processing code (", delta$previous_code, " → ",
                     delta$current_code, ").*"))
      } else {
        c("", paste0("> *Neither the input records of the ", scope_word, " range nor the ",
                     "processing code changed, so the cause of these differences cannot be ",
                     "told from the data.*"))
      }
    }
  }

  c(header, "",
    paste0("- **Current package:** ", output_version, " (", Sys.Date(), ")"),
    paste0("- **Previous package of this taxon:** ", pv,
           if (!is.null(delta$previous_date)) paste0(" (", delta$previous_date, ")") else ""),
    paste0("- **Records:** ", .delta_pair(pd$prev$records, pd$curr$records)),
    paste0("- **AOO:** ", .delta_pair(pd$prev$aoo, pd$curr$aoo, " km²")),
    paste0("- **EOO:** ", .delta_pair(pd$prev$eoo, pd$curr$eoo, " km²")),
    paste0("- **Documented local extinctions:** ",
           .delta_pair(pd$prev$extinctions, pd$curr$extinctions)),
    paste0("- **Input changes since ", pv, ":** ", changes_line),
    cause,
    "")
}

#' The section-5 sentence on a range against the previous package: what was
#' measured, never a trend. "smaller" / "larger" only when AOO and EOO agree
#' (or one is unchanged or undefined), "differs" when they disagree.
.range_change_sentence <- function(delta, pd, range_word) {
  if (is.null(delta) || is.null(pd) || !isTRUE(pd$curr$records > 0)) return("")
  pv <- paste0("v", sub("^v", "", delta$previous_version))
  d <- c(pd$curr$aoo - pd$prev$aoo,
         if (is.finite(pd$prev$eoo) && is.finite(pd$curr$eoo)) pd$curr$eoo - pd$prev$eoo)
  d <- d[is.finite(d)]
  # An EOO that became defined or undefined has no direction of its own (it can
  # be undefined for too few localities or for too wide a spread), but it is
  # not "unchanged" either.
  eoo_flip <- !identical(is.finite(pd$prev$eoo), is.finite(pd$curr$eoo))
  verb <- if ((!length(d) || all(d == 0)) && !eoo_flip) "is unchanged from"
          else if (length(d) && all(d <= 0) && any(d < 0)) "is smaller than in"
          else if (length(d) && all(d >= 0) && any(d > 0)) "is larger than in"
          else "differs from"
  vals <- if (identical(verb, "is unchanged from"))
    paste0("AOO ", format(pd$curr$aoo, big.mark = ","), " km², EOO ",
           if (is.finite(pd$curr$eoo)) paste0(format(pd$curr$eoo, big.mark = ","), " km²") else "undefined")
  else
    paste0("AOO ", .delta_pair(pd$prev$aoo, pd$curr$aoo, " km²"),
           ", EOO ", .delta_pair(pd$prev$eoo, pd$curr$eoo, " km²"))
  paste0(" The documented ", range_word, " range ", verb, " the previous package of this taxon, ",
         pv, " (", vals, ").")
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
  
  # Type locality: whether a record of the type locality is among the records
  # analyzed (report flag type_locality_present, the same one package_metadata
  # carries). This read `report$type_locality`, a field no report has: `$`
  # partially matched type_locality_present, so every package printed the
  # bare flag as "Locality: TRUE" or "Locality: FALSE" (Lucian, 2026-10). A
  # structured type_locality block, if a report ever carries one, is listed.
  type_loc  <- report[["type_locality"]]
  type_flag <- report[["type_locality_present"]]
  if (is.list(type_loc)) {
    lines <- c(lines, "### Type locality", "")
    if (!is.null(type_loc$geographic_unit))
      lines <- c(lines, paste0("- **Geographic unit:** ", type_loc$geographic_unit))
    if (!is.null(type_loc$protected_area) && nzchar(type_loc$protected_area))
      lines <- c(lines, paste0("- **Protected area:** ", type_loc$protected_area))
    if (!is.null(type_loc$reference))
      lines <- c(lines, paste0("- **Reference:** ", type_loc$reference))
    lines <- c(lines, "")
  } else if (!is.null(type_flag)) {
    lines <- c(lines, "### Type locality", "",
               paste0("- **Type locality represented among the analyzed records:** ",
                      if (isTRUE(as.logical(type_flag))) "yes" else "no"),
               "")
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
  aoo_val  <- ind_report$metrics$aoo_km2       %||% NA
  
  lines <- c(lines,
             paste0("- **Distribution category:** ", category),
             paste0("- **Area of occupancy (AOO):** ",
                    if (!is.na(aoo_val)) paste0(format(round(aoo_val, 0), big.mark = ","), " km\u00b2") else "N/A"),
             paste0("- **Extent of occurrence (EOO):** ", .eoo_text(ind_report)),
             ""
  )

  # --- Geographic distribution ---
  lines <- c(lines, "#### Geographic distribution", "")
  
  # Resolve basin and FEOW codes to human-readable names before tabulation
  basin_col <- if (!is.null(sp_ind)) .resolve_basin_cells(sp_ind$hydrobasin, hydrobasin_names) else character(0)
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
  
  # Named river basins (river-aware names) — a DISTINCT quantity from the fine
  # HydroBASINS UNIT count (metadata basins_count). Labelled separately so the
  # two are never confused (Lucian, 2026-07).
  basin_ft <- .freq_table(basin_col)
  n_named  <- nrow(basin_ft)
  n_units  <- ind_report$counts$n_hydrobasins %||% n_named
  if (n_named > 0) {
    basin_str <- paste(paste0(basin_ft$value, " (n=", basin_ft$n, ")"), collapse = "; ")
    lines <- c(lines,
               paste0("- **Hydrographic basins (native):** ", basin_str),
               paste0("  - Distinct names: ", n_named, "  |  HydroBASINS units: ", n_units),
               "")
  }
  
  # --- Conservation context ---
  lines <- c(lines, "#### Conservation context", "")
  
  n_prot      <- ind_report$conservation$n_protected_records %||% NA
  pct_prot    <- ind_report$conservation$protection_percentage %||% NA
  pa_ft       <- .freq_table_multi(sp_ind$protected_area[!is.na(sp_ind$protected_area) & nzchar(sp_ind$protected_area)])
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
  rs     <- .recent_stats(sp_ind, ind_report)
  
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
  
  # --- 2.3 Spatial Clustering ---
  # Renamed from "Fragmentation Assessment" (Lucian, 2026-07). "Fragmentation"
  # is reserved for checKOLOGY, where it is computed properly; in cheCkOVER this
  # is a purely descriptive spatial signal with no ecological connotation, so the
  # two can never be confused. Logic and thresholds are unchanged — labels only.
  frag <- ind_report$spatial_clustering %||% ind_report$fragmentation

  if (!is.null(frag) && isTRUE(frag$computed)) {
    is_applicable <- !identical(tolower(as.character(category)), "cosmopolitan")
    if (is_applicable) {
      lines <- c(lines,
                 "### 2.3 Spatial Clustering (Indigenous Range Only)",
                 "",
                 "**Method summary:** Adaptive spatial clustering of indigenous localities in an equal-area projection.",
                 ""
      )

      if (identical(as.character(frag$status), "detected")) {
        n_cl   <- frag$n_clusters %||% "N/A"
        cl_sz  <- frag$cluster_sizes %||% frag$cluster_sizes_n %||% "N/A"
        lines  <- c(lines,
                    paste0("- **Spatial clustering signal:** detected"),
                    paste0("- **Number of clusters:** ", n_cl),
                    paste0("- **Cluster composition:** ", cl_sz),
                    "- **Interpretation:** Native occurrences resolve into multiple distinct spatial clusters.",
                    "",
                    "> *This is a conservative descriptive signal of spatial structure and does not represent a formal connectivity, fragmentation, or Red List assessment.*",
                    ""
        )
      } else {
        lines <- c(lines,
                   "- **Spatial clustering signal:** not detected",
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

  aoo_val <- nind_report$metrics$aoo_km2 %||% NA
  
  lines <- c(lines,
             paste0("- **Area of occupancy (AOO):** ",
                    if (!is.na(aoo_val)) paste0(format(round(aoo_val, 0), big.mark = ","), " km\u00b2") else "N/A"),
             paste0("- **Extent of occurrence (EOO):** ", .eoo_text(nind_report)),
             ""
  )

  # Resolve basin and FEOW codes to human-readable names before tabulation
  basin_col <- .resolve_basin_cells(sp_nind$hydrobasin,         hydrobasin_names)
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
  
  # Hydrographic basins — resolved, deduplicated.
  # Reports BOTH numbers, in the same shape and wording as the native block
  # (Lucian, 2026-08): this side used to print a bare "Count: 9" that never
  # said whether it counted names or units, and never stated the unit count
  # at all. Distinct names and HydroBASINS units are different quantities and
  # each is now labelled for what it is.
  basin_ft <- .freq_table(basin_col)
  if (nrow(basin_ft) > 0) {
    basin_str <- paste(paste0(basin_ft$value, " (n=", basin_ft$n, ")"), collapse = "; ")
    n_named_ni <- nrow(basin_ft)
    n_units_ni <- nind_report$counts$n_hydrobasins %||% n_named_ni
    lines <- c(lines,
               paste0("- **Hydrographic basins:** ", basin_str),
               paste0("  - Distinct names: ", n_named_ni,
                      "  |  HydroBASINS units: ", n_units_ni),
               "")
  }
  
  # Protected areas
  pa_ft_nind    <- .freq_table_multi(sp_nind$protected_area[!is.na(sp_nind$protected_area) & nzchar(sp_nind$protected_area)])
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
                            delta = NULL, citations = NULL) {

  # Record counts are bound to the canonical metrics object (report$metrics$
  # n_records = the analyzed, post-suppression set), NOT recomputed from the
  # occurrence slice. This is the headline "N validated occurrence records"
  # number and must equal package_metadata.json (Lucian bug 2).
  n_ind  <- as.integer(ind_report$metrics$n_records  %||% (if (!is.null(sp_ind))  nrow(sp_ind)  else 0L))
  n_nind <- as.integer(nind_report$metrics$n_records %||% (if (!is.null(sp_nind)) nrow(sp_nind) else 0L))
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
  dq_score    <- if (n_total > 0) round(n_high_acc / n_total * 100, 1) else NA_real_
  dq_str      <- if (!is.na(dq_score))
    paste0(dq_score, "% high-accuracy records (", n_high_acc, "/", n_total,
           "; low-accuracy: ", n_low_acc, ")")
  else "N/A"

  # Bibliography: the figures of the package's own bibliography (Module 7),
  # never recounted here. This counted distinct DOI strings plus distinct
  # citation strings of the records, 7 + 8 = 15 for A. bihariensis, whose
  # bibliography holds 8 references (Lucian, 2026-10).
  biblio <- if (!is.null(citations$bibliography_json) && file.exists(citations$bibliography_json))
    tryCatch(jsonlite::read_json(citations$bibliography_json, simplifyVector = FALSE),
             error = function(e) NULL) else NULL
  n_refs  <- as.integer(biblio$totals$total_references %||% 0L)
  dois    <- unlist(lapply(biblio$references %||% list(), function(r) r$doi))
  dois    <- sort(unique(dois[!is.na(dois) & nzchar(dois)]))
  doi_str <- if (length(dois) > 0) paste(dois, collapse = "; ") else "none"

  lines <- c(lines,
             "### 4.1 Data summary", "",
             paste0("- **Total records analyzed:** ", format(n_total, big.mark = ",")),
             paste0("  - **Indigenous records:** ",     format(n_ind,   big.mark = ",")),
             paste0("  - **Non-indigenous records:** ", format(n_nind,  big.mark = ",")),
             paste0("- **High-accuracy records:** ", n_high_acc, " (", round(n_high_acc/max(n_total,1)*100,1), "%)"),
             paste0("- **Data quality score:** ", dq_str),
             ""
  )

  # --- 4.2 / 4.3 Version delta, against the taxon's previous package ---
  # Only when there is one; a population's subsection appears when it has
  # records now or had them then.
  if (!is.null(delta)) {
    has_pop <- function(report, pd) !is.null(report) || isTRUE(pd$prev$records > 0)
    if (has_pop(ind_report, delta$indigenous))
      lines <- c(lines, .render_delta_block("### 4.2 Version delta: indigenous range",
                                            delta, delta$indigenous, output_version, "native"))
    if (has_pop(nind_report, delta$non_indigenous))
      lines <- c(lines, .render_delta_block("### 4.3 Version delta: non-indigenous range",
                                            delta, delta$non_indigenous, output_version,
                                            "non-indigenous"))
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
                            ind_report, nind_report, delta = NULL,
                            output_version, checkover_version = "1.0",
                            feow_map = NULL, hydrobasin_names = NULL) {

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
    paragraphs <- c(paragraphs, .p3_conservation(sp, sp_ind, ind_report, delta))
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
    paragraphs <- c(paragraphs, .p4_non_indigenous(sp, sp_nind, nind_report, delta))
  }
  
  # --- Paragraph 5: Data quality, provenance, and interpretation (80-100 words) ---
  paragraphs <- c(paragraphs, .p5_provenance(sp, sp_ind, sp_nind, ind_report, nind_report,
                                             output_version, checkover_version, delta))
  
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
  basin_col <- if (!is.null(sp_ind)) .resolve_basin_cells(sp_ind$hydrobasin,         hydrobasin_names) else character(0)
  feow_col  <- if (!is.null(sp_ind)) .resolve_feow_col(sp_ind$freshwater_ecoregion,  feow_map)         else character(0)
  
  # Top basins (up to 3 by frequency) for the illustrative "including ..." list.
  basin_ft   <- if (!is.null(sp_ind)) .freq_table(basin_col) else data.frame(value = character(0), n = integer(0))
  # Headline basin COUNT is bound to the canonical metrics object, never
  # recomputed here — this is the number that populates
  # package_metadata.json:metrics.indigenous.basins_count (Lucian bug 1). The
  # freq table above only supplies example basin names, not the count.
  # Two DISTINCT basin quantities (Lucian 2026-07):
  #   n_units = fine HydroBASINS polygon count (== package_metadata basins_count)
  #   n_named = distinct river/basin NAMES (the human-readable enumeration)
  n_units    <- ind_report$counts$n_hydrobasins  %||% nrow(basin_ft)
  n_named    <- ind_report$counts$n_named_basins %||% nrow(basin_ft)
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
  # post-2000 % is bound to the canonical metrics object (report$temporal),
  # never recomputed, so it equals package_metadata.json exactly (Lucian bug 3).
  rs        <- .recent_stats(sp_ind, ind_report)
  
  # --- Build sentence components ---
  # Basin phrase quotes the NAME count and states the fine unit count
  # separately, e.g.:
  #   "**11 hydrographic basins**, including Tisza - Crișul Alb, ...
  #    (across 21 HydroBASINS units)"
  #
  # The bolded figure is n_named, never n_units. It read n_units for 483 of 676
  # species — "424 named river basins ... across 424 units" for a species with
  # 13 names — because n_named_basins in the report had silently collapsed to
  # the unit count (see resolve_basin_names() in 00_helpers.R). The two now come
  # from one resolver, and the suffix says "HydroBASINS units" so the sentence
  # does not say "hydrographic basin" twice (Lucian, 2026-08).
  units_suffix <- if (!is.na(n_units))
    paste0(" (across ", format(n_units, big.mark = ","), " HydroBASINS unit",
           if (n_units != 1) "s" else "", ")")
  else ""
  basin_phrase <- if (length(top_basins) > 0) {
    incl_suffix <- if (!is.na(n_named) && n_named > length(top_basins))
      paste0(", including ", paste(top_basins, collapse = ", "))
    else ""
    paste0("**", n_named, " hydrographic basin",
           if (!is.na(n_named) && n_named != 1) "s" else "",
           "**", incl_suffix, units_suffix)
  } else if (!is.na(n_units)) {
    paste0(format(n_units, big.mark = ","), " HydroBASINS unit",
           if (n_units != 1) "s" else "")
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
  
  join_sentences(
    paste0("Native occurrences span ", basin_phrase, ", recorded in ",
           country_phrase, ". The species inhabits the **", teow_phrase,
           "** terrestrial ecoregion(s), and is associated with the **", feow_phrase,
           "** freshwater ecoregion(s)."),
    metrics_phrase, temporal_phrase
  )
}


# ---------------------------------------------------------------------------
# Paragraph 3: Conservation context
# ---------------------------------------------------------------------------
.p3_conservation <- function(sp, sp_ind, ind_report, delta = NULL) {
  prot_pct <- ind_report$conservation$protection_percentage %||% NA
  prot_n   <- ind_report$conservation$n_protected_records   %||% NA
  
  # Protected area coverage
  pa_phrase <- if (!is.na(prot_pct) && !is.na(prot_n))
    paste0(round(prot_pct, 1), "% of native records (n=", prot_n, ") fall within designated protected areas.")
  else
    "Protected area coverage data are unavailable."
  
  # Spatial clustering (was "fragmentation" — reserved for checKOLOGY; this is a
  # descriptive spatial signal only, with no ecological connotation).
  frag     <- ind_report$spatial_clustering %||% ind_report$fragmentation
  frag_str <- ""
  if (!is.null(frag) && isTRUE(frag$computed) && identical(as.character(frag$status), "detected")) {
    n_cl     <- frag$n_clusters %||% "multiple"
    frag_str <- paste0(" Spatial analysis identifies ", n_cl, " distinct occurrence cluster",
                       if (!is.na(suppressWarnings(as.integer(n_cl))) &&
                           suppressWarnings(as.integer(n_cl)) != 1) "s" else "",
                       " within the native range.")
  }
  
  # Extinctions (bound to canonical counts.n_extinctions; narrated only when >0).
  # This is a DATA-STATE signal, not a biological extinction verdict — the
  # wording must never read as a field determination (Lucian).
  n_ext    <- as.integer(ind_report$counts$n_extinctions %||% 0L)
  ext_str  <- if (!is.na(n_ext) && n_ext > 0L) {
    paste0(" The dataset documents **", n_ext, " local extinction",
           if (n_ext != 1L) "s" else "",
           "** (native localities where all occurrences are recorded as extirpated); ",
           "these reflect the state of the WoC data and warrant field verification, ",
           "not a conclusion of biological extinction.")
  } else ""

  # Against the previous package: what was measured, never a trend. "Native
  # range exhibits decline / remains stable" stated a difference between two
  # packages as a population trend, and followed the AOO class alone, so A.
  # astacus read "stable" with 110 newly documented local extinctions
  # (Lucian, 2026-10).
  change_str <- .range_change_sentence(delta, delta$indigenous, "native")

  paste0(pa_phrase, frag_str, ext_str, change_str)
}


# ---------------------------------------------------------------------------
# Paragraph 4: Non-indigenous populations
# ---------------------------------------------------------------------------
.p4_non_indigenous <- function(sp, sp_nind, nind_report, delta = NULL) {
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
  
  # Against the previous package: measured values, no trend (as paragraph 3).
  change_str <- .range_change_sentence(delta, delta$non_indigenous, "non-indigenous")
  
  # Non-indigenous extinctions (bound to canonical counts.n_extinctions).
  n_ext_ni <- as.integer(nind_report$counts$n_extinctions %||% 0L)
  ext_ni_str <- if (!is.na(n_ext_ni) && n_ext_ni > 0L) {
    paste0(" The data record **", n_ext_ni, " extirpated non-indigenous localit",
           if (n_ext_ni != 1L) "ies" else "y",
           "** (reflecting the state of the WoC data, not a field-verified outcome).")
  } else ""

  paste0(
    "Outside its native range, non-indigenous populations classified as **", category,
    "** have been documented in ", country_phrase, first_yr_phrase, ".", ext_ni_str, change_str
  )
}


# ---------------------------------------------------------------------------
# Paragraph 5: Data quality, provenance, and interpretation
# ---------------------------------------------------------------------------
.p5_provenance <- function(sp, sp_ind, sp_nind, ind_report, nind_report,
                           output_version, checkover_version = "1.0", delta = NULL) {
  # Bound to canonical metrics.records (analyzed/post-suppression set), not the
  # occurrence slice — headline "N validated occurrence records" (Lucian bug 2).
  n_ind  <- as.integer(ind_report$metrics$n_records  %||% (if (!is.null(sp_ind))  nrow(sp_ind)  else 0L))
  n_nind <- as.integer(nind_report$metrics$n_records %||% (if (!is.null(sp_nind)) nrow(sp_nind) else 0L))
  n_total <- n_ind + n_nind

  # Version prefix whenever the taxon has a previous package (not by the version
  # number: a taxon first packaged in 1.3 has none).
  version_prefix <- if (!is.null(delta))
    paste0("This synthesis represents **version ", sub("^v", "", output_version),
           "** (", Sys.Date(), "), updating the previous package of this taxon (v",
           sub("^v", "", delta$previous_version), "). ")
  else ""

  # The sentence "Bibliographic coverage statistics and high-accuracy record
  # percentages are not yet tracked at per-species level in this version" went
  # in 2026-10: section 4 gives both (Lucian).
  paste0(
    version_prefix,
    "This synthesis is based on **",
    format(n_total, big.mark = ","),
    " validated occurrence records** (",
    format(n_ind,  big.mark = ","), " indigenous, ",
    format(n_nind, big.mark = ","), " non-indigenous). ",
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