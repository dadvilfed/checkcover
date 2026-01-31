#### MODULE 10: CANONICAL NARRATIVE TEMPLATE GENERATOR ####
# Follows File_S1.md template specification v0.9
# Generates complete geo-narratives with all sections

#' Generate canonical geo-narrative template for all species
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param indigenous_reports Indigenous reports
#' @param non_indigenous_reports Non-indigenous reports
#' @param scenario3_merged Scenario 3 merged reports
#' @param vernacular_lookup Vernacular names lookup
#' @param output_dir Output directory
#' @param feow_lookup_path FEOW name lookup path
#' @param hydrobasin_names HydroBASINS name lookup
#' @return List of generated canonical narratives
generate_canonical_narratives <- function(scenario_table,
                                          result_indigenous,
                                          result_non_indigenous,
                                          indigenous_reports,
                                          non_indigenous_reports,
                                          scenario3_merged,
                                          vernacular_lookup = NULL,
                                          output_dir = "checkover_output",
                                          feow_lookup_path = NULL,
                                          hydrobasin_names = NULL) {
  module <- "MODULE10_CANONICAL"

  with_log_section(module, {
    log_info("=== MODULE 10: CANONICAL NARRATIVE GENERATION ===", module = module)

    # Create directories
    canonical_dir <- file.path(output_dir, "narratives_canonical")
    formal_dir <- file.path(output_dir, "narratives_formal")

    for (d in c(canonical_dir, formal_dir)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }

    # Load helpers
    vern_map <- .extract_vernacular_map(vernacular_lookup)
    feow_map <- .load_feow_map(feow_lookup_path, module)

    # Process all species
    all_species <- unique(scenario_table$species)
    all_species <- all_species[!is.na(all_species)]

    log_info("Generating canonical narratives for %d species...", length(all_species), module = module)

    canonical_narratives <- list()

    for (sp in all_species) {
      log_info("Processing: %s", sp, module = module)

      sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
      scenario <- scenario_table$scenario[scenario_table$species == sp][1]

      # Build canonical narrative
      canonical <- .build_canonical_narrative(
        sp, scenario, scenario_table,
        result_indigenous, result_non_indigenous,
        indigenous_reports, non_indigenous_reports, scenario3_merged,
        vern_map, feow_map, hydrobasin_names, output_dir, module
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
          species = sp,
          scenario = scenario,
          narrative = canonical$formal_narrative_text,
          word_count = canonical$formal_narrative_word_count,
          metadata = canonical$metadata
        )

        formal_json_file <- file.path(formal_dir, paste0(sp_clean, "_narrative.json"))
        jsonlite::write_json(formal_json, formal_json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")

        canonical_narratives[[sp]] <- list(
          canonical_markdown = canonical_file,
          formal_text = formal_txt_file,
          formal_json = formal_json_file
        )
      }
    }

    log_info("Generated %d canonical narratives.", length(canonical_narratives), module = module)

    return(list(
      narratives = canonical_narratives
    ))
  })
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

.extract_vernacular_map <- function(vernacular_lookup) {
  if (is.null(vernacular_lookup)) return(NULL)

  if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup)) {
    return(vernacular_lookup$wide)
  } else if (is.data.frame(vernacular_lookup)) {
    return(vernacular_lookup)
  }

  return(NULL)
}

.load_feow_map <- function(feow_lookup_path, module) {
  if (is.null(feow_lookup_path) || !file.exists(feow_lookup_path)) return(NULL)

  ext <- tolower(tools::file_ext(feow_lookup_path))
  tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      if (requireNamespace("readxl", quietly = TRUE)) {
        feow_map <- as.data.frame(readxl::read_excel(feow_lookup_path))
        names(feow_map) <- toupper(names(feow_map))
        if ("ID" %in% names(feow_map)) feow_map$ID <- as.integer(feow_map$ID)
        return(feow_map)
      }
    } else {
      feow_map <- read.csv(feow_lookup_path, stringsAsFactors = FALSE)
      names(feow_map) <- toupper(names(feow_map))
      if ("ID" %in% names(feow_map)) feow_map$ID <- as.integer(feow_map$ID)
      return(feow_map)
    }
  }, error = function(e) {
    log_warn("Failed to load FEOW lookup: %s", conditionMessage(e), module = module)
  })

  return(NULL)
}

# Format number with commas
.fmt_num <- function(x, digits = 0) {
  if (is.null(x) || is.na(x)) return("N/A")
  format(round(as.numeric(x), digits), big.mark = ",", scientific = FALSE)
}


# ==============================================================================
# MAIN BUILD FUNCTION
# ==============================================================================

.build_canonical_narrative <- function(sp, scenario, scenario_table,
                                       result_indigenous, result_non_indigenous,
                                       indigenous_reports, non_indigenous_reports, scenario3_merged,
                                       vern_map, feow_map, hydrobasin_names, output_dir, module) {

  sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)

  # Get species data from result objects (for direct access to all columns)
  ind_data <- NULL
  non_ind_data <- NULL
  ind_report <- NULL
  non_ind_report <- NULL

  if (scenario %in% c(1, 3) && !is.null(result_indigenous$clean_data)) {
    ind_data <- result_indigenous$clean_data[result_indigenous$clean_data$species == sp, ]

    # Load JSON report - use output_dir directly
    report_file <- file.path(output_dir, "reports", "indigenous", paste0(sp_clean, ".json"))
    if (file.exists(report_file)) {
      ind_report <- jsonlite::read_json(report_file, simplifyVector = TRUE)
    }
  }

  if (scenario %in% c(2, 3) && !is.null(result_non_indigenous$clean_data)) {
    non_ind_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]

    # Load JSON report - use output_dir directly
    report_file <- file.path(output_dir, "reports", "non_indigenous", paste0(sp_clean, ".json"))
    if (file.exists(report_file)) {
      non_ind_report <- jsonlite::read_json(report_file, simplifyVector = TRUE)
    }
  }

  if (is.null(ind_report) && is.null(non_ind_report)) {
    log_warn("  No report found for %s", sp, module = module)
    return(NULL)
  }

  # Extract vernacular names
  vern_str <- NA_character_
  if (!is.null(vern_map) && "species" %in% names(vern_map)) {
    idx <- match(sp, vern_map$species)
    if (!is.na(idx) && "vernacular_string" %in% names(vern_map)) {
      raw_v <- vern_map$vernacular_string[idx]
      if (!is.na(raw_v) && nzchar(raw_v)) {
        vern_str <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
      }
    }
  }

  # Build all sections
  section1 <- .build_section1_taxonomy(sp, vern_str, ind_data, ind_report, non_ind_report)
  section2 <- .build_section2_indigenous(sp, ind_data, ind_report, feow_map, hydrobasin_names)
  section3 <- .build_section3_non_indigenous(sp, scenario, non_ind_data, non_ind_report, feow_map, hydrobasin_names)
  section4 <- .build_section4_provenance(sp, ind_data, non_ind_data, ind_report, non_ind_report)
  section5 <- .build_section5_formal_narrative(sp, scenario, vern_str, ind_data, non_ind_data,
                                               ind_report, non_ind_report, feow_map, hydrobasin_names)

  # Assemble full canonical markdown
  full_markdown <- paste(
    "---",
    "title: Canonical Geo-Narrative",
    paste0("species: ", sp),
    paste0("date: ", Sys.Date()),
    "version: 0.9",
    "---",
    "",
    section1,
    "",
    section2,
    "",
    section3,
    "",
    section4,
    "",
    section5$markdown,
    sep = "\n"
  )

  return(list(
    full_markdown = full_markdown,
    formal_narrative_text = section5$text,
    formal_narrative_word_count = section5$word_count,
    metadata = list(
      species = sp,
      scenario = scenario,
      vernacular = vern_str,
      generated_date = as.character(Sys.Date())
    )
  ))
}


# ==============================================================================
# SECTION 1: TAXONOMIC IDENTITY
# ==============================================================================

.build_section1_taxonomy <- function(sp, vern_str, ind_data, ind_report, non_ind_report) {
  lines <- c(
    "## 1. TAXONOMIC IDENTITY",
    "",
    paste0("**Scientific name:** *", sp, "*"),
    ""
  )

  # Add vernacular if available (format: "English: name1; Romanian: name2")
  if (!is.na(vern_str) && nzchar(vern_str)) {
    formatted_vern <- .format_vernacular_names(vern_str)
    lines <- c(lines, paste0("**Common names:** ", formatted_vern), "")
  }

  # Type locality section (conditional)
  type_loc_section <- .build_type_locality_section(ind_data)
  if (!is.null(type_loc_section)) {
    lines <- c(lines, type_loc_section)
  }

  paste(lines, collapse = "\n")
}

.format_vernacular_names <- function(vern_str) {
  # Input: "idle crayfish(eng) | racul bihorean(ron)"
  # Output: "English: idle crayfish | Romanian: racul bihorean"

  lang_map <- c(
    "eng" = "English", "ron" = "Romanian", "deu" = "German", "fra" = "French",
    "spa" = "Spanish", "ita" = "Italian", "por" = "Portuguese", "pol" = "Polish",
    "hun" = "Hungarian", "ces" = "Czech", "slk" = "Slovak", "hrv" = "Croatian",
    "srp" = "Serbian", "bul" = "Bulgarian", "rus" = "Russian", "ukr" = "Ukrainian",
    "tur" = "Turkish", "jpn" = "Japanese", "zho" = "Chinese", "kor" = "Korean"
  )

  parts <- strsplit(vern_str, " \\| ")[[1]]
  formatted_parts <- sapply(parts, function(p) {
    match <- regmatches(p, regexec("(.+)\\(([a-z]{3})\\)", p))[[1]]
    if (length(match) == 3) {
      name <- trimws(match[2])
      code <- match[3]
      lang_name <- if (code %in% names(lang_map)) lang_map[code] else code
      paste0(lang_name, ": ", name)
    } else {
      p
    }
  })

  paste(formatted_parts, collapse = " | ")
}

.build_type_locality_section <- function(ind_data) {
  if (is.null(ind_data) || nrow(ind_data) == 0) return(NULL)
  if (!"is_type_locality" %in% names(ind_data)) return(NULL)

  type_locs <- ind_data[ind_data$is_type_locality == TRUE, ]
  if (nrow(type_locs) == 0) return(NULL)

  lines <- c("### Type Locality", "")
  tl <- type_locs[1, ]

  geo_parts <- c()
  if ("hydrobasin" %in% names(tl) && !is.na(tl$hydrobasin) && nzchar(tl$hydrobasin)) {
    geo_parts <- c(geo_parts, tl$hydrobasin)
  }
  if ("country" %in% names(tl) && !is.na(tl$country)) {
    geo_parts <- c(geo_parts, tl$country)
  }
  if (length(geo_parts) > 0) {
    lines <- c(lines, paste0("- **Geographic unit:** ", paste(geo_parts, collapse = ", ")))
  }

  if ("protected_area" %in% names(tl) && !is.na(tl$protected_area) && nzchar(tl$protected_area)) {
    lines <- c(lines, paste0("- **Protected area:** ", tl$protected_area))
  }

  if ("citation" %in% names(tl) && !is.na(tl$citation) && nzchar(tl$citation)) {
    lines <- c(lines, paste0("- **Bibliographic reference:** ", tl$citation))
  } else if ("doi" %in% names(tl) && !is.na(tl$doi) && nzchar(tl$doi)) {
    lines <- c(lines, paste0("- **DOI:** ", tl$doi))
  }

  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}


# ==============================================================================
# SECTION 2: INDIGENOUS RANGE OVERVIEW
# ==============================================================================

.build_section2_indigenous <- function(sp, ind_data, ind_report, feow_map, hydrobasin_names) {
  if (is.null(ind_report)) {
    return("## 2. INDIGENOUS RANGE OVERVIEW\n\n*Not applicable (non-indigenous populations only)*\n")
  }

  lines <- c(
    "## 2. INDIGENOUS RANGE OVERVIEW",
    "",
    "### 2.1 Distribution Summary",
    ""
  )

  # Required Metrics
  category <- ind_report$metrics$iucn_category %||% "unknown"
  eoo <- .fmt_num(ind_report$metrics$eoo_km2)
  aoo <- .fmt_num(ind_report$metrics$aoo_km2)

  lines <- c(lines,
             paste0("- **Distribution category:** ", category),
             paste0("- **Extent of occurrence (EOO):** ", eoo, " km\u00B2"),
             paste0("- **Area of occupancy (AOO):** ", aoo, " km\u00B2"),
             ""
  )

  # Geographic Distribution
  lines <- c(lines, "#### Geographic Distribution", "")

  countries <- ind_report$spatial_context$countries %||% "N/A"
  n_countries <- ind_report$counts$n_countries %||% 0
  lines <- c(lines, paste0("- **Countries (native):** ", countries, " (n=", n_countries, ")"))

  admin_units <- ind_report$spatial_context$admin_units %||% ""
  if (nzchar(admin_units)) {
    n_admin <- length(unique(strsplit(admin_units, " \\| ")[[1]]))
    lines <- c(lines, paste0("- **Subnational administrative units:** ", admin_units, " (n=", n_admin, ")"))
  }

  n_basins <- ind_report$counts$n_hydrobasins %||% 0
  lines <- c(lines, paste0("- **Hydrographic basins:** (n=", n_basins, ")"), "")

  # Conservation Context
  lines <- c(lines, "#### Conservation Context", "")
  prot_n <- ind_report$conservation$n_protected_records %||% 0
  prot_pct <- ind_report$conservation$protection_percentage %||% 0
  lines <- c(lines, paste0("- **Protected area coverage:** ", prot_n, " records (", round(prot_pct, 1), "%)"))

  prot_areas <- ind_report$spatial_context$protected_areas %||% ""
  if (nzchar(prot_areas)) {
    lines <- c(lines, paste0("- **Protected areas represented:** ", prot_areas))
  }
  lines <- c(lines, "")

  # Biogeographic Context
  lines <- c(lines, "#### Biogeographic Context", "")
  teow <- ind_report$spatial_context$ecoregions_teow %||% ""
  if (nzchar(teow)) lines <- c(lines, paste0("- **Terrestrial ecoregions (TEOW):** ", teow))
  feow <- ind_report$spatial_context$ecoregions_feow %||% ""
  if (nzchar(feow)) lines <- c(lines, paste0("- **Freshwater ecoregions (FEOW):** ", feow))
  lines <- c(lines, "")

  # Temporal Context
  lines <- c(lines, "#### Temporal Context", "")
  year_min <- ind_report$temporal$year_min
  year_max <- ind_report$temporal$year_max
  if (!is.null(year_min) && !is.null(year_max) && !is.infinite(year_min) && !is.infinite(year_max)) {
    lines <- c(lines, paste0("- **Temporal coverage:** ", year_min, "-", year_max))
  }

  if (!is.null(ind_data) && nrow(ind_data) > 0 && "year" %in% names(ind_data)) {
    n_total <- nrow(ind_data)
    n_recent <- sum(ind_data$year >= 2000, na.rm = TRUE)
    pct_recent <- round((n_recent / n_total) * 100, 1)
    lines <- c(lines, paste0("- **Recent records (post-2000):** ", n_recent, " (", pct_recent, "%)"))
  }
  lines <- c(lines, "")

  # 2.2 Auto-Generated Contextual Statements
  statements <- .generate_contextual_statements(category, prot_pct, ind_data, ind_report)
  if (length(statements) > 0) {
    lines <- c(lines, "### 2.2 Auto-Generated Contextual Statements", "")
    for (stmt in statements) lines <- c(lines, paste0("- ", stmt))
    lines <- c(lines, "")
  }

  # 2.3 Range Dynamics (Extinction)
  extinction_section <- .build_extinction_section(ind_data, ind_report)
  if (!is.null(extinction_section)) lines <- c(lines, extinction_section)

  # 2.4 Fragmentation Assessment
  frag_section <- .build_fragmentation_section(ind_report)
  if (!is.null(frag_section)) lines <- c(lines, frag_section)

  paste(lines, collapse = "\n")
}

.generate_contextual_statements <- function(category, prot_pct, ind_data, ind_report) {
  statements <- c()

  if (category == "endemic") statements <- c(statements, "The species is restricted to a limited geographic range.")
  else if (category == "regional") statements <- c(statements, "The species is native across a broader regional extent.")
  else if (category == "cosmopolitan") statements <- c(statements, "The species' native range spans multiple continents.")

  if (!is.na(prot_pct) && prot_pct < 25) statements <- c(statements, "Only a minority of native occurrences fall within protected areas.")
  else if (!is.na(prot_pct) && prot_pct >= 75) statements <- c(statements, "Most native occurrences fall within protected areas.")

  if (!is.null(ind_report$temporal$year_range) && ind_report$temporal$year_range < 10) {
    statements <- c(statements, "Occurrence data span a limited temporal window.")
  }

  if (!is.null(ind_data) && nrow(ind_data) > 0 && "year" %in% names(ind_data)) {
    pct_recent <- (sum(ind_data$year >= 2000, na.rm = TRUE) / nrow(ind_data)) * 100
    if (pct_recent < 25) statements <- c(statements, "Most records predate 2000, suggesting potentially outdated distributional knowledge.")
  }

  statements
}

.build_extinction_section <- function(ind_data, ind_report = NULL) {
  if (is.null(ind_data) || nrow(ind_data) == 0 || !"is_extinct" %in% names(ind_data)) return(NULL)

  extinct_records <- ind_data[ind_data$is_extinct == TRUE, ]
  if (nrow(extinct_records) == 0) return(NULL)

  lines <- c("### 2.3 Indigenous Range Dynamics (Historical Context)", "", "*Extinction records detected*", "")
  n_extinct <- nrow(extinct_records)

  # --- Range Metrics (Historical vs Current) ---
  # Try to get from report first, then from data
  eoo_hist <- NULL
  eoo_curr <- NULL
  aoo_hist <- NULL
  aoo_curr <- NULL

  if (!is.null(ind_report) && !is.null(ind_report$metrics)) {
    eoo_hist <- ind_report$metrics$eoo_historical_km2
    eoo_curr <- ind_report$metrics$eoo_current_km2
    aoo_hist <- ind_report$metrics$aoo_historical_km2
    aoo_curr <- ind_report$metrics$aoo_current_km2
  }

  # Fallback to data columns if available
  if (is.null(eoo_hist) && "eoo_historical" %in% names(ind_data)) {
    eoo_hist <- ind_data$eoo_historical[1]
  }
  if (is.null(eoo_curr) && "eoo_current" %in% names(ind_data)) {
    eoo_curr <- ind_data$eoo_current[1]
  }
  if (is.null(aoo_hist) && "aoo_historical" %in% names(ind_data)) {
    aoo_hist <- ind_data$aoo_historical[1]
  }
  if (is.null(aoo_curr) && "aoo_current" %in% names(ind_data)) {
    aoo_curr <- ind_data$aoo_current[1]
  }

  # Display historical vs current metrics if both available
  if (!is.null(eoo_hist) && !is.na(eoo_hist) && !is.null(eoo_curr) && !is.na(eoo_curr)) {
    lines <- c(lines, "#### Range Metrics", "")
    lines <- c(lines, paste0("- **Historical EOO:** ", .fmt_num(eoo_hist), " km\u00B2"))
    lines <- c(lines, paste0("- **Current EOO:** ", .fmt_num(eoo_curr), " km\u00B2"))

    if (!is.null(aoo_hist) && !is.na(aoo_hist) && !is.null(aoo_curr) && !is.na(aoo_curr)) {
      lines <- c(lines, paste0("- **Historical AOO:** ", .fmt_num(aoo_hist), " km\u00B2"))
      lines <- c(lines, paste0("- **Current AOO:** ", .fmt_num(aoo_curr), " km\u00B2"))
    }

    # Range change signal and magnitude
    if (eoo_hist > 0) {
      delta_eoo_pct <- round(((eoo_curr - eoo_hist) / eoo_hist) * 100, 1)

      if (abs(delta_eoo_pct) < 5) {
        signal <- "stable"
      } else if (delta_eoo_pct < 0) {
        signal <- "contraction"
      } else {
        signal <- "expansion"
      }

      lines <- c(lines, paste0("- **Range change signal:** ", signal))
      lines <- c(lines, paste0("- **EOO change:** ", ifelse(delta_eoo_pct >= 0, "+", ""), delta_eoo_pct, "%"))

      if (!is.null(aoo_hist) && !is.na(aoo_hist) && aoo_hist > 0 && !is.null(aoo_curr) && !is.na(aoo_curr)) {
        delta_aoo_pct <- round(((aoo_curr - aoo_hist) / aoo_hist) * 100, 1)
        lines <- c(lines, paste0("- **AOO change:** ", ifelse(delta_aoo_pct >= 0, "+", ""), delta_aoo_pct, "%"))
      }
    }

    lines <- c(lines, "")
  }

  # --- Extinction Summary ---
  lines <- c(lines, "#### Extinction Summary", "")

  if ("year" %in% names(extinct_records)) {
    years <- extinct_records$year[!is.na(extinct_records$year)]
    if (length(years) > 0) {
      lines <- c(lines, paste0("- **Extinct localities:** ", n_extinct))
      lines <- c(lines, paste0("- **Extinction timeframe:** ", min(years), "-", max(years)))
    }
  } else {
    lines <- c(lines, paste0("- **Extinct localities:** ", n_extinct))
  }

  if ("country" %in% names(extinct_records)) {
    country_counts <- table(extinct_records$country[!is.na(extinct_records$country)])
    if (length(country_counts) > 0) {
      breakdown <- paste(names(country_counts), country_counts, sep = ": ", collapse = ", ")
      lines <- c(lines, paste0("- **Regional breakdown:** ", breakdown))
    }
  }

  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

.build_fragmentation_section <- function(ind_report) {
  if (is.null(ind_report$fragmentation) || !ind_report$fragmentation$computed) return(NULL)

  lines <- c("### 2.4 Fragmentation Assessment", "", "*Applied only to endemic and regional species.*", "")

  if (ind_report$fragmentation$status == "detected") {
    n_clusters <- ind_report$fragmentation$n_clusters %||% "N/A"
    cluster_sizes <- ind_report$fragmentation$cluster_sizes %||% "N/A"
    lines <- c(lines,
               "- **Fragmentation signal:** Detected",
               paste0("- **Number of clusters:** ", n_clusters),
               paste0("- **Cluster composition:** ", cluster_sizes),
               "",
               "*Interpretation:* Native occurrences are distributed across multiple spatially disjunct clusters, which may indicate barriers to gene flow or heightened vulnerability to localized threats.",
               ""
    )
  } else {
    lines <- c(lines, "- **Fragmentation signal:** Not detected", "", "*Interpretation:* Native occurrences do not show significant spatial fragmentation.", "")
  }

  lines <- c(lines, "*Note:* This is a conservative descriptive signal and does not represent a formal connectivity assessment.", "")
  paste(lines, collapse = "\n")
}


# ==============================================================================
# SECTION 3: NON-INDIGENOUS RANGE OVERVIEW
# ==============================================================================

.build_section3_non_indigenous <- function(sp, scenario, non_ind_data, non_ind_report, feow_map, hydrobasin_names) {
  if (scenario == 1 || is.null(non_ind_report)) {
    return("## 3. NON-INDIGENOUS RANGE OVERVIEW\n\n*Not applicable (no non-indigenous populations detected)*\n")
  }

  lines <- c(
    "## 3. NON-INDIGENOUS RANGE OVERVIEW",
    "",
    "### 3.1 Status",
    "",
    "- **Non-indigenous populations:** Present",
    paste0("- **Category:** ", non_ind_report$metrics$category %||% "N/A"),
    "",
    "### 3.2 Informative Metrics",
    "",
    paste0("- **Extent of occurrence (EOO):** ", .fmt_num(non_ind_report$metrics$eoo_km2), " km\u00B2"),
    paste0("- **Area of occupancy (AOO):** ", .fmt_num(non_ind_report$metrics$aoo_km2), " km\u00B2")
  )

  countries <- non_ind_report$spatial_context$countries %||% ""
  if (nzchar(countries)) lines <- c(lines, paste0("- **Countries:** ", countries))

  admin_units <- non_ind_report$spatial_context$admin_units %||% ""
  if (nzchar(admin_units)) lines <- c(lines, paste0("- **Subnational administrative units:** ", admin_units))

  n_basins <- non_ind_report$counts$n_hydrobasins %||% 0
  lines <- c(lines, paste0("- **Hydrographic basins:** (n=", n_basins, ")"))

  prot_areas <- non_ind_report$spatial_context$protected_areas %||% ""
  if (nzchar(prot_areas)) lines <- c(lines, paste0("- **Protected areas represented:** ", prot_areas))

  year_min <- non_ind_report$temporal$year_min
  year_max <- non_ind_report$temporal$year_max
  if (!is.null(year_min) && !is.null(year_max) && !is.infinite(year_min) && !is.infinite(year_max)) {
    lines <- c(lines, paste0("- **Temporal coverage:** ", year_min, "-", year_max))
    lines <- c(lines, paste0("- **First record year:** ", year_min))
  }

  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}


# ==============================================================================
# SECTION 4: DATA QUALITY, TRACEABILITY, AND PROVENANCE
# ==============================================================================

.build_section4_provenance <- function(sp, ind_data, non_ind_data, ind_report, non_ind_report) {
  n_indigenous <- if (!is.null(ind_data)) nrow(ind_data) else 0
  n_non_indigenous <- if (!is.null(non_ind_data)) nrow(non_ind_data) else 0
  n_total <- n_indigenous + n_non_indigenous

  lines <- c(
    "## 4. DATA QUALITY, TRACEABILITY, AND PROVENANCE",
    "",
    "### 4.1 Data Summary",
    "",
    paste0("- **Total records analyzed:** ", n_total),
    paste0("  - Indigenous records: ", n_indigenous),
    paste0("  - Non-indigenous records: ", n_non_indigenous)
  )
  
  #all_data <- rbind(ind_data, non_ind_data)
  all_data <- dplyr::bind_rows(ind_data, non_ind_data)
  if (!is.null(all_data) && "accuracy" %in% names(all_data)) {
    n_high <- sum(all_data$accuracy == "exact", na.rm = TRUE)
    pct_high <- round((n_high / n_total) * 100, 1)
    lines <- c(lines, paste0("- **High-accuracy records:** ", n_high, " (", pct_high, "%)"))
  }
  lines <- c(lines, "")

  if (!is.null(all_data) && "doi" %in% names(all_data)) {
    n_doi <- sum(!is.na(all_data$doi) & nzchar(all_data$doi), na.rm = TRUE)
    pct_doi <- round((n_doi / n_total) * 100, 1)
    lines <- c(lines, "### 4.2 Bibliographic Coverage", "", paste0("- **DOI-linked records:** ", n_doi, " (", pct_doi, "%)"), "")
  }

  lines <- c(lines,
             "### 4.3 Raw Data Provenance",
             "",
             "- **Raw data repository:** World of Crayfish\u00AE",
             "- **Raw data citation:** Ion et al. (2024) *World of Crayfish\u2122: A web platform towards real-time global mapping of freshwater crayfish and their pathogens.* PeerJ 12:e18229. https://doi.org/10.7717/peerj.18229",
             "",
             "### 4.4 Processing Framework Provenance",
             "",
             "- **Processing framework:** cheCkOVER",
             paste0("- **Processing date:** ", Sys.Date()),
             "",
             "### 4.5 Interpretation Note",
             "",
             "This geo-narrative is a **descriptive synthesis** derived from curated occurrence data and standardized spatial analyses. It does **not** constitute a formal conservation status assessment and should be interpreted in conjunction with primary sources and expert evaluation.",
             ""
  )

  paste(lines, collapse = "\n")
}


# ==============================================================================
# SECTION 5: FORMAL NARRATIVE SUMMARY
# ==============================================================================

.build_section5_formal_narrative <- function(sp, scenario, vern_str, ind_data, non_ind_data,
                                             ind_report, non_ind_report, feow_map, hydrobasin_names) {
  paragraphs <- c()

  # Paragraph 1: Taxonomic identity
  paragraphs <- c(paragraphs, .build_p1_taxonomy(sp, vern_str, ind_report, non_ind_report))

  # Paragraph 2: Indigenous range (if applicable)
  if (scenario %in% c(1, 3) && !is.null(ind_report)) {
    paragraphs <- c(paragraphs, .build_p2_indigenous_range(sp, ind_data, ind_report))
  }

  # Paragraph 3: Conservation context (if indigenous)
  if (scenario %in% c(1, 3) && !is.null(ind_report)) {
    paragraphs <- c(paragraphs, .build_p3_conservation(sp, ind_data, ind_report))
  }

  # Paragraph 4: Non-indigenous (if applicable)
  if (scenario %in% c(2, 3) && !is.null(non_ind_report)) {
    paragraphs <- c(paragraphs, .build_p4_non_indigenous(sp, non_ind_data, non_ind_report, scenario))
  }

  # Paragraph 5: Data quality
  paragraphs <- c(paragraphs, .build_p5_provenance(sp, ind_data, non_ind_data, ind_report, non_ind_report))

  full_text <- paste(paragraphs, collapse = "\n\n")
  word_count <- length(strsplit(full_text, "\\s+")[[1]])

  section_markdown <- paste("## 5. FORMAL NARRATIVE SUMMARY (Human-Readable)", "", full_text, sep = "\n")

  list(markdown = section_markdown, text = full_text, word_count = word_count)
}

.build_p1_taxonomy <- function(sp, vern_str, ind_report, non_ind_report) {
  category <- "unknown"
  eoo <- "undetermined"

  if (!is.null(ind_report)) {
    category <- ind_report$metrics$iucn_category %||% "unknown"
    eoo <- .fmt_num(ind_report$metrics$eoo_km2)
  } else if (!is.null(non_ind_report)) {
    category <- non_ind_report$metrics$category %||% "unknown"
    eoo <- .fmt_num(non_ind_report$metrics$eoo_km2)
  }

  if (!is.na(vern_str) && nzchar(vern_str)) {
    first_name <- .format_vernacular_names(strsplit(vern_str, " \\| ")[[1]][1])
    sprintf("*%s*, commonly known as %s, is categorized as **%s** with an extent of occurrence of %s km\u00B2.",
            sp, first_name, category, eoo)
  } else {
    sprintf("*%s* is categorized as **%s** with an extent of occurrence of %s km\u00B2.", sp, category, eoo)
  }
}

.build_p2_indigenous_range <- function(sp, ind_data, ind_report) {
  countries <- ind_report$spatial_context$countries %||% "unspecified regions"
  n_countries <- ind_report$counts$n_countries %||% 0
  n_basins <- ind_report$counts$n_hydrobasins %||% 0

  year_min <- ind_report$temporal$year_min
  year_max <- ind_report$temporal$year_max
  temporal_str <- ""
  if (!is.null(year_min) && !is.null(year_max) && !is.infinite(year_min) && !is.infinite(year_max)) {
    temporal_str <- sprintf(" Validated records span %d-%d.", year_min, year_max)
  }

  recent_str <- ""
  if (!is.null(ind_data) && nrow(ind_data) > 0 && "year" %in% names(ind_data)) {
    pct_recent <- round((sum(ind_data$year >= 2000, na.rm = TRUE) / nrow(ind_data)) * 100, 1)
    recent_str <- sprintf(" Of these, %s%% were collected post-2000.", pct_recent)
  }

  sprintf("Native occurrences span %d countries: %s. The species has been recorded across %d hydrographic basins and multiple ecoregions.%s%s",
          n_countries, countries, n_basins, temporal_str, recent_str)
}

.build_p3_conservation <- function(sp, ind_data, ind_report) {
  prot_pct <- ind_report$conservation$protection_percentage %||% 0
  prot_n <- ind_report$conservation$n_protected_records %||% 0

  if (prot_pct < 25) {
    prot_str <- sprintf("Only %d occurrence records (%.1f%%) fall within designated protected areas, indicating limited formal conservation coverage.", prot_n, prot_pct)
  } else if (prot_pct >= 75) {
    prot_str <- sprintf("A substantial %d occurrence records (%.1f%%) fall within designated protected areas.", prot_n, prot_pct)
  } else {
    prot_str <- sprintf("Approximately %d occurrence records (%.1f%%) fall within designated protected areas.", prot_n, prot_pct)
  }

  frag_str <- ""
  if (!is.null(ind_report$fragmentation) && ind_report$fragmentation$computed && ind_report$fragmentation$status == "detected") {
    n_clusters <- ind_report$fragmentation$n_clusters %||% "multiple"
    frag_str <- sprintf(" Distributional fragmentation has been detected, with %s spatial clusters identified, suggesting potential barriers to gene flow.", n_clusters)
  }

  extinct_str <- ""
  if (!is.null(ind_data) && "is_extinct" %in% names(ind_data)) {
    n_extinct <- sum(ind_data$is_extinct == TRUE, na.rm = TRUE)
    if (n_extinct > 0) extinct_str <- sprintf(" Additionally, %d extinct localities have been documented.", n_extinct)
  }

  paste0(prot_str, frag_str, extinct_str)
}

.build_p4_non_indigenous <- function(sp, non_ind_data, non_ind_report, scenario) {
  countries <- non_ind_report$spatial_context$countries %||% "various regions"
  n_basins <- non_ind_report$counts$n_hydrobasins %||% 0
  eoo <- .fmt_num(non_ind_report$metrics$eoo_km2)
  year_min <- non_ind_report$temporal$year_min

  first_year_str <- ""
  if (!is.null(year_min) && !is.infinite(year_min)) {
    first_year_str <- sprintf(" The earliest documented record dates to %d.", year_min)
  }

  if (scenario == 2) {
    sprintf("This species is recorded only as non-indigenous in the analyzed dataset, with populations documented in %s, spanning %d hydrographic basins and representing an extent of occurrence of %s km\u00B2.%s",
            countries, n_basins, eoo, first_year_str)
  } else {
    sprintf("Outside its native range, introduced populations have been documented in %s, spanning %d hydrographic basins%s.",
            countries, n_basins, if (nzchar(first_year_str)) paste0(", with the earliest introduction record dating to ", year_min) else "")
  }
}

.build_p5_provenance <- function(sp, ind_data, non_ind_data, ind_report, non_ind_report) {
  n_indigenous <- if (!is.null(ind_data)) nrow(ind_data) else 0
  n_non_indigenous <- if (!is.null(non_ind_data)) nrow(non_ind_data) else 0
  n_total <- n_indigenous + n_non_indigenous

  #all_data <- rbind(ind_data, non_ind_data)
  all_data <- dplyr::bind_rows(ind_data, non_ind_data)
  accuracy_str <- ""
  if (!is.null(all_data) && "accuracy" %in% names(all_data)) {
    pct_high <- round((sum(all_data$accuracy == "exact", na.rm = TRUE) / n_total) * 100, 1)
    accuracy_str <- sprintf(", of which %s%% meet high spatial accuracy standards", pct_high)
  }

  sprintf("This synthesis is based on %d validated occurrence records (%d native, %d non-indigenous)%s, processed through cheCkOVER v1.0 on %s. Raw data are maintained in the World of Crayfish\u00AE repository (Ion et al. 2024). **This geo-narrative is a descriptive synthesis and does not constitute a formal IUCN Red List assessment.**",
          n_total, n_indigenous, n_non_indigenous, accuracy_str, Sys.Date())
}

#### MODULE 10: CANONICAL NARRATIVE TEMPLATE GENERATOR ####

#' Safe accessor for nested list elements (handles both list and simplified structures)
#' @param x List or data frame
#' @param ... Path components
#' @param default Default value if not found
#' @return Value or default
# .safe_get <- function(x, ..., default = NULL) {
#   path <- c(...)
#   result <- x
#   for (key in path) {
#     if (is.null(result)) return(default)
#     if (is.list(result) && key %in% names(result)) {
#       result <- result[[key]]
#     } else if (is.data.frame(result) && key %in% names(result)) {
#       result <- result[[key]]
#     } else {
#       return(default)
#     }
#   }
#   if (is.null(result) || (length(result) == 1 && is.na(result))) {
#     return(default)
#   }
#   # If result is a list with one element, extract it
#   if (is.list(result) && length(result) == 1 && !is.data.frame(result)) {
#     result <- result[[1]]
#   }
#   return(result)
# }

#' Generate canonical geo-narrative template for all species
#' @param scenario_table Scenario detection table
#' @param result_indigenous Indigenous result object
#' @param result_non_indigenous Non-indigenous result object
#' @param indigenous_reports Indigenous reports
#' @param non_indigenous_reports Non-indigenous reports
#' @param scenario3_merged Scenario 3 merged reports
#' @param vernacular_lookup Vernacular names lookup
#' @param output_dir Output directory
#' @param feow_lookup_path FEOW name lookup path
#' @param hydrobasin_names HydroBASINS name lookup
#' @return List of generated canonical narratives
#' generate_canonical_narratives <- function(scenario_table,
#'                                           result_indigenous,
#'                                           result_non_indigenous,
#'                                           indigenous_reports,
#'                                           non_indigenous_reports,
#'                                           scenario3_merged,
#'                                           vernacular_lookup = NULL,
#'                                           output_dir = "checkover_output",
#'                                           feow_lookup_path = NULL,
#'                                           hydrobasin_names = NULL) {
#'   module <- "MODULE10_CANONICAL"
#'   
#'   with_log_section(module, {
#'     log_info("=== MODULE 10: CANONICAL NARRATIVE GENERATION ===", module = module)
#'     
#'     # Create directories
#'     canonical_dir <- file.path(output_dir, "narratives_canonical")
#'     formal_dir <- file.path(output_dir, "narratives_formal")
#'     
#'     for (d in c(canonical_dir, formal_dir)) {
#'       if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
#'     }
#'     
#'     # Load helpers
#'     vern_map <- .extract_vernacular_map(vernacular_lookup)
#'     feow_map <- .load_feow_map(feow_lookup_path, module)
#'     
#'     # Process all species
#'     all_species <- unique(scenario_table$species)
#'     all_species <- all_species[!is.na(all_species)]
#'     
#'     log_info("Generating canonical narratives for %d species...", length(all_species), module = module)
#'     
#'     canonical_narratives <- list()
#'     failed_species <- character(0)
#'     
#'     for (sp in all_species) {
#'       log_info("Processing: %s", sp, module = module)
#'       
#'       # Wrap each species in tryCatch for robustness
#'       tryCatch({
#'         sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
#'         scenario <- scenario_table$scenario[scenario_table$species == sp][1]
#'         
#'         # Build canonical narrative
#'         canonical <- .build_canonical_narrative(
#'           sp, scenario, scenario_table,
#'           result_indigenous, result_non_indigenous,
#'           indigenous_reports, non_indigenous_reports, scenario3_merged,
#'           vern_map, feow_map, hydrobasin_names, module
#'         )
#'         
#'         if (!is.null(canonical)) {
#'           # Save full canonical markdown
#'           canonical_file <- file.path(canonical_dir, paste0(sp_clean, "_canonical.md"))
#'           writeLines(canonical$full_markdown, canonical_file)
#'           
#'           # Save formal narrative (Section 5) as TXT
#'           formal_txt_file <- file.path(formal_dir, paste0(sp_clean, "_narrative.txt"))
#'           writeLines(canonical$formal_narrative_text, formal_txt_file)
#'           
#'           # Save formal narrative (Section 5) as JSON
#'           formal_json <- list(
#'             species = sp,
#'             scenario = scenario,
#'             narrative = canonical$formal_narrative_text,
#'             word_count = canonical$formal_narrative_word_count,
#'             metadata = canonical$metadata
#'           )
#'           
#'           formal_json_file <- file.path(formal_dir, paste0(sp_clean, "_narrative.json"))
#'           jsonlite::write_json(formal_json, formal_json_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
#'           
#'           canonical_narratives[[sp]] <- list(
#'             canonical_markdown = canonical_file,
#'             formal_text = formal_txt_file,
#'             formal_json = formal_json_file
#'           )
#'         }
#'       }, error = function(e) {
#'         log_warn("  Failed to process %s: %s", sp, conditionMessage(e), module = module)
#'         failed_species <<- c(failed_species, sp)
#'       })
#'     }
#'     
#'     log_info("Generated %d canonical narratives.", length(canonical_narratives), module = module)
#'     if (length(failed_species) > 0) {
#'       log_warn("Failed to generate narratives for %d species: %s", 
#'                length(failed_species), paste(failed_species, collapse = ", "), module = module)
#'     }
#'     
#'     return(list(
#'       narratives = canonical_narratives,
#'       failed_species = failed_species
#'     ))
#'   })
#' }
#' 
#' 
#' # --- HELPER: Extract Vernacular Map ---
#' .extract_vernacular_map <- function(vernacular_lookup) {
#'   if (is.null(vernacular_lookup)) return(NULL)
#'   
#'   if (is.list(vernacular_lookup) && "wide" %in% names(vernacular_lookup)) {
#'     return(vernacular_lookup$wide)
#'   } else if (is.data.frame(vernacular_lookup)) {
#'     return(vernacular_lookup)
#'   }
#'   
#'   return(NULL)
#' }
#' 
#' 
#' # --- HELPER: Load FEOW Map ---
#' .load_feow_map <- function(feow_lookup_path, module) {
#'   if (is.null(feow_lookup_path) || !file.exists(feow_lookup_path)) return(NULL)
#'   
#'   ext <- tolower(tools::file_ext(feow_lookup_path))
#'   tryCatch({
#'     if (ext %in% c("xlsx", "xls")) {
#'       if (requireNamespace("readxl", quietly = TRUE)) {
#'         feow_map <- as.data.frame(readxl::read_excel(feow_lookup_path))
#'         names(feow_map) <- toupper(names(feow_map))
#'         if ("ID" %in% names(feow_map)) feow_map$ID <- as.integer(feow_map$ID)
#'         return(feow_map)
#'       }
#'     } else {
#'       feow_map <- read.csv(feow_lookup_path, stringsAsFactors = FALSE)
#'       names(feow_map) <- toupper(names(feow_map))
#'       if ("ID" %in% names(feow_map)) feow_map$ID <- as.integer(feow_map$ID)
#'       return(feow_map)
#'     }
#'   }, error = function(e) {
#'     log_warn("Failed to load FEOW lookup: %s", conditionMessage(e), module = module)
#'   })
#'   
#'   return(NULL)
#' }
#' 
#' 
#' #' Safely read JSON report file
#' #' @param report_file Path to JSON file
#' #' @param module Module name for logging
#' #' @return Parsed report list or NULL
#' .safe_read_report <- function(report_file, module) {
#'   if (!file.exists(report_file)) return(NULL)
#'   
#'   tryCatch({
#'     # Use simplifyVector = FALSE to avoid rbind issues with inconsistent structures
#'     # This returns pure lists which we handle with .safe_get()
#'     report <- jsonlite::read_json(report_file, simplifyVector = FALSE)
#'     return(report)
#'   }, error = function(e) {
#'     log_warn("  Failed to read report %s: %s", basename(report_file), conditionMessage(e), module = module)
#'     return(NULL)
#'   })
#' }
#' 
#' 
#' # --- MAIN: Build Canonical Narrative ---
#' .build_canonical_narrative <- function(sp, scenario, scenario_table,
#'                                        result_indigenous, result_non_indigenous,
#'                                        indigenous_reports, non_indigenous_reports, scenario3_merged,
#'                                        vern_map, feow_map, hydrobasin_names, module) {
#'   
#'   # Get report data
#'   sp_clean <- gsub("[^A-Za-z0-9_]", "_", sp)
#'   
#'   report <- NULL
#'   if (scenario == 1 || scenario == 3) {
#'     report_file <- file.path(dirname(dirname(indigenous_reports$per_species[[1]])), 
#'                              "indigenous", paste0(sp_clean, ".json"))
#'     report <- .safe_read_report(report_file, module)
#'   } else if (scenario == 2) {
#'     report_file <- file.path(dirname(dirname(non_indigenous_reports$per_species[[1]])), 
#'                              "non_indigenous", paste0(sp_clean, ".json"))
#'     report <- .safe_read_report(report_file, module)
#'   }
#'   
#'   if (is.null(report)) {
#'     log_warn("  No report found for %s", sp, module = module)
#'     return(NULL)
#'   }
#'   
#'   # Extract vernacular
#'   vern_str <- NA_character_
#'   if (!is.null(vern_map) && "species" %in% names(vern_map)) {
#'     idx <- match(sp, vern_map$species)
#'     if (!is.na(idx) && "vernacular_string" %in% names(vern_map)) {
#'       raw_v <- vern_map$vernacular_string[idx]
#'       if (!is.na(raw_v) && nzchar(raw_v)) {
#'         vern_str <- gsub("^[\"\u2018\u2019\u201C\u201D]+|[\"\u2018\u2019\u201C\u201D]+$", "", raw_v)
#'       }
#'     }
#'   }
#'   
#'   # Build sections
#'   section1 <- .build_section1_taxonomy(sp, vern_str, report)
#'   section2 <- .build_section2_indigenous(sp, report, feow_map, hydrobasin_names)
#'   section3 <- .build_section3_non_indigenous(sp, scenario, result_non_indigenous, feow_map, hydrobasin_names)
#'   section4 <- .build_section4_provenance(sp, report)
#'   section5 <- .build_section5_formal_narrative(sp, scenario, vern_str, report, section2, section3, feow_map, hydrobasin_names)
#'   
#'   # Assemble full canonical markdown
#'   full_markdown <- paste(
#'     "---",
#'     "title: Canonical Geo-Narrative",
#'     paste0("species: ", sp),
#'     paste0("date: ", Sys.Date()),
#'     "version: 0.9",
#'     "---",
#'     "",
#'     section1,
#'     "",
#'     section2,
#'     "",
#'     section3,
#'     "",
#'     section4,
#'     "",
#'     section5,
#'     sep = "\n"
#'   )
#'   
#'   return(list(
#'     full_markdown = full_markdown,
#'     formal_narrative_text = section5$text,
#'     formal_narrative_word_count = section5$word_count,
#'     metadata = list(
#'       species = sp,
#'       scenario = scenario,
#'       vernacular = vern_str,
#'       generated_date = as.character(Sys.Date())
#'     )
#'   ))
#' }
#' 
#' 
#' # --- SECTION 1: TAXONOMIC IDENTITY ---
#' .build_section1_taxonomy <- function(sp, vern_str, report) {
#'   lines <- c(
#'     "## 1. TAXONOMIC IDENTITY",
#'     "",
#'     paste0("**Scientific name:** *", sp, "*"),
#'     ""
#'   )
#'   
#'   # Add vernacular if available
#'   if (!is.na(vern_str) && nzchar(vern_str)) {
#'     # Parse vernacular string (format: "name1 | name2 | name3")
#'     names_list <- strsplit(vern_str, " \\| ")[[1]]
#'     
#'     # Group by language (assume English unless otherwise noted)
#'     lines <- c(lines, paste0("**Common names:** ", vern_str), "")
#'   }
#'   
#'   paste(lines, collapse = "\n")
#' }
#' 
#' 
#' # --- SECTION 2: INDIGENOUS RANGE OVERVIEW ---
#' .build_section2_indigenous <- function(sp, report, feow_map, hydrobasin_names) {
#'   # Use safe accessors for nested values
#'   iucn_category <- .safe_get(report, "metrics", "iucn_category")
#'   
#'   if (is.null(iucn_category)) {
#'     return("## 2. INDIGENOUS RANGE OVERVIEW\n\n*Not applicable (non-indigenous populations only)*\n")
#'   }
#'   
#'   lines <- c(
#'     "## 2. INDIGENOUS RANGE OVERVIEW",
#'     "",
#'     "### 2.1 Distribution Summary",
#'     ""
#'   )
#'   
#'   # Metrics - use safe accessors
#'   eoo <- .safe_get(report, "metrics", "eoo_km2", default = NA)
#'   aoo <- .safe_get(report, "metrics", "aoo_km2", default = NA)
#'   category <- iucn_category
#'   
#'   eoo_str <- if (!is.na(eoo) && is.numeric(eoo)) format(round(eoo, 0), big.mark = ",") else "N/A"
#'   aoo_str <- if (!is.na(aoo) && is.numeric(aoo)) format(round(aoo, 0), big.mark = ",") else "N/A"
#'   
#'   lines <- c(
#'     lines,
#'     paste0("- **Distribution category:** ", category),
#'     paste0("- **Extent of occurrence (EOO):** ", eoo_str, " km²"),
#'     paste0("- **Area of occupancy (AOO):** ", aoo_str, " km²"),
#'     ""
#'   )
#'   
#'   # Geographic distribution - use safe accessors
#'   countries <- .safe_get(report, "spatial_context", "countries", default = "N/A")
#'   n_countries <- .safe_get(report, "counts", "n_countries", default = 0)
#'   
#'   lines <- c(
#'     lines,
#'     "#### Geographic Distribution",
#'     "",
#'     paste0("- **Countries:** ", countries, " (n=", n_countries, ")"),
#'     ""
#'   )
#'   
#'   # Conservation - use safe accessors
#'   prot_pct <- .safe_get(report, "conservation", "protection_percentage", default = 0)
#'   prot_recs <- .safe_get(report, "conservation", "n_protected_records", default = 0)
#'   
#'   lines <- c(
#'     lines,
#'     "#### Conservation Context",
#'     "",
#'     paste0("- **Protected area coverage:** ", prot_recs, " records (", round(as.numeric(prot_pct), 1), "%)"),
#'     ""
#'   )
#'   
#'   # Fragmentation - use safe accessors
#'   frag_computed <- .safe_get(report, "fragmentation", "computed", default = FALSE)
#'   if (isTRUE(frag_computed)) {
#'     lines <- c(
#'       lines,
#'       "### 2.4 FRAGMENTATION ASSESSMENT",
#'       ""
#'     )
#'     
#'     frag_status <- .safe_get(report, "fragmentation", "status", default = "not_detected")
#'     if (frag_status == "detected") {
#'       n_clusters <- .safe_get(report, "fragmentation", "n_clusters", default = 0)
#'       cluster_sizes <- .safe_get(report, "fragmentation", "cluster_sizes_n", default = "N/A")
#'       lines <- c(
#'         lines,
#'         paste0("- **Fragmentation signal:** Detected"),
#'         paste0("- **Number of clusters:** ", n_clusters),
#'         paste0("- **Cluster composition:** ", cluster_sizes),
#'         ""
#'       )
#'     } else {
#'       lines <- c(
#'         lines,
#'         "- **Fragmentation signal:** None detected",
#'         ""
#'       )
#'     }
#'   }
#'   
#'   paste(lines, collapse = "\n")
#' }
#' 
#' 
#' # --- SECTION 3: NON-INDIGENOUS RANGE OVERVIEW ---
#' .build_section3_non_indigenous <- function(sp, scenario, result_non_indigenous, feow_map, hydrobasin_names) {
#'   if (scenario == 1) {
#'     return("## 3. NON-INDIGENOUS RANGE OVERVIEW\n\n*Not applicable (no non-indigenous populations detected)*\n")
#'   }
#'   
#'   # Get non-indigenous data - safely handle NULL or missing clean_data
#'   sp_data <- NULL
#'   if (!is.null(result_non_indigenous) && !is.null(result_non_indigenous$clean_data)) {
#'     sp_data <- result_non_indigenous$clean_data[result_non_indigenous$clean_data$species == sp, ]
#'   }
#'   
#'   if (is.null(sp_data) || nrow(sp_data) == 0) {
#'     return("## 3. NON-INDIGENOUS RANGE OVERVIEW\n\n*Not applicable*\n")
#'   }
#'   
#'   lines <- c(
#'     "## 3. NON-INDIGENOUS RANGE OVERVIEW",
#'     "",
#'     "- **Non-indigenous populations:** Present",
#'     paste0("- **Total records:** ", nrow(sp_data)),
#'     ""
#'   )
#'   
#'   paste(lines, collapse = "\n")
#' }
#' 
#' 
#' # --- SECTION 4: DATA QUALITY & PROVENANCE ---
#' .build_section4_provenance <- function(sp, report) {
#'   n_total <- .safe_get(report, "counts", "n_total", default = 0)
#'   
#'   lines <- c(
#'     "## 4. DATA QUALITY, TRACEABILITY, AND PROVENANCE",
#'     "",
#'     "### 4.1 Data Summary",
#'     "",
#'     paste0("- **Total records analyzed:** ", n_total),
#'     "",
#'     "### 4.2 Raw Data Provenance",
#'     "",
#'     "- **Raw data repository:** World of Crayfish®",
#'     "- **Raw data citation:** Ion et al. (2024) *World of Crayfish™: A web platform towards real-time global mapping of freshwater crayfish and their pathogens* *PeerJ 12. e18229* https://doi.org/10.7717/peerj.18229",
#'     "",
#'     "### 4.3 Processing Framework Provenance",
#'     "",
#'     "- **Processing framework:** cheCkOVER",
#'     paste0("- **Processing date:** ", Sys.Date()),
#'     "",
#'     "### 4.4 Interpretation Note",
#'     "",
#'     "This geo-narrative is a **descriptive synthesis** derived from curated occurrence data and standardized spatial analyses. It does **not** constitute a formal conservation status assessment and should be interpreted in conjunction with primary sources and expert evaluation.",
#'     ""
#'   )
#'   
#'   paste(lines, collapse = "\n")
#' }
#' 
#' 
#' # --- SECTION 5: FORMAL NARRATIVE SUMMARY ---
#' .build_section5_formal_narrative <- function(sp, scenario, vern_str, report, section2_data, section3_data, feow_map, hydrobasin_names) {
#'   paragraphs <- list()
#'   
#'   # Paragraph 1: Taxonomic identity and distributional category (50-80 words)
#'   p1 <- .build_paragraph1_taxonomy(sp, vern_str, report)
#'   paragraphs <- c(paragraphs, p1)
#'   
#'   # Paragraph 2: Indigenous range (100-120 words) - only if applicable
#'   if (scenario == 1 || scenario == 3) {
#'     p2 <- .build_paragraph2_indigenous(sp, report, feow_map, hydrobasin_names)
#'     paragraphs <- c(paragraphs, p2)
#'   }
#'   
#'   # Paragraph 3: Conservation context (80-100 words) - only if indigenous
#'   if (scenario == 1 || scenario == 3) {
#'     p3 <- .build_paragraph3_conservation(sp, report)
#'     paragraphs <- c(paragraphs, p3)
#'   }
#'   
#'   # Paragraph 4: Non-indigenous populations (50-70 words) - if applicable
#'   if (scenario == 2 || scenario == 3) {
#'     p4 <- .build_paragraph4_non_indigenous(sp, scenario)
#'     paragraphs <- c(paragraphs, p4)
#'   }
#'   
#'   # Paragraph 5: Data quality and provenance (60-80 words)
#'   p5 <- .build_paragraph5_provenance(sp, report)
#'   paragraphs <- c(paragraphs, p5)
#'   
#'   # Combine
#'   full_text <- paste(unlist(paragraphs), collapse = "\n\n")
#'   
#'   # Count words
#'   word_count <- length(strsplit(full_text, "\\s+")[[1]])
#'   
#'   section_text <- paste(
#'     "## 5. FORMAL NARRATIVE SUMMARY (Human-Readable)",
#'     "",
#'     full_text,
#'     sep = "\n"
#'   )
#'   
#'   return(list(
#'     markdown = section_text,
#'     text = full_text,
#'     word_count = word_count
#'   ))
#' }
#' 
#' 
#' # Paragraph builders
#' .build_paragraph1_taxonomy <- function(sp, vern_str, report) {
#'   category <- .safe_get(report, "metrics", "iucn_category")
#'   if (is.null(category)) {
#'     category <- .safe_get(report, "metrics", "category", default = "unknown")
#'   }
#'   
#'   eoo <- .safe_get(report, "metrics", "eoo_km2")
#'   eoo_str <- if (!is.null(eoo) && is.numeric(eoo)) {
#'     format(round(eoo, 0), big.mark = ",")
#'   } else {
#'     "undetermined"
#'   }
#'   
#'   if (!is.na(vern_str) && nzchar(vern_str)) {
#'     # Take first common name only
#'     first_name <- strsplit(vern_str, " \\| ")[[1]][1]
#'     sprintf("*%s*, commonly known as %s, is categorized as **%s** with an extent of occurrence of %s km².", 
#'             sp, first_name, category, eoo_str)
#'   } else {
#'     sprintf("*%s* is categorized as **%s** with an extent of occurrence of %s km².", 
#'             sp, category, eoo_str)
#'   }
#' }
#' 
#' .build_paragraph2_indigenous <- function(sp, report, feow_map, hydrobasin_names) {
#'   countries <- .safe_get(report, "spatial_context", "countries", default = "multiple regions")
#'   n_countries <- .safe_get(report, "counts", "n_countries", default = 0)
#'   
#'   sprintf("Native occurrences span %d countries: %s. The species has been recorded across multiple hydrographic basins and ecoregions.",
#'           as.integer(n_countries), countries)
#' }
#' 
#' .build_paragraph3_conservation <- function(sp, report) {
#'   prot_pct <- .safe_get(report, "conservation", "protection_percentage", default = 0)
#'   prot_recs <- .safe_get(report, "conservation", "n_protected_records", default = 0)
#'   
#'   frag_text <- ""
#'   frag_computed <- .safe_get(report, "fragmentation", "computed", default = FALSE)
#'   frag_status <- .safe_get(report, "fragmentation", "status")
#'   
#'   if (isTRUE(frag_computed) && frag_status == "detected") {
#'     n_clusters <- .safe_get(report, "fragmentation", "n_clusters", default = 0)
#'     frag_text <- sprintf(" Distributional fragmentation has been detected, with %d spatial clusters identified.", 
#'                          as.integer(n_clusters))
#'   }
#'   
#'   sprintf("Only %d occurrence records (%.1f%%) fall within designated protected areas.%s",
#'           as.integer(prot_recs), as.numeric(prot_pct), frag_text)
#' }
#' 
#' .build_paragraph4_non_indigenous <- function(sp, scenario) {
#'   if (scenario == 2) {
#'     return("This species is recorded only as a non-indigenous population in the analyzed region.")
#'   } else {
#'     return("Outside its native range, introduced populations have been documented.")
#'   }
#' }
#' 
#' .build_paragraph5_provenance <- function(sp, report) {
#'   n_total <- .safe_get(report, "counts", "n_total", default = 0)
#'   
#'   sprintf("This synthesis is based on %d validated occurrence records processed through cheCkOVER v1.0 on %s. Raw data are maintained in the World of Crayfish® repository (Ion et al. 2024). **This geo-narrative is a descriptive synthesis and does not constitute a formal IUCN Red List assessment.**",
#'           as.integer(n_total), Sys.Date())
#' }