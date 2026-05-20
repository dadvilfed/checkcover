#### MODULE 12: TEMPORAL OUTPUT RENDERING ####
# Author: 
# Phase 2 of temporal_delta. Consumes delta_data produced by Phase 1
# (R/11_temporal_delta.R) and produces:
#   1. A Section 6 markdown block for the canonical narrative
#   2. A wrapper that splices Section 6 into the v1.0 baseline canonical and
#      updates the YAML frontmatter
#   3. 4-layer temporal maps (active + suppressed + new + extinctions; NO
#      recolonization layer per Lucian's simplified spec)
#
# DESIGN CHOICES:
#   - We do NOT modify or renumber the existing v1.0 canonical sections. The
#     baseline canonical from Module 10 already uses 2.3 = Fragmentation
#     Assessment. Adding a parallel 2.3 for "Indigenous Range Dynamics" would
#     conflict. Instead we append a new Section 6 holding all temporal content.
#   - For Scenario 3 species (both indigenous and non-indigenous), Section 6
#     has sub-sections per scope (6.2 = indigenous, 6.3 = non-indigenous).
#     Sections 6.1, 6.4-6.7 are species-level (apply to whatever scope(s)
#     are present).
#   - Map colours follow cheCkOVER's existing convention from 08_maps.R:
#       Native:        #D48D00 (orange)
#       Non-indigenous:#4D0073 (purple)
#     Plus temporal layers:
#       Suppressed:    #999999 (gray, "x" symbol)
#       New detection: #00CC44 (bright green/lime, "star" symbol)
#       Extinction:    #CC0000 (red, "x" symbol)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(sf)
})

# ──────────────────────────────────────────────────────────────────────────────
# Internal helpers
# ──────────────────────────────────────────────────────────────────────────────

# Format a number with thousands separators; returns "n/a" for NA/NULL
.fmt_n <- function(x, digits = 0) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("n/a")
  formatC(x, format = "f", big.mark = ",", digits = digits)
}

# Format a percentage with sign; returns "n/a" for NA/NULL
.fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("n/a")
  sprintf("%+.*f%%", digits, x)
}

# Format a character vector as a comma-separated list, truncating if too long
.fmt_list <- function(v, max_show = 10, sep = ", ") {
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0L) return("(none)")
  if (length(v) <= max_show) return(paste(v, collapse = sep))
  shown <- paste(v[seq_len(max_show)], collapse = sep)
  paste0(shown, sprintf(" (and %d more)", length(v) - max_show))
}

# Render a small markdown table from a tibble; returns "" if empty
.fmt_table <- function(df, max_rows = 15) {
  if (is.null(df) || nrow(df) == 0L) return("(no rows)")
  df <- df[seq_len(min(nrow(df), max_rows)), , drop = FALSE]
  cols <- names(df)
  hdr  <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep  <- paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(vapply(df[i, ], function(x) {
      if (is.na(x)) "—" else as.character(x)
    }, character(1)), collapse = " | "), " |")
  }, character(1))
  paste(c(hdr, sep, rows), collapse = "\n")
}


# ──────────────────────────────────────────────────────────────────────────────
# Section 6 sub-renderers (one scope at a time)
# ──────────────────────────────────────────────────────────────────────────────

#' Render the indigenous or non-indigenous dynamics sub-section
#'
#' @param delta_scope Single-scope delta_data (the value of delta_data$indigenous
#'   or delta_data$non_indigenous).
#' @param scope_label "Indigenous" or "Non-indigenous".
#' @param section_num Section number string (e.g. "6.2").
#' @return Character markdown block.
#' @noRd
.render_scope_dynamics <- function(delta_scope, scope_label, section_num) {
  
  if (is.null(delta_scope)) {
    return(sprintf(
      "### %s %s Range Dynamics\n\n*Not applicable for this species (no %s population).*",
      section_num, scope_label, tolower(scope_label)
    ))
  }
  
  rd  <- delta_scope$range_delta
  ch  <- delta_scope$changes
  ms  <- delta_scope$masking_summary
  geo <- delta_scope$geo_changes
  
  # Range metrics block
  range_block <- sprintf(
    "**Range metrics (recalculated on active occurrences):**\n\n%s",
    .fmt_table(tibble::tibble(
      Metric        = c("EOO (km²)", "AOO (km²)"),
      Previous      = c(.fmt_n(rd$EOO_previous_km2), .fmt_n(rd$AOO_previous_km2)),
      Current       = c(.fmt_n(rd$EOO_current_km2),  .fmt_n(rd$AOO_current_km2)),
      `Change (km²)`= c(.fmt_n(rd$EOO_change_absolute), .fmt_n(rd$AOO_change_absolute)),
      `Change (%)`  = c(.fmt_pct(rd$EOO_change_percent), .fmt_pct(rd$AOO_change_percent))
    ))
  )
  
  # Changes block
  changes_block <- sprintf(
    "**Changes since %s:**\n\n- New presences: %d\n- Extinctions: %d\n- Net change: %d active localities",
    delta_scope$previous_version %||% "previous version",
    ch$n_new, ch$n_extinct, ch$net_change
  )
  
  # Masking block
  masking_block <- sprintf(
    paste(
      "**Spatial-temporal masking:**",
      "",
      "- Active occurrences: %d",
      "- Suppressed (within 500 m of an extinction event, predating it): %d",
      "- Extinct claim records (excluded from all metrics): %d",
      "- Extinction zones (500 m geodesic radius circles): %d",
      sep = "\n"
    ),
    ms$occurrences_active,
    ms$occurrences_suppressed,
    ms$occurrences_extinct %||% 0L,
    ms$extinction_zones
  )
  
  # Geographic changes (compact)
  geo_block <- sprintf(
    "**Geographic changes:**\n\n- Countries gained: %s\n- Countries extirpated: %s\n- Basins gained: %s\n- Basins extirpated: %s",
    .fmt_list(geo$countries_added),
    .fmt_list(geo$countries_extirpated),
    .fmt_list(geo$basins_added),
    .fmt_list(geo$basins_extirpated)
  )
  
  signal_note <- sprintf(
    "**Range signal:** **%s**", rd$range_signal %||% "unknown"
  )
  
  paste(
    sprintf("### %s %s Range Dynamics", section_num, scope_label),
    "",
    signal_note,
    "",
    range_block,
    "",
    changes_block,
    "",
    masking_block,
    "",
    geo_block,
    sep = "\n"
  )
}


#' Render the species-level extinction summary block (combines both scopes if present)
#'
#' @param delta_combined Top-level delta_data with possible $indigenous and
#'   $non_indigenous sub-blocks.
#' @return Character markdown block.
#' @noRd
.render_extinction_summary_combined <- function(delta_combined) {
  
  # Pool extinctions from both scopes for a unified summary
  scopes <- list()
  if (!is.null(delta_combined$indigenous))     scopes$indigenous     <- delta_combined$indigenous$extinction_summary
  if (!is.null(delta_combined$non_indigenous)) scopes$non_indigenous <- delta_combined$non_indigenous$extinction_summary
  
  total_ext <- sum(vapply(scopes, function(s) as.integer(s$total_extinctions %||% 0L), integer(1)))
  
  if (total_ext == 0L) {
    return("### 6.5 Extinction Summary\n\n*No extinction events documented in this update.*")
  }
  
  # Build per-scope tables
  scope_blocks <- vapply(names(scopes), function(sn) {
    s <- scopes[[sn]]
    if (is.null(s) || s$total_extinctions == 0L) return("")
    label <- if (sn == "indigenous") "Indigenous" else "Non-indigenous"
    
    cause_tbl <- if (!is.null(s$by_cause) && nrow(s$by_cause) > 0L) {
      sprintf("\n\n**By cause (%s):**\n\n%s", label, .fmt_table(s$by_cause))
    } else ""
    
    invader_tbl <- if (!is.null(s$primary_invaders) && nrow(s$primary_invaders) > 0L) {
      sprintf("\n\n**Primary invaders (%s):**\n\n%s", label, .fmt_table(s$primary_invaders))
    } else ""
    
    hotspot_block <- if (!is.null(s$hotspot)) {
      sprintf(
        "\n\n**Extinction hotspot (%s):** %s `%s` — %d event(s) out of %d baseline localities (%.1f%% of unit; minimum-N threshold applied). Primary cause: *%s*.",
        label,
        s$hotspot$unit_type %||% "unit",
        s$hotspot$unit_name %||% "(unnamed)",
        s$hotspot$extinctions %||% 0L,
        s$hotspot$baseline_localities %||% 0L,
        s$hotspot$percent_of_unit %||% 0,
        s$hotspot$primary_cause %||% "unknown"
      )
    } else ""
    
    # Per Lucian (2026-04): single-year timeframe is a backward-mask marker,
    # not a date range. Prefer narrative "extinct since YEAR" phrasing.
    tf <- s$extinction_timeframe %||% "n/a"
    timeframe_phrase <- if (!is.na(tf) && !grepl("–", tf, fixed = TRUE)) {
      sprintf("extinct since %s", tf)
    } else {
      sprintf("timeframe %s", tf)
    }
    
    sprintf(
      "**%s population:** %d extinction(s), %s, ~%.1f%% of previous-version range. Temporal trend: %s.%s%s%s",
      label,
      s$total_extinctions,
      timeframe_phrase,
      s$percent_range_lost %||% NA_real_,
      s$temporal_trend %||% "n/a",
      cause_tbl, invader_tbl, hotspot_block
    )
  }, character(1))
  
  paste(
    "### 6.5 Extinction Summary",
    "",
    paste(scope_blocks[scope_blocks != ""], collapse = "\n\n"),
    sep = "\n"
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# Top-level Section 6 renderer
# ──────────────────────────────────────────────────────────────────────────────

#' Render the full Section 6 (Temporal Change Detection) markdown block
#'
#' Designed to be appended to a v1.0 baseline canonical narrative. For v1.0
#' (no delta_data), returns an empty string — caller should not splice anything.
#'
#' @param delta_data Top-level delta_data list with possible `$indigenous` and
#'   `$non_indigenous` scope sub-blocks; or NULL (baseline → returns "").
#' @param version Current version string (e.g. "v1.1").
#' @return Character markdown block, or "" for baseline.
#' @export
render_temporal_section <- function(delta_data, version) {
  
  if (is.null(delta_data)) return("")
  
  # 6.1 Comparison metadata
  meta_block <- sprintf(
    paste(
      "## 6. TEMPORAL CHANGE DETECTION",
      "",
      "> This section is generated by the temporal_delta module and tracks changes vs the immediately preceding version. It is present only in v1.1 and later.",
      "",
      "### 6.1 Comparison Metadata",
      "",
      "- **Current version:** %s",
      "- **Compared against:** %s (processed %s)",
      "- **Baseline version reference:** %s",
      "- **Generation date:** %s",
      "",
      sep = "\n"
    ),
    version,
    delta_data$previous_version %||% "n/a",
    delta_data$previous_date    %||% "n/a",
    delta_data$baseline_version %||% "v1.0",
    delta_data$current_date     %||% as.character(Sys.Date())
  )
  
  # 6.2 / 6.3 Per-scope dynamics
  ind_block <- .render_scope_dynamics(delta_data$indigenous,     "Indigenous",     "6.2")
  ni_block  <- .render_scope_dynamics(delta_data$non_indigenous, "Non-indigenous", "6.3")
  
  # 6.4 Combined masking summary (across both scopes)
  total_supp <- (delta_data$indigenous$masking_summary$occurrences_suppressed %||% 0L) +
    (delta_data$non_indigenous$masking_summary$occurrences_suppressed %||% 0L)
  total_act  <- (delta_data$indigenous$masking_summary$occurrences_active     %||% 0L) +
    (delta_data$non_indigenous$masking_summary$occurrences_active     %||% 0L)
  total_ext  <- (delta_data$indigenous$masking_summary$occurrences_extinct    %||% 0L) +
    (delta_data$non_indigenous$masking_summary$occurrences_extinct %||% 0L)
  total_zones<- (delta_data$indigenous$masking_summary$extinction_zones        %||% 0L) +
    (delta_data$non_indigenous$masking_summary$extinction_zones    %||% 0L)
  masking_block <- sprintf(
    paste(
      "### 6.4 Spatial-Temporal Masking Summary (Species-Level)",
      "",
      "- **Total active occurrences (post-mask):** %d",
      "- **Total suppressed occurrences:** %d",
      "- **Total extinct claim records (excluded from all metrics):** %d",
      "- **Extinction zones generated (500 m radius geodesic circles):** %d",
      "",
      "Suppressed occurrences are records whose date predates an extinction event documented within 500 m of their location. Extinct claim records are the extinction events themselves: they contribute to no metrics (not EOO/AOO, not country/basin/PA counts, not occurrence counts) and act only as spatial+temporal markers for the backward mask. Records postdating an extinction event within the same radius remain active (recovery), per the simplified specification.",
      sep = "\n"
    ),
    total_act, total_supp, total_ext, total_zones
  )
  
  # 6.5 Extinction summary (combined)
  ext_block <- .render_extinction_summary_combined(delta_data)
  
  # 6.6 Geographic changes (recap, species level — pulled from whichever scopes are present)
  all_countries_added   <- unique(c(
    delta_data$indigenous$geo_changes$countries_added     %||% character(0),
    delta_data$non_indigenous$geo_changes$countries_added %||% character(0)
  ))
  all_countries_extirp  <- unique(c(
    delta_data$indigenous$geo_changes$countries_extirpated     %||% character(0),
    delta_data$non_indigenous$geo_changes$countries_extirpated %||% character(0)
  ))
  geo_block <- sprintf(
    paste(
      "### 6.6 Geographic Changes (Species-Level Recap)",
      "",
      "- **Countries gained (any scope):** %s",
      "- **Countries extirpated (any scope):** %s",
      "",
      "*Per-scope breakdowns appear in sections 6.2 and 6.3 above.*",
      sep = "\n"
    ),
    .fmt_list(all_countries_added),
    .fmt_list(all_countries_extirp)
  )
  
  # 6.7 JSON block
  json_str <- jsonlite::toJSON(delta_data, auto_unbox = TRUE, pretty = TRUE, na = "null")
  json_block <- sprintf(
    paste(
      "### 6.7 Temporal Change Summary (JSON)",
      "",
      "Machine-readable representation of the delta. The full structure is also written to `{species}_{version}.json`.",
      "",
      "```json",
      "%s",
      "```",
      sep = "\n"
    ),
    as.character(json_str)
  )
  
  paste(
    meta_block, ind_block, "", ni_block, "", masking_block, "", ext_block, "", geo_block, "", json_block,
    sep = "\n"
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# YAML frontmatter handling
# ──────────────────────────────────────────────────────────────────────────────

# Minimal YAML frontmatter parser/writer — we only need to read a few keys and
# write a few keys, so no external dependency.
.split_frontmatter <- function(md_text) {
  lines <- strsplit(md_text, "\n", fixed = TRUE)[[1]]
  if (length(lines) < 2L || !grepl("^---\\s*$", lines[1])) {
    return(list(yaml = NULL, body = md_text))
  }
  end_idx <- which(grepl("^---\\s*$", lines[-1]))[1] + 1L
  if (is.na(end_idx)) return(list(yaml = NULL, body = md_text))
  list(
    yaml = lines[2:(end_idx - 1L)],
    body = paste(lines[(end_idx + 1L):length(lines)], collapse = "\n")
  )
}

.update_yaml_kv <- function(yaml_lines, key, new_value) {
  pat <- sprintf("^%s\\s*:", key)
  hit <- grepl(pat, yaml_lines)
  fmt <- sprintf('%s: "%s"', key, new_value)
  if (any(hit)) {
    yaml_lines[which(hit)[1]] <- fmt
  } else {
    yaml_lines <- c(yaml_lines, fmt)
  }
  yaml_lines
}


# ──────────────────────────────────────────────────────────────────────────────
# Section 5 temporal paragraph (injected into existing formal narrative)
# ──────────────────────────────────────────────────────────────────────────────

#' Build a formal-register temporal dynamics paragraph for Section 5
#'
#' Matches the voice of Module 10's existing narrative prose. Returns "" for
#' baseline (no delta_data).
#'
#' @param delta_data Top-level delta_data (or NULL).
#' @param version Current version string.
#' @return Character — a single prose paragraph, or "".
#' @noRd
.build_temporal_narrative_paragraph <- function(delta_data, version) {
  
  if (is.null(delta_data)) return("")
  
  prev_ver  <- delta_data$previous_version %||% "the previous version"
  prev_date <- delta_data$previous_date    %||% "unknown date"
  
  # ── Per-scope range signal sentences ──
  scope_sentences <- character(0)
  
  .scope_sentence <- function(scope_dd, label) {
    if (is.null(scope_dd)) return(NULL)
    rd <- scope_dd$range_delta
    if (is.null(rd)) return(NULL)
    sig <- rd$range_signal %||% "undetermined"
    eoo_pct <- if (!is.na(rd$EOO_change_percent)) sprintf("%+.1f%%", rd$EOO_change_percent) else "n/a"
    aoo_pct <- if (!is.na(rd$AOO_change_percent)) sprintf("%+.1f%%", rd$AOO_change_percent) else "n/a"
    sprintf("%s range: **%s** (\u0394EOO: %s, \u0394AOO: %s)", label, sig, eoo_pct, aoo_pct)
  }
  
  ind_sent <- .scope_sentence(delta_data$indigenous,     "indigenous")
  ni_sent  <- .scope_sentence(delta_data$non_indigenous, "non-indigenous")
  
  if (!is.null(ind_sent) && !is.null(ni_sent)) {
    range_text <- sprintf("Range dynamics indicate %s; %s.", ind_sent, ni_sent)
  } else if (!is.null(ind_sent)) {
    range_text <- sprintf("Range dynamics indicate %s.", ind_sent)
  } else if (!is.null(ni_sent)) {
    range_text <- sprintf("Range dynamics indicate %s.", ni_sent)
  } else {
    range_text <- "Range dynamics could not be assessed."
  }
  
  # ── Extinction summary sentence ──
  total_ext <- (delta_data$indigenous$extinction_summary$total_extinctions     %||% 0L) +
    (delta_data$non_indigenous$extinction_summary$total_extinctions %||% 0L)
  
  if (total_ext > 0L) {
    # Collect timeframes and causes across scopes
    tfs <- c(delta_data$indigenous$extinction_summary$extinction_timeframe,
             delta_data$non_indigenous$extinction_summary$extinction_timeframe)
    tfs <- tfs[!is.na(tfs)]
    
    causes <- dplyr::bind_rows(
      delta_data$indigenous$extinction_summary$by_cause     %||% tibble::tibble(),
      delta_data$non_indigenous$extinction_summary$by_cause %||% tibble::tibble()
    )
    if (nrow(causes) > 0L) {
      top_causes <- causes %>%
        dplyr::group_by(.data$cause_category) %>%
        dplyr::summarise(n = sum(.data$count), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$n))
      cause_str <- paste(top_causes$cause_category, collapse = ", ")
    } else {
      cause_str <- "unknown"
    }
    
    # Build timeframe phrase using Lucian's "extinct since" convention
    if (length(tfs) == 1L) {
      tf_phrase <- if (!grepl("\u2013", tfs[1])) {
        sprintf("extinct since %s", tfs[1])
      } else {
        sprintf("timeframe %s", tfs[1])
      }
    } else {
      all_years <- unlist(lapply(tfs, function(t) {
        as.numeric(unlist(strsplit(t, "\u2013")))
      }))
      all_years <- all_years[!is.na(all_years)]
      if (length(all_years) > 0L) {
        mn <- min(all_years); mx <- max(all_years)
        tf_phrase <- if (mn == mx) sprintf("extinct since %d", mn)
        else sprintf("timeframe %d\u2013%d", mn, mx)
      } else tf_phrase <- "timeframe undetermined"
    }
    
    ext_text <- sprintf(
      "%d extinction event%s %s been documented (%s; primary driver%s: %s).",
      total_ext,
      if (total_ext > 1L) "s" else "",
      if (total_ext > 1L) "have" else "has",
      tf_phrase,
      if (nrow(top_causes) > 1L) "s" else "",
      cause_str
    )
  } else {
    ext_text <- "No extinction events have been documented in this update."
  }
  
  # ── Masking summary sentence ──
  n_supp <- (delta_data$indigenous$masking_summary$occurrences_suppressed     %||% 0L) +
    (delta_data$non_indigenous$masking_summary$occurrences_suppressed %||% 0L)
  n_ext  <- (delta_data$indigenous$masking_summary$occurrences_extinct     %||% 0L) +
    (delta_data$non_indigenous$masking_summary$occurrences_extinct %||% 0L)
  n_act  <- (delta_data$indigenous$masking_summary$occurrences_active     %||% 0L) +
    (delta_data$non_indigenous$masking_summary$occurrences_active %||% 0L)
  n_total <- n_act + n_supp + n_ext
  
  if (n_supp > 0L || n_ext > 0L) {
    masking_text <- sprintf(
      "Spatial-temporal masking has suppressed %d historical record%s within 500 m of documented extinction events; %d extinction claim record%s %s excluded from all metrics. The current assessment reflects %s active occurrence%s out of %s total records.",
      n_supp,
      if (n_supp != 1L) "s" else "",
      n_ext,
      if (n_ext != 1L) "s" else "",
      if (n_ext != 1L) "are" else "is",
      format(n_act, big.mark = ","),
      if (n_act != 1L) "s" else "",
      format(n_total, big.mark = ",")
    )
  } else {
    masking_text <- sprintf(
      "No spatial-temporal masking was applied. The current assessment reflects %s active occurrence%s.",
      format(n_act, big.mark = ","),
      if (n_act != 1L) "s" else ""
    )
  }
  
  # ── Assemble paragraph ──
  sprintf(
    "**Temporal change detection (%s vs %s, %s):** %s %s %s",
    version,
    prev_ver,
    as.character(Sys.Date()),
    range_text,
    ext_text,
    masking_text
  )
}


#' Splice temporal content into a baseline canonical narrative
#'
#' Takes the v1.0 canonical markdown produced by Module 10, updates the YAML
#' frontmatter to reflect the new version + comparison metadata, injects a
#' temporal dynamics paragraph into Section 5 (before the provenance
#' disclaimer), and appends Section 6 at the end.
#'
#' For v1.0 (delta_data == NULL), returns the input unchanged except for the
#' frontmatter `output_version` field — useful when rerunning a baseline.
#'
#' @param canonical_md The full v1.0 baseline canonical markdown as a string.
#' @param delta_data Top-level delta_data (or NULL for baseline).
#' @param version Current version string.
#' @return Character — the full versioned canonical markdown.
#' @export
append_temporal_to_canonical <- function(canonical_md, delta_data, version) {
  
  fm <- .split_frontmatter(canonical_md)
  
  yaml_lines <- fm$yaml %||% character(0)
  yaml_lines <- .update_yaml_kv(yaml_lines, "output_version", version)
  yaml_lines <- .update_yaml_kv(yaml_lines, "generated", as.character(Sys.Date()))
  if (!is.null(delta_data)) {
    yaml_lines <- .update_yaml_kv(yaml_lines, "comparison_base", delta_data$previous_version %||% "")
    yaml_lines <- .update_yaml_kv(yaml_lines, "baseline_version", delta_data$baseline_version %||% "v1.0")
  }
  
  body <- fm$body
  if (!is.null(delta_data)) {
    # 1. Build the temporal paragraph for Section 5
    temporal_para <- .build_temporal_narrative_paragraph(delta_data, version)
    
    if (nzchar(temporal_para)) {
      # 2. Inject into Section 5 — find the provenance/disclaimer paragraph
      #    which always contains "This synthesis is based on" or
      #    "does not constitute a formal". Insert the temporal paragraph
      #    immediately before it.
      lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
      
      # Find the provenance line
      prov_idx <- grep(
        "This synthesis is based on|does not constitute a formal",
        lines, ignore.case = TRUE
      )
      
      if (length(prov_idx) > 0L) {
        insert_at <- prov_idx[1]  # first match
        # Walk back to find the start of the paragraph (after blank line)
        while (insert_at > 1L && nzchar(trimws(lines[insert_at - 1L]))) {
          insert_at <- insert_at - 1L
        }
        # Insert temporal paragraph + blank line before provenance paragraph
        lines <- c(
          lines[seq_len(insert_at - 1L)],
          temporal_para,
          "",
          lines[insert_at:length(lines)]
        )
        body <- paste(lines, collapse = "\n")
      } else {
        # Fallback: append temporal paragraph at end of body (before Section 6)
        body <- paste(body, "", temporal_para, sep = "\n")
      }
    }
    
    # 3. Append Section 6 at the end
    body <- paste(body, "", "", render_temporal_section(delta_data, version), sep = "\n")
  }
  
  paste(
    "---",
    paste(yaml_lines, collapse = "\n"),
    "---",
    body,
    sep = "\n"
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# Temporal maps (4 layers; NO recolonization layer)
# ──────────────────────────────────────────────────────────────────────────────

#' Generate 4-layer temporal maps for a versioned species output
#'
#' Layers (suppressed empty layers are dropped):
#'   1. Active occurrences (orange = native, purple = non-indigenous)
#'   2. Suppressed occurrences (gray "x" — historical, now extinct)
#'   3. New detections vs previous version (lime "star")
#'   4. Documented extinctions (red "x" with year + cause in description)
#'
#' For v1.0 (delta_data == NULL), produces a single-layer baseline map.
#'
#' @param occurrences Combined occurrences for the species (with `temporal_status`,
#'   `status`, `population_status`).
#' @param delta_data Top-level delta_data (or NULL for baseline).
#' @param output_dir Directory where files will be written.
#' @param species_clean Path-safe species identifier (used for filenames).
#' @param version Version string (used in filenames).
#' @param formats Character vector — which formats to write. Supports "geojson", "kml".
#' @return Named list of written file paths (invisibly).
#' @export
generate_temporal_maps <- function(occurrences,
                                   delta_data,
                                   output_dir,
                                   species_clean,
                                   version,
                                   formats = c("geojson", "kml")) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Defensive: ensure required columns exist
  if (!"temporal_status" %in% names(occurrences)) {
    occurrences$temporal_status <- "active"
  }
  if (!"suppressed_by_extinction" %in% names(occurrences)) {
    occurrences$suppressed_by_extinction <- NA_character_
  }
  
  COL_NATIVE      <- "#D48D00"  # orange (cheCkOVER convention)
  COL_NONINDIG    <- "#4D0073"  # purple
  COL_SUPPRESSED  <- "#999999"  # gray
  COL_NEW         <- "#00CC44"  # lime
  COL_EXTINCT     <- "#CC0000"  # red
  
  build_layer <- function(df, layer, color, marker, description = NA_character_) {
    if (is.null(df) || nrow(df) == 0L) return(NULL)
    if (!all(c("longitude", "latitude") %in% names(df))) return(NULL)
    df <- df %>%
      dplyr::filter(!is.na(.data$longitude), !is.na(.data$latitude))
    if (nrow(df) == 0L) return(NULL)
    df_out <- df %>%
      dplyr::mutate(layer = layer, color = color, marker = marker)
    if (!"description" %in% names(df_out)) df_out$description <- description
    sf::st_as_sf(df_out, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  }
  
  # Layer 1 — active. Split visual style by population_status if available.
  active <- occurrences %>% dplyr::filter(.data$temporal_status == "active")
  active_sf <- if (nrow(active) > 0L) {
    if ("population_status" %in% names(active)) {
      ind <- active %>% dplyr::filter(.data$population_status == "indigenous")
      nin <- active %>% dplyr::filter(.data$population_status == "non-indigenous")
      pieces <- list(
        build_layer(ind, "Active (indigenous)",     COL_NATIVE,   "circle"),
        build_layer(nin, "Active (non-indigenous)", COL_NONINDIG, "circle")
      )
      pieces <- pieces[!vapply(pieces, is.null, logical(1))]
      if (length(pieces) > 0L) do.call(rbind, pieces) else NULL
    } else {
      build_layer(active, "Active", COL_NATIVE, "circle")
    }
  } else NULL
  
  # Layer 2 — suppressed
  suppressed <- occurrences %>% dplyr::filter(.data$temporal_status == "suppressed")
  suppressed_sf <- if (nrow(suppressed) > 0L) {
    suppressed$description <- sprintf(
      "Last recorded: %s | Suppressed by extinction at: %s",
      ifelse(is.na(suppressed$year), "unknown", as.character(suppressed$year)),
      ifelse(is.na(suppressed$suppressed_by_extinction), "unknown",
             suppressed$suppressed_by_extinction)
    )
    build_layer(suppressed, "Suppressed (historical, now extinct)", COL_SUPPRESSED, "x")
  } else NULL
  
  # Layers 3 + 4 — only when temporal mode (delta_data present)
  new_sf <- NULL
  ext_sf <- NULL
  if (!is.null(delta_data)) {
    # Pool new presences and extinctions from both scopes
    new_pool <- dplyr::bind_rows(
      delta_data$indigenous$changes$new_presences     %||% tibble::tibble(),
      delta_data$non_indigenous$changes$new_presences %||% tibble::tibble()
    )
    if (nrow(new_pool) > 0L) {
      new_sf <- build_layer(new_pool, "New detections (since previous version)",
                            COL_NEW, "star")
    }
    
    ext_pool <- dplyr::bind_rows(
      delta_data$indigenous$changes$extinctions     %||% tibble::tibble(),
      delta_data$non_indigenous$changes$extinctions %||% tibble::tibble()
    )
    if (nrow(ext_pool) > 0L) {
      ext_pool$description <- sprintf(
        "Extinct: %s | Cause: %s",
        ifelse(is.na(ext_pool$extinction_year), "unknown", as.character(ext_pool$extinction_year)),
        ifelse(is.na(ext_pool$cause_category),  "unknown", ext_pool$cause_category)
      )
      ext_sf <- build_layer(ext_pool, "Documented extinctions", COL_EXTINCT, "x")
    }
  }
  
  # Combine — bind_rows on sf objects requires aligned columns; reduce to common.
  layers <- list(active_sf, suppressed_sf, new_sf, ext_sf)
  layers <- layers[!vapply(layers, is.null, logical(1))]
  
  if (length(layers) == 0L) {
    warning("generate_temporal_maps: no occurrences or events to map for ",
            species_clean, " ", version)
    return(invisible(list()))
  }
  
  # Reduce each layer to a minimal common column set
  common_cols <- c("layer", "color", "marker", "description", "geometry")
  layers <- lapply(layers, function(s) {
    if (!"description" %in% names(s)) s$description <- NA_character_
    s[, intersect(names(s), common_cols)]
  })
  
  combined <- do.call(rbind, layers)
  
  written <- list()
  
  if ("geojson" %in% formats) {
    gj <- file.path(output_dir, sprintf("%s_temporal_map_%s.geojson", species_clean, version))
    suppressWarnings(sf::st_write(combined, gj, driver = "GeoJSON",
                                  delete_dsn = TRUE, quiet = TRUE))
    written$geojson <- gj
  }
  if ("kml" %in% formats) {
    kml <- file.path(output_dir, sprintf("%s_temporal_map_%s.kml", species_clean, version))
    suppressWarnings(sf::st_write(combined, kml, driver = "KML",
                                  delete_dsn = TRUE, quiet = TRUE))
    written$kml <- kml
  }
  
  invisible(written)
}


# ──────────────────────────────────────────────────────────────────────────────
# Stratified bibliography — STUB for Phase 2 wave 2
# ──────────────────────────────────────────────────────────────────────────────

#' Generate a stratified bibliography (baseline / new / extinction refs)
#'
#' NOTE: This functionality is now implemented directly in `07_citations.R`
#' via the `source_types` field on each reference and the
#' `temporal_record_tags` parameter on `generate_all_citations()`. This
#' function is retained as a no-op for backward compatibility.
#'
#' @param ... Ignored.
#' @return NULL invisibly.
#' @export
generate_stratified_bibliography <- function(...) {
  invisible(NULL)
}
