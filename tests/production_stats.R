#!/usr/bin/env Rscript
################################################################################
# production_stats.R — Paper-quality aggregate statistics for a cheCkOVER version
#
# Extracts everything Lucian's Claude requested for the Sci Data paper's
# Abstract + §3 + §4: cohort, records (raw/validated/rejected), scenario mix,
# distributional categorization, IUCN B1, NI breakdown, HydroBASINS coverage,
# runtime. Hardware is the only thing you fill in by hand.
#
# Usage:
#   Rscript production_stats.R <root_output_dir> <version> [<log_file_path>]
#
# Examples:
#   Rscript production_stats.R checkover_output 1.0 checkover_output/logs/checkover_20260519_100648.log
#   Rscript production_stats.R checkover_output 1.1 checkover_output/logs/checkover_20260519_xxxxxx.log
#
# Output: markdown to stdout. Pipe to a file:
#   Rscript production_stats.R checkover_output 1.0 ... > stats_v1.0.md
################################################################################

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(readr)
}))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript production_stats.R <root_output_dir> <version> [<log_file_path>]")
}
ROOT <- args[[1]]
VER  <- args[[2]]
LOG  <- if (length(args) >= 3L) args[[3]] else NA_character_

cat(sprintf("# Production stats — cheCkOVER v%s\n\n", VER))

# ── COHORT (from manifest) ────────────────────────────────────────────────────
manifest_path <- file.path(ROOT, VER, "checkover", "manifest.json")
if (!file.exists(manifest_path)) stop("Manifest not found: ", manifest_path)
manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)

cat("## Cohort\n")
cat(sprintf("- Total species in cohort: **%d**\n", length(manifest$species)))
totals <- manifest$totals
if (!is.null(totals)) {
  cat(sprintf("- New (this version): %d\n", totals$new %||% 0))
  cat(sprintf("- Reprocessed (changed vs prior): %d\n", totals$reprocessed %||% 0))
  cat(sprintf("- Unchanged (inherited from prior): %d\n", totals$unchanged %||% 0))
  cat(sprintf("- Active runtime species (new + reprocessed): **%d**\n",
              totals$active_runtime_species %||% 0))
}
cat("\n")

# Locate run directory for intermediate artifacts
run_id  <- manifest$run_id %||% NA_character_
run_dir <- if (!is.na(run_id)) file.path(ROOT, "runs", run_id) else NA_character_
if (is.na(run_dir) || !dir.exists(run_dir)) {
  cat(sprintf("> [warn] Run directory '%s' not found; some stats unavailable.\n\n",
              run_dir %||% "<unknown>"))
  run_dir <- NA_character_
}

# ── RECORDS (from clean_occurrences in version dir + log for raw count) ───────
co_path <- file.path(ROOT, VER, "checkover", "clean_occurrences.tsv")
if (file.exists(co_path)) {
  co <- suppressWarnings(readr::read_tsv(
    co_path, col_types = readr::cols(.default = readr::col_character()),
    quote = "", na = c("", "NA"), show_col_types = FALSE
  ))
  n_total <- nrow(co)
  pop_col <- intersect(c("population_status", "population_type"), names(co))[1]
  n_ind <- if (!is.na(pop_col)) sum(co[[pop_col]] == "indigenous",     na.rm = TRUE) else NA
  n_non <- if (!is.na(pop_col)) sum(co[[pop_col]] == "non-indigenous", na.rm = TRUE) else NA
  cat("## Records (validated)\n")
  cat(sprintf("- Total validated records: **%d**\n", n_total))
  if (!is.na(n_ind)) cat(sprintf("- Indigenous: %d (%.1f%%)\n", n_ind, 100*n_ind/n_total))
  if (!is.na(n_non)) cat(sprintf("- Non-indigenous: %d (%.1f%%)\n", n_non, 100*n_non/n_total))
  cat("\n")
}

# ── INGEST PIPELINE (raw / mapped / cleaned from log) ─────────────────────────
if (!is.na(LOG) && file.exists(LOG)) {
  lg <- readLines(LOG, warn = FALSE)
  raw_line     <- grep("Loaded \\d+ raw records",                lg, value = TRUE)[1]
  mapped_line  <- grep("Mapped \\d+ valid records from \\d+",    lg, value = TRUE)[1]
  cleaned_line <- grep("After cleaning: \\d+ records",           lg, value = TRUE)[1]

  if (!is.na(raw_line)) {
    cat("## Ingest pipeline (raw → validated)\n")
    raw_n <- as.integer(sub(".*Loaded (\\d+) raw records.*", "\\1", raw_line))
    cat(sprintf("- Raw records (input TSV): **%d**\n", raw_n))
    if (!is.na(mapped_line)) {
      mapped_n <- as.integer(sub(".*Mapped (\\d+) valid records.*", "\\1", mapped_line))
      input_n  <- as.integer(sub(".*from (\\d+) input records.*",    "\\1", mapped_line))
      cat(sprintf("- After mapping: %d (rejected at mapping: %d, %.1f%%)\n",
                  mapped_n, input_n - mapped_n, 100*(input_n - mapped_n)/input_n))
    }
    if (!is.na(cleaned_line)) {
      clean_n <- as.integer(sub(".*After cleaning: (\\d+) records.*", "\\1", cleaned_line))
      cat(sprintf("- After cleaning (validated): **%d** (rejected at cleaning: %d, %.1f%%)\n",
                  clean_n, raw_n - clean_n, 100*(raw_n - clean_n)/raw_n))
    }
    cat("\n")
  }
}

# ── SCENARIO DISTRIBUTION ─────────────────────────────────────────────────────
if (!is.na(run_dir)) {
  scen_path <- file.path(run_dir, "species_scenarios_summary.json")
  if (file.exists(scen_path)) {
    scen <- jsonlite::read_json(scen_path, simplifyVector = TRUE)
    cat("## Scenario distribution\n")
    cat(sprintf("- Scenario 1 (Indigenous only): %d species\n",
                scen$scenario_1$count %||% scen$scenario_1 %||% 0))
    cat(sprintf("- Scenario 2 (Non-indigenous only): %d species\n",
                scen$scenario_2$count %||% scen$scenario_2 %||% 0))
    cat(sprintf("- Scenario 3 (Both): %d species\n",
                scen$scenario_3$count %||% scen$scenario_3 %||% 0))
    cat("\n")
  }

  # ── INDIGENOUS DISTRIBUTIONAL CATEGORIES + IUCN B1 ──────────────────────────
  ind_path <- file.path(run_dir, "indigenous_metrics.tsv")
  if (file.exists(ind_path)) {
    ind <- suppressWarnings(readr::read_tsv(ind_path, show_col_types = FALSE))
    cat("## Indigenous species — distributional categorization\n")
    if ("iucn_category" %in% names(ind)) {
      tab <- table(ind$iucn_category, useNA = "ifany")
      for (k in names(tab)) cat(sprintf("- %s: %d species\n",
                                        if (is.na(k)) "(unassigned)" else k, tab[k]))
    } else {
      cat("> iucn_category column not found in indigenous_metrics.tsv — columns:",
          paste(names(ind), collapse = ", "), "\n")
    }
    cat("\n")

    # IUCN B1 (range-based threat criterion) — derived from EOO_km2 if not present as column
    cat("## IUCN B1 categorization (range-based, indigenous)\n")
    if ("iucn_b1" %in% names(ind)) {
      tab <- table(ind$iucn_b1, useNA = "ifany")
      for (k in names(tab)) cat(sprintf("- %s: %d species\n",
                                        if (is.na(k)) "(unassigned)" else k, tab[k]))
    } else if ("eoo_km2" %in% names(ind)) {
      eoo <- suppressWarnings(as.numeric(ind$eoo_km2))
      b1 <- ifelse(is.na(eoo),            "Not assessable",
            ifelse(eoo < 100,             "CR (EOO < 100 km²)",
            ifelse(eoo < 5000,            "EN (100 ≤ EOO < 5,000 km²)",
            ifelse(eoo < 20000,           "VU (5,000 ≤ EOO < 20,000 km²)",
                                          "LC/NT (EOO ≥ 20,000 km²)"))))
      tab <- table(b1)
      for (k in names(tab)) cat(sprintf("- %s: %d species\n", k, tab[k]))
      cat("> Derived from eoo_km2 column using IUCN B1 thresholds.\n")
    } else {
      cat("> No iucn_b1 or eoo_km2 column found.\n")
    }
    cat("\n")
  }

  # ── NON-INDIGENOUS BREAKDOWN ────────────────────────────────────────────────
  non_path <- file.path(run_dir, "non_indigenous_metrics.tsv")
  if (file.exists(non_path)) {
    non <- suppressWarnings(readr::read_tsv(non_path, show_col_types = FALSE))
    cat("## Non-indigenous species breakdown\n")
    cat(sprintf("- Total non-indigenous species: %d\n", nrow(non)))
    # Print any categorical-looking column
    for (col in c("ni_category", "iucn_category", "category", "invasiveness")) {
      if (col %in% names(non)) {
        tab <- table(non[[col]], useNA = "ifany")
        cat(sprintf("- By %s:\n", col))
        for (k in names(tab)) cat(sprintf("    - %s: %d species\n",
                                          if (is.na(k)) "(unassigned)" else k, tab[k]))
      }
    }
    cat("\n")
  }

  # ── HYDROBASINS COVERAGE ────────────────────────────────────────────────────
  for (branch in c("indigenous", "non_indigenous")) {
    fp <- file.path(run_dir, sprintf("clean_occurrences_with_hydrobasin_%s.tsv", branch))
    if (file.exists(fp)) {
      h <- suppressWarnings(readr::read_tsv(
        fp, col_types = readr::cols(.default = readr::col_character()),
        quote = "", na = c("","NA"), show_col_types = FALSE
      ))
      if ("hydrobasin" %in% names(h)) {
        n_assigned <- sum(!is.na(h$hydrobasin) & h$hydrobasin != "")
        n_total    <- nrow(h)
        if (branch == "indigenous") cat("## HydroBASINS coverage\n")
        cat(sprintf("- %s records with basin assignment: %d / %d (%.1f%%)\n",
                    tools::toTitleCase(gsub("_", "-", branch)),
                    n_assigned, n_total,
                    if (n_total > 0L) 100*n_assigned/n_total else 0))
      }
    }
  }
  cat("\n")
}

# ── RUNTIME (from log timestamps) ─────────────────────────────────────────────
if (!is.na(LOG) && file.exists(LOG)) {
  lg <- readLines(LOG, warn = FALSE)
  ts_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  ts_lines   <- grep(ts_pattern, lg, value = TRUE)
  if (length(ts_lines) >= 2) {
    first_ts <- substr(ts_lines[1],                    1, 19)
    last_ts  <- substr(ts_lines[length(ts_lines)],     1, 19)
    t_start  <- as.POSIXct(first_ts, tz = "UTC")
    t_end    <- as.POSIXct(last_ts,  tz = "UTC")
    elapsed  <- as.numeric(difftime(t_end, t_start, units = "secs"))
    h <- floor(elapsed / 3600)
    m <- floor((elapsed %% 3600) / 60)
    s <- as.integer(elapsed %% 60)
    cat("## Runtime (wall-clock)\n")
    cat(sprintf("- Started: %s\n", first_ts))
    cat(sprintf("- Ended:   %s\n", last_ts))
    cat(sprintf("- **Total: %dh %dm %ds** (%d seconds)\n", h, m, s, as.integer(elapsed)))
    cat("\n")
  }
}

# ── HARDWARE PLACEHOLDER ──────────────────────────────────────────────────────
cat("## Hardware (fill in)\n")
cat("- CPU: _[e.g. AMD EPYC 7763, 16 cores]_\n")
cat("- RAM: _[e.g. 64 GB]_\n")
cat("- OS: _[e.g. Ubuntu 24.04 LTS]_\n")
cat("- R version: _[run `R.version$version.string` to get exact value]_\n")
cat("- Parallel cores used: _[from CONFIG$parallel]_\n")
