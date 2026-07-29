#!/usr/bin/env Rscript
# =============================================================================
#  extract_extinction_delta.R
#  ---------------------------------------------------------------------------
#  Proof-of-mechanism extraction for the Conservation Letters manuscript
#  (rewrite after the ECOG rejection). Reads EXISTING v1.0 and v1.1 artifacts
#  produced by cheCkOVER — this is NOT a new pipeline run.
#
#  For every species that has a package_metadata.json under BOTH the v1.0 and
#  the v1.1 version folders, it extracts, per scope (indigenous / non_indigenous):
#     AOO_km2, EOO_km2, records, basins_count, countries_count,
#     protected_areas_count, extinctions_count
#  for both versions and computes raw deltas (absolute + percent).
#
#  It writes one CSV row per (species, scope) and prints the non_indigenous
#  extinction addendum (count + species list) to the console + a companion CSV.
#
#  -- DELIBERATE DECISIONS (please sanity-check these with Lucian) ------------
#   D1. delta_abs   = v1.1 - v1.0   (the "v1.0 -> v1.1" change). A real
#                     decrease therefore comes out NEGATIVE. This reproduces
#                     Lucian's Astacus example: a 2.54% AOO drop -> -2.54.
#   D2. delta_pct   = (v1.1 - v1.0) / v1.0 * 100, rounded to 4 dp.
#                     If v1.0 == 0 and v1.1 == 0  -> 0   (no change).
#                     If v1.0 == 0 and v1.1 != 0  -> NA  (undefined / infinite).
#   D3. suppressed  = records_v1.0 - records_v1.1   (Lucian's exact definition:
#                     "v1.0 minus v1.1"). NOTE this is the OPPOSITE sign of
#                     records_delta_abs by design. Computed per scope.
#   D4. coverage_pct= extinctions_count_v1.1 / records_v1.0 * 100, 4 dp.
#                     records_v1.0 == 0 -> NA. Computed per scope.
#   D5. We DO NOT read trend_vs_previous (it is categorical, thresholded, and
#                     hides sub-threshold effects — exactly what we want to show).
#   D6. FILTER_MODE (set below) controls which rows are emitted:
#         "scope"   (default) -> emit a (species, scope) row only when THAT
#                                scope has extinctions_count(v1.1) > 0.
#         "species"           -> if a species has extinctions_count(v1.1) > 0 in
#                                ANY scope, emit BOTH scope rows for it.
#   D7. The standalone `extinctions_count` column = the v1.1 value (the headline
#                     / filter value for the row). extinctions_count also appears
#                     in its own _v1.0/_v1.1/_delta_abs/_delta_pct group, per
#                     Lucian's column list. The duplication is intentional; drop
#                     the standalone column if you prefer.
#   D8. NO SILENT ZEROS. If a metric key is genuinely ABSENT from a JSON (schema
#                     drift between the v1.0 and v1.1 runs), the value is recorded
#                     as NA and flagged loudly at the end — never silently 0.
#                     (Explicit 0s written by the pipeline are kept as 0.)
# =============================================================================

suppressPackageStartupMessages(library(jsonlite))

# ---------------------------------------------------------------------------
# CONFIG  -- edit these for your server paths if needed
# ---------------------------------------------------------------------------
# Root output dir. Falls back to "checkover_output" if config.R isn't sourced.
# Override on the command line:  Rscript extract_extinction_delta.R /abs/path/to/checkover_output
.args <- commandArgs(trailingOnly = TRUE)
ROOT <- if (length(.args) >= 1L && nzchar(.args[1])) {
  .args[1]
} else if (exists("CONFIG") && !is.null(CONFIG$root_output_dir)) {
  CONFIG$root_output_dir
} else {
  "checkover_output"
}

V0_DIR    <- "1.0"          # version folder name on disk for the baseline
V1_DIR    <- "1.1"          # version folder name on disk for the current version
V0_LABEL  <- "v1.0"         # column-name suffix label (cosmetic only)
V1_LABEL  <- "v1.1"         # column-name suffix label (cosmetic only)
FILTER_MODE <- "scope"      # "scope" or "species"  (see D6)
OUT_CSV   <- file.path(ROOT, "extinction_delta_v1.0_v1.1.csv")
ADDENDUM_CSV <- file.path(ROOT, "non_indigenous_extinctions_v1.1.csv")

METRICS <- c("AOO_km2", "EOO_km2", "records", "basins_count",
             "countries_count", "protected_areas_count", "extinctions_count")
SCOPES  <- c("indigenous", "non_indigenous")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

# Collector for absent-field warnings (D8). Never silently zero a missing key.
.missing <- list()
flag_missing <- function(species, version, scope, field) {
  .missing[[length(.missing) + 1L]] <<- data.frame(
    species = species, version = version, scope = scope, field = field,
    stringsAsFactors = FALSE
  )
}

# Pull a metric from a scope block. Returns numeric value, or NA_real_ if the
# key is ABSENT (flagged). Explicit pipeline zeros are returned as 0.
get_metric <- function(block, field, species, version, scope) {
  if (is.null(block) || is.null(block[[field]])) {
    flag_missing(species, version, scope, field)
    return(NA_real_)
  }
  v <- block[[field]]
  if (length(v) == 0L) {            # empty array/list -> treat as absent
    flag_missing(species, version, scope, field)
    return(NA_real_)
  }
  suppressWarnings(as.numeric(v[[1]]))
}

# delta_abs + delta_pct for a (v0, v1) pair, honouring D1/D2.
compute_delta <- function(v0, v1) {
  abs_d <- v1 - v0
  pct_d <- if (is.na(v0) || is.na(v1)) {
    NA_real_
  } else if (v0 == 0) {
    if (v1 == 0) 0 else NA_real_
  } else {
    round((v1 - v0) / v0 * 100, 4)
  }
  list(delta_abs = abs_d, delta_pct = pct_d)
}

read_pkg <- function(version_dir, package_id) {
  f <- file.path(ROOT, version_dir, package_id, "package_metadata.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f, simplifyVector = TRUE),
           error = function(e) {
             message(sprintf("  ! Failed to parse %s: %s", f, conditionMessage(e)))
             NULL
           })
}

# ---------------------------------------------------------------------------
# Discover species present in BOTH version folders
# ---------------------------------------------------------------------------
list_packages <- function(version_dir) {
  d <- file.path(ROOT, version_dir)
  if (!dir.exists(d)) {
    stop(sprintf("Version folder not found: %s\n  (set ROOT / V0_DIR / V1_DIR at the top of the script)", d))
  }
  subs <- list.dirs(d, recursive = FALSE, full.names = FALSE)
  subs[file.exists(file.path(d, subs, "package_metadata.json"))]
}

pkgs_v0 <- list_packages(V0_DIR)
pkgs_v1 <- list_packages(V1_DIR)
both    <- sort(intersect(pkgs_v0, pkgs_v1))

cat(sprintf("Packages with package_metadata.json:\n  %s: %d\n  %s: %d\n  in BOTH: %d\n",
            V0_DIR, length(pkgs_v0), V1_DIR, length(pkgs_v1), length(both)))

only_v0 <- setdiff(pkgs_v0, pkgs_v1)
only_v1 <- setdiff(pkgs_v1, pkgs_v0)
if (length(only_v1)) cat(sprintf("  NOTE: %d package(s) physically present at %s but not %s (e.g. new species, or unchanged species referenced via manifest source_version) -> excluded, no v1.0 baseline to diff.\n",
                                  length(only_v1), V1_DIR, V0_DIR))
if (length(only_v0)) cat(sprintf("  NOTE: %d package(s) at %s but not %s -> excluded.\n",
                                  length(only_v0), V0_DIR, V1_DIR))

if (length(both) == 0L) stop("No species have artifacts in both versions — nothing to extract.")

# ---------------------------------------------------------------------------
# Build rows
# ---------------------------------------------------------------------------
rows <- list()
addendum <- list()   # non_indigenous extinction cases (v1.1 extinctions_count > 0)

for (pid in both) {
  pm0 <- read_pkg(V0_DIR, pid)
  pm1 <- read_pkg(V1_DIR, pid)
  if (is.null(pm0) || is.null(pm1)) next

  species  <- pm1$species %||% pm0$species %||% pid
  scenario <- pm1$scenario %||% pm0$scenario %||% NA
  ver0     <- pm0$snapshot$version %||% NA_character_  # actual recorded snapshot labels
  ver1     <- pm1$snapshot$version %||% NA_character_
  date1    <- pm1$snapshot$date    %||% NA_character_

  # Soft sanity check: does the on-disk folder name match the recorded version?
  if (!is.na(ver0) && ver0 != V0_DIR)
    message(sprintf("  ? %s: %s/ folder holds snapshot.version='%s'", species, V0_DIR, ver0))
  if (!is.na(ver1) && ver1 != V1_DIR)
    message(sprintf("  ? %s: %s/ folder holds snapshot.version='%s'", species, V1_DIR, ver1))

  # collect non_indigenous extinction case for the addendum (independent of FILTER_MODE)
  ni_ext1 <- get_metric(pm1$metrics$non_indigenous, "extinctions_count",
                        species, V1_DIR, "non_indigenous")
  if (!is.na(ni_ext1) && ni_ext1 > 0) {
    addendum[[length(addendum) + 1L]] <- data.frame(
      species = species, scenario = scenario,
      non_indigenous_extinctions_v1.1 = ni_ext1,
      non_indigenous_records_v1.1 = get_metric(pm1$metrics$non_indigenous, "records",
                                               species, V1_DIR, "non_indigenous"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  # decide, per FILTER_MODE, which scopes to emit for this species
  scope_ext1 <- sapply(SCOPES, function(s)
    get_metric(pm1$metrics[[s]], "extinctions_count", species, V1_DIR, s))
  emit_scopes <- if (FILTER_MODE == "species") {
    if (any(!is.na(scope_ext1) & scope_ext1 > 0)) SCOPES else character(0)
  } else { # "scope"
    SCOPES[!is.na(scope_ext1) & scope_ext1 > 0]
  }
  if (length(emit_scopes) == 0L) next

  for (scope in emit_scopes) {
    b0 <- pm0$metrics[[scope]]
    b1 <- pm1$metrics[[scope]]

    row <- list(
      species       = species,
      scope         = scope,
      scenario      = scenario,
      version_v1.0  = ver0,
      version_v1.1  = ver1,
      date          = date1
    )

    vals0 <- setNames(numeric(length(METRICS)), METRICS)
    vals1 <- setNames(numeric(length(METRICS)), METRICS)
    for (m in METRICS) {
      v0 <- get_metric(b0, m, species, V0_DIR, scope)
      v1 <- get_metric(b1, m, species, V1_DIR, scope)
      vals0[m] <- v0; vals1[m] <- v1
      d <- compute_delta(v0, v1)
      row[[paste0(m, "_", V0_LABEL)]]   <- v0
      row[[paste0(m, "_", V1_LABEL)]]   <- v1
      row[[paste0(m, "_delta_abs")]]    <- d$delta_abs
      row[[paste0(m, "_delta_pct")]]    <- d$delta_pct
    }

    # standalone headline + derived (D3, D4, D7)
    rec0 <- vals0["records"]; ext1 <- vals1["extinctions_count"]
    row[["extinctions_count"]] <- ext1
    row[["suppressed"]]        <- vals0["records"] - vals1["records"]   # D3
    row[["coverage_pct"]]      <- if (is.na(ext1) || is.na(rec0) || rec0 == 0)
                                    NA_real_ else round(ext1 / rec0 * 100, 4)  # D4

    rows[[length(rows) + 1L]] <- as.data.frame(row, check.names = FALSE,
                                               stringsAsFactors = FALSE)
  }
}

if (length(rows) == 0L) {
  cat("\nNo (species, scope) rows passed the extinctions_count(v1.1) > 0 filter.\n")
} else {
  out <- do.call(rbind, rows)
  # enforce exact column order
  ordered_cols <- c("species", "scope", "scenario", "version_v1.0", "version_v1.1", "date")
  for (m in METRICS)
    ordered_cols <- c(ordered_cols,
                      paste0(m, "_", V0_LABEL), paste0(m, "_", V1_LABEL),
                      paste0(m, "_delta_abs"), paste0(m, "_delta_pct"))
  ordered_cols <- c(ordered_cols, "extinctions_count", "suppressed", "coverage_pct")
  out <- out[, ordered_cols, drop = FALSE]

  write.csv(out, OUT_CSV, row.names = FALSE, na = "NA")
  cat(sprintf("\nWrote %d rows (%d species) -> %s\n",
              nrow(out), length(unique(out$species)), OUT_CSV))
  cat(sprintf("  indigenous rows:     %d\n", sum(out$scope == "indigenous")))
  cat(sprintf("  non_indigenous rows: %d\n", sum(out$scope == "non_indigenous")))
}

# ---------------------------------------------------------------------------
# ADDENDUM: non_indigenous extinction cases (invasive retreat)
# ---------------------------------------------------------------------------
cat("\n===== ADDENDUM: non_indigenous extinctions (v1.1) =====\n")
if (length(addendum) == 0L) {
  cat("0 species have extinctions_count > 0 on the non_indigenous branch.\n")
} else {
  add_df <- do.call(rbind, addendum)
  add_df <- add_df[order(-add_df$`non_indigenous_extinctions_v1.1`), ]
  cat(sprintf("%d species have extinctions_count > 0 on the non_indigenous branch:\n",
              nrow(add_df)))
  for (i in seq_len(nrow(add_df)))
    cat(sprintf("  - %s (scenario %s): %g non-indigenous extinctions, %g records\n",
                add_df$species[i], add_df$scenario[i],
                add_df$`non_indigenous_extinctions_v1.1`[i],
                add_df$`non_indigenous_records_v1.1`[i]))
  write.csv(add_df, ADDENDUM_CSV, row.names = FALSE, na = "NA")
  cat(sprintf("(saved -> %s)\n", ADDENDUM_CSV))
}

# ---------------------------------------------------------------------------
# DATA-INTEGRITY REPORT (D8): absent metric keys, never silently zeroed
# ---------------------------------------------------------------------------
if (length(.missing) > 0L) {
  miss_df <- unique(do.call(rbind, .missing))
  cat(sprintf("\n!!! %d metric value(s) were ABSENT from JSON and recorded as NA (NOT 0).\n",
              nrow(miss_df)))
  cat("    This usually means schema drift between the v1.0 and v1.1 runs. Review before using the CSV:\n")
  print(miss_df, row.names = FALSE)
} else {
  cat("\nData-integrity check: all requested metric keys were present in every JSON. No NA-from-absence.\n")
}

cat("\nDone.\n")
