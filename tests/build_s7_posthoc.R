#!/usr/bin/env Rscript
################################################################################
# build_s7_posthoc.R — Standalone Supplement S7 reconstruction
#
# Reconstructs supplement_S7.tsv AFTER two pipeline runs have completed, purely
# from on-disk artifacts. No pipeline dependency, no re-run, no RunContext.
#
# Usage:
#   Rscript build_s7_posthoc.R <root_output_dir> <current_version> [prior_version]
#
# Examples:
#   Rscript build_s7_posthoc.R checkover_output 1.1 1.0
#   Rscript build_s7_posthoc.R checkover_output 1.1        # prior auto-detected
#
# Output:
#   <root_output_dir>/<current_version>/checkover/supplement_S7.tsv
#
# What it reads (all persisted artifacts):
#   <root>/<curr>/checkover/manifest.json          - cohort + outcomes
#   <root>/<curr>/checkover/clean_occurrences.tsv  - current record IDs
#   <root>/<curr>/<sp>/package_metadata.json       - current metrics (reprocessed/new)
#   <root>/<prior>/checkover/clean_occurrences.tsv - prior record IDs
#   <root>/<prior>/<sp>/package_metadata.json      - prior metrics
#
# Columns (Lucian's refactor v2 spec):
#   species_name | outcome | source_version | delta_EOO_km2 | delta_AOO_km2 |
#   delta_country_count | delta_basin_count | fragmentation_transition |
#   loss_hotspot_flagged | n_records_added | n_records_retired
################################################################################

suppressWarnings(suppressMessages({
  library(jsonlite)
}))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript build_s7_posthoc.R <root_output_dir> <current_version> [prior_version]")
}
ROOT      <- args[[1]]
CURR_V    <- args[[2]]
PRIOR_V_ARG <- if (length(args) >= 3L) args[[3]] else NA_character_

# ── Resolve prior version (auto-detect if not supplied) ───────────────────────
.version_sort_key <- function(v) {
  parts <- as.numeric(strsplit(as.character(v), ".", fixed = TRUE)[[1]])
  if (length(parts) == 1L) parts <- c(parts, 0)
  parts[1] * 1e6 + parts[2]
}
detect_prior <- function(root, curr) {
  dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  vers <- dirs[grepl("^[0-9]+\\.[0-9]+$", dirs)]
  vers <- setdiff(vers, curr)
  if (length(vers) == 0L) return(NA_character_)
  keys <- vapply(vers, .version_sort_key, numeric(1))
  vers[which.max(keys)]
}
PRIOR_V <- if (!is.na(PRIOR_V_ARG)) PRIOR_V_ARG else detect_prior(ROOT, CURR_V)

cat(sprintf("S7 post-hoc: current=v%s, prior=%s, root=%s\n",
            CURR_V, if (is.na(PRIOR_V)) "(none)" else paste0("v", PRIOR_V), ROOT))

# ── Read current manifest (the cohort source of truth) ────────────────────────
curr_manifest_path <- file.path(ROOT, CURR_V, "checkover", "manifest.json")
if (!file.exists(curr_manifest_path)) {
  stop("Current manifest not found: ", curr_manifest_path)
}
manifest <- jsonlite::read_json(curr_manifest_path, simplifyVector = FALSE)
species_entries <- manifest$species
if (is.null(species_entries) || length(species_entries) == 0L) {
  stop("Current manifest has no species entries.")
}

# ── Load clean_occurrences (current + prior) once ─────────────────────────────
read_co <- function(version) {
  f <- file.path(ROOT, as.character(version), "checkover", "clean_occurrences.tsv")
  if (!file.exists(f)) {
    cat(sprintf("  [warn] clean_occurrences.tsv missing for v%s (%s); record-set "
                , version, f),
        "deltas + hotspot will be blank for affected species\n", sep = "")
    return(NULL)
  }
  tryCatch(utils::read.table(f, header = TRUE, sep = "\t", quote = "",
                             comment.char = "", stringsAsFactors = FALSE,
                             na.strings = c("", "NA")),
           error = function(e) {
             cat(sprintf("  [warn] failed to read %s: %s\n", f, conditionMessage(e)))
             NULL
           })
}
curr_co  <- read_co(CURR_V)
prior_co <- if (!is.na(PRIOR_V)) read_co(PRIOR_V) else NULL

# ── package_metadata.json reader (resolves species_clean -> display via manifest)
read_pkg <- function(version, sp_clean) {
  f <- file.path(ROOT, as.character(version), sp_clean, "package_metadata.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f, simplifyVector = TRUE), error = function(e) NULL)
}

pool <- function(pkg, block_path) {
  # block_path e.g. c("metrics","indigenous","EOO_km2")
  ind <- pkg$metrics$indigenous[[block_path]]     %||% 0L
  non <- pkg$metrics$non_indigenous[[block_path]] %||% 0L
  as.numeric(ind) + as.numeric(non)
}

# ── Build rows ────────────────────────────────────────────────────────────────
empty_row <- function(species_name, outcome, source_version) {
  data.frame(
    species_name             = species_name,
    outcome                  = outcome,
    source_version           = as.character(source_version),
    delta_EOO_km2            = "",
    delta_AOO_km2            = "",
    delta_country_count      = "",
    delta_basin_count        = "",
    fragmentation_transition = "",
    loss_hotspot_flagged     = "",
    n_records_added          = "",
    n_records_retired        = "",
    stringsAsFactors         = FALSE
  )
}

rows <- list()

for (sp_clean in names(species_entries)) {
  entry        <- species_entries[[sp_clean]]
  outcome      <- entry$outcome %||% "unknown"
  source_v     <- entry$source_version %||% CURR_V
  prior_src    <- entry$prior_source_version %||% NA_character_
  # display name: prefer package_metadata's "species"; fall back to sp_clean
  disp_pkg <- read_pkg(source_v, sp_clean)
  species_name <- disp_pkg$species %||% gsub("_", " ", sp_clean)

  row <- empty_row(species_name, outcome, source_v)

  # Deltas only for reprocessed species with a real prior source
  if (identical(outcome, "reprocessed") &&
      !is.na(prior_src) && nzchar(as.character(prior_src))) {

    prior_pkg <- read_pkg(prior_src, sp_clean)
    curr_pkg  <- read_pkg(CURR_V,   sp_clean)

    if (!is.null(prior_pkg) && !is.null(curr_pkg)) {
      d_eoo <- pool(curr_pkg, "EOO_km2")            - pool(prior_pkg, "EOO_km2")
      d_aoo <- pool(curr_pkg, "AOO_km2")            - pool(prior_pkg, "AOO_km2")
      d_ctr <- pool(curr_pkg, "countries_count")    - pool(prior_pkg, "countries_count")
      d_bas <- pool(curr_pkg, "basins_count")       - pool(prior_pkg, "basins_count")
      f_pri <- pool(prior_pkg, "fragmentation_clusters")
      f_cur <- pool(curr_pkg,  "fragmentation_clusters")

      row$delta_EOO_km2       <- as.character(as.integer(round(d_eoo)))
      row$delta_AOO_km2       <- as.character(as.integer(round(d_aoo)))
      row$delta_country_count <- as.character(as.integer(d_ctr))
      row$delta_basin_count   <- as.character(as.integer(d_bas))
      row$fragmentation_transition <- if (f_pri == f_cur) "unchanged"
                                      else sprintf("%d -> %d",
                                                   as.integer(f_pri), as.integer(f_cur))
    }

    # Record-set deltas via record_id set-diff
    if (!is.null(curr_co) && !is.null(prior_co)) {
      curr_ids  <- curr_co$record_id[curr_co$species == species_name]
      prior_ids <- prior_co$record_id[prior_co$species == species_name]
      row$n_records_added   <- as.character(length(setdiff(curr_ids,  prior_ids)))
      row$n_records_retired <- as.character(length(setdiff(prior_ids, curr_ids)))

      # loss_hotspot_flagged: any country/basin with >=5 prior localities
      # that lost >=50% of records in current
      hotspot <- FALSE
      for (unit_col in c("country", "hydrobasin")) {
        if (!(unit_col %in% names(prior_co)) || !(unit_col %in% names(curr_co))) next
        prior_tab <- table(prior_co[prior_co$species == species_name, unit_col],
                           useNA = "no")
        curr_tab  <- table(curr_co[curr_co$species == species_name, unit_col],
                           useNA = "no")
        for (u in names(prior_tab)) {
          pn <- as.integer(prior_tab[u])
          cn <- as.integer(curr_tab[u] %||% 0L)
          if (pn >= 5L && cn <= pn * 0.5) { hotspot <- TRUE; break }
        }
        if (hotspot) break
      }
      row$loss_hotspot_flagged <- as.character(hotspot)
    }
  }

  rows[[length(rows) + 1L]] <- row
}

s7 <- do.call(rbind, rows)

# ── Write ─────────────────────────────────────────────────────────────────────
out_path <- file.path(ROOT, CURR_V, "checkover", "supplement_S7.tsv")
utils::write.table(s7, out_path, sep = "\t", quote = FALSE,
                   row.names = FALSE, na = "", fileEncoding = "UTF-8")

n_reproc <- sum(s7$outcome == "reprocessed")
cat(sprintf("S7 written: %d species (%d reprocessed with delta data)\n  -> %s\n",
            nrow(s7), n_reproc, out_path))
