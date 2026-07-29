#!/usr/bin/env Rscript
################################################################################
# refingerprint_v10.R — Recompute v1.0 fingerprints under fixed canonicalization
#
# WHY THIS EXISTS
# ---------------
# v1.0's fingerprints were computed with the OLD .canonicalize_column(), which
# was reader-type-sensitive (logical vs character vs numeric produced different
# canonical strings). After hardening Phase 1.5 to use readr::read_tsv (needed
# to survive WoRMS-corrupted TSVs) AND fixing .canonicalize_column() to be
# reader-invariant, v1.0's stored fingerprints no longer match what the new
# code produces for identical data.
#
# This script rewrites v1.0's fingerprint baseline ONCE, under the new
# canonicalization, so subsequent incremental runs (v1.1+) compare correctly.
# It does NOT re-run the pipeline. It only rewrites audit anchors:
#   - <root>/1.0/checkover/fingerprints/<sp_clean>.json   (fingerprint field)
#   - <root>/1.0/checkover/manifest.json                  (inline fingerprints)
#
# PRECONDITION
# ------------
# 00_run_context.R must ALREADY contain the fixed reader-invariant
# .canonicalize_column(). This script sources it and reuses its
# compute_species_fingerprint() so the hash is guaranteed identical to what
# Phase 1.5 will compute for v1.1.
#
# USAGE
# -----
#   Rscript refingerprint_v10.R <root_output_dir> [version]
#
#   Rscript refingerprint_v10.R checkover_output 1.0
#
# Idempotent: safe to run multiple times. Makes a .bak of manifest.json the
# first time only.
################################################################################

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(readr)
}))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript refingerprint_v10.R <root_output_dir> [version=1.0]")
}
ROOT <- args[[1]]
VER  <- if (length(args) >= 2L) args[[2]] else "1.0"

# Source the helpers (must contain the FIXED .canonicalize_column).
src_helpers <- "R/00_run_context.R"
if (!file.exists(src_helpers)) {
  stop("Cannot find ", src_helpers, " — run this from the project root.")
}
source(src_helpers)
# make_package_id() lives in 00_helpers.R
if (file.exists("R/00_helpers.R")) source("R/00_helpers.R")

ver_dir   <- file.path(ROOT, VER)
scaff_dir <- file.path(ver_dir, "checkover")
co_path   <- file.path(scaff_dir, "clean_occurrences.tsv")
fp_dir    <- file.path(scaff_dir, "fingerprints")
man_path  <- file.path(scaff_dir, "manifest.json")

for (p in c(co_path, fp_dir, man_path)) {
  if (!file.exists(p)) stop("Required path missing: ", p)
}

cat(sprintf("Re-fingerprinting v%s under fixed canonicalization\n", VER))
cat(sprintf("  clean_occurrences: %s\n", co_path))

# Read EXACTLY as Phase 1.5 now reads it (same reader, same options) so the
# recomputed fingerprints are guaranteed to match v1.1's computation path.
clean_data <- readr::read_tsv(
  co_path,
  col_types = readr::cols(.default = readr::col_character()),
  quote = "", na = c("", "NA"),
  progress = FALSE, show_col_types = FALSE
)
clean_data <- as.data.frame(clean_data, stringsAsFactors = FALSE)
for (.nc in c("longitude", "latitude", "year", "accuracy")) {
  if (.nc %in% names(clean_data)) {
    clean_data[[.nc]] <- suppressWarnings(as.numeric(clean_data[[.nc]]))
  }
}

if (!"species" %in% names(clean_data)) stop("clean_occurrences has no 'species' column")

species_all <- sort(unique(clean_data$species[!is.na(clean_data$species)]))
cat(sprintf("  species: %d\n", length(species_all)))

# Backup manifest once
if (!file.exists(paste0(man_path, ".bak"))) {
  file.copy(man_path, paste0(man_path, ".bak"))
  cat(sprintf("  backed up manifest -> %s.bak\n", man_path))
}

manifest <- jsonlite::read_json(man_path, simplifyVector = FALSE)

n_updated <- 0L
n_fp_files <- 0L

for (sp in species_all) {
  sp_clean <- make_package_id(sp)
  sp_data  <- clean_data[clean_data$species == sp, , drop = FALSE]
  new_fp   <- compute_species_fingerprint(sp_data)

  # 1. Rewrite per-species fingerprint file (if it exists)
  fp_file <- file.path(fp_dir, paste0(sp_clean, ".json"))
  if (file.exists(fp_file)) {
    payload <- jsonlite::read_json(fp_file, simplifyVector = TRUE)
    payload$fingerprint <- new_fp
    payload$refingerprinted <- TRUE
    jsonlite::write_json(payload, fp_file, pretty = TRUE,
                         auto_unbox = TRUE, na = "null")
    n_fp_files <- n_fp_files + 1L
  }

  # 2. Update inline fingerprint in manifest
  if (!is.null(manifest$species[[sp_clean]])) {
    manifest$species[[sp_clean]]$fingerprint <- new_fp
    if (!is.null(manifest$species[[sp_clean]]$fingerprint_at_source)) {
      manifest$species[[sp_clean]]$fingerprint_at_source <- new_fp
    }
    n_updated <- n_updated + 1L
  }
}

jsonlite::write_json(manifest, man_path, pretty = TRUE,
                     auto_unbox = TRUE, na = "null")

cat(sprintf("Done. Rewrote %d fingerprint files, %d manifest entries.\n",
            n_fp_files, n_updated))
cat("v1.0 baseline is now comparable with the fixed canonicalization.\n")
cat("Next incremental run (v1.1) will detect unchanged species correctly.\n")
