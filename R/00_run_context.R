#### MODULE 00_RUN_CONTEXT: SPARSE-VERSIONING FOUNDATIONS ####
#
# Pure helpers for the sparse-versioning refactor (cheCkOVER, May 2026).
#
# Contains:
#   - RunContext_init() + validate_RunContext()
#   - Path construction helpers (current_species_dir, current_scaffolding_dir,
#     species_dir_in)
#   - list_prior_versions() - discovers prior published versions on disk
#   - compute_species_fingerprint() - SHA-256 of canonicalised per-species data
#   - resolve_species_path() - cross-version path lookup via manifest
#
# Depends on:
#   - jsonlite (already a pipeline dep)
#   - digest (de-facto R sha256 library; add to install_missing() if absent)
#
# No side effects, no logging, no global state. Every function is pure and
# testable in isolation. See test_run_context.R for the test suite.
#
# Design ref: refactor_design.md sections 3 + 4 + 5, after Lucian's
# clarifications in checkover-spec-for-david-v4 (per-species folder shape
# stays drop-in compatible with v1.0).
# ──────────────────────────────────────────────────────────────────────────────


# ──────────────────────────────────────────────────────────────────────────────
# CONSTANTS
# ──────────────────────────────────────────────────────────────────────────────

# Columns of `clean_occurrences.tsv` that contribute to the per-species
# fingerprint. Goal: include everything that, if changed, should trigger a
# re-process. Exclude derived fields (taxonomic enrichment) and free-text
# (`comments`) that shouldn't trigger 30-min reruns on typo fixes.
#
# Column order is significant — it determines the canonical TSV string fed
# to sha256. Any reordering changes every fingerprint and triggers full
# reprocessing. Add new columns at the end if needed.
.FINGERPRINT_COLUMNS <- c(
  "record_id",
  "longitude", "latitude", "year",
  "is_extinct", "is_type_locality", "population_status",
  "status.x", "accuracy",
  "doi", "url", "citation", "contributor",
  "confidentiality_level", "is_sensitive"
)

# Regex Lucian's WoC platform uses to detect version folders. Anything that
# doesn't match `^\d+\.\d+$` is treated as non-version scaffolding (e.g.
# "checkover", "cache", "logs") and ignored by the consumer side.
.VERSION_FOLDER_REGEX <- "^[0-9]+\\.[0-9]+$"


# ──────────────────────────────────────────────────────────────────────────────
# RUN CONTEXT
# ──────────────────────────────────────────────────────────────────────────────

#' Initialize a RunContext for a pipeline run
#'
#' Single object carried through every module as the first argument. Modules
#' read from it; only three call sites (init, post-ingest, post-change-detection)
#' are permitted to mutate it. See `validate_RunContext()` for the contract.
#'
#' @param config        Result of sourcing config.R; must contain
#'                      `framework_version` and `root_output_dir`.
#' @param run_id        String identifying this execution (typically
#'                      `config$version`, e.g. "production_v1"). Used for
#'                      logging only — not part of the output path.
#' @param now           POSIXct timestamp; defaults to `Sys.time()`. Override
#'                      in tests for deterministic output.
#'
#' @return RunContext list with S3 class "RunContext".
#' @export
RunContext_init <- function(config, run_id, now = Sys.time()) {
  stopifnot(
    is.list(config),
    !is.null(config$framework_version),
    !is.null(config$root_output_dir),
    is.character(run_id), length(run_id) == 1L
  )

  fv <- as.character(config$framework_version)
  if (!grepl(.VERSION_FOLDER_REGEX, fv)) {
    stop(sprintf("framework_version '%s' does not match %s — sparse versioning will fail",
                 fv, .VERSION_FOLDER_REGEX))
  }

  ctx <- list(
    # Identity
    framework_version       = fv,
    run_id                  = run_id,
    generated_date          = now,

    # Paths (derived from config; caller is responsible for passing absolute
    # paths if needed — we don't normalizePath because that resolves symlinks
    # inconsistently across platforms and would break tempdir-based tests).
    root_output_dir         = config$root_output_dir,
    current_version_dir     = file.path(config$root_output_dir, fv),
    current_scaffolding_dir = file.path(config$root_output_dir, fv, "checkover"),

    # Version history (discovered at init, immutable thereafter)
    prior_versions          = list_prior_versions(config$root_output_dir,
                                                  current_version = fv),

    # Species universe (populated by Phase 1 = ingest, Phase 1.5 = change detection)
    all_species             = NULL,
    active_species          = NULL,
    species_outcomes        = NULL,

    # Logging
    log_module              = "MAIN"
  )

  class(ctx) <- "RunContext"
  ctx
}


#' Validate RunContext field presence at a known pipeline phase
#'
#' Confirms the context has been populated up to the expected pipeline phase.
#' Modules use this as a defensive guard at function entry; tests use it to
#' verify mutation discipline.
#'
#' @param ctx              A RunContext.
#' @param expected_phase   Optional. One of: `"init"` (just constructed),
#'                         `"post_ingest"` (all_species set),
#'                         `"post_change_detection"` (active_species and
#'                         species_outcomes set).
#'
#' @return TRUE invisibly if valid; stops with informative error otherwise.
#' @export
validate_RunContext <- function(ctx, expected_phase = NULL) {
  if (!inherits(ctx, "RunContext")) {
    stop("Argument is not a RunContext (class = ",
         paste(class(ctx), collapse = "/"), ")")
  }

  required <- c("framework_version", "run_id", "generated_date",
                "root_output_dir", "current_version_dir",
                "current_scaffolding_dir", "prior_versions", "log_module")
  missing  <- setdiff(required, names(ctx))
  if (length(missing) > 0L) {
    stop(sprintf("RunContext missing required fields: %s",
                 paste(missing, collapse = ", ")))
  }

  if (is.null(expected_phase)) return(invisible(TRUE))

  if (identical(expected_phase, "init")) {
    return(invisible(TRUE))
  }

  if (identical(expected_phase, "post_ingest")) {
    if (is.null(ctx$all_species)) {
      stop("RunContext: all_species is NULL (expected populated after Phase 1)")
    }
    if (!is.character(ctx$all_species) || length(ctx$all_species) == 0L) {
      stop("RunContext: all_species must be a non-empty character vector after Phase 1")
    }
    return(invisible(TRUE))
  }

  if (identical(expected_phase, "post_change_detection")) {
    if (is.null(ctx$active_species)) {
      stop("RunContext: active_species is NULL (expected populated after Phase 1.5)")
    }
    if (is.null(ctx$species_outcomes)) {
      stop("RunContext: species_outcomes is NULL (expected populated after Phase 1.5)")
    }
    if (!is.list(ctx$species_outcomes)) {
      stop("RunContext: species_outcomes must be a list")
    }
    return(invisible(TRUE))
  }

  stop(sprintf("Unknown expected_phase: '%s'", expected_phase))
}


# ──────────────────────────────────────────────────────────────────────────────
# PATH CONSTRUCTION
# ──────────────────────────────────────────────────────────────────────────────

#' Build a per-species directory path inside a specific version
#'
#' Path semantics (post-refactor): `<root>/<version>/<species_clean>/`.
#' No `species/` intermediate level, per Lucian's WoC drop-in requirement.
#'
#' @param ctx              A RunContext.
#' @param version          Version folder name (e.g. "1.0", "1.1"). No "v" prefix.
#' @param species_clean    Filesystem-safe species name from `make_package_id()`.
#' @param artifact_subpath Optional sub-path within the species folder
#'                         (e.g. "maps/Astacus_astacus_AOO.geojson").
#'
#' @return Absolute path.
#' @export
species_dir_in <- function(ctx, version, species_clean, artifact_subpath = NULL) {
  base <- file.path(ctx$root_output_dir, version, species_clean)
  if (is.null(artifact_subpath)) base else file.path(base, artifact_subpath)
}


#' Build a per-species directory path inside the current run's version
#'
#' Shortcut for `species_dir_in(ctx, ctx$framework_version, species_clean)`.
#' Used by all modules that write fresh artifacts for reprocessed / new species.
#'
#' @export
current_species_dir <- function(ctx, species_clean, artifact_subpath = NULL) {
  species_dir_in(ctx, ctx$framework_version, species_clean, artifact_subpath)
}


# ──────────────────────────────────────────────────────────────────────────────
# PRIOR VERSION DISCOVERY
# ──────────────────────────────────────────────────────────────────────────────

#' Sort key for numeric version comparison (so 1.10 > 1.2, not lexical)
.version_sort_key <- function(v) {
  parts <- as.numeric(strsplit(as.character(v), ".", fixed = TRUE)[[1]])
  if (length(parts) == 1L) parts <- c(parts, 0)
  parts[1] * 1e6 + parts[2]
}


#' List published versions on disk under `<root_dir>`, newest first
#'
#' Walks `<root_dir>` for sub-directories matching the version regex
#' (`^\d+\.\d+$`). Returns them in numeric-descending order. Used by
#' `RunContext_init()` to discover history, and by change detection to
#' walk backwards looking for the last version containing a species.
#'
#' @param root_dir         Pipeline output root.
#' @param current_version  Optional. If supplied, excluded from the returned
#'                         list (you usually don't want the current run to
#'                         be "prior" to itself).
#'
#' @return Character vector of version strings (e.g. c("1.10", "1.9", "1.0")).
#'         Empty character(0) if no prior versions exist.
#' @export
list_prior_versions <- function(root_dir, current_version = NULL) {
  if (!dir.exists(root_dir)) return(character(0))

  all_dirs    <- list.dirs(root_dir, recursive = FALSE, full.names = FALSE)
  version_dirs <- all_dirs[grepl(.VERSION_FOLDER_REGEX, all_dirs)]

  if (!is.null(current_version)) {
    version_dirs <- setdiff(version_dirs, as.character(current_version))
  }
  if (length(version_dirs) == 0L) return(character(0))

  keys <- vapply(version_dirs, .version_sort_key, numeric(1))
  version_dirs[order(keys, decreasing = TRUE)]
}


# ──────────────────────────────────────────────────────────────────────────────
# FINGERPRINTING
# ──────────────────────────────────────────────────────────────────────────────

#' Canonicalize a column to stable string form for hashing
#'
#' Type-aware coercion: numerics get fixed-precision formatting, logicals
#' map to "TRUE"/"FALSE"/"NA", strings get trimmed. The goal is that any two
#' columns with semantically identical content produce identical canonical
#' strings, regardless of R's internal type or printing quirks.
.canonicalize_column <- function(x) {
  # Reader-invariant canonicalization. The fingerprint must be identical
  # whether a column arrived as logical/integer/numeric (utils::read.table)
  # or as character (readr::read_tsv with col_character). We normalize on
  # VALUE, not on R type — otherwise hardening the TSV reader silently
  # invalidates every prior fingerprint (May 2026 incident).
  cx <- trimws(as.character(x))
  na_mask <- is.na(x) | cx == "" | cx == "NA"
  
  # Sentinel for NA: a string that cannot occur in real TSV cell content.
  .NA_SENTINEL <- "__CHECKOVER_NA__"
  cx[na_mask] <- .NA_SENTINEL
  
  # Booleans: TRUE/True/true/T -> "true"; FALSE/False/false/F -> "false"
  cx[cx %in% c("TRUE",  "True",  "true",  "T")] <- "true"
  cx[cx %in% c("FALSE", "False", "false", "F")] <- "false"
  
  # Numerics: any value that is textually a finite number, in any form
  # ("2002", "2002.0", "2.5e1", " 25.50 "), -> fixed 6-decimal form.
  # Non-numeric strings pass through untouched.
  num <- suppressWarnings(as.numeric(cx))
  is_num <- !is.na(num) &
    grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", cx)
  cx[is_num] <- sprintf("%.6f", num[is_num])
  
  cx[cx == .NA_SENTINEL] <- "NA"
  cx
}


#' Compute a SHA-256 fingerprint of one species' record set
#'
#' Deterministic hash of the per-species data slice, intended as the
#' comparison anchor for sparse versioning. Two runs that produce
#' semantically identical data for a species will produce identical
#' fingerprints; any change in the comparison columns will produce a
#' different fingerprint.
#'
#' Algorithm:
#'   1. Subset to the comparison columns that exist in the input
#'   2. Type-canonicalise each column to stable string form
#'   3. Sort rows by record_id (so input row-order doesn't matter)
#'   4. Concatenate as TSV (tab between cells, newline between rows)
#'   5. SHA-256 hash the result
#'
#' Caller is responsible for filtering to a single species before passing
#' `sp_data` in. The fingerprint is "this set of records", not "this species".
#' The species name is not part of the fingerprint.
#'
#' @param sp_data    Data.frame of records for ONE species, post-ingest.
#'                   Empty / NULL is allowed (produces empty-set fingerprint).
#' @param columns    Optional override of which columns to include.
#'                   Defaults to `.FINGERPRINT_COLUMNS`. Column order is
#'                   significant.
#'
#' @return String of the form "sha256:<64hex>".
#' @export
compute_species_fingerprint <- function(sp_data, columns = .FINGERPRINT_COLUMNS) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for fingerprinting. Install with install.packages('digest').")
  }

  # Empty / NULL: hash of empty string. Stable across runs.
  if (is.null(sp_data) || nrow(sp_data) == 0L) {
    return(paste0("sha256:",
                  digest::digest("", algo = "sha256", serialize = FALSE)))
  }

  # Subset to comparison columns that exist in this slice (preserves `columns` order)
  available_cols <- intersect(columns, names(sp_data))
  if (length(available_cols) == 0L) {
    stop("None of the fingerprint columns are present in sp_data. ",
         "Expected one or more of: ", paste(columns, collapse = ", "))
  }

  df <- sp_data[, available_cols, drop = FALSE]

  # Canonicalize per-column
  df_canon <- as.data.frame(
    lapply(df, .canonicalize_column),
    stringsAsFactors = FALSE
  )
  names(df_canon) <- available_cols

  # Sort rows by record_id when available, for row-order independence
  if ("record_id" %in% names(df_canon)) {
    df_canon <- df_canon[order(df_canon$record_id), , drop = FALSE]
  }

  # Concatenate to single TSV string
  rows <- do.call(paste, c(df_canon, sep = "\t"))
  body <- paste(rows, collapse = "\n")

  paste0("sha256:",
         digest::digest(body, algo = "sha256", serialize = FALSE))
}


# ──────────────────────────────────────────────────────────────────────────────
# CROSS-VERSION PATH RESOLUTION
# ──────────────────────────────────────────────────────────────────────────────

#' Resolve a species' artifact path via the version manifest
#'
#' Central lookup for "where do <species>'s artifacts physically live for
#' version <V>?" Reads `<root>/<V>/checkover/manifest.json`, finds the
#' species entry, follows the entry's `source_version` to the actual path.
#'
#' For unchanged species in version V, `source_version` points back to a
#' prior version (where the artifacts actually live). For reprocessed / new
#' species, `source_version == V`.
#'
#' @param ctx                A RunContext.
#' @param species_clean      Filesystem-safe species name.
#' @param artifact_subpath   Optional file within the species folder
#'                           (e.g. "package_metadata.json", "maps/x.geojson").
#' @param version            Optional version to look up; defaults to
#'                           ctx$framework_version. Overriding is useful
#'                           when reading from a prior manifest, e.g. for
#'                           change-detection.
#'
#' @return Absolute path (does NOT check that the file exists).
#' @export
resolve_species_path <- function(ctx, species_clean,
                                 artifact_subpath = NULL,
                                 version = NULL) {
  if (is.null(version)) version <- ctx$framework_version

  manifest_path <- file.path(ctx$root_output_dir, version, "checkover", "manifest.json")

  # If no manifest exists yet (e.g. mid-build of the current version),
  # fall back to assuming the artifact lives in `version` itself.
  if (!file.exists(manifest_path)) {
    return(species_dir_in(ctx, version, species_clean, artifact_subpath))
  }

  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  entry <- manifest$species[[species_clean]]
  if (is.null(entry)) {
    stop(sprintf("Species '%s' not found in manifest for v%s",
                 species_clean, version))
  }

  source_v <- entry$source_version
  if (is.null(source_v) || !nzchar(as.character(source_v))) {
    stop(sprintf("Manifest entry for '%s' in v%s has no source_version",
                 species_clean, version))
  }

  species_dir_in(ctx, as.character(source_v), species_clean, artifact_subpath)
}


# ──────────────────────────────────────────────────────────────────────────────
# MANIFEST READ HELPER
# ──────────────────────────────────────────────────────────────────────────────

#' Read a version's manifest, with NULL fallback
#'
#' Convenience read that returns NULL (not stop()) when the file is absent
#' or unreadable. Useful for change detection — walking back through prior
#' versions, some may legitimately not exist.
#'
#' @param ctx       A RunContext.
#' @param version   Version string (e.g. "1.0").
#' @return Parsed manifest as a list, or NULL.
#' @export
read_version_manifest <- function(ctx, version) {
  path <- file.path(ctx$root_output_dir, as.character(version),
                    "checkover", "manifest.json")
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
           error = function(e) NULL)
}
