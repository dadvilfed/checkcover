#### MODULE 01E_CHANGE_DETECTION: PHASE 1.5 ####
#
# Detects per-species outcome (unchanged / reprocessed / new) by comparing
# the current run's fingerprint for each species against the most recent
# prior version where that species was present.
#
# Position in pipeline: runs immediately after 01_ingest.R, before any
# enrichment (modules 02a-02f). Its output (ctx$active_species,
# ctx$species_outcomes) gates all downstream work.
#
# Side effects:
#   - Writes per-species fingerprint files to
#     <root>/<v>/checkover/fingerprints/<sp_clean>.json
#   - Reads clean_occurrences.tsv from <root>/<v>/checkover/
#   - Reads prior version manifests for fingerprint lookup
#
# Mutates ctx:
#   - ctx$species_outcomes (list, one entry per species in current cohort)
#   - ctx$active_species (subset of all_species: reprocessed + new)
#
# Design ref: refactor_design.md §2, §4; Lucian clarifications May 2026.
# ──────────────────────────────────────────────────────────────────────────────


# ──────────────────────────────────────────────────────────────────────────────
# PRIOR-VERSION FINGERPRINT LOOKUP
# ──────────────────────────────────────────────────────────────────────────────

#' Walk prior versions backwards to find the most recent fingerprint for a species
#'
#' For each prior version (already in numeric-descending order in
#' `ctx$prior_versions`), reads that version's manifest and checks whether
#' it has an entry for this species. Returns the first hit's fingerprint
#' along with the version it came from.
#'
#' If a manifest has the species with `outcome: "unchanged"`, the entry's
#' `source_version` (where artifacts physically live) is what we want — that
#' is the version we'd compare against for sparse semantics.
#'
#' For `outcome: "new"` or `outcome: "reprocessed"`, the manifest's version
#' IS the source (artifacts live in this version's folder).
#'
#' @param ctx                  A RunContext (post-init).
#' @param species_clean        Filesystem-safe species name.
#'
#' @return List with `$found` (logical), and if TRUE:
#'           `$source_version` (where artifacts live),
#'           `$source_fingerprint` (sha256 string),
#'           `$found_in_manifest_version` (which manifest we looked at).
#'         When found = FALSE, the species is genuinely new.
.find_prior_species_fingerprint <- function(ctx, species_clean) {

  for (v in ctx$prior_versions) {
    m <- read_version_manifest(ctx, v)
    if (is.null(m) || is.null(m$species)) next

    entry <- m$species[[species_clean]]
    if (is.null(entry)) next  # species not in this version's cohort

    # Found. Resolve source_version (where artifacts live) and the
    # fingerprint stored at source.
    source_v <- entry$source_version %||% v
    fp_at_source <- entry$fingerprint_at_source %||% entry$fingerprint

    return(list(
      found = TRUE,
      source_version = as.character(source_v),
      source_fingerprint = as.character(fp_at_source),
      found_in_manifest_version = as.character(v)
    ))
  }

  list(found = FALSE)
}


# Small null-coalesce operator used above. We don't want to depend on
# 00_helpers.R sourcing order, so define a private one if not already present.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}


# ──────────────────────────────────────────────────────────────────────────────
# FINGERPRINT FILE WRITING
# ──────────────────────────────────────────────────────────────────────────────

#' Write a per-species fingerprint file (audit deposit)
#'
#' Stores the fingerprint plus a compact provenance block under
#' `<root>/<v>/checkover/fingerprints/<sp_clean>.json`. This is the
#' authoritative audit anchor for the run; the manifest also stores the
#' fingerprint inline, but these per-species files make ad-hoc forensic
#' inspection straightforward.
.write_species_fingerprint_file <- function(ctx, species_clean,
                                            fingerprint, n_records,
                                            outcome, source_version,
                                            prior_source_version = NULL) {
  fp_dir <- file.path(ctx$current_scaffolding_dir, "fingerprints")
  if (!dir.exists(fp_dir)) dir.create(fp_dir, recursive = TRUE, showWarnings = FALSE)

  payload <- list(
    species_clean        = species_clean,
    framework_version    = ctx$framework_version,
    generated_date       = format(ctx$generated_date, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    fingerprint          = fingerprint,
    n_records            = as.integer(n_records),
    outcome              = outcome,
    source_version       = source_version,
    prior_source_version = prior_source_version
  )

  jsonlite::write_json(payload,
                       file.path(fp_dir, paste0(species_clean, ".json")),
                       pretty = TRUE, auto_unbox = TRUE, na = "null")
}


# ──────────────────────────────────────────────────────────────────────────────
# PHASE 1.5 ORCHESTRATOR
# ──────────────────────────────────────────────────────────────────────────────

#' Phase 1.5: detect per-species outcomes and populate active_species
#'
#' Reads the just-ingested clean_occurrences.tsv from disk, computes a
#' fingerprint per species, compares against the most recent prior occurrence
#' of that species in any prior manifest, and writes per-species fingerprint
#' files plus updates the RunContext.
#'
#' First-run case: if `ctx$prior_versions` is empty, every species is `new`,
#' every species ends up in `active_species`. No manifest walking, no
#' comparison — but fingerprints are still computed and written so future
#' runs can compare against this baseline.
#'
#' @param ctx           A RunContext with `all_species` populated (post-ingest).
#' @param clean_data    Optional. Data frame of all clean records. If NULL,
#'                      read from `<v>/checkover/clean_occurrences.tsv`.
#'                      In-memory option exists for tests; production passes
#'                      NULL and we read from disk.
#'
#' @return Updated RunContext (with active_species + species_outcomes set).
#' @export
detect_species_changes <- function(ctx, clean_data = NULL) {

  validate_RunContext(ctx, expected_phase = "post_ingest")

  module <- "PHASE1.5_CHANGE_DETECTION"
  if (exists("log_info", mode = "function")) {
    log_info("=== PHASE 1.5: CHANGE DETECTION ===", module = module)
  }

  # ── Ensure scaffolding dir exists (we write fingerprints into it) ──
  if (!dir.exists(ctx$current_scaffolding_dir)) {
    dir.create(ctx$current_scaffolding_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ── Resolve clean_data source: arg, or read from canonical path ──
  if (is.null(clean_data)) {
    src <- file.path(ctx$current_scaffolding_dir, "clean_occurrences.tsv")
    if (!file.exists(src)) {
      stop(sprintf(
        "Phase 1.5: clean_occurrences.tsv not found at %s. ",
        src),
        "Phase 1 (ingest) must run first and write to <v>/checkover/.")
    }
    if (exists("log_info", mode = "function")) {
      log_info("Reading clean_occurrences from %s", src, module = module)
    }
    # Robust read: real-world clean_occurrences.tsv can contain embedded
    # newlines/quotes from failed API enrichment (e.g. WoRMS timeout writing
    # a multi-line error string into a cell). readr::read_tsv tolerates and
    # reports these instead of hard-failing like utils::read.table.
    clean_data <- readr::read_tsv(
      src,
      col_types = readr::cols(.default = readr::col_character()),
      quote = "", na = c("", "NA"),
      progress = FALSE,
      show_col_types = FALSE
    )
    clean_data <- as.data.frame(clean_data, stringsAsFactors = FALSE)
    # Coerce the numeric columns we actually use for fingerprinting
    for (.nc in c("longitude", "latitude", "year", "accuracy")) {
      if (.nc %in% names(clean_data)) {
        clean_data[[.nc]] <- suppressWarnings(as.numeric(clean_data[[.nc]]))
      }
    }
  }
  if (!"species" %in% names(clean_data)) {
    stop("clean_data has no 'species' column.")
  }

  # ── First-run short-circuit ──
  first_run <- length(ctx$prior_versions) == 0L
  if (first_run && exists("log_info", mode = "function")) {
    log_info("First run (no prior versions): all species will be 'new'.",
             module = module)
  }

  # ── Per-species fingerprinting + outcome assignment ──
  species_universe <- ctx$all_species

  outcomes <- vector("list", length(species_universe))
  names(outcomes) <- species_universe

  n_new <- 0L; n_unchanged <- 0L; n_reprocessed <- 0L

  for (sp in species_universe) {
    sp_clean <- make_package_id(sp)
    sp_data  <- clean_data[clean_data$species == sp, , drop = FALSE]

    current_fp <- compute_species_fingerprint(sp_data)
    n_records  <- nrow(sp_data)

    if (first_run) {
      outcome           <- "new"
      source_version    <- ctx$framework_version
      prior_source_v    <- NULL
      change_summary    <- sprintf("first appearance (%d records)", n_records)
    } else {
      prior <- .find_prior_species_fingerprint(ctx, sp_clean)
      if (!prior$found) {
        outcome           <- "new"
        source_version    <- ctx$framework_version
        prior_source_v    <- NULL
        change_summary    <- sprintf("new species (%d records)", n_records)
      } else if (identical(prior$source_fingerprint, current_fp)) {
        outcome           <- "unchanged"
        source_version    <- prior$source_version
        prior_source_v    <- prior$source_version
        change_summary    <- sprintf("identical to v%s (%d records)",
                                     prior$source_version, n_records)
      } else {
        outcome           <- "reprocessed"
        source_version    <- ctx$framework_version
        prior_source_v    <- prior$source_version
        change_summary    <- sprintf("data changed vs v%s",
                                     prior$source_version)
      }
    }

    outcomes[[sp]] <- list(
      species_clean        = sp_clean,
      outcome              = outcome,
      source_version       = source_version,
      prior_source_version = prior_source_v,
      fingerprint          = current_fp,
      n_records            = n_records,
      change_summary       = change_summary
    )

    # Persist fingerprint file (audit deposit)
    .write_species_fingerprint_file(
      ctx                  = ctx,
      species_clean        = sp_clean,
      fingerprint          = current_fp,
      n_records            = n_records,
      outcome              = outcome,
      source_version       = source_version,
      prior_source_version = prior_source_v
    )

    if      (outcome == "new")         n_new         <- n_new         + 1L
    else if (outcome == "unchanged")   n_unchanged   <- n_unchanged   + 1L
    else if (outcome == "reprocessed") n_reprocessed <- n_reprocessed + 1L
  }

  # ── active_species = species the downstream modules will actually process ──
  active <- vapply(outcomes, \(o) o$outcome %in% c("reprocessed", "new"),
                   logical(1))
  active_species <- species_universe[active]

  ctx$species_outcomes <- outcomes
  ctx$active_species   <- active_species

  if (exists("log_info", mode = "function")) {
    log_info(sprintf(
      "Phase 1.5 complete: total=%d, new=%d, reprocessed=%d, unchanged=%d, active=%d",
      length(species_universe), n_new, n_reprocessed, n_unchanged,
      length(active_species)),
      module = module)
  }

  validate_RunContext(ctx, expected_phase = "post_change_detection")
  ctx
}
