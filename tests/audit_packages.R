#!/usr/bin/env Rscript
#### cheCkOVER PACKAGE AUDITS ####
# Two permanent regression gates, run AFTER a v1.x rerun and before publishing
# any package or SEB (Lucian, 2026-06):
#
#   1. audit_folder_integrity()      — every species folder must contain the
#      expected artifacts (package_metadata.json, >=1 narrative, and, unless the
#      species is Extinct/zero-active, the map layers), each existing and NON-empty.
#      Catches SILENT writer failures (partial folders) like the total-extinction
#      edge case that produced a folder with no AOO/narrative.
#
#   2. audit_narrative_metadata_consistency() — package_metadata.json's metrics
#      object is the single source of truth; the narrative is a pure formatter.
#      This tokenizes the headline numeric claims in each narrative (md + txt)
#      and diffs them against that package's metadata. Zero mismatches = pass.
#
# Usage:
#   Rscript audit_packages.R <version_dir> [--strict]
#   e.g. Rscript audit_packages.R checkover_output/1.1
#
# <version_dir> is the folder holding one directory per species package
# (each with package_metadata.json). Exits non-zero if any species fails.

suppressPackageStartupMessages(library(jsonlite))

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.nonempty_file <- function(path) file.exists(path) && file.info(path)$size > 0

# Numbers as they may appear in prose: strip thousands separators, keep decimals.
# ALWAYS returns a length-1 numeric. A missing metadata key yields numeric(0),
# and `is.na(numeric(0)) && ...` is a hard error ("missing value where TRUE/FALSE
# needed") — which only shows up on packages where a key is absent entirely, so
# it stayed hidden until the full 676-species cohort was audited.
.num <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", "", as.character(x)[1])))
}

# Pull the first capture group of a regex from text, as numeric; NA if absent.
.grab <- function(text, pattern) {
  m <- regmatches(text, regexec(pattern, text, ignore.case = TRUE))[[1]]
  if (length(m) < 2) return(NA_real_)
  .num(m[2])
}

# Pull ALL first-capture matches (for internal-consistency checks).
.grab_all <- function(text, pattern) {
  gg <- gregexpr(pattern, text, ignore.case = TRUE)
  mm <- regmatches(text, gg)[[1]]
  if (length(mm) == 0) return(numeric(0))
  vapply(mm, function(s) .grab(s, pattern), numeric(1))
}

# Compare with a tolerance appropriate for the field (percentages rounded to 0.1).
.mismatch <- function(meta_val, narr_val, tol = 0) {
  if (is.na(meta_val) || is.na(narr_val)) return(FALSE)  # absence handled elsewhere
  abs(meta_val - narr_val) > tol + 1e-9
}


# ===========================================================================
# AUDIT 1 — folder integrity
# ===========================================================================
audit_folder_integrity <- function(version_dir) {
  species_dirs <- list.dirs(version_dir, recursive = FALSE)
  # Ignore scaffolding (e.g. <v>/checkover/) — only real species packages have
  # a package_metadata.json.
  species_dirs <- species_dirs[file.exists(file.path(species_dirs, "package_metadata.json"))]

  flags <- list()
  for (d in species_dirs) {
    sp <- basename(d)
    meta_path <- file.path(d, "package_metadata.json")
    meta <- tryCatch(jsonlite::read_json(meta_path, simplifyVector = TRUE),
                     error = function(e) NULL)
    problems <- character(0)

    if (is.null(meta)) {
      problems <- c(problems, "package_metadata.json unreadable/invalid JSON")
    }

    # Narrative: at least one canonical/formal file, non-empty.
    narr_dir <- file.path(d, "narratives")
    narr_files <- if (dir.exists(narr_dir)) list.files(narr_dir, full.names = TRUE) else character(0)
    if (length(narr_files) == 0) {
      problems <- c(problems, "no narrative files")
    } else if (!any(vapply(narr_files, .nonempty_file, logical(1)))) {
      problems <- c(problems, "all narrative files empty")
    }

    # Maps: required UNLESS the species is Extinct/zero-active (which legitimately
    # ships no AOO/EOO/basin layers).
    status <- meta$status %||% "Extant"
    is_extinct <- identical(status, "Extinct")
    map_dir <- file.path(d, "maps")
    map_files <- if (dir.exists(map_dir)) list.files(map_dir, full.names = TRUE) else character(0)
    if (!is_extinct) {
      if (length(map_files) == 0) {
        problems <- c(problems, "no map files (non-extinct species)")
      } else if (!any(vapply(map_files, .nonempty_file, logical(1)))) {
        problems <- c(problems, "all map files empty")
      }
      # A non-extinct species with basins_count>0 must actually carry a basins layer.
      bc <- (meta$metrics$indigenous$basins_count %||% 0L) +
            (meta$metrics$non_indigenous$basins_count %||% 0L)
      has_basin_layer <- any(grepl("_basins\\.", map_files, ignore.case = TRUE))
      if (isTRUE(bc > 0) && !has_basin_layer)
        problems <- c(problems, sprintf("basins_count=%d but no basins layer on disk", bc))
      if (isTRUE(bc == 0) && has_basin_layer)
        problems <- c(problems, "basins_count=0 but a basins layer is present")
    } else {
      # Extinct: must carry the mandatory disclaimer and must NOT paint basins.
      if (is.null(meta$data_disclaimer) && is.null(meta$disclaimer))
        problems <- c(problems, "Extinct package missing data_disclaimer")
      if (any(grepl("_basins\\.", map_files, ignore.case = TRUE)))
        problems <- c(problems, "Extinct package still ships a basins layer")
    }

    if (length(problems) > 0) flags[[sp]] <- problems
  }

  list(
    n_checked = length(species_dirs),
    n_flagged = length(flags),
    flags     = flags
  )
}


# ===========================================================================
# AUDIT 2 — narrative vs metadata numeric consistency
# ===========================================================================
# For each package, extract the headline numeric claims from the narrative using
# anchors that match the generator's own phrasing, and diff against metadata.
# These are exactly the tokens that historically drifted (records, basins,
# post-2000 %, PA %, AOO, EOO, front-matter version).
audit_narrative_metadata_consistency <- function(version_dir) {
  species_dirs <- list.dirs(version_dir, recursive = FALSE)
  species_dirs <- species_dirs[file.exists(file.path(species_dirs, "package_metadata.json"))]

  mismatches <- list()
  for (d in species_dirs) {
    sp <- basename(d)
    meta <- tryCatch(jsonlite::read_json(file.path(d, "package_metadata.json"), simplifyVector = TRUE),
                     error = function(e) NULL)
    if (is.null(meta)) next

    narr_dir <- file.path(d, "narratives")
    md_path  <- list.files(narr_dir, pattern = "_canonical\\.md$",  full.names = TRUE)[1]
    txt_path <- list.files(narr_dir, pattern = "_narrative\\.txt$", full.names = TRUE)[1]
    md  <- if (!is.na(md_path)  && file.exists(md_path))  paste(readLines(md_path,  warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
    txt <- if (!is.na(txt_path) && file.exists(txt_path)) paste(readLines(txt_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
    both <- paste(md, txt, sep = "\n")

    # AOO/EOO bullets appear in BOTH canonical section 2.1 (indigenous) and 3.2
    # (non-indigenous), in identical wording. Comparing a match from section 3
    # against the indigenous metadata block flags every non-indigenous-only
    # species as a mismatch. Slice off everything from "## 3." so the AOO/EOO
    # checks only ever see the indigenous section. (The other checks — records,
    # basin units, post-2000 %, PA % — are worded distinctly and are safe on the
    # whole document.)
    cut_ind <- regexpr("\n## 3", md, fixed = TRUE)
    md_ind  <- if (cut_ind > 0) substr(md, 1, cut_ind) else md

    ind <- meta$metrics$indigenous
    non <- meta$metrics$non_indigenous
    problems <- character(0)

    # --- Front-matter version (bug 0) ---
    fm_out <- regmatches(md, regexec('output_version:\\s*"?v?([0-9.]+)"?', md))[[1]]
    if (length(fm_out) >= 2) {
      if (!identical(gsub("^v", "", fm_out[2]), gsub("^v", "", as.character(meta$snapshot$version %||% "")))) {
        problems <- c(problems, sprintf("front-matter output_version '%s' != snapshot.version '%s'",
                                        fm_out[2], meta$snapshot$version))
      }
    }

    # --- Headline record count (bug 2): "based on N validated occurrence records" ---
    narr_total <- .grab(both, "based on \\*\\*([0-9,]+) validated occurrence records")
    meta_total <- (.num(ind$records) %||% 0) + (.num(non$records) %||% 0)
    if (.mismatch(meta_total, narr_total))
      problems <- c(problems, sprintf("total records: narrative %s vs metadata %s", narr_total, meta_total))

    # indigenous / non-indigenous split "(A indigenous, B non-indigenous)"
    narr_split <- regmatches(both, regexec("\\(([0-9,]+) indigenous, ([0-9,]+) non-indigenous\\)", both))[[1]]
    if (length(narr_split) >= 3) {
      if (.mismatch(.num(ind$records), .num(narr_split[2])))
        problems <- c(problems, sprintf("indigenous records: narrative %s vs metadata %s", narr_split[2], ind$records))
      if (.mismatch(.num(non$records), .num(narr_split[3])))
        problems <- c(problems, sprintf("non-indigenous records: narrative %s vs metadata %s", narr_split[3], non$records))
    }

    # --- Basin UNITS (bug 1 + Lucian 2026-07): "N hydrographic basin unit(s)"
    # must equal metadata basins_count (the fine HydroBASINS unit count). The
    # separate "named river basins" figure is intentionally NOT compared here.
    narr_basins <- .grab(both, "([0-9,]+) hydrographic basin unit")
    if (is.na(narr_basins)) narr_basins <- .grab(both, "([0-9,]+) hydrographic basin")  # legacy phrasing
    if (.mismatch(.num(ind$basins_count), narr_basins))
      problems <- c(problems, sprintf("basin units: narrative %s vs metadata %s", narr_basins, ind$basins_count))

    # --- post-2000 % (bug 3): "X% of records post-2000" ---
    narr_p2k <- .grab(both, "([0-9.]+)% of records post-2000")
    if (.mismatch(.num(ind$records_post_2000_pct), narr_p2k, tol = 0.05))
      problems <- c(problems, sprintf("post-2000%%: narrative %s vs metadata %s", narr_p2k, ind$records_post_2000_pct))

    # --- PA % : "X% of native records (n=Y)" ---
    narr_pa <- .grab(both, "([0-9.]+)% of native records")
    if (.mismatch(.num(ind$records_in_protected_areas_pct), narr_pa, tol = 0.05))
      problems <- c(problems, sprintf("PA%%: narrative %s vs metadata %s", narr_pa, ind$records_in_protected_areas_pct))

    # --- AOO / EOO from Section 2.1 only (indigenous slice) ---
    narr_aoo <- .grab(md_ind, "Area of occupancy \\(AOO\\):\\*\\* ([0-9,]+) km")
    if (.mismatch(.num(ind$AOO_km2), narr_aoo))
      problems <- c(problems, sprintf("AOO: narrative %s vs metadata %s", narr_aoo, ind$AOO_km2))
    # EOO: NA in metadata must NOT surface as a real number in the narrative.
    narr_eoo <- .grab(md_ind, "Extent of occurrence \\(EOO\\):\\*\\* ([0-9,]+) km")
    if (is.na(.num(ind$EOO_km2)) && !is.na(narr_eoo))
      problems <- c(problems, sprintf("EOO is NA in metadata but narrative shows %s", narr_eoo))
    if (!is.na(.num(ind$EOO_km2)) && .mismatch(.num(ind$EOO_km2), narr_eoo))
      problems <- c(problems, sprintf("EOO: narrative %s vs metadata %s", narr_eoo, ind$EOO_km2))

    # --- extinctions must be narrated when >0 ---
    ext_ct <- (.num(ind$extinctions_count) %||% 0) + (.num(non$extinctions_count) %||% 0)
    if (isTRUE(ext_ct > 0) && !grepl("extinct|extirpat", both, ignore.case = TRUE))
      problems <- c(problems, sprintf("extinctions_count=%s but narrative never mentions extinction/extirpation", ext_ct))

    # --- narrative-internal consistency: the basin-UNIT figure, if repeated,
    #     must agree (named-basin figures are a separate quantity, not checked).
    all_basins <- .grab_all(both, "([0-9,]+) hydrographic basin unit")
    all_basins <- all_basins[!is.na(all_basins)]
    if (length(unique(all_basins)) > 1)
      problems <- c(problems, sprintf("narrative cites inconsistent basin-unit counts: %s",
                                      paste(unique(all_basins), collapse = ", ")))

    if (length(problems) > 0) mismatches[[sp]] <- problems
  }

  list(
    n_checked    = length(species_dirs),
    n_mismatched = length(mismatches),
    mismatches   = mismatches
  )
}


# ===========================================================================
# CLI entry point
# ===========================================================================
# Run the CLI only when this file is the top-level Rscript target — not when it
# is source()'d by another script (e.g. the test harness), which would otherwise
# hit the usage/quit path.
if (sys.nframe() == 0L && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    cat("Usage: Rscript audit_packages.R <version_dir> [--strict]\n")
    quit(status = 2)
  }
  version_dir <- args[1]
  if (!dir.exists(version_dir)) {
    cat(sprintf("ERROR: version dir not found: %s\n", version_dir))
    quit(status = 2)
  }

  cat(sprintf("\n=== cheCkOVER package audit: %s ===\n\n", version_dir))

  integ <- audit_folder_integrity(version_dir)
  cat(sprintf("[1] Folder integrity: %d/%d species flagged\n", integ$n_flagged, integ$n_checked))
  for (sp in names(integ$flags)) {
    cat(sprintf("    - %s:\n", sp))
    for (p in integ$flags[[sp]]) cat(sprintf("        * %s\n", p))
  }

  cons <- audit_narrative_metadata_consistency(version_dir)
  cat(sprintf("\n[2] Narrative/metadata consistency: %d/%d species with mismatches\n",
              cons$n_mismatched, cons$n_checked))
  for (sp in names(cons$mismatches)) {
    cat(sprintf("    - %s:\n", sp))
    for (p in cons$mismatches[[sp]]) cat(sprintf("        * %s\n", p))
  }

  # Persist a machine-readable report next to the version dir.
  report <- list(
    version_dir = version_dir,
    generated   = as.character(Sys.time()),
    integrity   = integ,
    consistency = cons
  )
  out <- file.path(version_dir, "_audit_report.json")
  jsonlite::write_json(report, out, pretty = TRUE, auto_unbox = TRUE, na = "null")
  cat(sprintf("\nReport written to %s\n", out))

  failed <- integ$n_flagged > 0 || cons$n_mismatched > 0
  cat(sprintf("\nRESULT: %s\n\n", if (failed) "FAIL" else "PASS"))
  quit(status = if (failed) 1 else 0)
}
