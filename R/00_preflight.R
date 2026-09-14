#### MODULE 00_PREFLIGHT: FAIL FAST, AND FAIL WITH EVERYTHING AT ONCE ####
#
# cheCkOVER used to discover a missing file at the moment it first needed it,
# which for the reference layers is well into a run. A user with three things
# wrong therefore fixed one, waited, hit the next, fixed it, waited again
# (Reviewer 1, Ecological Informatics, 2026-09):
#
#   "the software checks for the data it needs AFTER performing all the prior
#    processing steps, and then halts with an error [...] there is no way to
#    resume where you left off"
#
# preflight_check() runs before any processing and reports EVERY problem it can
# find in one pass, each with the setting that controls it and what to do about
# it. It never fixes anything silently.
# ──────────────────────────────────────────────────────────────────────────────

#' Verify that a run can complete before it starts
#'
#' @param config The CONFIG list from config.R.
#' @param strict When TRUE (default) a fatal problem stops the run. FALSE
#'   reports and returns, for tests and for inspecting a partial setup.
#' @return Invisibly, a data frame of findings (severity, item, detail).
preflight_check <- function(config = NULL, strict = TRUE, module = "PREFLIGHT") {
  if (is.null(config)) {
    config <- if (exists("CONFIG", envir = globalenv())) get("CONFIG", envir = globalenv()) else NULL
  }
  if (is.null(config)) stop("preflight_check(): no CONFIG available.", call. = FALSE)

  findings <- list()
  add <- function(severity, item, detail) {
    findings[[length(findings) + 1L]] <<-
      data.frame(severity = severity, item = item, detail = detail,
                 stringsAsFactors = FALSE)
  }

  need_file <- function(path, item, hint, severity = "FATAL") {
    if (is.null(path) || !nzchar(path)) {
      add(severity, item, paste0("not configured. ", hint))
    } else if (!file.exists(path)) {
      add(severity, item, sprintf("'%s' not found. %s", path, hint))
    }
  }

  # ── 1. Occurrence input ───────────────────────────────────────────────────
  need_file(config$input_file, "input_file",
            "Set CONFIG$input_file to your occurrence export.")

  # ── 2. Lookup tables ──────────────────────────────────────────────────────
  need_file(config$vernaculars$path, "vernaculars$path",
            "Shipped as '(Table_S2)vernacular_names_wide.tsv'.")
  need_file(config$dictionaries$feow, "dictionaries$feow",
            "Shipped as '(Table_S4)ecoregions_list.tsv'.")
  need_file(config$dictionaries$hydrobasins, "dictionaries$hydrobasins",
            "Shipped as 'Table_S3.tsv' (the HydroBASINS name lookup).")

  # ── 3. Reference layers you must download yourself ────────────────────────
  hydro_dir <- config$spatial$hydro_dir
  if (is.null(hydro_dir) || !dir.exists(hydro_dir)) {
    add("FATAL", "spatial$hydro_dir",
        sprintf("'%s' not found. Download HydroBASINS and point CONFIG$spatial$hydro_dir at it - see README 'Reference data'.",
                hydro_dir %||% "<unset>"))
  } else {
    for (lvl in names(config$spatial$hydro_files)) {
      f <- file.path(hydro_dir, config$spatial$hydro_files[[lvl]])
      if (!file.exists(f)) {
        add("FATAL", sprintf("HydroBASINS level %s", lvl),
            sprintf("'%s' not found. Levels 6, 8 and 10 are all required.", f))
      }
    }
  }

  # FEOW has two sources; only the configured one has to be present.
  feow_source <- config$spatial$feow_source %||% "auto"
  if (feow_source == "local") {
    need_file(config$spatial$feow_path, "spatial$feow_path",
              "Or set CONFIG$spatial$feow_source to 'feowR' to use the package instead.")
  } else if (feow_source == "feowR" && !requireNamespace("feowR", quietly = TRUE)) {
    add("FATAL", "feowR package",
        "CONFIG$spatial$feow_source is 'feowR' but the package is absent. Install with remotes::install_github(\"mhpob/feowR\"), or set feow_source to 'local'.")
  }

  # ── 4. Non-CRAN packages ──────────────────────────────────────────────────
  # TEOW has no local-file alternative, so `ecoregions` is genuinely required.
  if (!requireNamespace("ecoregions", quietly = TRUE)) {
    add("FATAL", "ecoregions package",
        "Required for TEOW terrestrial ecoregions (Module 2C) and not on CRAN. Install with remotes::install_github(\"jeffreyhanson/ecoregions\").")
  }

  # ── 5. Output directory ───────────────────────────────────────────────────
  out <- config$root_output_dir
  if (is.null(out) || !nzchar(out)) {
    add("FATAL", "root_output_dir", "not configured.")
  } else {
    if (!dir.exists(out)) {
      created <- dir.create(out, recursive = TRUE, showWarnings = FALSE)
      if (!created) add("FATAL", "root_output_dir",
                        sprintf("'%s' does not exist and could not be created.", out))
    }
    if (dir.exists(out)) {
      probe <- file.path(out, ".preflight_write_test")
      okw <- tryCatch({ writeLines("x", probe); unlink(probe); TRUE },
                      error = function(e) FALSE, warning = function(w) FALSE)
      if (!okw) add("FATAL", "root_output_dir",
                    sprintf("'%s' is not writable.", out))
    }
  }

  # ── 6. Input columns ──────────────────────────────────────────────────────
  # Read the header only: the file can be hundreds of MB.
  if (!is.null(config$input_file) && file.exists(config$input_file)) {
    hdr <- tryCatch(
      names(utils::read.delim(config$input_file, sep = "\t", nrows = 1,
                              check.names = FALSE, quote = "")),
      error = function(e) NULL)
    if (is.null(hdr)) {
      add("FATAL", "input_file", "could not be parsed as a tab-separated file.")
    } else {
      # Accept either the DwC-aligned name or the legacy WoC header.
      alias <- if (exists("DWC_INPUT_ALIASES")) DWC_INPUT_ALIASES else character(0)
      has_col <- function(dwc) {
        legacy <- unname(alias[dwc])
        any(c(dwc, legacy) %in% hdr, na.rm = TRUE)
      }
      for (col in c("scientificName", "decimalLatitude", "decimalLongitude", "year")) {
        if (!has_col(col)) add("FATAL", sprintf("input column '%s'", col),
                               "mandatory and absent from the input file.")
      }
      # Not fatal: the run proceeds and 01c_split reports the exclusion in full.
      if (!has_col("establishmentMeans")) {
        add("WARNING", "input column 'establishmentMeans'",
            "absent. cheCkOVER splits occurrences into indigenous and non-indigenous streams, so EVERY record will be excluded and all outputs will be empty. Accepted values: 'indigenous', 'non-indigenous'.")
      }
    }
  }

  res <- if (length(findings)) do.call(rbind, findings) else
    data.frame(severity = character(), item = character(), detail = character())

  .preflight_report(res, module = module)

  n_fatal <- sum(res$severity == "FATAL")
  if (strict && n_fatal > 0) {
    stop(sprintf("Preflight found %d blocking problem(s) - see the report above. Nothing has been processed.",
                 n_fatal), call. = FALSE)
  }

  invisible(res)
}

.preflight_report <- function(res, module = "PREFLIGHT") {
  cat("\n")
  cat("  Preflight check\n")
  cat("  ---------------\n")
  if (nrow(res) == 0L) {
    cat("  All inputs, lookups, reference layers and packages present.\n\n")
    if (exists("log_info", mode = "function")) {
      log_info("Preflight passed.", module = module)
    }
    return(invisible(NULL))
  }
  for (sev in c("FATAL", "WARNING")) {
    rows <- res[res$severity == sev, , drop = FALSE]
    if (nrow(rows) == 0L) next
    cat(sprintf("\n  %s (%d)\n", sev, nrow(rows)))
    for (i in seq_len(nrow(rows))) {
      cat(sprintf("    %s\n", rows$item[i]))
      cat(sprintf("      %s\n", rows$detail[i]))
      if (exists("log_error", mode = "function") && sev == "FATAL") {
        log_error("Preflight: %s - %s", rows$item[i], rows$detail[i], module = module)
      } else if (exists("log_warn", mode = "function")) {
        log_warn("Preflight: %s - %s", rows$item[i], rows$detail[i], module = module)
      }
    }
  }
  cat("\n  Every problem found is listed above, so they can be fixed in one pass\n")
  cat("  rather than one run at a time.\n\n")
  invisible(NULL)
}
