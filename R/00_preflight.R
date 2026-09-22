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
        sprintf("CONFIG$spatial$feow_source is 'feowR' but the package is absent. Install with %s, or set feow_source to 'local'.",
                github_install_hint("feowR")))
  }

  # ── 4. Non-CRAN packages ──────────────────────────────────────────────────
  # TEOW has no local-file alternative, so `ecoregions` is genuinely required.
  if (!requireNamespace("ecoregions", quietly = TRUE)) {
    add("FATAL", "ecoregions package",
        sprintf("Required for TEOW terrestrial ecoregions (Module 2C) and not on CRAN. Install with %s.",
                github_install_hint("ecoregions")))
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

  # ── 5b. Working state, and what the output root may hold ──────────────────
  lay <- check_state_layout(config)
  for (i in seq_len(nrow(lay))) add(lay$severity[i], lay$item[i], lay$detail[i])

  # ── 5c. The revision number ───────────────────────────────────────────────
  num <- check_version_number(config)
  for (i in seq_len(nrow(num))) add(num$severity[i], num$item[i], num$detail[i])

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

  # A service run leaves the full list in <run home>/preflight.json, pass or
  # refuse, for the runner to show on the workbench (R/00_run_file.R).
  if (strict && n_fatal > 0) {
    msg <- sprintf("Preflight found %d blocking problem(s) - see the report above. Nothing has been processed.",
                   n_fatal)
    if (exists("refuse_run", mode = "function")) refuse_run(res, msg, config)
    stop(msg, call. = FALSE)
  }
  if (strict && exists("write_preflight_file", mode = "function")) {
    write_preflight_file(config, res, "passed")
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

#' What the output root and the state dir hold, checked before a run.
#'
#' The output root is what a platform mirrors and installs, so it may hold
#' revision folders only; working state lives in state_dir (UVT server setup,
#' sections 05-07). And one state dir belongs to one output root: temporal/ is
#' the history of that root's revisions, and reading another series' history
#' compares every taxon against the wrong baseline without raising any error.
#'
#' @return A data frame of findings (severity, item, detail), possibly empty.
check_state_layout <- function(config) {
  f <- list()
  add <- function(severity, item, detail) {
    f[[length(f) + 1L]] <<- data.frame(severity = severity, item = item,
                                       detail = detail, stringsAsFactors = FALSE)
  }
  done <- function() {
    if (length(f)) do.call(rbind, f) else
      data.frame(severity = character(), item = character(), detail = character(),
                 stringsAsFactors = FALSE)
  }

  out <- config$root_output_dir
  if (is.null(out) || !nzchar(out)) return(done())
  service <- !is.null(config$service)
  rev_rx  <- "^[0-9]+\\.[0-9]+$"

  # ── What the output root holds ──
  if (dir.exists(out)) {
    entries <- setdiff(list.files(out, all.files = TRUE, no.. = TRUE),
                       ".preflight_write_test")
    is_rev <- grepl(rev_rx, entries) & dir.exists(file.path(out, entries))
    other  <- entries[!is_rev]
    if (length(other) > 0L) {
      coord <- intersect(other, c("runs", "temporal"))
      add(if (service) "FATAL" else "WARNING", "root_output_dir contents",
          sprintf(paste0(
            "'%s' holds %s besides revision folders. The output root is mirrored ",
            "and installed as it is, so it must hold revision folders only.%s ",
            "Move working state to state_dir - see README 'Where things live'."),
            out, paste(sprintf("'%s'", other), collapse = ", "),
            if (length(coord)) sprintf(" %s hold%s record coordinates.",
                                       paste(coord, collapse = " and "),
                                       if (length(coord) == 1L) "s" else "")
            else ""))
    }
  }

  state <- config$state_dir
  if (is.null(state) || !nzchar(state)) return(done())

  # ── The state dir must not live inside the output root ──
  .abs <- function(p) {
    p <- gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
    if (!grepl("^(/|[A-Za-z]:/)", p)) p <- file.path(normalizePath(getwd(), winslash = "/"), p)
    sub("/+$", "", p)
  }
  a_out <- .abs(out); a_state <- .abs(state)
  if (identical(a_state, a_out) || startsWith(a_state, paste0(a_out, "/"))) {
    add("FATAL", "state_dir", sprintf(paste0(
      "'%s' is inside root_output_dir '%s'. Working state holds record ",
      "coordinates, and the output root is what gets mirrored and installed."),
      state, out))
  }

  # ── The state dir belongs to this output root ──
  revs <- if (dir.exists(out)) {
    d <- list.dirs(out, recursive = FALSE, full.names = FALSE); d[grepl(rev_rx, d)]
  } else character(0)
  tdir   <- file.path(state, "temporal")
  # After a run that did not complete, temporal/ holds that run's provisional
  # writes and the run about to start restores the checkpoint first (see
  # temporal_checkpoint_open()), so judge the history it will restore.
  ck     <- file.path(state, "temporal.checkpoint")
  t_eff  <- if (dir.exists(ck)) file.path(ck, "temporal") else tdir
  n_hist <- if (dir.exists(t_eff)) length(list.dirs(t_eff, recursive = FALSE)) else 0L
  legacy <- file.path(out, "temporal")

  if (length(revs) == 0L && n_hist > 0L) {
    add("FATAL", "state_dir", sprintf(paste0(
      "'%s' holds temporal history for %d taxa, but root_output_dir '%s' has no ",
      "revisions: the history belongs to another series. Use an empty state_dir. ",
      "Its cache/ holds reference layers only and may be copied into the new one."),
      tdir, n_hist, out))
  }
  if (length(revs) > 0L && n_hist == 0L && dir.exists(legacy)) {
    add("FATAL", "state_dir", sprintf(paste0(
      "the temporal history of '%s' is still in the old place. Move '%s' to '%s' ",
      "before running; otherwise every taxon's temporal record restarts as a baseline."),
      out, legacy, tdir))
  } else if (length(revs) > 0L && n_hist == 0L && isTRUE(config$temporal$enabled)) {
    add("WARNING", "state_dir", sprintf(paste0(
      "root_output_dir has revisions (%s) but '%s' holds no temporal history, ",
      "so every taxon's temporal record restarts as a baseline."),
      paste(revs, collapse = ", "), tdir))
  }

  if (dir.exists(state)) {
    probe <- file.path(state, ".preflight_write_test")
    okw <- tryCatch({ writeLines("x", probe); unlink(probe); TRUE },
                    error = function(e) FALSE, warning = function(w) FALSE)
    if (!okw) add("FATAL", "state_dir", sprintf("'%s' is not writable.", state))
  }

  done()
}

#' The revision number: a MAJOR.MINOR string, after every existing revision.
#'
#' Versions compare by numeric component, so 1.10 follows 1.9 and 1.100 follows
#' 1.99 (Lucian, 2026-09: the series counts 1.9, 1.10, ... 1.100 without limit).
#' A revision inherits from the revisions before it, so a number that is not
#' after the latest one would inherit from the wrong predecessor: FATAL. A
#' skipped number (1.9 -> 1.11) is a WARNING: the series has no gaps by rule,
#' but nothing breaks.
#'
#' @return A data frame of findings (severity, item, detail), possibly empty.
check_version_number <- function(config) {
  f <- list()
  add <- function(severity, item, detail) {
    f[[length(f) + 1L]] <<- data.frame(severity = severity, item = item,
                                       detail = detail, stringsAsFactors = FALSE)
  }
  done <- function() {
    if (length(f)) do.call(rbind, f) else
      data.frame(severity = character(), item = character(), detail = character(),
                 stringsAsFactors = FALSE)
  }
  rx <- "^[0-9]+\\.[0-9]+$"
  fv <- config$framework_version
  if (!is.character(fv) || length(fv) != 1L || is.na(fv) || !grepl(rx, fv)) {
    add("FATAL", "framework_version", sprintf(paste0(
      "must be a string of the form MAJOR.MINOR such as \"1.10\" (got %s). ",
      "Written as a number, 1.10 would be read as 1.1."),
      paste(format(fv), collapse = ", ")))
    return(done())
  }

  parts <- function(v) as.numeric(strsplit(v, ".", fixed = TRUE)[[1]])
  after <- function(a, b) {        # is version a after version b?
    pa <- parts(a); pb <- parts(b)
    pa[1] > pb[1] || (pa[1] == pb[1] && pa[2] > pb[2])
  }

  out <- config$root_output_dir
  if (is.null(out) || !dir.exists(out)) return(done())
  revs <- list.dirs(out, recursive = FALSE, full.names = FALSE)
  revs <- setdiff(revs[grepl(rx, revs)], fv)
  if (!length(revs)) return(done())
  latest <- revs[1]
  for (r in revs[-1]) if (after(r, latest)) latest <- r

  if (!after(fv, latest)) {
    add("FATAL", "framework_version", sprintf(paste0(
      "%s is not after the latest revision %s in '%s'. Revisions compare by ",
      "numeric component (1.10 follows 1.9), and a revision inherits from the ",
      "ones before it, so the new number must be the next one after %s."),
      fv, latest, out, latest))
  } else {
    p <- parts(fv); l <- parts(latest)
    skipped <- (p[1] == l[1] && p[2] > l[2] + 1) || (p[1] > l[1] && p[2] != 0)
    if (skipped) add("WARNING", "framework_version", sprintf(
      "%s skips numbers after the latest revision %s; the series normally has no gaps.",
      fv, latest))
  }
  done()
}
