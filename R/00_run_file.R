#### MODULE 00_RUN_FILE: RUN PARAMETERS FROM A FILE, AND THE SERVICE CONTRACT ####
#
# A service must never edit code, so cheCkOVER reads its run parameters from a
# small JSON file when the environment variable CHECKOVER_RUN names one (UVT
# server setup, section 03). Without it nothing changes: config.R is the whole
# configuration, exactly as for a manual run.
#
#   { "run_id":            "run_20261015_a1b2",
#     "framework_version": "1.3",
#     "input_file":        "/data/runs/run_20261015_a1b2/input.tsv",
#     "root_output_dir":   "/data/output",
#     "state_dir":         "/data/state",
#     "species_scope":     ["Austropotamobius_fulcisianus", "Faxonius_validus"],
#     "code_tag":          "runtime-1.3" }
#
# framework_version, input_file, root_output_dir and state_dir are required: a
# service run must never fall back to whatever config.R happens to say. Any
# other top-level key of CONFIG may be set too, and an object merges into the
# matching section, e.g. "spatial": {"hydro_dir": "/data/spatial/hydrobasins"}.
# An unknown key is refused, not ignored: a typo must not silently become a
# config.R default.
#
# The directory holding the run file is the RUN HOME. Everything a service run
# writes outside the revision folder goes there:
#
#   run.json        the run file (written by the runner)
#   input.tsv       the frozen input (written by the runner)
#   run.log         the log, for progress
#   preflight.json  what the preflight found; "refused" means nothing ran
#   status.json     running / succeeded / failed / refused, with the message
#   work/           working files, including coordinates; never uploaded
#
# Exit codes, for a non-interactive run (Rscript, the container):
#   0  succeeded: the revision folder is complete
#   1  failed during the run: a partial revision folder may exist
#   2  refused before processing: nothing was written to the output root
# ──────────────────────────────────────────────────────────────────────────────

RUN_FILE_REQUIRED <- c("framework_version", "input_file", "root_output_dir", "state_dir")
RUN_FILE_ONLY     <- c("run_id")   # keys that are not CONFIG settings

.CHECKOVER_EXIT <- new.env(parent = emptyenv())
.CHECKOVER_EXIT$refused <- FALSE
.CHECKOVER_EXIT$started <- Sys.time()
.CHECKOVER_EXIT$phases  <- list()

#' Peak resident memory of this R process so far, in MB (Linux; NULL elsewhere).
#'
#' VmHWM is the high-water mark of the process's resident set. cheCkOVER runs as
#' a single R process, so this is the peak RAM of the whole run.
peak_rss_mb <- function(status_file = "/proc/self/status") {
  if (!file.exists(status_file)) return(NULL)
  l <- grep("^VmHWM:", readLines(status_file, warn = FALSE), value = TRUE)
  if (!length(l)) return(NULL)
  round(as.numeric(gsub("[^0-9]", "", l[1])) / 1024, 1)
}

#' Start a named phase of the run. Its duration runs until the next phase
#' starts, or until the final status is written. A service run's status.json is
#' refreshed at every phase, so the runner can also read progress from it.
mark_phase <- function(name, config = NULL) {
  ph <- .CHECKOVER_EXIT$phases
  ph[[length(ph) + 1L]] <- list(name = name, started = Sys.time())
  .CHECKOVER_EXIT$phases <- ph
  if (is.null(config) && exists("CONFIG", envir = globalenv())) config <- get("CONFIG", envir = globalenv())
  if (!is.null(config$service)) write_run_status(config, "running")
  invisible(name)
}

.phase_table <- function(now = Sys.time()) {
  ph <- .CHECKOVER_EXIT$phases
  lapply(seq_along(ph), function(i) {
    end <- if (i < length(ph)) ph[[i + 1L]]$started else now
    list(phase = ph[[i]]$name,
         seconds = round(as.numeric(difftime(end, ph[[i]]$started, units = "secs")), 1))
  })
}

.rf_or <- function(a, b) if (is.null(a)) b else a

#' The working-state directory of a configuration.
state_dir_of <- function(config) {
  .rf_or(config$state_dir, "checkover_state")
}

.problem <- function(severity, item, detail) {
  data.frame(severity = severity, item = item, detail = detail,
             stringsAsFactors = FALSE)
}

.single_string <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)

# Taxon lists in a run file (species_scope, and force_reprocess when it is a
# list) hold PACKAGE IDS: the manifest's keys and the folder names, e.g.
# "Astacus_astacus", "Cambarellus_pandicambarus_rotatus". One form, so a
# manifest's deferred entries become the next run's species_scope unchanged. A
# display name (spaces, parentheses) is refused, never translated.
.package_id_form_problems <- function(key, v) {
  if (!is.character(v)) return(character(0))
  bad <- v[is.na(v) | !nzchar(v) | grepl("[[:space:]()]", v) | grepl("^_|_$|__", v)]
  vapply(unique(bad), function(b) sprintf(
    "entry \"%s\" is not a package id. Taxa are listed by package id, e.g. \"Astacus_astacus\" (the manifest key and folder name).",
    b), character(1))
}

# Returns NULL when the value is acceptable, otherwise what is wrong with it.
.run_value_problem <- function(key, v) {
  if (key %in% c("input_file", "root_output_dir", "state_dir", "code_tag", "run_id")) {
    if (!.single_string(v)) return("must be a single non-empty string.")
  }
  if (key == "framework_version") {
    # A JSON number would reach R as a double, and 1.10 prints as "1.1".
    if (!.single_string(v))
      return("must be a string such as \"1.3\", not a number: 1.10 would be read as 1.1.")
    if (!grepl("^[0-9]+\\.[0-9]+$", v))
      return(sprintf("'%s' is not of the form MAJOR.MINOR, e.g. \"1.3\".", v))
  }
  if (key == "run_id" && !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", v)) {
    return("may contain only letters, digits, '.', '_' and '-'.")
  }
  if (key == "species_scope" && !is.null(v)) {
    if (is.list(v) && length(v) == 0L)
      return("is an empty list, which would approve nothing. List the approved taxa, or use null for a run without a scope.")
    if (!is.character(v)) return("must be a list of species names or package ids, or null.")
  }
  if (key == "force_reprocess") {
    ok <- (is.logical(v) && length(v) == 1L && !is.na(v)) || is.character(v) ||
          (is.list(v) && length(v) == 0L)
    if (!ok) return("must be true, false, or a list of species names or package ids.")
  }
  NULL
}

#' Service paths derived from the location of the run file.
service_paths <- function(run_file, run_id = NULL) {
  home <- normalizePath(dirname(run_file), winslash = "/", mustWork = FALSE)
  list(
    run_file       = normalizePath(run_file, winslash = "/", mustWork = FALSE),
    run_home       = home,
    run_id         = .rf_or(run_id, basename(home)),
    work_dir       = file.path(home, "work"),
    log_file       = file.path(home, "run.log"),
    preflight_file = file.path(home, "preflight.json"),
    status_file    = file.path(home, "status.json")
  )
}

#' Apply the run file named by CHECKOVER_RUN, if any.
#'
#' @return `config`, unchanged when no run file is set. Otherwise the run file's
#'   values override it, and `config$service` carries the service paths (see
#'   service_paths()). A manual run has `config$service == NULL`, which is how
#'   the rest of the pipeline tells the two apart.
apply_run_file <- function(config, path = Sys.getenv("CHECKOVER_RUN", unset = "")) {
  config$service <- NULL
  if (!nzchar(path)) return(config)

  svc <- service_paths(path)
  config$service <- svc
  # What a refusal below reports: the run file's own framework_version when it
  # has a usable one, never config.R's, which this run was not asked to build.
  pre <- config
  pre$framework_version <- NA_character_

  if (!file.exists(path)) {
    refuse_run(.problem("FATAL", "CHECKOVER_RUN", sprintf("'%s' does not exist.", path)),
               "The run file named by CHECKOVER_RUN does not exist.", pre)
  }

  rf <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                       simplifyMatrix = FALSE),
    error = function(e) e)
  if (inherits(rf, "error")) {
    refuse_run(.problem("FATAL", "run file", paste("not valid JSON:", conditionMessage(rf))),
               "The run file is not valid JSON.", pre)
  }
  if (!is.list(rf) || is.null(names(rf)) || any(!nzchar(names(rf)))) {
    refuse_run(.problem("FATAL", "run file", "must be a JSON object of settings."),
               "The run file is not a JSON object.", pre)
  }

  if (.single_string(rf$framework_version)) pre$framework_version <- rf$framework_version
  if (.single_string(rf$run_id)) pre$service$run_id <- rf$run_id

  problems <- list()
  settable <- c(setdiff(names(config), "service"), RUN_FILE_ONLY)
  unknown <- setdiff(names(rf), settable)
  for (k in unknown) {
    problems[[length(problems) + 1L]] <- .problem("FATAL", k,
      "is not a cheCkOVER setting. Check the spelling against config.R.")
  }
  for (k in setdiff(RUN_FILE_REQUIRED, names(rf))) {
    problems[[length(problems) + 1L]] <- .problem("FATAL", k,
      "is required in a run file; a service run never falls back to config.R for it.")
  }
  for (k in intersect(names(rf), settable)) {
    msg <- .run_value_problem(k, rf[[k]])
    if (!is.null(msg)) problems[[length(problems) + 1L]] <- .problem("FATAL", k, msg)
    if (is.null(msg) && k %in% c("species_scope", "force_reprocess")) {
      for (m in .package_id_form_problems(k, rf[[k]])) {
        problems[[length(problems) + 1L]] <- .problem("FATAL", k, m)
      }
    }
  }
  if (length(problems) > 0L) {
    refuse_run(do.call(rbind, problems),
               sprintf("The run file has %d problem(s).", length(problems)), pre)
  }

  for (k in setdiff(names(rf), RUN_FILE_ONLY)) {
    v <- rf[[k]]
    if (k == "force_reprocess" && is.list(v) && length(v) == 0L) v <- FALSE
    if (is.list(config[[k]]) && is.list(v) && !is.null(names(v))) {
      config[[k]] <- utils::modifyList(config[[k]], v)
    } else {
      config[k] <- list(v)   # keeps an explicit null, e.g. "species_scope": null
    }
  }

  config$service <- service_paths(path, run_id = rf$run_id)
  config
}

#' Taxa named by a run file that are not in the input.
#'
#' Called right after ingest, when the input's taxa are known and before
#' anything is written to the output root, so a refusal is still exit code 2.
#' A taxon counts as present when its package id (make_package_id() of the
#' normalised name) equals the entry.
#'
#' @param species The input's taxa (normalised names, ctx$all_species).
#' @return A data frame of problems, one row per refused entry (may be empty).
check_run_taxa <- function(config, species, id_fun = make_package_id) {
  if (is.null(config$service)) return(.problem(character(), character(), character()))
  ids <- unique(id_fun(species))
  rows <- list()
  for (k in c("species_scope", "force_reprocess")) {
    v <- config[[k]]
    if (!is.character(v)) next
    for (e in unique(setdiff(v, ids))) {
      rows[[length(rows) + 1L]] <- .problem("FATAL", k, sprintf(
        "entry \"%s\" matches no taxon in the input file. Taxa are listed by package id; check the spelling against the manifest keys.",
        e))
    }
  }
  if (length(rows)) do.call(rbind, rows) else .problem(character(), character(), character())
}

# ── Status and refusal files ─────────────────────────────────────────────────

.write_json_file <- function(x, file) {
  tryCatch({
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(x, file, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")
    TRUE
  }, error = function(e) FALSE)
}

#' Write <run home>/preflight.json. No-op for a manual run.
#'
#' @param findings data frame of severity, item, detail (may be empty).
#' @param status "passed" or "refused".
write_preflight_file <- function(config, findings, status) {
  svc <- config$service
  if (is.null(svc)) return(invisible(FALSE))
  findings <- if (is.null(findings)) .problem(character(), character(), character()) else findings
  invisible(.write_json_file(list(
    status     = status,
    run_id     = svc$run_id,
    framework_version = .rf_or(config$framework_version, NA_character_),
    checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    n_fatal    = sum(findings$severity == "FATAL"),
    n_warning  = sum(findings$severity == "WARNING"),
    problems   = lapply(seq_len(nrow(findings)), function(i) as.list(findings[i, ]))
  ), svc$preflight_file))
}

#' Write <run home>/status.json. No-op for a manual run.
#'
#' @param status "running", "succeeded", "failed" or "refused".
write_run_status <- function(config, status, message = NULL, extra = list()) {
  svc <- config$service
  if (is.null(svc)) return(invisible(FALSE))
  exit_code <- switch(status, succeeded = 0L, failed = 1L, refused = 2L, NULL)
  now <- Sys.time()
  ph  <- .phase_table(now)
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%S%z")
  x <- c(list(
    status            = status,
    run_id            = svc$run_id,
    framework_version = .rf_or(config$framework_version, NA_character_),
    exit_code         = exit_code,
    message           = message,
    current_phase     = if (status == "running" && length(ph)) ph[[length(ph)]]$phase else NULL,
    started_at        = fmt(.CHECKOVER_EXIT$started),
    updated_at        = fmt(now),
    elapsed_seconds   = round(as.numeric(difftime(now, .CHECKOVER_EXIT$started, units = "secs")), 1),
    peak_rss_mb       = peak_rss_mb(),
    phases            = if (length(ph)) ph else NULL
  ), extra)
  invisible(.write_json_file(x[!vapply(x, is.null, logical(1))], svc$status_file))
}

#' Record that the run is refused before processing, so it exits with code 2.
mark_refused <- function() {
  .CHECKOVER_EXIT$refused <- TRUE
  invisible(TRUE)
}

#' Refuse to start: write the problem list, then stop.
#'
#' For a service run the problems go to <run home>/preflight.json. The stop() is
#' caught by the exit handler, which exits with code 2.
refuse_run <- function(findings, message, config) {
  write_preflight_file(config, findings, "refused")
  write_run_status(config, "refused", message)
  mark_refused()
  stop(message, call. = FALSE)
}

#' Make a non-interactive run exit with the service's exit codes.
#'
#' Rscript already exits 1 on an uncaught error; this adds exit code 2 for a
#' refusal and writes status.json with the error message for a service run. An
#' interactive session is left alone: quitting R from source() would be hostile.
install_exit_handler <- function() {
  if (interactive()) return(invisible(FALSE))
  options(error = function() {
    refused <- isTRUE(.CHECKOVER_EXIT$refused)
    cfg <- if (exists("CONFIG", envir = globalenv())) get("CONFIG", envir = globalenv()) else list()
    if (!refused) {
      # One clean line: "Error in f(x) : msg" / "Error: msg" -> "msg". A long
      # message starts on the line after "Error in f(x) :", and Rscript
      # appends its own "Calls: ..." line; both are removed.
      msg <- geterrmessage()
      msg <- sub("^Error(?: in [^\n]*?)? ?:\\s*", "", msg, perl = TRUE)
      msg <- sub("(?s)\\s*\\nCalls: .*$", "", msg, perl = TRUE)
      msg <- trimws(gsub("\\s*\\n\\s*", " ", msg))
      # Into run.log too, with the call chain: R prints a fatal error only on
      # the console, so the log of a failed run used to just stop (the first
      # single-taxon trial, 2026-09-23). The runner tails run.log.
      calls <- vapply(utils::head(sys.calls(), -1L), function(cl) {
        f <- cl[[1]]
        if (is.name(f)) as.character(f) else paste(deparse(f, width.cutoff = 60L), collapse = "")
      }, character(1))
      calls <- calls[!calls %in% c("tryCatch", "tryCatchList", "tryCatchOne", "doTryCatch",
                                   "withCallingHandlers", "with_log_section", "force",
                                   "suppressWarnings", "suppressMessages", "local", "eval",
                                   "withVisible", "source", "stop", "(function() {")]
      if (exists("log_error", mode = "function")) {
        try(log_error("FATAL: %s", msg, module = "MAIN"), silent = TRUE)
        if (length(calls))
          try(log_error("  Calls: %s", paste(utils::tail(calls, 12L), collapse = " -> "),
                        module = "MAIN"), silent = TRUE)
      }
      write_run_status(cfg, "failed", msg)
    }
    quit(save = "no", status = if (refused) 2L else 1L, runLast = FALSE)
  })
  invisible(TRUE)
}

# ── The temporal history is transactional across a run ───────────────────────
#
# <state_dir>/temporal/ is the one piece of working state a run changes that a
# retry cannot simply delete: Phase 5C writes a new snapshot per taxon before
# the packages are exported. If a run then dies, a retry would compare those
# taxa against snapshots from the failed attempt, find them "unchanged", and
# silently skip their temporal output.
#
# So a run takes a checkpoint of temporal/ before processing and discards it
# only on success. A run that finds a checkpoint knows the previous run did not
# complete, whatever the cause (error, kill, reboot, full disk), and restores it
# first. The copy is taken under a ".partial" name and renamed when complete, so
# an interrupted copy is never mistaken for a checkpoint.
#
# This assumes one run at a time per state dir, which the service guarantees
# (UVT server setup, section 09) and a manual run must respect.

.log_or_message <- function(level, fmt, ...) {
  f <- paste0("log_", level)
  if (exists(f, mode = "function")) get(f)(fmt, ..., module = "STATE")
  else message(sprintf(fmt, ...))
}

temporal_checkpoint_paths <- function(state_dir) {
  list(temporal = file.path(state_dir, "temporal"),
       ck       = file.path(state_dir, "temporal.checkpoint"),
       partial  = file.path(state_dir, "temporal.checkpoint.partial"))
}

#' Restore after an incomplete run, then (if `take`) checkpoint temporal/.
#'
#' @return Invisibly, TRUE when a previous run's changes were rolled back.
temporal_checkpoint_open <- function(state_dir, take = TRUE) {
  p <- temporal_checkpoint_paths(state_dir)
  rolled_back <- FALSE

  if (dir.exists(p$partial)) unlink(p$partial, recursive = TRUE)

  if (dir.exists(p$ck)) {
    .log_or_message("warn", paste0(
      "The previous run did not complete: restoring the temporal history ",
      "from its checkpoint (%s)."), p$ck)
    if (dir.exists(p$temporal)) unlink(p$temporal, recursive = TRUE)
    saved <- file.path(p$ck, "temporal")
    if (dir.exists(saved) && !file.rename(saved, p$temporal)) {
      stop(sprintf("Could not restore '%s' from '%s'. Restore it by hand before running.",
                   p$temporal, saved), call. = FALSE)
    }
    unlink(p$ck, recursive = TRUE)
    rolled_back <- TRUE
  }

  if (take) {
    dir.create(p$partial, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(p$temporal) &&
        !isTRUE(all(file.copy(p$temporal, p$partial, recursive = TRUE, copy.date = TRUE)))) {
      unlink(p$partial, recursive = TRUE)
      stop(sprintf("Could not checkpoint '%s' (disk full?). Nothing has been processed.",
                   p$temporal), call. = FALSE)
    }
    if (!file.rename(p$partial, p$ck)) {
      stop(sprintf("Could not finalise the checkpoint '%s'.", p$ck), call. = FALSE)
    }
  }
  invisible(rolled_back)
}

#' The run completed: its temporal changes stand, the checkpoint goes.
temporal_checkpoint_close <- function(state_dir) {
  p <- temporal_checkpoint_paths(state_dir)
  if (dir.exists(p$ck)) unlink(p$ck, recursive = TRUE)
  invisible(TRUE)
}
