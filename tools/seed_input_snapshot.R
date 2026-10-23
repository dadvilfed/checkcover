#!/usr/bin/env Rscript
# Store a revision's input snapshot from its frozen input, for revisions made
# before runs stored their own (R/00_run_context.R, write_input_snapshot()).
#
# A package reports what changed in its taxon's input since the taxon's
# previous package. From the code that writes them, each run keeps its
# fingerprint input at <state_dir>/inputs/<version>/fingerprint_input.rds.
# 1.0 and 1.1 were made before that, so the first packages to follow them would
# say "not available". This rebuilds the snapshot from the frozen input the
# revision ran on, through the real ingest, and stores it only if EVERY taxon's
# fingerprint equals the one in that revision's manifest: then the snapshot is
# exactly what that run fingerprinted, whatever has changed in the code since.
#
#   Rscript tools/seed_input_snapshot.R <input.tsv> <version> <root_output_dir> <state_dir>
#
# e.g. on UVT, in the container, for the two revisions of runtime-1.0:
#   Rscript tools/seed_input_snapshot.R /data/frozen/1.0/input.tsv 1.0 /data/output /data/state
#   Rscript tools/seed_input_snapshot.R /data/frozen/1.1/input.tsv 1.1 /data/output /data/state
#
# The 1.0 input is the export with the claims cleared (the baseline). Prints
# counts and package ids only, never values: the input carries exact
# coordinates. Its working files go to a folder in the state dir and are
# removed. Refuses to replace an existing snapshot.
#
# Exit codes: 0 stored, 1 fingerprints differ (nothing stored), 2 usage.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  cat("usage: Rscript tools/seed_input_snapshot.R <input.tsv> <version> <root_output_dir> <state_dir>\n")
  quit(status = 2)
}
input <- args[1]; version <- args[2]; root <- args[3]; state <- args[4]
if (!file.exists(input)) { cat("input not found:", input, "\n"); quit(status = 2) }
man_path <- file.path(root, version, "checkover", "manifest.json")
if (!file.exists(man_path)) { cat("no manifest for", version, "at", man_path, "\n"); quit(status = 2) }

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
input <- normalizePath(input); root <- normalizePath(root)
dir.create(state, recursive = TRUE, showWarnings = FALSE); state <- normalizePath(state)
owd <- setwd(.root)
suppressWarnings(suppressMessages({
  library(dplyr); library(stringr); library(readr)
  source("config.R")
  source("R/00_run_file.R"); source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_geo_canon.R"); source("R/00_dwc_fields.R"); source("R/00_run_context.R")
  source("R/01_ingest.R"); source("R/01e_change_detection.R")
}))
log_info <- log_warn <- log_debug <- log_error <- log_memory <- function(...) invisible(NULL)
setwd(owd)

target <- input_snapshot_path(state, version)
if (file.exists(target)) { cat("a snapshot of", version, "is already stored:", target, "\n"); quit(status = 2) }

# The working files carry the input's coordinates: removed on every way out
# (on.exit() does nothing at the top level of a script, and quit() skips it).
work <- file.path(state, sprintf("seed_%s_%d", version, Sys.getpid()))
dir.create(work, recursive = TRUE)
finish <- function(status) { unlink(work, recursive = TRUE); quit(status = status) }

# The run's own ingest, without the WoRMS lookup: taxonomy adds columns but
# none the fingerprint reads (the origin is `status` either way).
clean <- tryCatch({
  invisible(capture.output(suppressWarnings(suppressMessages(
    ingest_clean(input, output_dir = work, resolve_taxonomy = FALSE)))))
  read_clean_occurrences(file.path(work, "clean_occurrences.tsv"))
}, error = function(e) { cat("ingest failed:", conditionMessage(e), "\n"); finish(1) })
clean$package_id <- make_package_id(clean$species)
unlink(work, recursive = TRUE)

man <- jsonlite::read_json(man_path, simplifyVector = FALSE)
ids <- names(man$species)
fp_of <- function(id) compute_species_fingerprint(clean[clean$package_id == id, , drop = FALSE])
expected <- vapply(ids, function(id) as.character(man$species[[id]]$fingerprint %||% NA), "")
got      <- vapply(ids, fp_of, "")
checked  <- !is.na(expected)
bad      <- ids[checked & got != expected]
extra    <- setdiff(unique(clean$package_id), ids)

cat(sprintf("%s: %d records, %d taxa in the input; manifest %d taxa, %d with a fingerprint\n",
            version, nrow(clean), length(unique(clean$package_id)), length(ids), sum(checked)))
cat(sprintf("fingerprints equal: %d   differ: %d   taxa not in the manifest: %d\n",
            sum(checked) - length(bad), length(bad), length(extra)))
if (length(bad) || length(extra)) {
  if (length(bad))   cat("differ:", paste(head(bad, 30), collapse = ", "), if (length(bad) > 30) "...", "\n")
  if (length(extra)) cat("not in the manifest:", paste(head(extra, 30), collapse = ", "), "\n")
  cat("This is not the input", version, "ran on, or not read as it was. Nothing stored.\n")
  finish(1)
}

clean$package_id <- NULL
write_input_snapshot(clean, state, version)
cat("stored", target, "\n")
finish(0)
