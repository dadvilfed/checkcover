#!/usr/bin/env Rscript
# tools/seed_input_snapshot.R stores a revision's input snapshot from its
# frozen input only when every taxon's fingerprint equals the revision's
# manifest, and never replaces one (the snapshots let a package say what
# changed in its taxon's input since its previous package).
#
# Runs the tool on the public demo input against a manifest made from that
# input's own fingerprints.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("sf", "dplyr", "readr", "jsonlite", "digest")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_seed_input_snapshot] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  library(dplyr); library(stringr); library(readr)
  source("config.R"); source("R/00_run_file.R"); source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_geo_canon.R"); source("R/00_dwc_fields.R"); source("R/00_run_context.R")
  source("R/01_ingest.R"); source("R/01e_change_detection.R")
}))
log_info <- log_warn <- log_debug <- log_error <- log_memory <- function(...) invisible(NULL)

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
cat("[test_seed_input_snapshot]\n")

demo <- normalizePath("demo_data/WoC_demo_Pontastacus.tsv")
tmp  <- file.path(tempdir(), paste0("seed_", as.integer(runif(1) * 1e8)))
root <- file.path(tmp, "output"); state <- file.path(tmp, "state")

# The manifest a run on the demo would write: one fingerprint per taxon.
work <- file.path(tmp, "work"); dir.create(work, recursive = TRUE)
invisible(capture.output(suppressWarnings(suppressMessages(
  ingest_clean(demo, output_dir = work, resolve_taxonomy = FALSE)))))
clean <- read_clean_occurrences(file.path(work, "clean_occurrences.tsv"))
ids <- unique(make_package_id(clean$species))
write_manifest <- function(v, tamper = FALSE) {
  sp <- lapply(ids, function(id) list(outcome = "new", source_version = v,
    fingerprint = compute_species_fingerprint(clean[make_package_id(clean$species) == id, , drop = FALSE])))
  names(sp) <- ids
  if (tamper) sp[[1]]$fingerprint <- "sha256:0"
  dir.create(file.path(root, v, "checkover"), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(framework_version = v, species = sp),
                       file.path(root, v, "checkover", "manifest.json"), auto_unbox = TRUE)
}
write_manifest("1.0"); write_manifest("1.1", tamper = TRUE)

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
run <- function(v) {
  out <- suppressWarnings(system2(rscript, c("tools/seed_input_snapshot.R", shQuote(demo), v,
                                             shQuote(root), shQuote(state)), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) 0L else st
}

ok(run("1.0") == 0L, "every fingerprint equal: the snapshot is stored")
snap <- read_input_snapshot(state, "1.0")
ok(!is.null(snap) && nrow(snap) == nrow(clean) && setequal(unique(snap$package_id), ids),
   "the stored snapshot holds every record, by package id")
ok(identical(compute_species_fingerprint(snap[snap$package_id == ids[1], setdiff(names(snap), "package_id")]),
             compute_species_fingerprint(clean[make_package_id(clean$species) == ids[1], , drop = FALSE])),
   "a taxon's rows in the snapshot fingerprint as its input does")
ok(run("1.0") == 2L, "an existing snapshot is never replaced")
ok(run("1.1") == 1L && !file.exists(input_snapshot_path(state, "1.1")),
   "a fingerprint that differs: refused, nothing stored")
ok(length(list.files(state, pattern = "^seed_")) == 0L, "its working files (coordinates) are removed")

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_seed_input_snapshot] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
