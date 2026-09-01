#!/usr/bin/env Rscript
# force_reprocess override (Lucian, 2026-08).
#
# Sparse versioning fingerprints the INPUT DATA, so a code-only change is
# invisible to it: every species comes out "unchanged", exits at the Phase 1.5
# cutoff, and inherits the previous version's artifacts. The fix never reaches
# the output. This covers the escape hatch for that case.
#
# The two properties that matter most and are easy to get wrong:
#   1. a forced species becomes "reprocessed", NEVER "new" — "new" drops
#      prior_source_version and Modules 11-13 need it for the temporal delta;
#   2. the change_summary says the reprocess was FORCED, so a manifest full of
#      reprocessed species is not mistaken for that many real data changes.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("digest", "jsonlite", "readr")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_force_reprocess] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_run_context.R"); source("R/01e_change_detection.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

SPP <- c("Astacus astacus", "Cherax destructor", "Austropotamobius bihariensis")

cat("[test_force_reprocess]\n")

# ── Part 1: .resolve_force_spec() ──────────────────────────────────────────
r <- .resolve_force_spec(FALSE, SPP);  ok(r$mode == "none", "FALSE -> no override")
r <- .resolve_force_spec(NULL,  SPP);  ok(r$mode == "none", "NULL -> no override")
r <- .resolve_force_spec(NA,    SPP);  ok(r$mode == "none", "NA -> no override")
r <- .resolve_force_spec(TRUE,  SPP);  ok(r$mode == "all",  "TRUE -> force all")

r <- .resolve_force_spec("Cherax destructor", SPP)
ok(r$mode == "subset" && identical(r$species, "Cherax destructor"),
   "display name -> subset of one")

r <- .resolve_force_spec("Cherax_destructor", SPP)
ok(r$mode == "subset" && identical(r$species, "Cherax destructor"),
   "package id (underscore) resolves to the same species")

r <- .resolve_force_spec(c("Astacus astacus", "Cherax_destructor"), SPP)
ok(r$mode == "subset" && setequal(r$species, c("Astacus astacus", "Cherax destructor")),
   "mixed spellings in one vector both resolve")

r <- suppressWarnings(.resolve_force_spec(c("Cherax destructor", "Typo species"), SPP))
ok(r$mode == "subset" && identical(r$species, "Cherax destructor"),
   "unknown name ignored, known name still forced")

r <- .resolve_force_spec(character(0), SPP); ok(r$mode == "none", "empty vector -> no override")
r <- suppressWarnings(.resolve_force_spec(list(1), SPP))
ok(r$mode == "none", "non-character junk -> no override, not an error")

# ── Part 2: end-to-end through detect_species_changes() ────────────────────
# Build a two-version output tree: v1.1 manifest declares all three species
# unchanged-and-identical, then v1.2 re-runs against byte-identical data.

root <- file.path(tempdir(), paste0("fr_", as.integer(runif(1) * 1e8)))
dir.create(file.path(root, "1.1", "checkover"), recursive = TRUE, showWarnings = FALSE)

clean <- do.call(rbind, lapply(SPP, function(s) data.frame(
  species = s, record_id = NA_character_,
  longitude = c(22.1, 22.2), latitude = c(46.1, 46.2),
  year = c(1990, 2000), is_extinct = FALSE,
  population_status = "indigenous", accuracy = "exact",
  stringsAsFactors = FALSE)))

fps <- lapply(SPP, function(s)
  compute_species_fingerprint(clean[clean$species == s, , drop = FALSE]))
names(fps) <- vapply(SPP, make_package_id, character(1))

jsonlite::write_json(
  list(framework_version = "1.1",
       species = lapply(names(fps), function(id)
         list(species_clean = id, outcome = "new", source_version = "1.1",
              fingerprint = fps[[id]], fingerprint_at_source = fps[[id]])) |>
         setNames(names(fps))),
  file.path(root, "1.1", "checkover", "manifest.json"),
  auto_unbox = TRUE, pretty = TRUE)

run <- function(force) {
  cfg <- list(framework_version = "1.2", root_output_dir = root,
              force_reprocess = force)
  ctx <- RunContext_init(cfg, run_id = "test")
  ctx$all_species <- SPP
  suppressWarnings(suppressMessages(
    detect_species_changes(ctx, clean_data = clean)))
}

ok(identical(RunContext_init(list(framework_version = "1.2", root_output_dir = root),
                             run_id = "t")$force_reprocess, FALSE),
   "config without the key defaults to FALSE")

# Baseline: no override, data identical -> everything unchanged, nothing active.
c0 <- run(FALSE)
ok(all(vapply(c0$species_outcomes, \(o) o$outcome, character(1)) == "unchanged"),
   "no override: identical data -> all unchanged")
ok(length(c0$active_species) == 0L,
   "no override: nothing active (this is the bug the switch exists for)")

# force_reprocess = TRUE
c1 <- run(TRUE)
out1 <- vapply(c1$species_outcomes, \(o) o$outcome, character(1))
ok(all(out1 == "reprocessed"), "TRUE: all species reprocessed despite identical data")
ok(!any(out1 == "new"), "TRUE: forced species are 'reprocessed', never 'new'")
ok(setequal(c1$active_species, SPP), "TRUE: every species active")
ok(all(vapply(c1$species_outcomes, \(o) identical(o$prior_source_version, "1.1"),
              logical(1))),
   "TRUE: prior_source_version preserved (temporal delta still computable)")
ok(all(vapply(c1$species_outcomes, \(o) grepl("forced", o$change_summary), logical(1))),
   "TRUE: change_summary records that the reprocess was forced")
ok(all(vapply(c1$species_outcomes, \(o) identical(o$source_version, "1.2"), logical(1))),
   "TRUE: source_version points at the new version")

# Vector form: only the named species is forced.
c2 <- run("Cherax destructor")
out2 <- vapply(c2$species_outcomes, \(o) o$outcome, character(1))
ok(out2[["Cherax destructor"]] == "reprocessed", "vector: named species reprocessed")
ok(out2[["Astacus astacus"]] == "unchanged",     "vector: unnamed species left unchanged")
ok(identical(c2$active_species, "Cherax destructor"),
   "vector: exactly one species active")

# Fingerprints themselves must be untouched by the override — forcing changes
# what we RECOMPUTE, never what the data is claimed to be.
fp0 <- vapply(c0$species_outcomes, \(o) o$fingerprint, character(1))
fp1 <- vapply(c1$species_outcomes, \(o) o$fingerprint, character(1))
ok(identical(fp0, fp1), "fingerprints identical with and without the override")

unlink(root, recursive = TRUE)
cat(sprintf("\n[test_force_reprocess] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
