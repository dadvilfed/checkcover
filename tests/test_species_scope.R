#!/usr/bin/env Rscript
# Species scope, option (b) (UVT server setup section 04; confirmed by David,
# 2026-09).
#
# A run gets the full cohort plus the taxa an admin approved. Only approved taxa
# may be reprocessed; any other taxon whose data changed is carried over and
# recorded as "deferred", and a never-seen taxon outside the scope is
# "deferred_new" with no package.
#
# The property that matters most is across TWO runs: a deferred change must
# still be visible to the next run. That works only because the manifest keeps
# the SOURCE fingerprint for a deferred taxon; had it recorded the current one,
# the next run would compare equal, call the taxon "unchanged", and the change
# would be silently lost.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("digest", "jsonlite", "readr")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_species_scope] SKIP (%s not installed)\n", p)); quit(status = 0)
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

cat("[test_species_scope]\n")

root <- file.path(tempdir(), paste0("scope_", as.integer(runif(1) * 1e8)))
recs <- function(sp, n, shift = 0) data.frame(
  species = sp, record_id = paste0(sp, "_", seq_len(n)),
  longitude = 22 + seq_len(n) / 100 + shift, latitude = 46 + seq_len(n) / 100,
  year = 2000, is_extinct = FALSE, population_status = "indigenous",
  accuracy = "exact", stringsAsFactors = FALSE)

# The manifest entry logic, as 09_package_export.R writes it.
write_manifest <- function(ctx) {
  dir.create(ctx$current_scaffolding_dir, recursive = TRUE, showWarnings = FALSE)
  entries <- lapply(ctx$species_outcomes, function(o) list(
    outcome = o$outcome, source_version = o$source_version,
    fingerprint = o$fingerprint,
    fingerprint_at_source = o$fingerprint_at_source %||% o$fingerprint))
  names(entries) <- vapply(ctx$species_outcomes, \(o) o$species_clean, character(1))
  jsonlite::write_json(list(framework_version = ctx$framework_version, species = entries),
                       file.path(ctx$current_scaffolding_dir, "manifest.json"),
                       auto_unbox = TRUE, na = "null")
}
run <- function(v, data, scope = NULL, force = FALSE) {
  ctx <- RunContext_init(list(framework_version = v, root_output_dir = root,
                              species_scope = scope, force_reprocess = force),
                         run_id = paste0("t", v))
  ctx$all_species <- sort(unique(data$species))
  ctx <- suppressWarnings(suppressMessages(detect_species_changes(ctx, clean_data = data)))
  write_manifest(ctx)
  ctx
}
outc <- function(ctx) vapply(ctx$species_outcomes, \(o) o$outcome, character(1))

# v1.0: A, B, C, clean run, no scope.
d10 <- rbind(recs("Astacus a", 5), recs("Bstacus b", 5), recs("Cstacus c", 5))
c10 <- run("1.0", d10)
ok(all(outc(c10) == "new"), "clean 1.0: every taxon new")

# v1.1: A and B changed, D appears; only A approved.
d11 <- rbind(recs("Astacus a", 6), recs("Bstacus b", 7), recs("Cstacus c", 5), recs("Dstacus d", 5))
c11 <- run("1.1", d11, scope = "Astacus a")
o11 <- outc(c11)
ok(o11[["Astacus a"]] == "reprocessed", "approved + changed -> reprocessed")
ok(o11[["Bstacus b"]] == "deferred",    "not approved + changed -> deferred")
ok(o11[["Cstacus c"]] == "unchanged",   "not approved + identical -> unchanged")
ok(o11[["Dstacus d"]] == "deferred_new","not approved + never seen -> deferred_new")
ok(identical(sort(c11$active_species), "Astacus a"),
   "only the approved taxon is processed (the admin approved what ran)")

b11 <- c11$species_outcomes[["Bstacus b"]]
ok(identical(b11$source_version, "1.0"), "deferred taxon inherits artifacts from 1.0")
ok(identical(b11$fingerprint_at_source, c10$species_outcomes[["Bstacus b"]]$fingerprint),
   "deferred taxon keeps the SOURCE fingerprint as fingerprint_at_source")
ok(!identical(b11$fingerprint, b11$fingerprint_at_source),
   "and records the current (changed) fingerprint separately")
ok(is.na(c11$species_outcomes[["Dstacus d"]]$source_version),
   "deferred_new has no source_version (nothing to inherit)")

# v1.2: same data as 1.1, no scope. The deferred change must still be seen.
c12 <- run("1.2", d11)
o12 <- outc(c12)
ok(o12[["Bstacus b"]] == "reprocessed",
   "NEXT run: the deferred change is still visible and B is reprocessed")
ok(o12[["Dstacus d"]] == "new",
   "NEXT run: the deferred_new taxon is new (the 1.1 entry is skipped)")
ok(o12[["Astacus a"]] == "unchanged" &&
   identical(c12$species_outcomes[["Astacus a"]]$source_version, "1.1"),
   "NEXT run: A unchanged, served from 1.1 where it was reprocessed")
ok(o12[["Cstacus c"]] == "unchanged" &&
   identical(c12$species_outcomes[["Cstacus c"]]$source_version, "1.0"),
   "NEXT run: C unchanged, still served from 1.0")

# Forced taxa are in scope even when not listed.
c13 <- run("1.3", d11, scope = "Astacus a", force = "Cstacus c")
ok(outc(c13)[["Cstacus c"]] == "reprocessed", "a forced taxon is in scope even if not listed")

# The package manifest writer must use the source fingerprint, not overwrite it.
f09 <- paste(readLines("R/09_package_export.R", warn = FALSE), collapse = "\n")
ok(grepl("o$fingerprint_at_source %||% o$fingerprint", f09, fixed = TRUE),
   "09_package_export.R writes fingerprint_at_source from the outcome")

unlink(root, recursive = TRUE)
cat(sprintf("\n[test_species_scope] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
