#!/usr/bin/env Rscript
# Revision numbers compare by numeric component (Lucian, 2026-09): the series
# runs 1.9, 1.10, 1.11 ... 1.99, 1.100 with no limit, and 1.100 is after 1.99.
# As text, "1.100" < "1.2" and "1.10" < "1.9", so any lexical sort picks the
# wrong predecessor and a revision inherits from the wrong place.
#
# Builds the series 1.2 -> 1.9 -> 1.10 -> 1.11 -> 1.100 with the real change
# detection and checks, at every step, which revisions count as prior, which
# one is the predecessor, and where each taxon's artifacts come from. Then the
# preflight: a new number must come after the latest one.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("digest", "jsonlite", "readr")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_version_order] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_run_context.R"); source("R/01e_change_detection.R")
  source("R/00_preflight.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

cat("[test_version_order]\n")

root <- file.path(tempdir(), paste0("vorder_", as.integer(runif(1) * 1e8)))
recs <- function(sp, n) data.frame(
  species = sp, record_id = paste0(gsub(" ", "", sp), "_", seq_len(n)),
  longitude = 22 + seq_len(n) / 100, latitude = 46 + seq_len(n) / 100,
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
  jsonlite::write_json(list(framework_version = ctx$framework_version,
                            prior_version = if (length(ctx$prior_versions)) ctx$prior_versions[1] else NA,
                            species = entries),
                       file.path(ctx$current_scaffolding_dir, "manifest.json"),
                       auto_unbox = TRUE, na = "null")
}
run <- function(v, data) {
  ctx <- RunContext_init(list(framework_version = v, root_output_dir = root),
                         run_id = paste0("t", v))
  ctx$all_species <- sort(unique(data$species))
  ctx <- suppressWarnings(suppressMessages(detect_species_changes(ctx, clean_data = data)))
  write_manifest(ctx)
  ctx
}
o <- function(ctx, sp) ctx$species_outcomes[[sp]]

# A changes at every revision; B never changes; C changes only at 1.10.
A <- "Astacus a"; B <- "Bstacus b"; C <- "Cstacus c"
data_at <- function(nA, nC) rbind(recs(A, nA), recs(B, 5), recs(C, nC))

c12  <- run("1.2",   data_at(5, 5))
c19  <- run("1.9",   data_at(6, 5))
c110 <- run("1.10",  data_at(7, 6))
c111 <- run("1.11",  data_at(8, 6))
c1100 <- run("1.100", data_at(9, 6))

# ---- ordering ----
ok(identical(list_prior_versions(root, "1.101"), c("1.100", "1.11", "1.10", "1.9", "1.2")),
   "prior revisions of 1.101, newest first: 1.100, 1.11, 1.10, 1.9, 1.2")
ok(identical(list_prior_versions(root)[1], "1.100"), "1.100 is the latest revision, not 1.9")
ok(identical(list_prior_versions(root, "1.10"), c("1.9", "1.2")),
   "only EARLIER revisions are prior: 1.11 and 1.100 never precede 1.10")

# ---- each revision's predecessor ----
ok(identical(c19$prior_versions[1],   "1.2"),  "1.9 follows 1.2")
ok(identical(c110$prior_versions[1],  "1.9"),  "1.10 follows 1.9 (not 1.2, as a text sort would say)")
ok(identical(c111$prior_versions[1],  "1.10"), "1.11 follows 1.10")
ok(identical(c1100$prior_versions[1], "1.11"), "1.100 follows 1.11 (not 1.10 or 1.9)")
m <- jsonlite::read_json(file.path(root, "1.100", "checkover", "manifest.json"))
ok(identical(m$prior_version, "1.11") && identical(m$framework_version, "1.100"),
   "the 1.100 manifest names 1.11 as prior_version; versions stay strings")

# ---- where each taxon's artifacts come from ----
ok(identical(o(c110, A)$outcome, "reprocessed") && identical(o(c110, A)$prior_source_version, "1.9"),
   "A changed at 1.10: reprocessed, previous source 1.9")
ok(identical(o(c1100, A)$prior_source_version, "1.11"),
   "A changed at 1.100: previous source 1.11")
ok(all(vapply(list(c19, c110, c111, c1100), \(x) identical(o(x, B)$source_version, "1.2"), logical(1))),
   "B never changed: served from 1.2 at 1.9, 1.10, 1.11 and 1.100")
ok(identical(o(c110, C)$outcome, "reprocessed") &&
   identical(o(c111, C)$source_version, "1.10") && identical(o(c1100, C)$source_version, "1.10"),
   "C changed only at 1.10: served from 1.10 at 1.11 and at 1.100")

# ---- the preflight: a new number comes after the latest ----
chk <- function(v) check_version_number(list(framework_version = v, root_output_dir = root))
sev <- function(r) if (nrow(r)) paste(sort(unique(r$severity)), collapse = ",") else "none"
ok(sev(chk("1.101")) == "none", "1.101 after 1.100: accepted")
ok(sev(chk("1.99")) == "FATAL",  "1.99 is before 1.100: refused (it would inherit from the wrong place)")
ok(sev(chk("1.11")) == "FATAL",  "1.11 again, below the latest 1.100: refused")
ok(sev(chk("1.102")) == "WARNING", "1.102 skips 1.101: a warning")
ok(sev(chk("2.0")) == "none", "2.0 after 1.100: accepted")
ok(sev(chk(1.1)) == "FATAL" && sev(chk("1.10.1")) == "FATAL",
   "a numeric or malformed framework_version is refused")

unlink(root, recursive = TRUE)
cat(sprintf("\n[test_version_order] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
