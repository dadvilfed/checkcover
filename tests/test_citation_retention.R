#!/usr/bin/env Rscript
# De-duplication must keep EVERY source citation, not just the survivor's
# (Lucian, 2026-09).
#
# Records are consolidated on species + coordinates + year. That key is correct:
# two records of one occurrence at the same place and time are duplicates
# however many publications reported them, and adding source to the key would
# inflate every record count in the paper.
#
# The defect was that only the surviving row's citation reached the
# bibliography, so a publication that independently reported an occurrence
# vanished from the provenance record. Reviewer 1 read this as a wording
# mismatch against manuscript lines 151-152; it is a real loss of provenance in
# a workflow whose central claim is provenance.
#
# Every distinct citation is now carried onto the survivor in citation_all, and
# Module 7 expands those back out so each publication is counted.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("dplyr", "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_citation_retention] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  library(dplyr); library(stringr)
  source("config.R")   # CHECKOVER_REFERENCE, for the CITATION.cff Module 7 writes
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/07_citations.R")
  source("R/00_geo_canon.R"); source("R/00_dwc_fields.R"); source("R/01_ingest.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
quiet <- function(e) { t <- tempfile(); sink(t); on.exit({sink(); unlink(t)}, add = TRUE); force(e) }

cat("[test_citation_retention]\n")

# The real consolidation step of 01_ingest.R.
consolidate <- function(d) consolidate_duplicates(d)

# One occurrence, reported by three publications; plus a separate occurrence.
raw <- data.frame(
  species   = "Astacus astacus",
  record_id = paste0("R", 1:4),
  longitude = c(22.0, 22.0, 22.0, 23.5),
  latitude  = c(46.0, 46.0, 46.0, 47.1),
  year      = c(2001, 2001, 2001, 2005),
  citation  = c("Smith 1999", "Jones 2003", "Brown 2010", "Solo 2020"),
  doi = NA_character_, url = NA_character_,
  population_type = "indigenous", stringsAsFactors = FALSE)

con <- consolidate(raw)

ok(nrow(con) == 2, "four rows consolidate to two records (counts do not inflate)")
ok(con$n_sources[1] == 3, "the consolidated record records 3 sources")
ok(grepl("Smith 1999", con$citation_all[1]) &&
   grepl("Jones 2003", con$citation_all[1]) &&
   grepl("Brown 2010", con$citation_all[1]),
   "all three citations are retained, not just the survivor's")
ok(con$n_sources[2] == 1, "an unconsolidated record has one source")

# Duplicate citations within a group collapse.
dup <- raw; dup$citation <- c("Smith 1999", "Smith 1999", "Jones 2003", "Solo 2020")
ok(consolidate(dup)$n_sources[1] == 2, "identical citations are not double-counted")

# ---- the export's row order must not matter ----
# The survivor's record_id and citation enter the fingerprint, so a survivor
# that depended on row order would make a re-export of identical data look
# changed, and the run would disagree with a freshness card computed on the data.
shuffled <- raw[c(3, 1, 4, 2), ]
ok(identical(consolidate(shuffled), con),
   "the same records in another row order consolidate identically (survivor and citation_all)")
ok(identical(con$record_id[1], "R1"),
   "the survivor is the record with the smallest record_id, whatever the input order")

variants <- raw; variants$species[2] <- "Astacus   astacus "
ok(nrow(consolidate(variants)) == 2 && consolidate(variants)$n_sources[1] == 3,
   "names are normalised before de-duplication: spacing variants meet in one group")

# ---- Module 7 must expand them back out ----
out <- file.path(tempdir(), paste0("cit_", as.integer(runif(1) * 1e8)))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

res <- try(quiet(generate_all_citations(
  scenario_table = data.frame(species = "Astacus astacus", scenario = 1L,
                              stringsAsFactors = FALSE),
  result_indigenous = list(clean_data = con),
  result_non_indigenous = list(clean_data = con[0, , drop = FALSE]),
  output_dir = out)), silent = TRUE)

ok(!inherits(res, "try-error"), "Module 7 runs on consolidated records")

jf <- list.files(file.path(out, "citations"), pattern = "Astacus.*\\.json$",
                 full.names = TRUE, recursive = TRUE)
jf <- jf[!grepl("summary|_runs", jf)]
if (length(jf)) {
  j <- jsonlite::fromJSON(jf[1], simplifyVector = FALSE)
  refs <- j$references %||% list()
  txt  <- paste(vapply(refs, function(r) r$full_reference_APA %||% "", character(1)),
                collapse = " ;; ")
  ok(length(refs) >= 4,
     sprintf("all four publications reach the bibliography (got %d)", length(refs)))
  for (nm in c("Smith 1999", "Jones 2003", "Brown 2010", "Solo 2020")) {
    ok(grepl(nm, txt, fixed = TRUE), sprintf("bibliography contains '%s'", nm))
  }
} else {
  ok(FALSE, "citation JSON was written")
}

# Without expansion only one of the three would have survived -- the defect.
# (sources_all carries the same sources, each with its own DOI and URL.)
no_all <- con; no_all$citation_all <- NULL; no_all$sources_all <- NULL
out2 <- file.path(tempdir(), paste0("cit2_", as.integer(runif(1) * 1e8)))
dir.create(out2, recursive = TRUE, showWarnings = FALSE)
quiet(try(generate_all_citations(
  scenario_table = data.frame(species = "Astacus astacus", scenario = 1L,
                              stringsAsFactors = FALSE),
  result_indigenous = list(clean_data = no_all),
  result_non_indigenous = list(clean_data = no_all[0, , drop = FALSE]),
  output_dir = out2), silent = TRUE))
jf2 <- list.files(file.path(out2, "citations"), pattern = "Astacus.*\\.json$",
                  full.names = TRUE, recursive = TRUE)
jf2 <- jf2[!grepl("summary|_runs", jf2)]
if (length(jf2)) {
  j2 <- jsonlite::fromJSON(jf2[1], simplifyVector = FALSE)
  ok(length(j2$references %||% list()) < 4,
     "without citation_all the bibliography loses sources (the defect)")
}

# ---- the geo-narrative states the bibliography's own figures ----
# Section 4.4 counted the records' distinct DOI strings plus their distinct
# citation strings (7 + 8 = 15 for A. bihariensis, whose bibliography holds 8
# references). It now reads the package's bibliography, so the two cannot
# disagree (Lucian, 2026-10).
suppressWarnings(suppressMessages(source("R/10_canonical_narratives.R")))
cit_obj <- res$citations[["Astacus astacus"]]
sec4 <- .build_section4("Astacus astacus", con, NULL, list(metrics = list(n_records = 2L)), NULL,
                        "1.1", "v1.1", "2026-10-23", delta = NULL, citations = cit_obj)
bj <- jsonlite::fromJSON(cit_obj$bibliography_json, simplifyVector = FALSE)
ok(grepl(sprintf("**Total references:** %d\n", bj$totals$total_references), sec4, fixed = TRUE),
   "4.4 Total references = the bibliography's totals.total_references")
ok(grepl("**DOI-linked references:** none", sec4, fixed = TRUE),
   "4.4 lists the bibliography's DOIs (none here)")
# The audit accepts that section against its bibliography, also when no
# reference has a DOI (it called "none" vs no DOI a mismatch on the
# runtime-1.1 demo trial, Pontastacus cubanicus).
suppressWarnings(suppressMessages(source("tests/audit_packages.R")))
pk <- file.path(out, "pkg", "Astacus_astacus")
dir.create(file.path(pk, "narratives"), recursive = TRUE); dir.create(file.path(pk, "citations"))
writeLines(sec4, file.path(pk, "narratives", "Astacus_astacus_canonical.md"), useBytes = TRUE)
file.copy(cit_obj$bibliography_json, file.path(pk, "citations"))
jsonlite::write_json(list(snapshot = list(version = "1.1"),
                          metrics = list(indigenous = list(records = 2L), non_indigenous = list(records = 0L))),
                     file.path(pk, "package_metadata.json"), auto_unbox = TRUE)
aud <- audit_narrative_metadata_consistency(file.path(out, "pkg"))
ok(aud$n_mismatched == 0, sprintf("the audit passes 4.4 against a DOI-less bibliography (%s)",
                                  paste(unlist(aud$mismatches), collapse = "; ")))
ok(grepl("**Total references:** 0\n",
         .build_section4("X", NULL, NULL, NULL, NULL, "1.1", "v1.1", "2026-10-23"), fixed = TRUE),
   "no bibliography -> 0 references")

unlink(c(out, out2), recursive = TRUE)
cat(sprintf("\n[test_citation_retention] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
