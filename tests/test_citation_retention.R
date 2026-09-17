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
  library(dplyr)
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/07_citations.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
quiet <- function(e) { t <- tempfile(); sink(t); on.exit({sink(); unlink(t)}, add = TRUE); force(e) }

cat("[test_citation_retention]\n")

# The consolidation step, as 01_ingest.R performs it.
consolidate <- function(d) {
  key <- paste(d$species, d$longitude, d$latitude, d$year, sep = "\r")
  ju <- function(x) { v <- trimws(as.character(x)); v <- v[!is.na(v) & nzchar(v)]
                      if (!length(v)) NA_character_ else paste(unique(v), collapse = " | ") }
  d$citation_all <- unname(vapply(split(d$citation, key), ju, character(1))[key])
  d$n_sources <- lengths(strsplit(ifelse(is.na(d$citation_all), "", d$citation_all),
                                  "\\s*\\|\\s*"))
  dplyr::distinct(d, species, longitude, latitude, year, .keep_all = TRUE)
}

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
no_all <- con; no_all$citation_all <- NULL
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

# ---- the geo-narrative's bibliography must see them too ----
# Module 7 was updated to expand citation_all; the narrative's bibliography
# section was not, and kept reading the survivor-only `citation` column. The two
# representations in one package then disagreed on how many sources exist.
src <- readLines("R/10_canonical_narratives.R", warn = FALSE)
i1 <- grep("^\\.narrative_sources <- function", src)[1]
i2 <- grep("^\\.freq_table_multi <- function", src)[1]
i2 <- max(grep("^#'", src[seq_len(i2 - 1)])[1], i1 + 1)
blk_end <- which(src == "}" & seq_along(src) > i1)[1]
eval(parse(text = paste(src[i1:blk_end], collapse = "\n")))

cites <- .narrative_sources(con, "citation")
ok(setequal(unique(cites), c("Smith 1999", "Jones 2003", "Brown 2010", "Solo 2020")),
   "narrative bibliography sees all four sources, not just survivors")
ok(length(unique(.narrative_sources(no_all, "citation"))) == 2,
   "without citation_all it falls back to the survivor column (pre-change data)")
ok(length(.narrative_sources(NULL, "citation")) == 0, "NULL branch -> no sources")

unlink(c(out, out2), recursive = TRUE)
cat(sprintf("\n[test_citation_retention] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
