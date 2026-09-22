#!/usr/bin/env Rscript
# Regression: Module 7 must survive a species whose published citations are ALL NA.
#
# table() drops NA by default, so an all-NA citation vector yields a 0-level
# table, and as.data.frame() of a 0-level table collapses to a SINGLE "Freq"
# column. Naming two columns then died with:
#   'names' attribute [2] must be the same length as the vector [1]
#
# Hit on Cherax cartalacoolah (2026-08). The data was not new — force_reprocess
# simply put the 501 previously-"unchanged" species through this module for the
# first time since their baseline.
#
# Also pins the two neighbouring shapes that must keep working unchanged: a
# mixed NA/non-NA species (13 of these in v1.1, they emit a count=NA row) and a
# normal species.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("jsonlite", "dplyr")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_citations_na] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  source("config.R")   # CHECKOVER_REFERENCE, for the CITATION.cff Module 7 writes
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/07_citations.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

cat("[test_citations_na]\n")

# ── The exact failing construction, isolated ─────────────────────────────────
# This is what the module does; it must be shape-stable for every input.
count_frame <- function(keys) {
  tab <- table(keys, useNA = "no")
  data.frame(dedup_key = if (length(tab)) names(tab)      else character(0),
             count     = if (length(tab)) as.integer(tab) else integer(0),
             stringsAsFactors = FALSE)
}

ok(identical(dim(count_frame(c("A", "B", "A"))), c(2L, 2L)), "normal keys -> 2x2 frame")
ok(identical(dim(count_frame("A")),               c(1L, 2L)), "single key -> 1x2 frame")
ok(identical(dim(count_frame(c("", ""))),         c(1L, 2L)), "empty-string key is a real level")
ok(identical(dim(count_frame(c(NA_character_, "A"))), c(1L, 2L)), "mixed NA -> NA dropped, 1 level")
ok(identical(dim(count_frame(c(NA_character_, NA_character_))), c(0L, 2L)),
   "ALL NA -> 0x2 frame, still two columns (was 0x1 and crashed)")
ok(identical(names(count_frame(rep(NA_character_, 3))), c("dedup_key", "count")),
   "ALL NA frame keeps both column names")

# The merge that consumes it must yield count = NA, not error.
pd <- data.frame(dedup_key = NA_character_, citation_clean = NA_character_,
                 stringsAsFactors = FALSE)
m  <- merge(pd, count_frame(rep(NA_character_, 2)), by = "dedup_key", all.x = TRUE)
ok(nrow(m) == 1L && is.na(m$count), "merge against an empty count frame yields count=NA")

# ── End-to-end through the real module entry point ───────────────────────────
mk <- function(species, citations, urls = NA_character_) data.frame(
  species = species, record_id = paste0("R", seq_along(citations)),
  citation = citations, doi = NA_character_, url = urls,
  latitude = 46, longitude = 22, year = 2014,
  population_type = "indigenous", stringsAsFactors = FALSE)

# The crash case, exactly as it occurs in the data: sourceCitation is empty but
# associatedReferences carries a URL, so the record SURVIVES the
# citation|doi|url filter at the top of the loop and reaches the count block
# with an all-NA citation vector. Without the URL the record is dropped earlier
# and the bug is never reached — which is what makes this fixture load-bearing.
clean <- rbind(
  mk("Cherax cartalacoolah", NA_character_,
     "https://www.ncbi.nlm.nih.gov/nuccore/KM039079"),             # the crash case
  mk("Mixed species",        c(NA_character_, "Smith 2001")),      # must keep working
  mk("Normal species",       c("Jones 1999", "Jones 1999"))        # must keep working
)
spp <- unique(clean$species)

out <- file.path(tempdir(), paste0("cit_", as.integer(runif(1) * 1e8)))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

res <- try(suppressWarnings(suppressMessages(
  generate_all_citations(
    scenario_table        = data.frame(species = spp, scenario = 1L,
                                       stringsAsFactors = FALSE),
    result_indigenous     = list(clean_data = clean),
    result_non_indigenous = list(clean_data = clean[0, , drop = FALSE]),
    output_dir            = out)
)), silent = TRUE)

ok(!inherits(res, "try-error"),
   sprintf("Module 7 completes on an all-NA-citation species%s",
           if (inherits(res, "try-error"))
             paste0(" -- ", trimws(conditionMessage(attr(res, "condition")))) else ""))

if (!inherits(res, "try-error")) {
  jf <- list.files(file.path(out, "citations"), pattern = "\\.json$",
                   full.names = TRUE, recursive = TRUE)
  ok(length(jf) > 0, "citation files were written")

  got <- basename(jf)
  ok(any(grepl("Normal_species", got)),  "normal species still produced output")
  ok(any(grepl("Mixed_species",  got)),  "mixed-NA species still produced output")

  nf <- jf[grepl("Normal_species", got)][1]
  if (!is.na(nf)) {
    j <- jsonlite::fromJSON(nf, simplifyVector = FALSE)
    refs <- j$references %||% j$refs %||% list()
    ok(length(refs) >= 1L, "normal species has >=1 reference")
    ok(identical(refs[[1]]$count, 2L) || identical(refs[[1]]$count, 2),
       "normal species reference count is still 2 (unchanged semantics)")
  }
}

unlink(out, recursive = TRUE)
cat(sprintf("\n[test_citations_na] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
