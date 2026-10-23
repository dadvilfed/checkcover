#!/usr/bin/env Rscript
# The bibliography takes each reference's DOI from the records that cite it,
# and counts one reference per work (Lucian, SEB review, 2026-10).
#
# 1. Wrong DOI. A. bihariensis REF_005 (Satmari et al. 2023, NeoBiota) carried
#    10.1016/j.jcz.2019.01.006, the DOI of REF_002 (Pârvulescu 2019), in 1.0
#    and 1.1. Its records carry 10.3897/neobiota.89.110085. De-duplication
#    kept the group's citations and DOIs in two separate lists, and Module 7
#    gave every expanded citation the surviving record's DOI.
# 2. One work, several references. Keyed on the citation text, one paper in
#    several citation styles became several REF ids: 219 DOIs under two or
#    more ids across 1.0, five for A. astacus' 10.2478/v10191-010-0007-6.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("dplyr", "jsonlite", "stringr", "digest", "glue")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_bibliography_doi] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  library(dplyr); library(stringr)
  source("config.R")
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/07_citations.R")
  source("R/00_geo_canon.R"); source("R/00_dwc_fields.R"); source("R/01_ingest.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
quiet <- function(e) { t <- tempfile(); sink(t); on.exit({sink(); unlink(t)}, add = TRUE); force(e) }

cat("[test_bibliography_doi]\n")

PARV <- "Pârvulescu, L. (2019). Introducing a new Austropotamobius crayfish species. Zoologischer Anzeiger, 279, 94-102."
SATM <- "Satmari, A., Miok, K., Ion, M. C., Zaharia, C., Schrimpf, A., & Pârvulescu, L. (2023). Headwater refuges. NeoBiota, 89, 71-94."
SATM2 <- "Satmari A, Miok K, Ion MC, Zaharia C, Schrimpf A, Pârvulescu L (2023) Headwater refuges. NeoBiota 89: 71-94"

bib_of <- function(clean, sp = "Austropotamobius bihariensis") {
  out <- file.path(tempdir(), paste0("bib_", as.integer(runif(1) * 1e8)))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  res <- quiet(generate_all_citations(
    scenario_table = data.frame(species = sp, scenario = 1L, stringsAsFactors = FALSE),
    result_indigenous = list(clean_data = clean),
    result_non_indigenous = list(clean_data = clean[0, , drop = FALSE]),
    output_dir = out))
  j <- jsonlite::fromJSON(res$citations[[sp]]$bibliography_json, simplifyVector = FALSE)
  unlink(out, recursive = TRUE)
  j
}
ref_with <- function(j, pat) Filter(function(r) grepl(pat, r$full_reference_APA, fixed = TRUE), j$references)

# ---- 1. the bihariensis case: one occurrence reported by both papers ----
raw <- data.frame(
  species   = "Austropotamobius bihariensis",
  record_id = paste0("W", 1:4),
  longitude = c(22.5, 22.5, 22.7, 22.8), latitude = c(46.6, 46.6, 46.5, 46.4),
  year      = c(2019, 2019, 2021, 2021),
  citation  = c(PARV, SATM, SATM, PARV),
  doi       = c("10.1016/j.jcz.2019.01.006", "10.3897/neobiota.89.110085",
                "10.3897/neobiota.89.110085", "10.1016/j.jcz.2019.01.006"),
  url = NA_character_, is_extinct = FALSE, stringsAsFactors = FALSE)
con <- consolidate_duplicates(raw)
ok(nrow(con) == 3, "W1 and W2 (same place and year) consolidate to one record")
ok("sources_all" %in% names(con), "consolidation keeps each source's citation, DOI and URL together")
src1 <- decode_sources(con$sources_all[con$record_id == "W1"])
ok(nrow(src1) == 2 && identical(src1$doi[src1$citation == SATM], "10.3897/neobiota.89.110085"),
   "the consolidated record keeps Satmari's own DOI beside Satmari's citation")

j <- bib_of(con)
sat <- ref_with(j, "Satmari")
ok(length(sat) == 1, "Satmari et al. 2023 is one reference")
ok(length(sat) == 1 && identical(sat[[1]]$doi, "10.3897/neobiota.89.110085"),
   "Satmari's reference carries its own DOI, not Pârvulescu 2019's")
ok(length(sat) == 1 && !grepl("jcz.2019", sat[[1]]$full_reference_APA, fixed = TRUE),
   "Satmari's APA line does not show the other paper's DOI")
ok(length(sat) == 1 && sat[[1]]$count == 2, "Satmari counted on both records that cite it")
ok(j$totals$total_references == 2, "two works, two references")

# ---- 2. one work in two citation styles is one reference ----
raw2 <- raw
raw2$citation[3] <- SATM2                       # W3 cites Satmari in another style
raw2$is_extinct[3] <- TRUE
j2 <- bib_of(consolidate_duplicates(raw2))
sat2 <- Filter(function(r) identical(r$doi, "10.3897/neobiota.89.110085"), j2$references)
ok(length(sat2) == 1, "two citation styles of one DOI -> one reference")
ok(length(sat2) == 1 && sat2[[1]]$count == 2, "their counts add up")
ok(length(sat2) == 1 && setequal(unlist(sat2[[1]]$source_types), c("baseline", "extinction_evidence")),
   "their source types are united")
ok(j2$totals$total_references == 2, "the bibliography total counts works, not styles")

# ---- 3. a record that omits the DOI does not split the work ----
raw3 <- raw[3:4, ]; raw3$record_id <- c("W3", "W4")
raw3 <- rbind(raw3, transform(raw3[1, ], record_id = "W5", longitude = 23.1, doi = NA_character_))
j3 <- bib_of(consolidate_duplicates(raw3))
ok(length(ref_with(j3, "Satmari")) == 1 && ref_with(j3, "Satmari")[[1]]$count == 2,
   "Satmari's records with and without the DOI make one reference")

# ---- 4. DOI normalisation ----
nd <- normalize_doi(c("https://doi.org/10.1002/ECE3.4888", "doi:10.1002/ece3.4888",
                      "DOI: 10.1002/ece3.4888.", "http://dx.doi.org/10.1002/ece3.4888",
                      "10.1899/0887-3593(2005)024[0386:ROTMEO]2.0.CO;2", "not a doi", "", NA))
ok(all(nd[1:4] == "10.1002/ece3.4888"), "case, resolver prefix and trailing punctuation are normalised away")
ok(identical(nd[5], "10.1899/0887-3593(2005)024[0386:rotmeo]2.0.co;2"), "a SICI DOI is kept whole")
ok(all(is.na(nd[6:8])), "anything that is not a DOI is NA")

# ---- 5. records from before sources_all: no DOI is better than another work's ----
legacy <- con; legacy$sources_all <- NULL
jl <- bib_of(legacy)
satl <- ref_with(jl, "Satmari")
ok(length(satl) == 1 && !identical(satl[[1]]$doi, "10.1016/j.jcz.2019.01.006"),
   "without sources_all, an expanded citation never takes the survivor's DOI")

# ---- 6. a work known only by its DOI is listed by it, not as 'NA DOI: ...' ----
raw6 <- raw[1, ]; raw6$citation <- NA_character_
j6 <- bib_of(consolidate_duplicates(raw6))
ok(length(j6$references) == 1 && j6$references[[1]]$count == 1 &&
   identical(j6$references[[1]]$full_reference_APA, "DOI: https://doi.org/10.1016/j.jcz.2019.01.006"),
   "a DOI-only source gets a count and no 'NA' in its reference")

cat(sprintf("\n[test_bibliography_doi] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
