#!/usr/bin/env Rscript
# How to cite cheCkOVER (Lucian, 2026-09): every species package cites it with
# exactly the agreed reference, as preferred-citation in CITATION.cff and as
# preferred_citation in package_metadata.json, so packages and the World of
# Crayfish site say the same thing. It lives in ONE place, CHECKOVER_REFERENCE
# in config.R; this test pins the text and checks every output uses it.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("dplyr", "jsonlite", "glue")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_citation] SKIP (%s not installed)\n", p)); quit(status = 0)
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

cat("[test_citation]\n")

ref <- CHECKOVER_REFERENCE

# The reference exactly as agreed with Lucian (2026-09-22), code point for code
# point: S-comma U+0218, a-breve U+0103, t-comma U+021B, registered sign U+00AE.
agreed <- paste0(
  "Livadariu D, Bâcu VI, Nandra CI, Ștefănuț TT, Sabou A, ",
  "WoC® Contributors, Crandall KA, Pârvulescu L: cheCkOVER: ",
  "Assessment-support workflow for biogeographic metrics from species ",
  "occurrence data. https://doi.org/10.64898/2025.12.29.696807")
ok(identical(ref$text, agreed), "the reference text is exactly the agreed one")
ok(identical(checkover_reference_from_parts(), ref$text),
   "the structured fields rebuild the text exactly (the CFF cannot drift from it)")
cp <- utf8ToInt(enc2utf8(ref$text))
ok(all(c(0x0218, 0x0103, 0x021B, 0x00AE, 0x00E2) %in% cp) && !any(c(0x015E, 0x0163) %in% cp),
   "Romanian letters use comma-below (U+0218, U+021B), not cedilla")

# ---- the CFF block ----
cff <- checkover_reference_cff()
ok(cff[1] == "preferred-citation:" &&
   any(cff == '  doi: "10.64898/2025.12.29.696807"') &&
   sum(grepl("^    - family-names: ", cff)) == 7L &&
   any(cff == '    - name: "WoC® Contributors"'),
   "preferred-citation block: DOI, seven named authors and the WoC contributors")

# ---- package_metadata.json's field ----
md <- checkover_reference_metadata()
ok(identical(md$text, ref$text) && identical(md$doi, ref$doi) &&
   identical(md$url, "https://doi.org/10.64898/2025.12.29.696807") && length(md$authors) == 8L,
   "preferred_citation for package_metadata.json: the same text, DOI and URL")

# ---- every output uses the one definition ----
src <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok(grepl("preferred_citation = checkover_reference_metadata()", src("R/09_package_export.R"), fixed = TRUE),
   "09_package_export.R writes preferred_citation into package_metadata.json")
ok(grepl("CHECKOVER_REFERENCE$text", src("R/09_package_export.R"), fixed = TRUE),
   "the package README cites the same reference")
ok(grepl("checkover_reference_cff()", src("R/07_citations.R"), fixed = TRUE),
   "07_citations.R appends the preferred-citation block to every CITATION.cff")

# ---- end to end: the CFF Module 7 actually writes ----
raw <- data.frame(species = "Astacus astacus", record_id = c("R1", "R2"),
                  longitude = c(22, 23), latitude = c(46, 47), year = c(2001, 2005),
                  citation = c("Smith 1999", "Jones 2003"), doi = NA_character_, url = NA_character_,
                  population_type = "indigenous", stringsAsFactors = FALSE)
con <- consolidate_duplicates(raw)
out <- file.path(tempdir(), paste0("cff_", as.integer(runif(1) * 1e8)))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
res <- try(quiet(generate_all_citations(
  scenario_table = data.frame(species = "Astacus astacus", scenario = 1L, stringsAsFactors = FALSE),
  result_indigenous = list(clean_data = con),
  result_non_indigenous = list(clean_data = con[0, , drop = FALSE]),
  output_dir = out)), silent = TRUE)
cf <- list.files(out, pattern = "_CITATION\\.cff$", recursive = TRUE, full.names = TRUE)
txt <- if (length(cf)) readLines(cf[1], warn = FALSE, encoding = "UTF-8") else character(0)
ok(!inherits(res, "try-error") && any(txt == "preferred-citation:") &&
   any(grepl("10.64898/2025.12.29.696807", txt, fixed = TRUE)) &&
   any(txt == enc2utf8('    - family-names: "Ștefănuț"')),
   "a CITATION.cff written by Module 7 carries the preferred-citation, in UTF-8")
unlink(out, recursive = TRUE)

# ---- the repository README says the same (not in the container image) ----
if (file.exists("README.md")) {
  rd <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  i <- grep("^## Citation", rd)
  block <- rd[seq(i + 1L, length(rd))]
  q <- sub("^> ?", "", block[cumsum(!grepl("^>", block)) == min(cumsum(!grepl("^>", block))[grepl("^>", block)])])
  q <- gsub("[<>]", "", paste(q[nzchar(q)], collapse = " "))
  ok(identical(trimws(gsub("\\s+", " ", q)), ref$text),
     "the repository README cites exactly the same reference")
}

cat(sprintf("\n[test_citation] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
