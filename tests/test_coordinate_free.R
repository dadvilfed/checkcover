#!/usr/bin/env Rscript
# Nothing with coordinates may be written under <rev>/ (UVT server setup,
# section 05; Lucian, 2026-09).
#
# Every delivery up to 1.2 carried <rev>/checkover/clean_occurrences.tsv: the
# cleaned input with exact coordinates for ~120k records, ~66k of them
# confidentiality level 1 or 2, served from the World of Crayfish web server.
# Pinned here:
#   * the audit's coordinate check catches every way coordinates can appear,
#     and does NOT false-positive on reference lists (DOIs look like number pairs)
#   * records_used.tsv, which replaces the table, is exactly three columns
#   * the pipeline no longer copies the table into <rev>/checkover/

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  source("tests/audit_packages.R")          # CLI guarded; defines the checks
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/09_package_export.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

cat("[test_coordinate_free]\n")

rev <- file.path(tempdir(), paste0("rev_", as.integer(runif(1) * 1e8)))
w <- function(rel, txt) {
  p <- file.path(rev, rel); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, p); p
}
flagged <- function() names(audit_coordinate_free(rev)$flags)

# ---- clean revision: must pass ----
w("Sp_one/package_metadata.json", '{"metrics":{"AOO_km2":180,"EOO_km2":3389},"threshold_km":10}')
w("Sp_one/narratives/Sp_one_canonical.md",
  c("- **DOI-linked references:** 10.1002/ECE3.4817; 10.1016/j.gecco.2024.e02847",
    "AOO of 180 km2 and an EOO of 3,389 km2; clustering at 10.0 km."))
w("Sp_one/citations/Sp_one_bibliography.bib", "@misc{a, doi = {10.1111/fwb.12345}, year = {2020}}")
w("Sp_one/maps/Sp_one_basins.geojson",
  '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[22.1234567,46.1234567],[22.2234567,46.1234567],[22.2234567,46.2234567],[22.1234567,46.1234567]]]}}]}')
w("checkover/manifest.json", '{"species":{"Sp_one":{"outcome":"new","fingerprint":"ab12cd"}}}')
w("checkover/records_used.tsv", c("record_id\tspecies\tstate", "W1\tSp one\tactive"))
ok(length(flagged()) == 0, "a clean revision passes (DOIs, polygons, metrics not flagged)")

# ---- every leak shape must fail ----
w("checkover/clean_occurrences.tsv", c("record_id\tlongitude\tlatitude", "W1\t22.123456\t46.123456"))
ok("checkover/clean_occurrences.tsv" %in% flagged(), "a table with coordinate columns is flagged")
unlink(file.path(rev, "checkover/clean_occurrences.tsv"))

w("Sp_one/extra.json", '{"type_locality":{"decimalLatitude":46.12,"decimalLongitude":22.34}}')
ok("Sp_one/extra.json" %in% flagged(), "a JSON coordinate key is flagged, even with few decimals")
unlink(file.path(rev, "Sp_one/extra.json"))

w("Sp_one/narratives/leak.txt", "Type locality recorded at 46.123456, 22.567890 near the spring.")
ok("Sp_one/narratives/leak.txt" %in% flagged(), "a decimal coordinate pair in prose is flagged")
unlink(file.path(rev, "Sp_one/narratives/leak.txt"))

w("Sp_one/maps/Sp_one_points.geojson",
  '{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Point","coordinates":[22.1,46.1]}}]}')
ok("Sp_one/maps/Sp_one_points.geojson" %in% flagged(), "a POINT layer under maps/ is flagged")
unlink(file.path(rev, "Sp_one/maps/Sp_one_points.geojson"))

saveRDS(data.frame(a = 1), file.path(rev, "checkover", "snapshot.rds"))
ok("checkover/snapshot.rds" %in% flagged(), "an R binary is refused outright")
unlink(file.path(rev, "checkover", "snapshot.rds"))

ok(length(flagged()) == 0, "after removing each leak the revision passes again")

# ---- the pair detector itself ----
ok(.has_coord_pair("at -33.865143, 151.209900 (Sydney)"), "southern/eastern pair detected")
ok(!.has_coord_pair("10.1002/ECE3.4817; 10.1016/j.x.2020.1802405"), "DOI list is not a coordinate pair")
ok(!.has_coord_pair("ECE3.4817 10.1016"), "numbers glued to letters are not a pair")
ok(!.has_coord_pair("200.1234, 95.1234"), "out-of-range combination is not a pair")
ok(.has_coord_pair("180.0000, 12.1234"), "longitude 180 is legal, so that one IS a pair")

# ---- records_used.tsv: exactly three columns, never coordinates ----
ctx <- list(current_scaffolding_dir = file.path(rev, "checkover2"))
branch <- list(clean_data = data.frame(
  record_id = c("W2", "W1"), species = c("Sp one", "Sp one"),
  longitude = c(22.1, 22.2), latitude = c(46.1, 46.2),
  confidentiality_level = c(1, 2), temporal_status = c("active", "extinct"),
  stringsAsFactors = FALSE))
suppressMessages(write_records_used(ctx, list(branch, list(clean_data = NULL))))
ru <- read.delim(file.path(ctx$current_scaffolding_dir, "records_used.tsv"), sep = "\t")
ok(identical(names(ru), c("record_id", "species", "state")), "records_used.tsv has exactly record_id, species, state")
ok(identical(ru$record_id, c("W1", "W2")) && identical(ru$state, c("extinct", "active")),
   "rows sorted by species then id, state carried from temporal_status")
ok(length(names(audit_coordinate_free(file.path(rev, "checkover2"))$flags)) == 0,
   "records_used.tsv passes the coordinate check")

# ---- the pipeline must not copy the table into <rev>/checkover/ ----
main <- paste(readLines("checkcover_main.R", warn = FALSE), collapse = "\n")
ok(!grepl('file.path(ctx$current_scaffolding_dir, "clean_occurrences.tsv")', main, fixed = TRUE),
   "checkcover_main.R no longer writes clean_occurrences.tsv under <rev>/")
f01e <- paste(readLines("R/01e_change_detection.R", warn = FALSE), collapse = "\n")
ok(grepl('file.path(ctx$work_dir, "clean_occurrences.tsv")', f01e, fixed = TRUE),
   "change detection reads the table from the work directory")
aud <- paste(readLines("tests/audit_packages.R", warn = FALSE), collapse = "\n")
ok(grepl('file.path(version_dir, "checkover")', aud, fixed = TRUE),
   "the audit report is written into <rev>/checkover/")

# ---- the verdict, end to end through the real CLI ----
# `passed` is what a platform checks before installing (SERVICE_CONTRACT.md).
# It must agree with the exit code and with the three per-check verdicts, and a
# coordinate leak must turn it false.
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
run_audit <- function() {
  code <- suppressWarnings(system2(rscript, c("--vanilla", "tests/audit_packages.R", shQuote(rev)),
                                   stdout = FALSE, stderr = FALSE))
  list(code = as.integer(code),
       rep  = jsonlite::read_json(file.path(rev, "checkover", "_audit_report.json"),
                                  simplifyVector = TRUE))
}
agree <- function(a) {
  checks <- c(a$rep$checks$integrity$passed, a$rep$checks$consistency$passed,
              a$rep$checks$coordinate_free$passed)
  is.logical(a$rep$passed) && identical(a$rep$passed, all(checks)) &&
    identical(a$code, if (isTRUE(a$rep$passed)) 0L else 1L) &&
    identical(a$rep$result, if (isTRUE(a$rep$passed)) "PASS" else "FAIL")
}
a1 <- run_audit()
ok(agree(a1) && isTRUE(a1$rep$checks$coordinate_free$passed),
   "report: passed / result / exit code agree; the coordinate check passes a clean revision")
invisible(w("checkover/clean_occurrences.tsv", c("record_id\tlongitude\tlatitude", "W1\t22.123456\t46.123456")))
a2 <- run_audit()
ok(agree(a2) && identical(a2$rep$passed, FALSE) && identical(a2$code, 1L) &&
   identical(a2$rep$checks$coordinate_free$passed, FALSE),
   "report: a coordinate leak sets passed = false and exits 1 (the runner must not upload)")
ok(!grepl("22.123456", paste(readLines(file.path(rev, "checkover", "_audit_report.json")), collapse = "")),
   "report: names the leaking file, never the coordinates it found")
unlink(file.path(rev, "checkover/clean_occurrences.tsv"))

unlink(rev, recursive = TRUE)
cat(sprintf("\n[test_coordinate_free] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
