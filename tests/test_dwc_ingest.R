#!/usr/bin/env Rscript
# Darwin Core ingest (Lucian email 4): the new DwC-aligned template header must
# map to the internal columns, keep establishmentMeans / occurrenceOrigin
# separate, carry voucher fields, and DERIVE basisOfRecord. Exercises the REAL
# map_woc_to_checkover(). occurrenceID is intentionally absent (auto at upload).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  library(dplyr); library(stringr)
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_geo_canon.R")   # ingest validates native geography against the canon
  source("R/00_dwc_fields.R"); source("R/01_ingest.R")
}))

woc <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
  scientificName        = c("Astacus astacus","Astacus astacus","Cambarus veitchorum"),
  decimalLatitude       = c("46,5","47.1","36.0"),        # comma + period decimals
  decimalLongitude      = c("22,1","23.2","-81.0"),
  establishmentMeans    = c("indigenous","indigenous","indigenous"),
  occurrenceOrigin      = c("native","type locality","native"),
  year                  = c("1998","2003","1968"),
  claimExtinction       = c("","", "Extinct"),   # real WoC vocabulary (NOT "yes")
  accuracy              = c("high","low","high"),
  bibliographicCitation = c("10.1/x","",""),
  sourceCitation        = c("Ion 2024","",""),
  confidentialityLevel  = c("0","0","0"),
  contributor           = c("Jane Doe","Ana Pop","J. Cooper"),
  occurrenceRemarks     = c("note1","",""),
  catalogNumber         = c("USNM 12345", "",""),
  institutionCode       = c("USNM","",""))

out <- map_woc_to_checkover(woc)

pass <- all(c("species","latitude","longitude","population_status","occurrence_origin",
              "catalog_number","institution_code","basis_of_record") %in% names(out)) &&
  identical(out$species[1], "Astacus astacus") &&
  abs(out$latitude[1]  - 46.5) < 1e-9 &&      # comma-decimal parsed
  abs(out$longitude[1] - 22.1) < 1e-9 &&
  identical(out$population_status[1], "indigenous") &&
  identical(out$occurrence_origin[2], "type locality") &&
  isTRUE(out$is_type_locality[2]) &&
  identical(out$basis_of_record[1], "PreservedSpecimen") &&
  identical(out$basis_of_record[2], "HumanObservation") &&
  # CRITICAL: claimExtinction="Extinct" must set is_extinct (case-insensitive,
  # NOT the old "yes"-only / case-mismatched check). Row 3 is Extinct; 1-2 not.
  isTRUE(out$is_extinct[3]) && isFALSE(out$is_extinct[1]) && isFALSE(out$is_extinct[2])

cat("[test_dwc_ingest] basisOfRecord:", out$basis_of_record[1], "/", out$basis_of_record[2],
    "| lat1:", out$latitude[1], "| type_locality2:", out$is_type_locality[2],
    "| is_extinct(Extinct claim):", out$is_extinct[3], "\n")
cat(sprintf("[test_dwc_ingest] %s\n", if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
