#!/usr/bin/env Rscript
# Unit tests for the pure functions touched by the 2026-06 cheCkOVER fixes:
# vernacular GBIF trailing-code inheritance, Darwin Core input/output mapping +
# export crosswalk, the single-source trend rule, and the zero-active helper.
# No sf/dplyr needed — safe everywhere.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  source("R/00_helpers.R")
  source("R/00_dwc_fields.R")
  source("R/10_canonical_narratives.R")   # vernacular parser + iso map (defs only)
  try(source("R/00_run_context.R"), silent = TRUE)  # compute_trend_vs_previous
}))

PASS <- 0L; FAIL <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  ok   %s\n", label)) }
  else              { FAIL <<- FAIL + 1L; cat(sprintf("  FAIL %s\n", label)) }
}
eqset <- function(a, b) setequal(a, b)

cat("== Vernacular GBIF trailing-code inheritance ==\n")
iso <- .iso639_language_map()
v1 <- .parse_vernacular_by_language("японский рак(rus) | ニホンザリガニ | ヤマトザリガニ (jpn)", iso)
ok("rus name bucketed as Russian",  identical(v1[["Russian"]], "японский рак"))
ok("untagged JP names inherit (jpn)", eqset(v1[["Japanese"]], c("ニホンザリガニ","ヤマトザリガニ")))
ok("no English bucket created",     is.null(v1[["English"]]))

v2 <- .parse_vernacular_by_language(
  "Signal crayfish(eng) | ウチダザリガニ | タンカイザリガニ | シグナルザリガニ (jpn) | écrevisse signal(fra)", iso)
ok("tagged English stays English", eqset(v2[["English"]], "Signal crayfish"))
ok("3 JP names all Japanese",      eqset(v2[["Japanese"]], c("ウチダザリガニ","タンカイザリガニ","シグナルザリガニ")))
ok("fra name is French",           eqset(v2[["French"]], "écrevisse signal"))

v3 <- .parse_vernacular_by_language("racul(ron) | noble crayfish", iso)
ok("trailing untagged -> English", eqset(v3[["English"]], "noble crayfish"))
ok("ron name is Romanian",         eqset(v3[["Romanian"]], "racul"))

cat("== DwC input header normalization ==\n")
hdr <- c("scientificName","decimalLatitude","decimalLongitude","establishmentMeans",
         "occurrenceOrigin","year","claimExtinction","accuracy","bibliographicCitation",
         "associatedReferences","sourceCitation","confidentialityLevel","contributor",
         "occurrenceRemarks","catalogNumber","institutionCode")
df <- as.data.frame(setNames(as.list(rep("x", length(hdr))), hdr), stringsAsFactors = FALSE)
nn <- names(dwc_normalize_input_headers(df))
ok("scientificName -> Crayfish_scientific_name", "Crayfish_scientific_name" %in% nn)
ok("decimalLatitude -> Lat",                     "Lat" %in% nn)
ok("decimalLongitude -> Long",                   "Long" %in% nn)
ok("establishmentMeans -> Population_status",    "Population_status" %in% nn)
ok("occurrenceOrigin -> Occurrence_origin",      "Occurrence_origin" %in% nn)
ok("claimExtinction -> Claim_extinction",        "Claim_extinction" %in% nn)
ok("catalogNumber preserved",                    "catalogNumber" %in% nn)
leg <- data.frame(Lat = 1, Long = 2, Crayfish_scientific_name = "x")
ok("legacy headers untouched", eqset(names(dwc_normalize_input_headers(leg)),
                                     c("Lat","Long","Crayfish_scientific_name")))
pfx <- data.frame(check.names = FALSE, `dwc:scientificName` = "x")
ok("dwc: prefix stripped+mapped", "Crayfish_scientific_name" %in% names(dwc_normalize_input_headers(pfx)))

cat("== DwC output rename ==\n")
outdf <- data.frame(species="Astacus astacus", year=1990, population_status="indigenous",
                    occurrence_origin="native", catalog_number=NA, stringsAsFactors=FALSE)
on <- names(dwc_rename_output_columns(outdf))
ok("species -> scientificName",              "scientificName" %in% on)
ok("population_status -> establishmentMeans", "establishmentMeans" %in% on)
ok("occurrence_origin -> occurrenceOrigin",   "occurrenceOrigin" %in% on)

cat("== DwC crosswalk / voucher ==\n")
ok("basisOfRecord PreservedSpecimen", dwc_derive_basis_of_record("USNM 12345") == "PreservedSpecimen")
ok("basisOfRecord HumanObservation",  dwc_derive_basis_of_record(NA) == "HumanObservation")
ok("basisOfRecord empty->HumanObs",   dwc_derive_basis_of_record("") == "HumanObservation")
ok("establishmentMeans indigenous->native",         dwc_establishment_means("indigenous") == "native")
ok("establishmentMeans non-indigenous->introduced", dwc_establishment_means("non-indigenous") == "introduced")
ok("degreeOfEstablishment invasive",       dwc_degree_of_establishment("invasive") == "invasive")
ok("degreeOfEstablishment cryptogenic->NA", is.na(dwc_degree_of_establishment("cryptogenic")))
ok("dynamicProperties json", grepl('"contributor":"Jane Doe"', dwc_dynamic_properties("Jane Doe")))

cat("== Basin display name (river-aware, finest level) ==\n")
ok("river-level -> Subbasin - river", basin_display_name("Danube","Tisza","Crișul Alb","code") == "Tisza - Crișul Alb")
ok("subbasin NA -> Basin - river",    basin_display_name("Danube",NA,"Casimcea","code") == "Danube - Casimcea")
ok("only basin -> basin",             basin_display_name("Red Sea",NA,NA,"code") == "Red Sea")
ok("all empty -> fallback code",      basin_display_name(NA,NA,NA,"L10:x") == "L10:x")
ok("dedupe identical components",     basin_display_name("Danube","Danube",NA,"code") == "Danube")

cat("== Trend rule (single source) ==\n")
if (exists("compute_trend_vs_previous")) {
  ok("+10% -> increase",          compute_trend_vs_previous(110, 100) == "increase")
  ok("-10% -> decrease",          compute_trend_vs_previous(90, 100) == "decrease")
  ok("+2% -> stable",             compute_trend_vs_previous(102, 100) == "stable")
  ok("prev 0 curr 0 -> stable",   compute_trend_vs_previous(0, 0) == "stable")
  ok("prev 0 curr>0 -> increase", compute_trend_vs_previous(4, 0) == "increase")
  ok("NA prev -> NA",             is.na(compute_trend_vs_previous(100, NULL)))
} else cat("  (compute_trend_vs_previous not loaded)\n")

cat("== zero-active helper ==\n")
ok("0 active + 1 known -> terminal",              isTRUE(is_zero_active_terminal(0L, 1L)))
ok("0 active + 0 known -> not terminal (absent)", isFALSE(is_zero_active_terminal(0L, 0L)))
ok("3 active -> not terminal",                    isFALSE(is_zero_active_terminal(3L, 3L)))
ok("disclaimer present",                          nchar(CHECKOVER_EXTINCTION_DISCLAIMER) > 50)

cat(sprintf("\n[test_unit] %d passed, %d failed\n", PASS, FAIL))
quit(status = if (FAIL > 0) 1 else 0)
