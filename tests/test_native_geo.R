#!/usr/bin/env Rscript
# WoC-native geography takes priority; cheCkOVER's overlay only fills gaps and
# must never overwrite a populated native value (Lucian, 2026-07, task B).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  library(dplyr); library(stringr)
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_geo_canon.R")   # 02a now normalises via the canonical vocabulary
  source("R/00_dwc_fields.R"); source("R/01_ingest.R"); source("R/02a_continents.R")
}))

PASS <- 0L; FAIL <- 0L
ok <- function(l, c) { if (isTRUE(c)) { PASS <<- PASS + 1L; cat("  ok  ", l, "\n") }
                       else           { FAIL <<- FAIL + 1L; cat("  FAIL", l, "\n") } }

# ── 1. Ingest carries the native geo columns ────────────────────────────────
woc <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
  scientificName = c("Astacus astacus", "Astacus astacus", "Cambarus jonesi"),
  decimalLatitude = c("46.5", "47.1", "34.8"), decimalLongitude = c("22.1", "23.2", "-86.5"),
  establishmentMeans = "indigenous", occurrenceOrigin = "native",
  year = c("1998", "2003", "1990"), accuracy = "high",
  # WoC-native geography; row 2 deliberately blank -> must fall through to lookup
  continent = c("Europe", "", "North America"),
  country   = c("România", "", "United States"),
  stateProvince = c("Bihor", "", "Alabama"))

out <- map_woc_to_checkover(woc)
ok("native country carried",        identical(out$country[1], "România"))
ok("diacritics preserved",          grepl("â", out$country[1]))
ok("native continent carried",      identical(out$continents[1], "Europe"))
ok("native admin_1 carried",        identical(out$admin_1[1], "Bihor"))
ok("blank native -> NA (gap)",      is.na(out$country[2]) && is.na(out$continents[2]) && is.na(out$admin_1[2]))
ok("provenance marked WoC",         identical(out$country_source[1], "WoC"))
ok("provenance NA on the gap row",  is.na(out$country_source[2]))
ok("third row native carried",      identical(out$country[3], "United States"))

# ── 2. Legacy input with no geo columns still works ─────────────────────────
legacy <- woc[, setdiff(names(woc), c("continent", "country", "stateProvince"))]
out2 <- map_woc_to_checkover(legacy)
ok("no geo columns -> all NA, no error", all(is.na(out2$country)) && all(is.na(out2$continents)))

# ── 3. Module 2A skips the overlay when every record is native ──────────────
# (n_need == 0 short-circuit: no Natural Earth load, values untouched)
res <- list(
  clean_data = data.frame(species = c("A", "B"), continents = c("Europe", "Asia"),
                          stringsAsFactors = FALSE),
  clean_sf   = data.frame(species = c("A", "B"), continents = c("Europe", "Asia"),
                          stringsAsFactors = FALSE),
  files_created = character(0))
res2 <- tryCatch(enrich_with_continents(res, output_dir = tempdir()),
                 error = function(e) { cat("  (2A error: ", conditionMessage(e), ")\n", sep = ""); NULL })
if (!is.null(res2)) {
  ok("2A preserved native continents", identical(res2$clean_data$continents, c("Europe", "Asia")))
  ok("2A marked both as WoC-native",   identical(res2$clean_data$continent_source, c("WoC", "WoC")))
} else { FAIL <- FAIL + 2L; cat("  FAIL 2A skip path\n") }

# ── 4. Encoding repair is element-wise ──────────────────────────────────────
# The WoC export is Latin-1 for non-ASCII (county "Baden-W<fc>rttemberg",
# contributor "P<e2>rvulescu"). Repair must fix ONLY the invalid elements: a
# blanket latin1 conversion would double-encode already-valid UTF-8 rows into
# mojibake and destroy the curated diacritics.
mixed <- c("Bihor",                                   # ASCII
           rawToChar(as.raw(c(0x48, 0xe9, 0x72, 0x61, 0x75, 0x6c, 0x74))),  # "Herault" latin1
           "Timiș",                              # already valid UTF-8 (Timiș)
           NA_character_)
fixed <- fix_utf8_encoding(data.frame(county = mixed, stringsAsFactors = FALSE))$county
ok("latin1 element repaired",      identical(fixed[2], "Hérault"))
ok("valid UTF-8 left untouched",   identical(fixed[3], "Timiș"))
ok("ASCII untouched",              identical(fixed[1], "Bihor"))
ok("NA preserved",                 is.na(fixed[4]))
ok("all output valid UTF-8",       all(stringi::stri_enc_isutf8(na.omit(fixed))))

# ── 5. Sanity gate on native geography (column-shift defence) ───────────────
# A delimiter inside a text field shifts every later column by one, so
# `continent` can hold the contributor's name, `country` the continent and
# `county` the country. 12 such rows in the 2026-07 export made two Euastacus
# species look cosmopolitan. A native value that is not a real continent must be
# rejected — together with the rest of that row's geo block — so the overlay
# recomputes it, rather than letting garbage outrank the computed value.
shifted <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
  scientificName = c("Euastacus armatus", "Euastacus armatus"),
  decimalLatitude = c("-32.8", "-37.1"), decimalLongitude = c("150.2", "145.0"),
  establishmentMeans = "indigenous", occurrenceOrigin = "native",
  year = c("2017", "2016"), accuracy = "high",
  continent = c("Lucian Pârvulescu", "Oceania"),  # row 1 shifted, row 2 clean
  country   = c("Oceania", "Australia"),          # row 1 holds the continent
  stateProvince = c("Australia", "Victoria"))     # row 1 holds the country
sg <- map_woc_to_checkover(shifted)
ok("bogus continent rejected",        is.na(sg$continents[1]))
ok("shifted row's country dropped",   is.na(sg$country[1]))
ok("shifted row's admin_1 dropped",   is.na(sg$admin_1[1]))
ok("shifted row flagged for recompute", is.na(sg$country_source[1]))
ok("clean row untouched",             identical(sg$continents[2], "Oceania") &&
                                      identical(sg$country[2], "Australia") &&
                                      identical(sg$admin_1[2], "Victoria"))
ok("clean row still WoC-native",      identical(sg$continent_source[2], "WoC"))

cat(sprintf("\n[test_native_geo] %d passed, %d failed\n", PASS, FAIL))
quit(status = if (FAIL > 0) 1 else 0)
