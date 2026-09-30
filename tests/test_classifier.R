#!/usr/bin/env Rscript
# Classifier fix (Lucian bug 1 — whole dataset): fewer than three DISTINCT
# localities must be classified "endemic" (short-range), with EOO = NA (not 0).
# Exercises the REAL calculate_indigenous_metrics() and
# calculate_non_indigenous_metrics(). Part 1 uses only the undefined-EOO paths,
# which never invoke sf (the convex hull needs >=3 unique points); part 2, which
# computes a real EOO, runs only where sf is installed.
#
# 2026-09: the rule counted RECORDS. C (3 records at 2 localities) and D (4
# records at 1 locality) therefore fell through to "regional" / "widespread"
# with an undefined EOO — 8 taxa in 1.0, among them Cambarus veitchorum. This
# test used to assert C == "regional"; it now asserts the rule as published.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  library(dplyr)
  source("R/00_logging.R"); source("R/00_helpers.R")
  if (exists("init_logger")) try(init_logger(), silent = TRUE)
  source("R/03a_metrics_indigenous.R")
  source("R/04a_metrics_non_indigenous.R")
}))

# A: 1 record; B: 2 records; C: 3 records at 2 unique coords; D: 4 records at
# 1 unique coord. EOO undefined for all four. All single-continent.
cd <- data.frame(
  species = c("A","B","B","C","C","C","D","D","D","D"),
  country = "RO", continents = "Europe",
  longitude = c(22,23,24,25,25,26,27,27,27,27), latitude = c(46,47,48,49,49,50,45,45,45,45),
  year = 1990:1999, temporal_status = "active", stringsAsFactors = FALSE)

res <- calculate_indigenous_metrics(list(clean_data = cd, clean_sf = NULL), output_dir = tempdir())
m <- res$metrics[order(res$metrics$species), ]
cat_of <- function(d, col, sp) d[[col]][d$species == sp]

pass <- all(vapply(c("A","B","C","D"), function(s) identical(cat_of(m, "iucn_category", s), "endemic"), NA)) &&
        all(is.na(m$eoo_km2)) &&
        identical(as.integer(m$n_localities), c(1L, 2L, 2L, 1L))

cat("[test_classifier] indigenous: ",
    paste0(m$species, "=", m$iucn_category, collapse = " "),
    " | all EOO NA=", all(is.na(m$eoo_km2)), "\n", sep = "")

# --- Non-indigenous classifier (fix C) --------------------------------------
# <3 localities => EOO undefined. Previously these fell through `TRUE ~ "widespread"`
# and were labelled widespread purely because EOO was NA (10 of 29 in v1.0).
# They must be "local", mirroring the indigenous <3 => endemic rule.
resn <- calculate_non_indigenous_metrics(list(clean_data = cd, clean_sf = NULL),
                                         output_dir = tempdir())
mn <- resn$metrics[order(resn$metrics$species), ]
pass_n <- all(vapply(c("A","B","C","D"), function(s) identical(cat_of(mn, "category", s), "local"), NA)) &&
          all(is.na(mn$eoo_km2))

cat("[test_classifier] non-indigenous: ",
    paste0(mn$species, "=", mn$category, collapse = " "), "\n", sep = "")
pass <- pass && pass_n

# --- Part 2: three distinct localities are NOT forced to the short-range class
# E: 3 records at 3 localities several degrees apart (EOO far above 5,000 km²).
if (requireNamespace("sf", quietly = TRUE)) {
  ce <- data.frame(species = "E", country = "RO", continents = "Europe",
                   longitude = c(21, 26, 23), latitude = c(44, 44, 48),
                   year = 2000:2002, temporal_status = "active", stringsAsFactors = FALSE)
  me  <- calculate_indigenous_metrics(list(clean_data = ce, clean_sf = NULL), output_dir = tempdir())$metrics
  mne <- calculate_non_indigenous_metrics(list(clean_data = ce, clean_sf = NULL), output_dir = tempdir())$metrics
  pass_e <- identical(me$iucn_category, "regional") && identical(mne$category, "widespread") &&
            isTRUE(me$eoo_km2 > 5000)
  cat(sprintf("[test_classifier] 3 distinct localities: indigenous=%s non-indigenous=%s EOO=%.0f km2\n",
              me$iucn_category, mne$category, me$eoo_km2))
  pass <- pass && pass_e
} else cat("[test_classifier] sf not installed: part 2 skipped\n")

cat(sprintf("[test_classifier] %s\n", if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
