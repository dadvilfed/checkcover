#!/usr/bin/env Rscript
# Classifier fix (Lucian bug 1 — whole dataset): <3 active records must be
# classified "endemic" (short-range), with EOO = NA (not 0). Exercises the REAL
# calculate_indigenous_metrics(). Uses only the <3-record and undefined-EOO
# paths, which never invoke sf (the convex hull needs >=3 unique points).

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

# A: 1 record; B: 2 records; C: 3 records at 2 unique coords (EOO undefined).
# All single-continent.
cd <- data.frame(
  species = c("A","B","B","C","C","C"),
  country = "RO", continents = "Europe",
  longitude = c(22,23,24,25,25,26), latitude = c(46,47,48,49,49,50),
  year = 1990:1995, temporal_status = "active", stringsAsFactors = FALSE)

res <- calculate_indigenous_metrics(list(clean_data = cd, clean_sf = NULL), output_dir = tempdir())
m <- res$metrics[order(res$metrics$species), ]

pass <- identical(m$iucn_category[m$species=="A"], "endemic") &&
        identical(m$iucn_category[m$species=="B"], "endemic") &&
        identical(m$iucn_category[m$species=="C"], "regional") &&
        is.na(m$eoo_km2[m$species=="A"]) && is.na(m$eoo_km2[m$species=="B"])

cat("[test_classifier] indigenous: A=", m$iucn_category[m$species=="A"],
    " B=", m$iucn_category[m$species=="B"], " C=", m$iucn_category[m$species=="C"],
    " | eooA_na=", is.na(m$eoo_km2[m$species=="A"]), "\n", sep = "")

# --- Non-indigenous classifier (fix C) --------------------------------------
# <3 records => EOO undefined. Previously these fell through `TRUE ~ "widespread"`
# and were labelled widespread purely because EOO was NA (10 of 29 in v1.0).
# They must now be "local", mirroring the indigenous <3 => endemic rule.
resn <- calculate_non_indigenous_metrics(list(clean_data = cd, clean_sf = NULL),
                                         output_dir = tempdir())
mn <- resn$metrics[order(resn$metrics$species), ]
pass_n <- identical(mn$category[mn$species == "A"], "local") &&
          identical(mn$category[mn$species == "B"], "local") &&
          is.na(mn$eoo_km2[mn$species == "A"])

cat("[test_classifier] non-indigenous: A=", mn$category[mn$species=="A"],
    " B=", mn$category[mn$species=="B"], "\n", sep = "")
pass <- pass && pass_n
cat(sprintf("[test_classifier] %s\n", if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
