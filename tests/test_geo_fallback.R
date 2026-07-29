#!/usr/bin/env Rscript
# Fallback safety net (Lucian, 2026-07). Reproduces the Euastacus suttoni case:
# 5 indigenous records with blank continent/country/admin-1 whose longitude has
# a dropped digit (15.5 instead of ~150.5), putting them in the South Atlantic
# off Africa. cheCkOVER's nearest-land snap moved them ~8,000 km onto Africa,
# and that alone flipped the species into the cosmopolitan class.
#
# Required behaviour:
#   1. the fallback must NOT assign a continent to those records -> `unresolved`
#   2. `unresolved` must not enter continent counts or classification
#   3. the rejection must be reported

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("sf", quietly = TRUE) ||
    !requireNamespace("rnaturalearth", quietly = TRUE)) {
  cat("[test_geo_fallback] SKIP (sf / rnaturalearth not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages({
  library(sf); library(dplyr)
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/00_geo_canon.R")
  source("R/00_spatial_sanitize.R"); source("R/02a_continents.R")
}))

PASS <- 0L; FAIL <- 0L
ok <- function(l, c) { if (isTRUE(c)) { PASS <<- PASS + 1L; cat("  ok  ", l, "\n") }
                       else           { FAIL <<- FAIL + 1L; cat("  FAIL", l, "\n") } }

# 20 good Australian records + 5 with the dropped-digit longitude.
n_good <- 20L; n_bad <- 5L
good_lon <- seq(150.2, 153.0, length.out = n_good)
good_lat <- seq(-28.5, -33.0, length.out = n_good)
bad_lon  <- c(15.5333, 15.4333, 15.4333, 15.2500, 15.5000)   # real corrupt values
bad_lat  <- c(-34.6333, -35.1167, -35.1833, -35.3667, -34.6833)

cd <- data.frame(
  species    = "Euastacus suttoni",
  longitude  = c(good_lon, bad_lon),
  latitude   = c(good_lat, bad_lat),
  # WoC-native continent present for the good records, blank for the corrupt ones
  continents = c(rep("Oceania", n_good), rep(NA_character_, n_bad)),
  stringsAsFactors = FALSE)
sfo <- sf::st_as_sf(cd, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
res <- list(clean_data = cd, clean_sf = sfo, files_created = character(0))

out <- suppressWarnings(suppressMessages(
  enrich_with_continents(res, output_dir = tempdir())))
got <- out$clean_data$continents
bad_idx <- (n_good + 1L):(n_good + n_bad)

cat("== 1. fallback marks the bad records unresolved ==\n")
ok("corrupt records are NOT assigned a continent",
   all(got[bad_idx] == GEO_UNRESOLVED))
ok("corrupt records were NOT snapped to Africa",
   !any(got[bad_idx] == "Africa"))
ok("good records keep their native Oceania",
   all(got[seq_len(n_good)] == "Oceania"))
ok("provenance marks the good records WoC-native",
   all(out$clean_data$continent_source[seq_len(n_good)] == "WoC"))
ok("provenance marks the rejected ones unresolved",
   all(out$clean_data$continent_source[bad_idx] == GEO_UNRESOLVED))

cat("== 2. unresolved never enters classification ==\n")
ok("count_continents sees ONE continent (not 2)", count_continents(got) == 1L)
ok("count_continents ignores the sentinel entirely",
   count_continents(c(rep("Oceania", 20), rep(GEO_UNRESOLVED, 5))) == 1L)
ok("n_distinct_geo excludes unresolved",
   n_distinct_geo(c("Australia", GEO_UNRESOLVED, NA, "")) == 1L)

cat("== 3. the rejection is reported ==\n")
ok("a rejection reason is recorded for each bad record",
   all(!is.na(out$clean_data$continent_reject[bad_idx])))
# NB: the snap distance is ~270 km (these points sit in the South Atlantic off
# Africa). The "~8,000 km" figure is how far the record was DISPLACED from where
# it belongs in Australia, which is not the same quantity as the distance to the
# nearest land. What matters is that the snap exceeds GEO_MAX_SNAP_KM.
ok("snap distance measured and exceeds the limit",
   all(out$clean_data$continent_snap_km[bad_idx] > GEO_MAX_SNAP_KM, na.rm = TRUE))
rj <- unique(out$clean_data$continent_reject[bad_idx])
cat("     reasons:", paste(rj, collapse = " | "), "\n")
cat("     snap distances (km):",
    paste(round(out$clean_data$continent_snap_km[bad_idx]), collapse = ", "), "\n")

cat("== 4. a genuinely coastal point is still rescued ==\n")
# A point ~10 km off the Australian coast must still resolve to Oceania.
cd2 <- data.frame(species = "Coastal testus",
                  longitude = c(153.6, 150.5), latitude = c(-28.6, -30.0),
                  continents = c(NA_character_, "Oceania"), stringsAsFactors = FALSE)
sf2 <- sf::st_as_sf(cd2, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
out2 <- suppressWarnings(suppressMessages(
  enrich_with_continents(list(clean_data = cd2, clean_sf = sf2, files_created = character(0)),
                         output_dir = tempdir())))
ok("near-shore point still resolves (not over-rejected)",
   out2$clean_data$continents[1] %in% CHECKOVER_CONTINENTS)

cat("== 5. the dissolved continents layer keeps Europe ==\n")
# Europe spans ~216 deg of longitude once Russia (reaching -169) is unioned in,
# so the extent audit used to drop the whole polygon — which would push every
# European fallback record into nearest-land snapping.
conts <- suppressWarnings(suppressMessages(
  load_ne_continents_medium(cache_dir = file.path(tempdir(), "cache_eu"))))
ok("Europe present in the continents layer", "Europe" %in% conts$continent)
ok("all six canonical continents present",
   all(CHECKOVER_CONTINENTS %in% canon_continent(conts$continent)))
# A European point must resolve to Europe, not get snapped elsewhere.
cd3 <- data.frame(species = "Euro testus", longitude = 21.9, latitude = 47.0,
                  continents = NA_character_, stringsAsFactors = FALSE)
sf3 <- sf::st_as_sf(cd3, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
out3 <- suppressWarnings(suppressMessages(
  enrich_with_continents(list(clean_data = cd3, clean_sf = sf3, files_created = character(0)),
                         output_dir = tempdir())))
ok("a European gap record resolves to Europe",
   identical(out3$clean_data$continents[1], "Europe"))

cat("== 6. per-run integrity report ==\n")
suppressWarnings(suppressMessages(source("R/02c_geo_integrity.R")))
rep_dir <- file.path(tempdir(), "geo_report"); dir.create(rep_dir, showWarnings = FALSE)
# Give the fixture the country/admin columns the report expects.
out$clean_data$country        <- c(rep("Australia", n_good), rep(GEO_UNRESOLVED, n_bad))
out$clean_data$country_source <- c(rep("WoC", n_good),       rep(GEO_UNRESOLVED, n_bad))
out$clean_data$admin_1        <- c(rep("Queensland", n_good), rep(GEO_UNRESOLVED, n_bad))
out$clean_data$admin1_source  <- c(rep("WoC", n_good),        rep(GEO_UNRESOLVED, n_bad))
rep_res <- suppressWarnings(suppressMessages(
  report_geographic_integrity(out, output_dir = rep_dir)))
gi <- rep_res$geo_integrity

ok("report produced",                    !is.null(gi))
ok("JSON + TSV written",
   file.exists(file.path(rep_dir, "geographic_integrity.json")) &&
   file.exists(file.path(rep_dir, "geographic_integrity.tsv")))
ok("native count correct",               gi$totals$continent$native == n_good)
ok("unresolved count correct",           gi$totals$continent$unresolved == n_bad)
ok("species-level breakdown present",    gi$species_affected == 1L)
ok("rejected snaps captured",            !is.null(gi$rejected_snaps) &&
                                          nrow(gi$rejected_snaps) == n_bad)
ok("snap limit recorded",                gi$snap_limit_km == GEO_MAX_SNAP_KM)
ok("no canon violations in clean output",
   length(gi$canon_violations$continents) == 0)

cat(sprintf("\n[test_geo_fallback] %d passed, %d failed\n", PASS, FAIL))
quit(status = if (FAIL > 0) 1 else 0)
