#!/usr/bin/env Rscript
# EOO is undefined, not the whole Earth, when no hemisphere contains the
# localities (Lucian, SEB review, 2026-10).
#
# s2 returns the full sphere as the convex hull of points that no hemisphere
# contains, so the multi-continent non-native ranges of Procambarus clarkii,
# Cherax destructor, C. quadricarinatus and P. virginalis read 510,066,073 km²
# in 1.0. calc_eoo_km2() now returns NA for them, eoo_status() says why, and
# eoo_hull() draws no layer.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("sf", "dplyr")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_eoo_hemisphere] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  library(dplyr); source("R/00_logging.R"); source("R/00_helpers.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
cat("[test_eoo_hemisphere]\n")

# One continent: an ordinary hull.
lon1 <- c(22.0, 22.6, 23.1, 22.4); lat1 <- c(46.2, 46.9, 46.4, 46.0)
e1 <- calc_eoo_km2(lon1, lat1)
ok(is.finite(e1) && e1 > 1000 && e1 < 10000, sprintf("a regional range has a finite EOO (%.0f km²)", e1))
ok(identical(eoo_status(lon1, lat1, e1), "computed"), "status: computed")

# Introduced on five continents, as P. clarkii: no hemisphere holds them all.
lonw <- c(-90.1, -47.9, 2.3, 36.8, 139.7, 151.2, -122.4)
latw <- c( 30.0, -15.8, 48.9, -1.3, 35.7, -33.9,  37.8)
pts  <- sf::st_as_sf(data.frame(lon = lonw, lat = latw), coords = c("lon", "lat"), crs = 4326)
whole <- as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(pts)))) / 1e6
if (isTRUE(sf::sf_use_s2())) {
  ok(whole > 5e8, sprintf("s2 alone gives the whole sphere for them (%.0f km²)", whole))
}
ew <- calc_eoo_km2(lonw, latw)
ok(is.na(ew), "calc_eoo_km2(): NA, not the area of the Earth")
ok(identical(eoo_status(lonw, latw, ew), "exceeds_hemisphere"), "status: exceeds_hemisphere")
ok(is.null(eoo_hull(pts)), "eoo_hull(): no layer either")

# Fewer than three distinct localities stays its own case.
ok(identical(eoo_status(c(1, 1, 2), c(1, 1, 2), calc_eoo_km2(c(1, 1, 2), c(1, 1, 2))),
             "fewer_than_3_localities"), "two localities: fewer_than_3_localities")
ok(identical(eoo_status(numeric(0), numeric(0), NA_real_), "no_localities"), "no localities: no_localities")

# The threshold is half the sphere: a hull that fits in a hemisphere is below it.
ok(CHECKOVER_EARTH_AREA_KM2 > 5.09e8 && CHECKOVER_EARTH_AREA_KM2 < 5.11e8, "Earth area constant")

cat(sprintf("\n[test_eoo_hemisphere] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
