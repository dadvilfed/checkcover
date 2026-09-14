#!/usr/bin/env Rscript
# AOO must be computed on an equal-area lattice (Reviewer 1, Ecological
# Informatics, 2026-09).
#
# Until 2026-09 the lattice was 0.018 degrees with every occupied cell credited
# a flat 4 km2. A degree of longitude shrinks with latitude, so the same
# physical area occupied more cells the further north it sat, and each of those
# smaller cells was still credited 4 km2. The manuscript disclosed the bias but
# justified it as the cost of avoiding per-species reprojection -- which the
# clustering module was already doing.
#
# The property that matters: AOO is a physical area, so the same physical
# footprint must score the same AOO wherever on the globe it sits.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("[test_aoo_equal_area] SKIP (sf not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages({
  library(sf); source("R/00_logging.R"); source("R/00_helpers.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

# The superseded implementation, kept so the regression cannot return unnoticed.
aoo_old <- function(lon, lat) {
  g <- 0.018
  length(unique(paste(floor(lon / g), floor(lat / g)))) * 4
}

# A filled square `side_km` across, centred at (lon0, lat0).
patch <- function(lon0, lat0, side_km = 10, n = 40) {
  dlat <- (side_km / 111.32) / 2
  dlon <- dlat / cos(lat0 * pi / 180)
  g <- expand.grid(x = seq(lon0 - dlon, lon0 + dlon, length.out = n),
                   y = seq(lat0 - dlat, lat0 + dlat, length.out = n))
  list(lon = g$x, lat = g$y)
}

cat("[test_aoo_equal_area]\n")

eq <- patch(0,  0);  hi <- patch(0, 65);  mid <- patch(0, 45)
a_eq <- calc_aoo_km2(eq$lon, eq$lat)
a_hi <- calc_aoo_km2(hi$lon, hi$lat)
a_md <- calc_aoo_km2(mid$lon, mid$lat)
o_eq <- aoo_old(eq$lon, eq$lat)
o_hi <- aoo_old(hi$lon, hi$lat)

cat(sprintf("  same 10x10 km patch: equator / 45N / 65N\n"))
cat(sprintf("    equal-area : %6.0f  %6.0f  %6.0f km2\n", a_eq, a_md, a_hi))
cat(sprintf("    old lattice: %6.0f     -    %6.0f km2  (%.1fx inflation)\n",
            o_eq, o_hi, o_hi / o_eq))

ok(abs(a_hi - a_eq) / a_eq < 0.15,
   "equal-area: same physical patch scores the same AOO at 0N and 65N")
ok(abs(a_md - a_eq) / a_eq < 0.15, "and at 45N")
ok(o_hi > o_eq * 1.5,
   "old lattice inflated the identical patch at high latitude (the defect)")

# A 10 x 10 km patch is 100 km2; on 2 km cells that is ~25 cells.
ok(a_eq >= 80 && a_eq <= 200,
   sprintf("a 10x10 km patch scores ~100 km2 (got %.0f)", a_eq))

# Cell size is the IUCN 2 km standard, and configurable.
ok(identical(CHECKOVER_AOO_CELL_KM, 2), "default cell edge is the IUCN 2 km")
one_pt <- calc_aoo_km2(12.0, 46.0)
ok(identical(one_pt, 4), "a single occurrence occupies exactly one 4 km2 cell")
ok(calc_aoo_km2(12.0, 46.0, cell_km = 10) == 100, "cell_km is honoured")

# Monotonic and additive in the obvious ways.
far <- list(lon = c(eq$lon, hi$lon), lat = c(eq$lat, hi$lat))
ok(calc_aoo_km2(far$lon, far$lat) > a_eq,
   "adding a distant patch increases AOO")
ok(calc_aoo_km2(c(12, 12), c(46, 46)) == 4,
   "duplicate coordinates do not double-count a cell")

# Degenerate input.
ok(is.na(calc_aoo_km2(numeric(0), numeric(0))), "no coordinates -> NA")
ok(is.na(calc_aoo_km2(NA_real_, NA_real_)), "all-NA coordinates -> NA")
ok(calc_aoo_km2(c(12, NA), c(46, NA)) == 4, "non-finite coordinates are dropped")

# Antimeridian: a patch straddling 180 must not be counted as globe-spanning.
am <- list(lon = c(179.99, 179.995, -179.995, -179.99),
           lat = rep(-16.0, 4))
ok(calc_aoo_km2(am$lon, am$lat) <= 3 * 4,
   "a patch straddling 180 stays a handful of cells, not a globe-wide span")

cat(sprintf("\n[test_aoo_equal_area] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
