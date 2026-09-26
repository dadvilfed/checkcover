#!/usr/bin/env Rscript
# The EOO layer follows the EOO metric's rule: no hull below three DISTINCT
# localities. In 1.0 (2026-09-26) Module 8 counted records instead. 8 taxa with
# three or more records at one or two localities got an EOO layer whose s2 hull
# was a thin triangle with an invented corner (up to 1.9 degrees from any
# record), while their metadata said EOO undefined.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

# Static: Module 8 draws the layer through eoo_hull(), never its own hull.
maps <- paste(readLines("R/08_maps.R", warn = FALSE), collapse = "\n")
ok(grepl("eoo_hull(eoo_sf_source)", maps, fixed = TRUE), "Module 8 builds the EOO layer with eoo_hull()")
ok(!grepl("st_convex_hull", maps, fixed = TRUE), "Module 8 computes no convex hull of its own")

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("  (sf not installed: behaviour checks skipped)\n")
} else {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
  source("R/00_helpers.R")
  pts <- function(lon, lat) sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)

  two_loc <- pts(c(22.10, 22.10, 22.35), c(46.20, 46.20, 46.41))       # 3 records, 2 localities
  one_loc <- pts(c(22.10, 22.10, 22.10), c(46.20, 46.20, 46.20))
  four    <- pts(c(22.10, 22.35, 22.60, 22.30), c(46.20, 46.41, 46.15, 46.25))  # triangle + interior point

  ok(is.null(eoo_hull(two_loc)), "3 records at 2 localities: no EOO layer")
  ok(is.null(eoo_hull(one_loc)), "3 records at 1 locality: no EOO layer")
  ok(is.na(.calc_eoo_val(c(22.10, 22.10, 22.35), c(46.20, 46.20, 46.41))),
     "... and the EOO metric agrees: undefined")
  h <- eoo_hull(four)
  ok(!is.null(h) && all(sf::st_geometry_type(h) == "POLYGON"), "3 distinct localities: a polygon")
  v <- unique(round(sf::st_coordinates(h)[, c("X", "Y")], 9))
  p <- round(sf::st_coordinates(four), 9)
  ok(all(paste(v[, 1], v[, 2]) %in% paste(p[, 1], p[, 2])), "every corner is a record locality: none invented")
  ok(nrow(v) == 3L, "the interior point is not a corner")
  ok(is.null(eoo_hull(four[0, ])) && is.null(eoo_hull(NULL)), "no points: no layer")
}

cat(sprintf("\n[test_eoo_hull] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
