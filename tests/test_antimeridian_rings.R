#!/usr/bin/env Rscript
# Regression: normalize_antimeridian_rings() (Lucian, 2026-08).
#
# HydroBASINS polygon 5060081550 (Fiji, on Cherax destructor) has rings with
# vertices on both sides of 180 in raw form (-179.9994 followed by +180.0).
# Leaflet joins that pair along the short planar path, drawing a line straight
# across the world map at ~16S. The guard shifts negative longitudes of any
# ring spanning >180 deg by +360 so the ring stays continuous.
#
# Distinct from test_antimeridian.R: that one covers sanitize_spatial_layer(),
# which protects the spatial JOIN by splitting features. This covers the
# export-time guard, which protects the WRITTEN GEOMETRY without splitting.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("[test_antimeridian_rings] SKIP (sf not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages({
  library(sf)
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/00_spatial_sanitize.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

ring <- function(lons, lats)
  matrix(c(lons, lons[1], lats, lats[1]), ncol = 2)

spans <- function(x) {
  s <- c()
  walk <- function(z) if (is.matrix(z)) s <<- c(s, max(z[,1]) - min(z[,1])) else lapply(z, walk)
  for (i in seq_len(nrow(x))) walk(unclass(st_geometry(x)[[i]]))
  s
}

cat("[test_antimeridian_rings]\n")

# Fiji-shaped: one ring straddling the dateline, plus a normal ring.
fiji   <- st_polygon(list(ring(c(179.9, 180.0, -179.9994, -179.95),
                               c(-16.1, -16.0, -16.05,    -16.2))))
normal <- st_polygon(list(ring(c(143.0, 144.0, 144.0, 143.0),
                               c(-35.0, -35.0, -34.0, -34.0))))
x <- st_sf(HB_LABEL = c("5060081550", "5080066880"),
           geometry = st_sfc(fiji, normal, crs = 4326))

ok(sum(spans(x) > 180) == 1L, "fixture has exactly one ring spanning >180 deg")

y <- normalize_antimeridian_rings(x, layer_name = "test")

ok(sum(spans(y) > 180) == 0L,        "no ring spans >180 deg after normalization")
ok(attr(y, "n_rings_normalized") == 1L, "exactly one ring reported as normalized")
ok(nrow(y) == nrow(x),               "feature count unchanged (normalize, not split)")
ok(length(spans(y)) == length(spans(x)), "ring count unchanged")

# The untouched feature must be bit-identical.
ok(identical(st_geometry(x)[[2]], st_geometry(y)[[2]]),
   "non-crossing feature left byte-identical")

# Shifted vertices land just past 180, not wrapped back.
lon_fixed <- unclass(st_geometry(y)[[1]])[[1]][, 1]
ok(all(lon_fixed >= 179.9) && max(lon_fixed) > 180,
   "negative longitudes shifted by +360, ring continuous through 180")

# Idempotent — a second pass must be a no-op.
z <- normalize_antimeridian_rings(y)
ok(attr(z, "n_rings_normalized") == 0L, "idempotent on a second pass")

# MULTIPOLYGON: nested ring structure must be walked, not just POLYGON.
mp <- st_sf(HB_LABEL = "mp",
            geometry = st_sfc(st_multipolygon(list(list(ring(c(179.9, 180.0, -179.99, -179.95),
                                                             c(-16.1, -16.0, -16.05, -16.2))),
                                                   list(ring(c(143, 144, 144, 143),
                                                             c(-35, -35, -34, -34))))),
                              crs = 4326))
mpn <- normalize_antimeridian_rings(mp)
ok(attr(mpn, "n_rings_normalized") == 1L, "MULTIPOLYGON: inner ring reached and fixed")
ok(sum(spans(mpn) > 180) == 0L,           "MULTIPOLYGON: no ring spans >180 deg after")

# Polygons with holes: the hole is a ring too.
holed <- st_sf(HB_LABEL = "holed",
               geometry = st_sfc(st_polygon(list(
                 ring(c(179.5, 180.0, -179.5, -179.8), c(-16, -15.5, -15.6, -16.2)),
                 ring(c(179.8, 179.9, -179.9, -179.85), c(-15.9, -15.8, -15.85, -15.95))
               )), crs = 4326))
ok(attr(normalize_antimeridian_rings(holed), "n_rings_normalized") == 2L,
   "outer ring and hole both normalized")

# Must NOT touch a ring that is merely wide but does not cross.
wide <- st_sf(HB_LABEL = "wide",
              geometry = st_sfc(st_polygon(list(ring(c(-50, 100, 100, -50),
                                                     c(-10, -10, 10, 10)))), crs = 4326))
ok(attr(normalize_antimeridian_rings(wide), "n_rings_normalized") == 0L,
   "150-deg ring left alone (below the 180-deg trigger)")

# Empty input and non-polygon geometry pass through untouched.
pts <- st_sf(HB_LABEL = "p", geometry = st_sfc(st_point(c(179, -16)), crs = 4326))
ok(attr(normalize_antimeridian_rings(pts), "n_rings_normalized") == 0L,
   "POINT geometry passed through untouched")
ok(is.null(normalize_antimeridian_rings(NULL)), "NULL input returns NULL")

cat(sprintf("\n[test_antimeridian_rings] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
