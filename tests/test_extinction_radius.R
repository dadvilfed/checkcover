#!/usr/bin/env Rscript
# The extinction suppression radius must be a TRUE geodesic distance
# (Reviewer 1, Ecological Informatics, 2026-09).
#
# Until 2026-09 the mask built its circle with sf::st_buffer() in EPSG:3857 and
# passed the radius in projected units. Web Mercator units are metres only at
# the equator -- the scale factor is 1/cos(latitude) -- so a nominal 500 m
# radius covered about 500*cos(lat) m of ground: ~354 m at 45N, ~211 m at 65N.
# The manuscript described it as a 500 m geodesic buffer.
#
# Projecting is not the fix. EPSG:6933, used elsewhere for AOO, is equal-AREA,
# and preserving area means distorting distance: a 500 m radius there is roughly
# a 244 m x 1025 m ellipse at 65N. Only geodesic distance is right.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("[test_extinction_radius] SKIP (sf not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages(library(sf)))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

R_M <- 500

# What the mask now does.
inside_geodesic <- function(lon0, lat0, lon, lat, r_m = R_M) {
  p  <- st_sfc(st_point(c(lon0, lat0)), crs = 4326)
  pts <- st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326)
  as.numeric(st_distance(pts, p)[, 1]) <= r_m
}
# What it used to do.
inside_mercator <- function(lon0, lat0, lon, lat, r_m = R_M) {
  p <- st_transform(st_sfc(st_point(c(lon0, lat0)), crs = 4326), 3857)
  pts <- st_transform(
    st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326), 3857)
  st_intersects(pts, st_buffer(p, r_m), sparse = FALSE)[, 1]
}

# A point `d_m` due north of (lon0, lat0).
north_of <- function(lon0, lat0, d_m) c(lon0, lat0 + d_m / 111320)

cat("[test_extinction_radius]\n")

for (lat in c(0, 45, 65)) {
  # 400 m away: inside a true 500 m radius at every latitude.
  p_in  <- north_of(10, lat, 400)
  # 600 m away: outside a true 500 m radius at every latitude.
  p_out <- north_of(10, lat, 600)

  g_in  <- inside_geodesic(10, lat, p_in[1],  p_in[2])
  g_out <- inside_geodesic(10, lat, p_out[1], p_out[2])
  ok(isTRUE(g_in) && isFALSE(g_out),
     sprintf("geodesic at %2dN: 400 m in, 600 m out", lat))
}

# The defect: at high latitude the old test excluded points well inside 500 m.
p400_65 <- north_of(10, 65, 400)
ok(isFALSE(inside_mercator(10, 65, p400_65[1], p400_65[2])),
   "old Mercator test EXCLUDED a point 400 m away at 65N (effective radius ~211 m)")
ok(isTRUE(inside_geodesic(10, 65, p400_65[1], p400_65[2])),
   "geodesic test includes it, as the documented 500 m radius requires")

# Effective radius of the old approach, measured.
eff <- function(lat) {
  d <- seq(50, 700, by = 10)
  hits <- vapply(d, function(dd) {
    p <- north_of(10, lat, dd); isTRUE(inside_mercator(10, lat, p[1], p[2]))
  }, logical(1))
  if (!any(hits)) return(0)
  max(d[hits])
}
e0 <- eff(0); e65 <- eff(65)
cat(sprintf("  measured effective radius of the OLD rule: %d m at 0N, %d m at 65N\n", e0, e65))
ok(e0 > 450 && e0 <= 520, "old rule was ~correct at the equator")
ok(e65 < 300, "old rule shrank to well under half the stated radius at 65N")

# Equal-area is not a valid substitute -- it distorts distance by design.
inside_6933 <- function(lon0, lat0, lon, lat, r_m = R_M) {
  p <- st_transform(st_sfc(st_point(c(lon0, lat0)), crs = 4326), 6933)
  pts <- st_transform(
    st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326), 6933)
  st_intersects(pts, st_buffer(p, r_m), sparse = FALSE)[, 1]
}
p900 <- north_of(10, 65, 900)
ok(isTRUE(inside_6933(10, 65, p900[1], p900[2])),
   "EPSG:6933 would wrongly INCLUDE a point 900 m north at 65N (equal-area != equidistant)")

# Longitude wrap: a point just across the antimeridian is metres away.
ok(isTRUE(inside_geodesic(179.999, -16, -179.999, -16)),
   "points either side of 180 are correctly within the radius")

cat(sprintf("\n[test_extinction_radius] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
