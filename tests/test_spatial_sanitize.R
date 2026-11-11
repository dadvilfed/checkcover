#### TEST SUITE for 00_spatial_sanitize.R ####
#
# Self-contained. Uses synthetic sf objects to exercise every code path
# (Pacific-straddling polygon, corrupt-bbox feature, NA CRS layer, empty
# input, mixed-CRS join target, NULL).
#
# Run from project root:
#   Rscript test_spatial_sanitize.R
#
# Expected: "RESULTS: N passed, 0 failed" at the bottom.
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(sf)
})

source("R/00_spatial_sanitize.R")

# Test harness (same style as test_run_context.R)
PASS <- 0L
FAIL <- 0L
check <- function(label, condition, detail = NULL) {
  if (isTRUE(condition)) {
    PASS <<- PASS + 1L
    cat(sprintf("  [PASS] %s\n", label))
  } else {
    FAIL <<- FAIL + 1L
    cat(sprintf("  [FAIL] %s%s\n", label,
                if (!is.null(detail)) sprintf(" -- %s", detail) else ""))
  }
}

cat("\n========== 00_spatial_sanitize.R test suite ==========\n\n")


# ──────────────────────────────────────────────────────────────────────────
# Synthetic geometry helpers
# ──────────────────────────────────────────────────────────────────────────

# A clean Europe-ish polygon (10..20 E, 45..55 N)
mk_europe_poly <- function() {
  sf::st_polygon(list(rbind(
    c(10, 45), c(20, 45), c(20, 55), c(10, 55), c(10, 45)
  )))
}

# A clean Hawaii-area polygon (-160..-156 W) — entirely within one hemisphere
mk_hawaii_poly <- function() {
  sf::st_polygon(list(rbind(
    c(-160, 19), c(-156, 19), c(-156, 22), c(-160, 22), c(-160, 19)
  )))
}

# A "stretched across the globe" polygon — Hawaii NW archipelago coordinates
# misinterpreted as crossing the antimeridian without correction. Mimics
# the Papahanaumokuakea pathology: corners at +178 and -178 are interpreted
# as a polygon spanning ~356 deg longitude.
# We build it explicitly with both +178 and -178 vertices.
mk_corrupt_papahanaumokuakea <- function() {
  sf::st_polygon(list(rbind(
    c(-178, 23), c(178, 23), c(178, 29), c(-178, 29), c(-178, 23)
  )))
}

# Already-wrapped Pacific archipelago, expressed as MULTIPOLYGON with two
# parts on either side of +-180. This is what a properly-wrapped layer
# looks like and should pass through sanitization unchanged in feature count.
mk_wrapped_pacific <- function() {
  east <- sf::st_polygon(list(rbind(
    c(178, 23), c(180, 23), c(180, 29), c(178, 29), c(178, 23)
  )))
  west <- sf::st_polygon(list(rbind(
    c(-180, 23), c(-178, 23), c(-178, 29), c(-180, 29), c(-180, 23)
  )))
  sf::st_multipolygon(list(east, west))
}


# ──────────────────────────────────────────────────────────────────────────
# 1. Null and empty input
# ──────────────────────────────────────────────────────────────────────────
cat("[1] Null / empty / non-sf input\n")

check("NULL input returns NULL",
      is.null(sanitize_spatial_layer(NULL)))

check("non-sf input returned unchanged (data.frame)",
      identical(sanitize_spatial_layer(data.frame(x = 1)),
                data.frame(x = 1)))

empty_sf <- sf::st_sf(name = character(0),
                      geometry = sf::st_sfc(crs = 4326))
out_empty <- sanitize_spatial_layer(empty_sf, layer_name = "EMPTY")
check("empty sf returned with 0 rows",
      inherits(out_empty, "sf") && nrow(out_empty) == 0L)


# ──────────────────────────────────────────────────────────────────────────
# 2. Clean layer passes through unchanged
# ──────────────────────────────────────────────────────────────────────────
cat("\n[2] Clean Europe-only layer\n")

eu_sf <- sf::st_sf(name = "Europe",
                   geometry = sf::st_sfc(mk_europe_poly(), crs = 4326))
out_eu <- sanitize_spatial_layer(eu_sf, layer_name = "TEST_EU")
check("clean Europe layer returns 1 feature",
      inherits(out_eu, "sf") && nrow(out_eu) == 1L)
check("clean Europe layer geometry is valid",
      all(sf::st_is_valid(out_eu)))
check("clean Europe layer CRS unchanged (EPSG:4326)",
      sf::st_crs(out_eu) == sf::st_crs(4326))


# ──────────────────────────────────────────────────────────────────────────
# 3. CRS coercion
# ──────────────────────────────────────────────────────────────────────────
cat("\n[3] CRS coercion\n")

# Source layer in EPSG:3857 (web mercator)
eu_3857 <- sf::st_transform(eu_sf, 3857)
out_3857 <- sanitize_spatial_layer(eu_3857, target_crs = 4326,
                                   layer_name = "TEST_3857")
check("EPSG:3857 input -> EPSG:4326 output",
      sf::st_crs(out_3857) == sf::st_crs(4326))

# Layer with NA CRS gets assumed target_crs
naive_geom <- sf::st_sf(name = "no_crs",
                        geometry = sf::st_sfc(mk_europe_poly()))
out_naive <- sanitize_spatial_layer(naive_geom, layer_name = "TEST_NA_CRS")
check("NA CRS input is assumed and tagged target CRS",
      sf::st_crs(out_naive) == sf::st_crs(4326))


# ──────────────────────────────────────────────────────────────────────────
# 4. The Papahanaumokuakea bug — corrupt-bbox feature gets dropped
# ──────────────────────────────────────────────────────────────────────────
cat("\n[4] Antimeridian-corrupted feature\n")

corrupt <- sf::st_sf(
  WDPAID = c(123, 456),
  NAME = c("Papahanaumokuakea (corrupted)", "Good_PA"),
  geometry = sf::st_sfc(
    mk_corrupt_papahanaumokuakea(),
    mk_europe_poly(),
    crs = 4326
  )
)

out_corrupt <- sanitize_spatial_layer(corrupt, layer_name = "TEST_CORRUPT")

check("corrupt input had 2 features",
      nrow(corrupt) == 2L)

# After sanitization the broken feature should be dropped, wrapped into valid
# parts, or kept if it is a legitimate antimeridian-crosser.
#
# NOTE (2026-07): this assertion used to require "no remaining feature has a
# bbox wider than 180 deg". That proxy is wrong for features that genuinely
# straddle +-180: Papahanaumokuakea (and Russia, Fiji, the USA via the
# Aleutians) have vertices near BOTH -180 and +180, so their bbox is ~360 deg
# wide while the polygon itself is narrow and correct. Dropping on bbox alone
# removed USA/RUS from the country layer and left ~390 species with country=NA.
# The sanitizer now measures the minimal covering arc instead, so such features
# survive. What actually matters -- and what this proxy was standing in for --
# is that no kept feature falsely contains unrelated points, so we assert that
# directly: the fixture must contain near-dateline points and nothing else.
probe <- sf::st_sfc(
  sf::st_point(c(0, 26)), sf::st_point(c(100, 26)), sf::st_point(c(-100, 26)),
  crs = 4326
)
no_false_containment <- TRUE
if (nrow(out_corrupt) > 0L) {
  hits <- suppressMessages(sf::st_within(probe, sf::st_geometry(out_corrupt)))
  no_false_containment <- all(lengths(hits) == 0L)
}
check("no kept feature falsely contains far-away points (lon 0 / 100 / -100)",
      no_false_containment)

check("the good PA (Europe) survived sanitization",
      "Good_PA" %in% out_corrupt$NAME)


# ──────────────────────────────────────────────────────────────────────────
# 5. Properly-wrapped Pacific feature (MULTIPOLYGON) passes through
# ──────────────────────────────────────────────────────────────────────────
cat("\n[5] Already-wrapped Pacific MULTIPOLYGON\n")

wrapped <- sf::st_sf(
  name = "Pacific archipelago (wrapped)",
  geometry = sf::st_sfc(mk_wrapped_pacific(), crs = 4326)
)
out_wrapped <- sanitize_spatial_layer(wrapped, layer_name = "TEST_WRAPPED")

# A properly-wrapped MULTIPOLYGON's overall bbox spans the full longitude
# range (180 deg), but each sub-polygon part is small. The audit measures
# feature-level bbox (the MULTIPOLYGON's outer bbox), so it MIGHT drop this.
# Outcome we want: at minimum, the function does not error out, and any
# kept feature is geometrically valid.
check("wrapped Pacific layer survives without error",
      inherits(out_wrapped, "sf"))
if (nrow(out_wrapped) > 0L) {
  check("any kept feature is geometrically valid",
        all(sf::st_is_valid(out_wrapped)))
}


# ──────────────────────────────────────────────────────────────────────────
# 6. Idempotence: sanitizing twice is identical to sanitizing once
# ──────────────────────────────────────────────────────────────────────────
cat("\n[6] Idempotence\n")

mixed <- sf::st_sf(
  name = c("A", "B"),
  geometry = sf::st_sfc(mk_europe_poly(), mk_hawaii_poly(), crs = 4326)
)
once  <- sanitize_spatial_layer(mixed, layer_name = "TEST_ONCE")
twice <- sanitize_spatial_layer(once,  layer_name = "TEST_TWICE")

check("twice-sanitized layer has same feature count as once",
      nrow(once) == nrow(twice))
check("twice-sanitized geometries equal once-sanitized geometries",
      identical(sf::st_geometry(once), sf::st_geometry(twice)))


# ──────────────────────────────────────────────────────────────────────────
# 7. Bbox-audit can be disabled with drop_corrupt_bbox = FALSE
# ──────────────────────────────────────────────────────────────────────────
cat("\n[7] drop_corrupt_bbox = FALSE keeps wide features\n")

# Build a layer with a deliberately wide feature (covers all longitude in
# one polygon). With audit on -> dropped. With audit off -> kept.
wide_poly <- sf::st_polygon(list(rbind(
  c(-179, 10), c(179, 10), c(179, 20), c(-179, 20), c(-179, 10)
)))
wide <- sf::st_sf(
  name = "Wide feature",
  geometry = sf::st_sfc(wide_poly, crs = 4326)
)

out_drop <- sanitize_spatial_layer(wide, drop_corrupt_bbox = TRUE,
                                   layer_name = "TEST_DROP")
out_keep <- sanitize_spatial_layer(wide, drop_corrupt_bbox = FALSE,
                                   layer_name = "TEST_KEEP")

# With drop=TRUE, st_wrap_dateline often splits the wide polygon into two,
# but the bbox audit may still flag depending on what wrap produces. So
# we check: keep mode never drops anything by audit; drop mode may.
check("drop_corrupt_bbox = FALSE preserves all input features",
      nrow(out_keep) >= 1L)


# ──────────────────────────────────────────────────────────────────────────
# 8. A protected area split at +-180, on every engine
# ──────────────────────────────────────────────────────────────────────────
# WDPA stores Papahanaumokuakea split at the antimeridian. Steps 1 and 4 run
# st_make_valid() on the spherical engine, which healed the two parts into one
# ring across +-180. Section 4 only probed on s2, where that ring is right; the
# planar fallback of the WDPA intersection read it as stretched round the
# globe and put 6,035 records of 131 taxa, Texas to Florida, inside the
# monument in the published 1.0. Every engine must now agree.
cat("\n[8] Split protected area: s2, planar lon/lat, planar EPSG:6933\n")

mk_split_pa <- function() {
  east <- sf::st_polygon(list(rbind(c(175, 20), c(180, 20), c(180, 30), c(175, 30), c(175, 20))))
  west <- sf::st_polygon(list(rbind(c(-180, 20), c(-160, 20), c(-160, 30), c(-180, 30), c(-180, 20))))
  sf::st_multipolygon(list(east, west))
}
pa_layer <- sf::st_sf(NAME = c("Monument", "Good_PA"),
                      geometry = sf::st_sfc(mk_split_pa(), mk_europe_poly(), crs = 4326))
probes <- sf::st_sf(id = c("texas", "florida", "east", "west"),
                    geometry = sf::st_sfc(sf::st_point(c(-97, 29)), sf::st_point(c(-82, 28)),
                                          sf::st_point(c(178, 25)), sf::st_point(c(-170, 25)), crs = 4326))
want <- c(texas = 0L, florida = 0L, east = 1L, west = 1L)
hits_on <- function(polys, engine) {
  old <- sf::sf_use_s2(engine == "s2"); on.exit(sf::sf_use_s2(old), add = TRUE)
  p <- if (engine == "6933") sf::st_transform(probes, 6933) else probes
  g <- if (engine == "6933") sf::st_transform(polys, 6933) else polys
  setNames(suppressMessages(lengths(sf::st_intersects(p, g[1, ]))), probes$id)
}
merged <- suppressWarnings(sf::st_make_valid(pa_layer))   # what steps 1 and 4 make of it
check("the s2 repair heals the split monument into one ring across +-180 (the bug's premise)",
      .crosses_antimeridian(sf::st_geometry(merged)[[1]]))
for (src in c("split", "merged")) {
  out <- sanitize_spatial_layer(if (src == "split") pa_layer else merged, layer_name = "TEST_PA")
  check(sprintf("%s input: both protected areas kept", src), nrow(out) == 2L)
  check(sprintf("%s input: no ring of the sanitized monument crosses +-180", src),
        !.crosses_antimeridian(sf::st_geometry(out)[[1]]))
  for (eng in c("s2", "planar", "6933")) {
    h <- hits_on(out, eng)
    check(sprintf("%s input, %s engine: monument holds its own points and not Texas or Florida", src, eng),
          identical(h, want), paste(names(h), h, collapse = " "))
  }
}
rep <- repair_polygons(pa_layer)
check("repair_polygons() leaves no ring across +-180 for the planar retry",
      !.crosses_antimeridian(sf::st_geometry(rep)[[1]]) && identical(hits_on(rep, "planar"), want))
check("split_antimeridian() leaves features that do not cross untouched",
      identical(sf::st_geometry(split_antimeridian(merged))[[2]], sf::st_geometry(merged)[[2]]))
# Antarctica spans every longitude: no shift makes it contiguous, so it is
# left as it was rather than broken.
antarctica <- sf::st_polygon(list(rbind(c(-180, -60), c(-90, -65), c(0, -62), c(90, -66), c(180, -60),
                                        c(180, -90), c(-180, -90), c(-180, -60))))
check("a ring round the whole globe (Antarctica) is not split",
      is.null(.split_at_antimeridian(antarctica, sf::st_crs(4326))))
# The WDPA intersection itself, on the planar engine (as its fallback 2 runs):
# given the healed ring, it must split it before intersecting.
h_env <- new.env()
ok_helpers <- tryCatch({ sys.source("R/00_helpers.R", envir = h_env); TRUE }, error = function(e) FALSE)
if (ok_helpers) {
  old <- sf::sf_use_s2(FALSE)
  si <- suppressMessages(lengths(h_env$.safe_intersects(probes, merged[1, ])))
  sf::sf_use_s2(old)
  check(".safe_intersects() on the planar engine keeps Texas and Florida out of the monument",
        identical(setNames(si, probes$id), want), paste(si, collapse = " "))
}


# ──────────────────────────────────────────────────────────────────────────
# Final summary
# ──────────────────────────────────────────────────────────────────────────
cat("\n========== RESULTS ==========\n")
cat(sprintf("%d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0L) stop("Test suite has failures")
invisible(NULL)
