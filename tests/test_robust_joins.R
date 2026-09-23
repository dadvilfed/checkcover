#!/usr/bin/env Rscript
# Point-in-polygon work survives polygons the spherical engine refuses.
#
# The first single-taxon trial run (A. fulcisianus, 2026-09-23) died in the
# FEOW join: "Loop 2 is not valid: Edge 74 is degenerate (duplicate vertex)".
# Its records span a narrow bounding box, so the FEOW layer was cropped to it,
# and the crop left a ring with a repeated vertex, which s2 refuses; the join had
# no fallback. Single-taxon runs are most of what the service does.
#
#   1. the helpers in 00_spatial_sanitize.R repair and retry, and give exactly
#      the plain result when the polygons are fine;
#   2. enrich_with_feow() on a narrow-bbox taxon completes and assigns every
#      point when the layer holds such a ring (injected after loading, where
#      the crop put it: the loader repairs cached layers itself);
#   3. statically: no spatial module (02a-02f) calls st_join, st_intersects,
#      st_filter, st_nearest_feature or st_distance directly any more.

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
cat("[test_robust_joins]\n")

# ---- 3. static (no sf needed) ----
mods <- c("R/02a_continents.R", "R/02b_gadm.R", "R/02c_teow.R", "R/02d_feow.R",
          "R/02e_wdpa.R", "R/02f_hydrobasins.R")
direct <- character(0)
for (f in mods) {
  tx <- readLines(f, warn = FALSE, encoding = "UTF-8")
  tx <- sub("#.*$", "", tx)
  # A word boundary before st_: robust_distance( must not count as st_distance(.
  hit <- grep("(^|[^A-Za-z0-9_.])st_(join|intersects|filter|nearest_feature|distance)\\(", tx)
  if (length(hit)) direct <- c(direct, sprintf("%s:%d", f, hit))
}
ok(length(direct) == 0L,
   if (!length(direct)) "spatial modules join only through the robust helpers"
   else paste("direct sf joins left:", paste(direct, collapse = ", ")))

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("  SKIP  sf not installed\n")
} else {
  suppressWarnings(suppressMessages({
    library(sf); library(dplyr)
    source("R/00_logging.R"); source("R/00_helpers.R"); source("R/00_spatial_sanitize.R")
    source("R/02d_feow.R")
  }))
  log_quiet <- function(e) { t <- tempfile(); sink(t); on.exit({ sink(); unlink(t) }, add = TRUE); force(e) }

  # A ring with a repeated vertex: GEOS accepts it, s2 refuses it.
  ring_bad  <- st_polygon(list(rbind(c(12, 42), c(14, 42), c(14, 42), c(14, 44), c(12, 44), c(12, 42))))
  ring_good <- st_polygon(list(rbind(c(14, 42), c(16, 42), c(16, 44), c(14, 44), c(14, 42))))
  polys <- st_sf(freshwater_ecoregion = c("Italian Peninsula", "Adriatic"),
                 geometry = st_sfc(ring_bad, ring_good, crs = 4326))
  pts <- st_sf(record_id = c("r1", "r2", "r3"),
               geometry = st_sfc(st_point(c(13, 43)), st_point(c(15, 43)), st_point(c(30, 30)), crs = 4326))

  # ---- 1. the helpers ----
  plain <- tryCatch({ st_join(pts, polys, join = st_within); "ok" }, error = function(e) conditionMessage(e))
  ok(grepl("degenerate", plain), "the plain st_join fails on this ring, as on the server")
  j <- log_quiet(robust_join_within(pts, polys, "FEOW", "TEST"))
  ok(identical(j$freshwater_ecoregion, c("Italian Peninsula", "Adriatic", NA)),
     "robust_join_within assigns both points and leaves the outside one NA")
  ok(isTRUE(sf_use_s2()), "the spherical engine is switched back on afterwards")
  ok(identical(log_quiet(robust_nearest(pts, polys, "FEOW", "TEST")), c(1L, 2L, 2L)) &&
     identical(lengths(log_quiet(robust_intersects(pts, polys, "HB", "TEST"))), c(1L, 1L, 0L)) &&
     nrow(log_quiet(robust_filter(polys, pts, "HB", "TEST"))) == 2L,
     "nearest, intersects and filter survive the same ring")
  good <- polys[2, ]
  ok(identical(robust_join_within(pts, good, "X", "TEST"), st_join(pts, good, join = st_within, left = TRUE)),
     "with valid polygons the result is exactly the plain st_join")

  # ---- 2. enrich_with_feow on a narrow-bbox taxon ----
  out <- file.path(tempdir(), paste0("feow_", as.integer(runif(1) * 1e8)))
  dir.create(file.path(out, "cache"), recursive = TRUE)
  saveRDS(polys, file.path(out, "cache", "feow_min.rds"))
  sanitize_spatial_layer <- function(x, ...) x     # leave the ring as the crop left it
  cd <- data.frame(record_id = c("r1", "r2"), species = "Austropotamobius fulcisianus",
                   longitude = c(13, 15), latitude = c(43, 43), stringsAsFactors = FALSE)
  res <- list(clean_data = cd, clean_sf = st_as_sf(cd, coords = c("longitude", "latitude"),
                                                   crs = 4326, remove = FALSE))
  r <- tryCatch(log_quiet(enrich_with_feow(res, output_dir = out, feow_source = "local",
                                           crop_to_points_bbox = TRUE)),
                error = function(e) e)
  ok(!inherits(r, "error"),
     if (inherits(r, "error")) paste("enrich_with_feow failed:", conditionMessage(r))
     else "enrich_with_feow completes for a narrow-bbox taxon despite the ring")
  if (!inherits(r, "error")) {
    ok(identical(sort(unique(r$clean_data$freshwater_ecoregion)), c("Adriatic", "Italian Peninsula")),
       "and assigns each record its freshwater ecoregion")
  }
  unlink(out, recursive = TRUE)
}

cat(sprintf("\n[test_robust_joins] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
