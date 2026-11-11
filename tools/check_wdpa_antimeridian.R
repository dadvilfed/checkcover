#!/usr/bin/env Rscript
# Does a cached WDPA layer still put US Gulf points inside Papahanaumokuakea?
#
# WDPA stores Papahanaumokuakea Marine National Monument split at +-180. The
# spherical repair in sanitize_spatial_layer() healed it into one ring across
# the antimeridian, and the planar fallback of the WDPA intersection read that
# ring as stretched round the globe: 6,035 records of 130 taxa, Texas to
# Florida, landed inside the monument in the published 1.0 (2026-11). This
# loads the cached layers the pipeline uses, sanitizes them with the code of
# the image it runs in, and intersects synthetic probe points (never records)
# on every engine the pipeline can reach: s2, planar lon/lat, planar EPSG:6933
# (.safe_intersects() fallback 2), and .safe_intersects() itself.
#
#   Rscript tools/check_wdpa_antimeridian.R <cache_dir> [ISO3 ...]     (default USA)
#
# e.g. on UVT, new image, then the old one with this file mounted in:
#   podman run --rm -v /data/state/cache:/cache:ro checkover:<tag> \
#     Rscript tools/check_wdpa_antimeridian.R /cache
#
# Prints protected-area names and counts only. Exit codes: 0 no probe off the
# Pacific falls in a feature crossing +-180, 1 one does, 2 usage.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  cat("usage: Rscript tools/check_wdpa_antimeridian.R <cache_dir> [ISO3 ...]\n"); quit(status = 2)
}
cache <- args[1]; isos <- if (length(args) > 1) args[-1] else "USA"
if (!dir.exists(cache)) { cat("not a directory:", cache, "\n"); quit(status = 2) }

suppressWarnings(suppressMessages({
  library(sf)
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/00_spatial_sanitize.R")
}))
log_info <- log_warn <- log_debug <- function(...) invisible(NULL)
# An image built before the fix lacks this helper; the copy lets the same file
# show the old behaviour when mounted into that image.
if (!exists(".crosses_antimeridian", mode = "function")) {
  .crosses_antimeridian <- function(g) {
    cc <- tryCatch(st_coordinates(g), error = function(e) NULL)
    if (is.null(cc) || nrow(cc) < 2L) return(FALSE)
    lc <- grep("^L[0-9]", colnames(cc)); if (!length(lc)) return(FALSE)
    n <- nrow(cc)
    any(rowSums(cc[-1, lc, drop = FALSE] != cc[-n, lc, drop = FALSE]) == 0 & abs(diff(cc[, "X"])) > 180, na.rm = TRUE)
  }
}

# Off the Pacific: points the stretched ring covered in 1.0. In the Pacific:
# either side of +-180 at the monument's latitude, for reference.
probes <- st_sf(id = c("Texas", "Louisiana", "Florida", "Mexico_Gulf", "Pacific_179E", "Pacific_170W"),
                off_pacific = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
                geometry = st_sfc(st_point(c(-97, 29)), st_point(c(-92, 30.5)), st_point(c(-82, 28)),
                                  st_point(c(-90, 22)), st_point(c(179.5, 28.5)), st_point(c(-170, 25)),
                                  crs = 4326))
bad <- FALSE
for (iso in isos) {
  f <- list.files(cache, pattern = sprintf("^wdpa_%s\\.rds$", iso), recursive = TRUE, full.names = TRUE)[1]
  if (is.na(f)) { cat(sprintf("%s: no cached wdpa_%s.rds under %s\n", iso, iso, cache)); next }
  x <- st_zm(st_as_sf(readRDS(f)), drop = TRUE, what = "ZM")
  x <- sanitize_spatial_layer(x, layer_name = sprintf("WDPA_%s_cached", iso))
  gt <- as.character(st_geometry_type(x, by_geometry = TRUE))
  x <- x[gt %in% c("POLYGON", "MULTIPOLYGON") & !st_is_empty(x), ]
  name_col <- intersect(c("NAME", "ORIG_NAME", "WDPA_NAME", "SITE_NAME"), names(x))[1]
  nm <- if (is.na(name_col)) rep("(unnamed)", nrow(x)) else as.character(x[[name_col]])
  wide <- which(vapply(st_geometry(x), function(g) { b <- st_bbox(g); b[["xmax"]] - b[["xmin"]] > 180 }, logical(1)))
  across <- wide[vapply(wide, function(i) .crosses_antimeridian(st_geometry(x)[[i]]), logical(1))]
  cat(sprintf("\n%s: %d polygon features; %d span more than 180 deg of longitude; %d of them still have a ring crossing +-180\n",
              iso, nrow(x), length(wide), length(across)))
  if (length(wide)) cat("  wide features:", paste(unique(nm[wide]), collapse = "; "), "\n")
  s2_whole <- tryCatch({ st_intersects(probes, x); "ok" }, error = function(e) paste("FAILS:", conditionMessage(e)))
  cat(sprintf("  s2 intersection with the whole layer: %s\n", substr(s2_whole, 1, 160)))
  if (!length(wide)) next
  w <- x[wide, ]
  eng <- list(
    s2          = function() { old <- sf_use_s2(TRUE);  on.exit(sf_use_s2(old)); st_intersects(probes, w) },
    planar      = function() { old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old)); suppressMessages(st_intersects(probes, w)) },
    planar_6933 = function() { old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old)); st_intersects(st_transform(probes, 6933), st_transform(w, 6933)) },
    safe_intersects_planar = function() { old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old)); suppressMessages(.safe_intersects(probes, w)) })
  for (e in names(eng)) {
    h <- tryCatch(eng[[e]](), error = function(err) NULL)
    if (is.null(h)) { cat(sprintf("  %-24s error\n", e)); next }
    hit <- vapply(h, function(i) if (length(i)) paste(unique(nm[wide][i]), collapse = " | ") else "-", "")
    off <- probes$off_pacific & hit != "-"
    if (any(off)) bad <- TRUE
    cat(sprintf("  %-24s %s\n", e, paste(sprintf("%s: %s", probes$id, hit), collapse = "; ")))
  }
}
cat(sprintf("\nRESULT: %s\n", if (bad) "FAIL - a probe off the Pacific falls inside a feature crossing +-180" else "OK"))
quit(status = if (bad) 1 else 0)
