#!/usr/bin/env Rscript
# Regression: sanitize_spatial_layer() must KEEP countries that legitimately
# straddle the antimeridian. Dropping them removed USA and RUS from the
# country-detection layer, so GADM was never fetched for them and ~390 species
# ended up with country = NA (Lucian, 2026-07).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("[test_antimeridian] SKIP (sf not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages({
  library(sf)
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/00_spatial_sanitize.R")
}))

box <- function(xmin, xmax, ymin, ymax)
  st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
                         ncol = 2, byrow = TRUE)))

# 1. USA-like: two small parts either side of the antimeridian. Overall bbox is
#    ~358 deg wide, but the covering arc is small -> must be kept.
straddler <- st_multipolygon(list(list(box(172, 179, 50, 55)[[1]]),
                                  list(box(-179, -170, 50, 55)[[1]])))
# 2. Russia-like: ONE ring whose vertices sit near both +180 and -180. This is
#    the case st_wrap_dateline fails to split, so a part-wise bbox check still
#    calls it corrupt; the covering arc (~172 deg) shows it is fine.
rus_like <- st_polygon(list(matrix(c(
   20, 50,  90, 50, 150, 50, 179, 50, -180, 50, -170, 50,
  -170, 70, -180, 70, 179, 70, 150, 70,  90, 70,   20, 70,  20, 50),
  ncol = 2, byrow = TRUE)))
# 3. Genuinely corrupt: vertices spread right around the globe, so no large gap
#    exists and the covering arc stays ~340 deg -> must still be dropped.
corrupt <- st_polygon(list(matrix(c(
  -170, -10, -100, -10, -30, -10, 30, -10, 100, -10, 170, -10,
   170,  10,  100,  10,  30,  10, -30,  10, -100, 10, -170,  10, -170, -10),
  ncol = 2, byrow = TRUE)))
# 4. Ordinary country -> kept.
normal <- box(10, 20, 40, 50)

layer <- st_sf(name = c("STRADDLER_USA_LIKE", "RUS_LIKE", "CORRUPT_STRETCHED", "NORMAL"),
               geometry = st_sfc(straddler, rus_like, corrupt, normal, crs = 4326))

out   <- sanitize_spatial_layer(layer, layer_name = "antimeridian_test")
kept  <- if (!is.null(out) && nrow(out) > 0) as.character(out$name) else character(0)

pass <- ("STRADDLER_USA_LIKE" %in% kept) &&   # multi-part straddler
        ("RUS_LIKE"           %in% kept) &&   # single ring across the dateline
        ("NORMAL"             %in% kept) &&   # unchanged behaviour
        !("CORRUPT_STRETCHED" %in% kept)      # genuinely stretched still caught

cat("[test_antimeridian] kept:", paste(kept, collapse = ", "), "\n")
cat(sprintf("[test_antimeridian] straddler=%s rus_like=%s corrupt dropped=%s -> %s\n",
            "STRADDLER_USA_LIKE" %in% kept, "RUS_LIKE" %in% kept,
            !("CORRUPT_STRETCHED" %in% kept), if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
