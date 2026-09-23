#!/usr/bin/env Rscript
# The same data must give the same KML, byte for byte.
#
# Comparing the clean 1.0 demo on the host and in the container (2026-09-23),
# every GeoJSON map was identical but every KML differed: st_write() names the
# KML Schema and Folder after the dsn, and the maps module writes through a
# tempfile(), so each run embedded a random name ("file4d2a...") in every KML.
# Any two runs of the same revision gave different bytes and different md5s in
# file_manifest.csv. Fixed by passing `layer =` the target file's name.
#
#   1. static, everywhere: every st_write(..., driver = "KML") that writes to a
#      temp file passes `layer =`;
#   2. real, where sf can be loaded safely (not on the Windows development
#      machine, where sf segfaults under Rscript; it runs in the Linux image
#      build): write the same polygon twice and compare the bytes.

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
cat("[test_kml_determinism]\n")

# ---- 1. static ----
kml_writes <- list()
for (f in c("R/08_maps.R", "R/08_maps_parallel.R", "R/12_temporal_outputs.R")) {
  walk <- function(e) {
    if (is.call(e)) {
      h <- e[[1]]
      is_sw <- (is.name(h) && identical(as.character(h), "st_write")) ||
               (is.call(h) && identical(h[[1]], as.name("::")) && identical(as.character(h[[3]]), "st_write"))
      if (is_sw) {
        a <- as.list(e)[-1]
        if (identical(a$driver, "KML")) {
          dsn <- if (!is.null(a$dsn)) a$dsn else a[[2]]
          kml_writes[[length(kml_writes) + 1L]] <<- list(file = f, has_layer = "layer" %in% names(a),
                                                         to_temp = is.name(dsn) && identical(as.character(dsn), "tmp"))
        }
      }
      # Skip empty arguments (the `` in x[, 1]) by index: binding one to a
      # variable is itself an error.
      for (i in seq_along(e)) {
        if (!(is.symbol(e[[i]]) && !nzchar(as.character(e[[i]])))) walk(e[[i]])
      }
    }
  }
  for (e in as.list(parse(f, keep.source = FALSE))) walk(e)
}
temp_writes <- Filter(function(w) w$to_temp, kml_writes)
ok(length(temp_writes) >= 2L, sprintf("found the KML writes through a temp file (%d)", length(temp_writes)))
ok(all(vapply(temp_writes, `[[`, logical(1), "has_layer")),
   "every KML written through a temp file names its layer (no random 'file4d2a...' inside)")

# ---- 2. real bytes, where sf is safe to load ----
if (.Platform$OS.type != "windows" && requireNamespace("sf", quietly = TRUE)) {
  src <- readLines("R/08_maps.R", warn = FALSE)
  i <- grep("^\\.write_styled_kml <- function", src)
  j <- i + which(src[(i + 1):length(src)] == "}")[1]
  eval(parse(text = src[i:j]))
  poly <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(22.1, 46.1), c(22.4, 46.1), c(22.4, 46.4), c(22.1, 46.1)))), crs = 4326))
  d1 <- file.path(tempdir(), "kml_a"); d2 <- file.path(tempdir(), "kml_b")
  dir.create(d1); dir.create(d2)
  f1 <- file.path(d1, "Testus_astacus_EOO.kml"); f2 <- file.path(d2, "Testus_astacus_EOO.kml")
  .write_styled_kml(poly, f1, "EOO", "#D48D00", 0.4)
  .write_styled_kml(poly, f2, "EOO", "#D48D00", 0.4)
  k1 <- readLines(f1, warn = FALSE); k2 <- readLines(f2, warn = FALSE)
  ok(identical(k1, k2), "the same polygon written twice gives identical KML")
  ok(!any(grepl("file[0-9a-f]{4,}", k1)) && any(grepl("Testus_astacus_EOO", k1, fixed = TRUE)),
     "the KML's Schema/Folder carry the file's name, not the temp file's")
  unlink(c(d1, d2), recursive = TRUE)
} else {
  cat("  SKIP  real KML write (sf is not loaded on this platform; runs in the Linux image build)\n")
}

cat(sprintf("\n[test_kml_determinism] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
