#!/usr/bin/env Rscript
# Basin map features carry two names and the drainage topology (Lucian, 2026-09).
#
#   basin_name       root of the hierarchy ("Danube") -- unchanged from v1.2
#   basin_name_fine  leaf ("Tisza - Crisul Repede") -- the narrative's string
#   MAIN_BAS         outlet basin of the river system
#   NEXT_DOWN        immediately downstream basin ("0" at an outlet)
#
# Why: basins are used as spatial units for IUCN Green Status assessments, and
# the root name alone labels all 21 Austropotamobius bihariensis polygons
# "Danube". About one in five level-8 polygons has no name at all, and the
# topology lets a consumer resolve those through the real drainage hierarchy.
#
# Two traps pinned here:
#   * the HydroBASINS cache used to be validated by FEATURE COUNT ONLY, so a
#     cache written before these columns existed would be reused and every
#     feature exported with MAIN_BAS = NA, silently;
#   * the source stores ids as doubles, and a round id converted naively becomes
#     "3.1e+09" -- a destroyed join key.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("sf", quietly = TRUE)) {
  cat("[test_basin_feature_schema] SKIP (sf not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages({
  library(sf); source("R/00_logging.R"); source("R/00_helpers.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

cat("[test_basin_feature_schema]\n")

# ---- exact id formatting ----
ok(identical(hb_id_string(3100000000), "3100000000"),
   "round id stays exact (as.character would give '3.1e+09')")
ok(identical(as.character(3100000000), "3.1e+09"),
   "confirms the trap: naive conversion is scientific notation")
ok(identical(hb_id_string(c(2100513510, 0, NA)), c("2100513510", "0", NA)),
   "ordinary id exact, outlet NEXT_DOWN stays '0', NA stays NA")
ok(identical(hb_id_string("2080425720"), "2080425720"), "character input passes through exactly")

# ---- two granularities from one resolver ----
lut <- data.frame(
  Basin_level = c("L10", "L10", "L8"),
  HYBAS_ID    = c("2100513510", "2100513511", "2080350510"),
  Basin_name  = c("Danube", "Danube", "unnamed"),
  Subbasin_name = c("Tisza", "Tisza", ""),
  river_name  = c("Crisul Repede", "Crisul Negru", ""),
  stringsAsFactors = FALSE)
ids <- lut$HYBAS_ID

.hb_index_reset(); coarse <- resolve_basin_names(ids, lut, granularity = "coarse")$names
.hb_index_reset(); fine   <- resolve_basin_names(ids, lut)$names
.hb_index_reset(); mapf   <- resolve_basin_map_names(ids, lut)$names
.hb_index_reset(); narr   <- resolve_basin_names(paste0("L10:", ids), lut)$names

ok(identical(coarse, c("Danube", "Danube", "unnamed")), "coarse = root Basin_name")
ok(identical(fine[1:2], c("Tisza - Crisul Repede", "Tisza - Crisul Negru")),
   "fine = two finest components")
ok(length(unique(coarse[1:2])) == 1 && length(unique(fine[1:2])) == 2,
   "siblings share a root name but have distinct fine names")
ok(identical(mapf, narr), "basin_name_fine is exactly the narrative's string")
ok(identical(fine[3], "unnamed"), "an unnamed root with no finer components stays 'unnamed'")
ok(inherits(try(resolve_basin_names(ids, lut, granularity = "medium"), silent = TRUE),
            "try-error"), "an unknown granularity is rejected, not guessed")

# ---- feature schema, written and read back ----
sq <- function(x0) st_polygon(list(matrix(c(x0,46, x0+.1,46, x0+.1,46.1, x0,46.1, x0,46),
                                          ncol = 2, byrow = TRUE)))
ab <- st_sf(HB_LABEL = ids,
            MAIN_BAS  = c(3100000000, 3100000000, 2080000010),
            NEXT_DOWN = c(2100514090, 0, 2080000010),
            geometry  = st_sfc(sq(22), sq(22.2), sq(22.4), crs = 4326))
.hb_index_reset()
ab$basin_name      <- resolve_basin_names(ab$HB_LABEL, lut, granularity = "coarse")$names
.hb_index_reset()
ab$basin_name_fine <- resolve_basin_map_names(ab$HB_LABEL, lut)$names
for (fld in HB_TOPOLOGY_FIELDS) ab[[fld]] <- hb_id_string(ab[[fld]])
ab$status <- "Native"
ab <- ab[, c("HB_LABEL", "basin_name", "basin_name_fine", HB_TOPOLOGY_FIELDS, "status")]

gj <- tempfile(fileext = ".geojson")
st_write(ab, gj, quiet = TRUE)
props <- sub(', "geometry".*$', "", grep('"type": "Feature"', readLines(gj, warn = FALSE), value = TRUE)[1])
ok(grepl('"MAIN_BAS": "3100000000"', props, fixed = TRUE),
   "written GeoJSON carries MAIN_BAS as an exact quoted string")
ok(!grepl("e+0", paste(readLines(gj, warn = FALSE), collapse = ""), fixed = TRUE),
   "no scientific notation anywhere in the written file")
order_ok <- regexpr('"HB_LABEL"', props) < regexpr('"basin_name"', props) &&
            regexpr('"basin_name"', props) < regexpr('"basin_name_fine"', props) &&
            regexpr('"basin_name_fine"', props) < regexpr('"MAIN_BAS"', props) &&
            regexpr('"NEXT_DOWN"', props) < regexpr('"status"', props)
ok(order_ok, "property order: HB_LABEL, basin_name, basin_name_fine, MAIN_BAS, NEXT_DOWN, status")
back <- st_read(gj, quiet = TRUE)
ok(identical(back$NEXT_DOWN, c("2100514090", "0", "2080000010")),
   "NEXT_DOWN round-trips exactly, outlet as '0'")

# KML label is the fine name.
src <- readLines("R/08_maps.R", warn = FALSE)
b1 <- grep("^\\.write_basins_kml <- function", src)
b2 <- grep("^\\.kml_insert_placemark_names <- function", src)
b3 <- grep("^generate_all_maps_seq", src)[1]
eval(parse(text = paste(src[b1:(b2 - 3)], collapse = "\n")))
eval(parse(text = paste(src[b2:(b3 - 1)], collapse = "\n")))
km <- tempfile(fileext = ".kml")
suppressWarnings(.write_basins_kml(ab, km))
kt <- paste(readLines(km, warn = FALSE), collapse = "\n")
labels <- regmatches(kt, gregexpr("<name>[^<]*</name>", kt))[[1]][-1]  # [-1] = folder name
ok(identical(labels[1:2], c("<name>Tisza - Crisul Repede</name>", "<name>Tisza - Crisul Negru</name>")),
   "KML placemark labels are fine names, not the shared root")
ok(grepl('name="MAIN_BAS">3100000000<', kt, fixed = TRUE), "KML ExtendedData carries MAIN_BAS exactly")

# ---- source pins: the traps must stay closed ----
f02 <- paste(readLines("R/02f_hydrobasins.R", warn = FALSE), collapse = "\n")
ok(grepl("setdiff(HB_TOPOLOGY_FIELDS, names(cached))", f02, fixed = TRUE),
   "HydroBASINS cache is validated for the topology columns, not just feature count")
ok(grepl('c("HB_LABEL", HB_TOPOLOGY_FIELDS, "geometry")', f02, fixed = TRUE),
   "cache build keeps the topology columns")
f08 <- paste(src, collapse = "\n")
ok(grepl('c("HB_LABEL", HB_TOPOLOGY_FIELDS, "geometry")', f08, fixed = TRUE),
   "map export does not trim the topology columns away")
ok(grepl("hb_id_string(all_basins[[fld]])", f08, fixed = TRUE),
   "map export formats topology ids exactly")

unlink(c(gj, km))
cat(sprintf("\n[test_basin_feature_schema] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
