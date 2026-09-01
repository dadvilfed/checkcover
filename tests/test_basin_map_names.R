#!/usr/bin/env Rscript
# ONE basin-name resolver, shared by narratives, reports and map exports.
#
# Until 2026-08 there were four implementations: .resolve_basin_col() in the
# narrative module, .resolve_basin_3c() in BOTH report modules (04c's copy
# silently overwriting 03c's, since it is sourced later), and a coarse
# Basin_name-only lookup in the maps. Two of Lucian's four reported defects came
# straight out of that:
#
#   * the geojson said "Danube" for all 21 Austropotamobius bihariensis basins
#     while the narrative named 11 of them -- and 357 of 676 species had a
#     single distinct name across every basin they occupy;
#   * .resolve_basin_3c() had no case-insensitive rescue for the lookup's column
#     names, so in another case it returned the RAW CODES. Distinct raw codes ==
#     distinct units, so n_named_basins collapsed to n_hydrobasins and the
#     summary sentence quoted the unit count as the name count (483/676).
#
# The contract this pins: the string on a geojson feature is the string the
# narrative prints for that basin.
#
# sf-free on purpose (sf segfaults under the harness on this machine).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

# Shaped like the real bihariensis case: one coarse basin, distinct rivers.
lut <- data.frame(
  Basin_level   = c("L6",         "L10",        "L10",        "L10",        "L8"),
  HYBAS_ID      = c("2060000010", "2100513510", "2100513511", "2100513512", "2080350510"),
  Basin_name    = c("Danube",     "Danube",     "Danube",     "Danube",     "unnamed"),
  Subbasin_name = c("",           "Tisza",      "Tisza",      "Tisza",      ""),
  river_name    = c("",           "Crisul Alb", "Crisul Negru", "",         ""),
  stringsAsFactors = FALSE
)

cat("[test_basin_map_names]\n")

# ---- the defect Lucian reported: coarse names collapse, cascade does not ----
.hb_index_reset()
ids <- c("2100513510", "2100513511", "2100513512")
nm  <- resolve_basin_names(ids, lut)$names
ok(length(unique(nm)) == 3L,
   "three level-10 basins under one Basin_name resolve to three distinct names")
ok(identical(nm[1], "Tisza - Crisul Alb"), "finest label is basin > subbasin > river")
ok(identical(nm[3], "Danube - Tisza"),     "falls back to the two finest available")
ok(!identical(nm[1], nm[2]), "sibling rivers are not collapsed together")

# ---- geojson string == narrative string ----
.hb_index_reset()
map_side <- resolve_basin_map_names(ids, lut)$names
.hb_index_reset()
narr_side <- resolve_basin_names(paste0("L10:", ids), lut)$names
ok(identical(map_side, narr_side),
   "map wrapper and narrative path return identical strings")

# ---- prefixed and bare codes are the same key ----
.hb_index_reset()
ok(identical(resolve_basin_names("L10:2100513510", lut)$names,
             resolve_basin_names("2100513510", lut)$names),
   "'L10:<id>' and bare '<id>' resolve identically")
.hb_index_reset()
ok(identical(resolve_basin_names("L6:2060000010", lut)$names, "Danube"),
   "level prefix that disagrees with the row still matches on the id")

# ---- the case-insensitivity rescue .resolve_basin_3c lacked ----
lut_lower <- lut; names(lut_lower) <- tolower(names(lut_lower))
.hb_index_reset()
lo <- resolve_basin_names(ids, lut_lower)$names
.hb_index_reset()
hi <- resolve_basin_names(ids, lut)$names
ok(identical(lo, hi), "lowercase lookup columns resolve identically (no raw-code fallback)")
ok(!any(lo %in% ids), "lowercase columns never leak raw codes into the output")

lut_upper <- lut; names(lut_upper) <- toupper(names(lut_upper))
.hb_index_reset()
ok(identical(resolve_basin_names(ids, lut_upper)$names, hi),
   "uppercase lookup columns resolve identically")

# ---- "unnamed" is a value, not a failure ----
.hb_index_reset()
r <- resolve_basin_names("2080350510", lut)
ok(identical(r$names, "unnamed"), "literal 'unnamed' Basin_name is carried through")
ok(r$n_unmatched == 0L, "'unnamed' counts as matched, not unmatched")

.hb_index_reset()
r <- resolve_basin_names(c("2100513510", "9999999999"), lut)
ok(identical(r$names, c("Tisza - Crisul Alb", "unnamed")),
   "id absent from the lookup yields 'unnamed', never NA")
ok(r$n_unmatched == 1L && r$n_named == 1L, "unmatched and named counted separately")

# ---- counts the callers depend on ----
.hb_index_reset()
r <- resolve_basin_names(c(ids, "2080350510"), lut)
ok(length(r$names) == 4L, "output length equals input length")
ok(!any(is.na(r$names)) && all(nzchar(r$names)), "no NA and no empty string in any result")
ok(r$n_named == 3L && r$n_unnamed == 1L, "named/unnamed split is correct")

# n_named must count NAMES, not units -- the bug behind the summary sentence.
.hb_index_reset()
many <- resolve_basin_names(c("2100513510", "2100513510", "2100513511"), lut)
ok(length(unique(many$names)) == 2L,
   "repeated ids collapse to distinct names (names != units)")

# ---- degradation ----
.hb_index_reset()
ok(identical(resolve_basin_names(c("a", "b"), NULL)$names, c("unnamed", "unnamed")),
   "missing lookup degrades to 'unnamed'")
.hb_index_reset()
ok(identical(resolve_basin_names("2100513510", lut[, c("HYBAS_ID", "Basin_name")])$names,
             "Danube"),
   "lookup without Subbasin/river columns still resolves the coarse name")
.hb_index_reset()
ok(identical(resolve_basin_names(2100513510, lut)$names, "Tisza - Crisul Alb"),
   "numeric id coerced to character before matching")

# ---- the narrative cascade helper itself is unchanged ----
ok(identical(basin_display_name("Danube", "Tisza", "Crisul Alb"), "Tisza - Crisul Alb"),
   "basin_display_name still returns the two finest components")

cat(sprintf("\n[test_basin_map_names] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
