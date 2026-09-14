#!/usr/bin/env Rscript
# Preflight must find EVERY blocking problem in one pass, before any processing
# (Reviewer 1, Ecological Informatics, 2026-09):
#
#   "the software checks for the data it needs AFTER performing all the prior
#    processing steps, and then halts with an error [...] there is no way to
#    resume where you left off, so then the user is forced to repeat the
#    processing after figuring out how to solve the issue"
#
# The property that matters is completeness: a user with three things wrong must
# be told all three at once, not one per run.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_dwc_fields.R"); source("R/00_preflight.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

quiet <- function(expr) {
  tmp <- tempfile(); sink(tmp); on.exit({ sink(); unlink(tmp) }, add = TRUE)
  force(expr)
}

tmp <- file.path(tempdir(), paste0("pf_", as.integer(runif(1) * 1e8)))
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

# A complete, valid input file.
good_input <- file.path(tmp, "occ.tsv")
write.table(
  data.frame(scientificName = "Astacus astacus",
             decimalLatitude = 46, decimalLongitude = 22, year = 2020,
             establishmentMeans = "indigenous", occurrenceOrigin = "native"),
  good_input, sep = "\t", row.names = FALSE, quote = FALSE)

mk <- function(...) {
  base <- list(
    input_file = good_input,
    root_output_dir = file.path(tmp, "out"),
    vernaculars = list(path = file.path(tmp, "vern.tsv")),
    dictionaries = list(feow = file.path(tmp, "feow.tsv"),
                        hydrobasins = file.path(tmp, "hb.tsv")),
    spatial = list(hydro_dir = file.path(tmp, "hydro"),
                   hydro_files = list("6" = "l6.shp", "8" = "l8.shp", "10" = "l10.shp"),
                   feow_source = "local",
                   feow_path = file.path(tmp, "feow.shp"))
  )
  utils::modifyList(base, list(...))
}

touch <- function(p) { dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
                       writeLines("x", p) }

cat("[test_preflight]\n")

# ---- everything missing: all of it reported at once ----
res <- quiet(preflight_check(mk(), strict = FALSE))
items <- res$item
# Five file-level problems are configured above; `ecoregions` adds a sixth only
# when that package is absent, so the floor is 5.
ok(nrow(res) >= 5, sprintf("a bare setup reports every problem at once (%d)", nrow(res)))
for (want in c("vernaculars$path", "dictionaries$feow", "dictionaries$hydrobasins",
               "spatial$hydro_dir", "spatial$feow_path")) {
  ok(want %in% items, sprintf("reports missing %s", want))
}
ok(all(res$severity[res$item %in% c("vernaculars$path", "spatial$hydro_dir")] == "FATAL"),
   "missing lookups and layers are FATAL")

# ---- fixing one does not hide the rest ----
touch(file.path(tmp, "vern.tsv"))
res2 <- quiet(preflight_check(mk(), strict = FALSE))
ok(!("vernaculars$path" %in% res2$item), "a fixed item stops being reported")
ok("dictionaries$feow" %in% res2$item, "the remaining problems are still reported")
ok(nrow(res2) < nrow(res), "the finding count drops as things are fixed")

# ---- per-level HydroBASINS reporting ----
dir.create(file.path(tmp, "hydro"), showWarnings = FALSE)
touch(file.path(tmp, "hydro", "l6.shp"))
res3 <- quiet(preflight_check(mk(), strict = FALSE))
ok(any(grepl("level 8", res3$item)) && any(grepl("level 10", res3$item)),
   "names the specific HydroBASINS levels that are missing")
ok(!any(grepl("level 6", res3$item)), "does not report the level that is present")

# ---- FEOW: only the configured source has to exist ----
res4 <- quiet(preflight_check(mk(spatial = utils::modifyList(
  mk()$spatial, list(feow_source = "feowR"))), strict = FALSE))
ok(!("spatial$feow_path" %in% res4$item),
   "feow_source='feowR' does not demand the local shapefile")

# ---- input file checks ----
res5 <- quiet(preflight_check(mk(input_file = file.path(tmp, "nope.tsv")), strict = FALSE))
ok("input_file" %in% res5$item, "missing input file is reported")

bad_cols <- file.path(tmp, "bad.tsv")
write.table(data.frame(foo = 1, bar = 2), bad_cols, sep = "\t",
            row.names = FALSE, quote = FALSE)
res6 <- quiet(preflight_check(mk(input_file = bad_cols), strict = FALSE))
ok(any(grepl("scientificName", res6$item)), "missing mandatory column is reported")
ok(any(grepl("establishmentMeans", res6$item)), "missing establishmentMeans is reported")
ok(res6$severity[grepl("establishmentMeans", res6$item)] == "WARNING",
   "missing establishmentMeans is a WARNING, not FATAL (the run still reports)")

# Legacy WoC headers must satisfy the same checks as DwC names.
legacy <- file.path(tmp, "legacy.tsv")
write.table(data.frame(Crayfish_scientific_name = "Astacus astacus", Lat = 46,
                       Long = 22, Year_of_record = 2020,
                       Population_status = "indigenous"),
            legacy, sep = "\t", row.names = FALSE, quote = FALSE)
res7 <- quiet(preflight_check(mk(input_file = legacy), strict = FALSE))
ok(!any(grepl("input column", res7$item)),
   "legacy WoC column names satisfy the column check")

# ---- strict mode stops; non-strict returns ----
threw <- inherits(try(quiet(preflight_check(mk(), strict = TRUE)), silent = TRUE),
                  "try-error")
ok(threw, "strict mode stops the run before any processing")

# ---- a complete setup passes clean ----
for (p in c("feow.tsv", "hb.tsv", "feow.shp")) touch(file.path(tmp, p))
for (p in c("l8.shp", "l10.shp")) touch(file.path(tmp, "hydro", p))
res8 <- quiet(preflight_check(mk(), strict = FALSE))
fatals <- res8[res8$severity == "FATAL", , drop = FALSE]
non_pkg <- fatals[!grepl("package", fatals$item), , drop = FALSE]
ok(nrow(non_pkg) == 0,
   sprintf("a complete setup reports no file-level FATALs (%d left, package-only)",
           nrow(fatals)))

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_preflight] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
