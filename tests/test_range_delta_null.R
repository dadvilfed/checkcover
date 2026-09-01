#!/usr/bin/env Rscript
# Regression: calculate_range_delta() must survive a previous snapshot whose
# EOO_km2 / AOO_km2 is NULL rather than NA.
#
# A species with <3 records has an undefined EOO. That serialises to JSON null
# and reads back as NULL, so previous_metrics$EOO_km2 is NULL, not NA. Then:
#     is.na(NULL)              -> logical(0)
#     FALSE || logical(0)      -> NA
#     if (NA)                  -> "missing value where TRUE/FALSE needed"
# which killed Phase 5C for Cherax longipes, lorentzi, misolicus and solus
# (2026-08), traced to 11_temporal_delta.R#679.
#
# The asymmetry that hid it for so long: pct() already guarded is.null(), so
# *_change_percent was fine and only *_change_absolute crashed. And a species
# whose previous EOO was a genuine NA (Cherax pallidus) never tripped it,
# because is.na(NA) short-circuits the || before the second operand is reached.
# Both survivors are pinned below so the asymmetry cannot come back.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("dplyr", "tibble")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_range_delta_null] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/11_temporal_delta.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

cat("[test_range_delta_null]\n")

# The exact R semantics that produced the bug — pinned so nobody "simplifies"
# the guard back to is.na() alone.
ok(length(is.na(NULL)) == 0L, "is.na(NULL) is logical(0), not FALSE")
ok(is.na(FALSE || logical(0)), "FALSE || logical(0) evaluates to NA")
ok(inherits(try(if (is.na(NULL) || is.na(5)) 1, silent = TRUE), "try-error"),
   "if (is.na(NULL) || is.na(x)) raises the reported error")

occ <- data.frame(
  temporal_status = "active",
  longitude = c(140.1, 140.2, 140.3),
  latitude  = c(-4.1, -4.2, -4.3),
  stringsAsFactors = FALSE
)

run <- function(prev) try(calculate_range_delta(occ, prev), silent = TRUE)

# 1. The crash case: previous EOO absent (JSON null -> NULL).
r <- run(list(AOO_km2 = 12))
ok(!inherits(r, "try-error"),
   sprintf("previous metrics with EOO_km2 ABSENT%s",
           if (inherits(r, "try-error"))
             paste0(" -- ", trimws(conditionMessage(attr(r, "condition")))) else ""))
if (!inherits(r, "try-error")) {
  ok(is.na(r$EOO_change_absolute), "absent previous EOO -> EOO_change_absolute = NA")
  ok(is.na(r$EOO_change_percent),  "absent previous EOO -> EOO_change_percent = NA")
  ok(!is.null(r$AOO_change_absolute), "the other metric still computes")
}

# 2. Explicit NULL, same thing.
ok(!inherits(run(list(EOO_km2 = NULL, AOO_km2 = 12)), "try-error"),
   "previous metrics with EOO_km2 = NULL")

# 3. Both absent -> everything NA, signal "unknown", still no error.
r <- run(list())
ok(!inherits(r, "try-error"), "previous metrics entirely empty")
if (!inherits(r, "try-error"))
  ok(identical(r$range_signal, "unknown"), "both metrics missing -> signal 'unknown'")

# 4. The survivors must keep working exactly as before.
r <- run(list(EOO_km2 = NA_real_, AOO_km2 = 12))
ok(!inherits(r, "try-error") && is.na(r$EOO_change_absolute),
   "previous EOO = NA still works (the Cherax pallidus case)")

r <- run(list(EOO_km2 = 100, AOO_km2 = 12))
ok(!inherits(r, "try-error") && !is.na(r$EOO_change_absolute),
   "real previous EOO still computes a delta (the Cherax murido case)")

# 5. Zero and malformed previous values degrade instead of erroring.
ok(!inherits(run(list(EOO_km2 = 0, AOO_km2 = 12)), "try-error"),
   "previous EOO = 0 does not divide by zero")
r <- run(list(EOO_km2 = c(1, 2), AOO_km2 = 12))
ok(!inherits(r, "try-error") && is.na(r$EOO_change_absolute),
   "malformed multi-value previous EOO degrades to NA")
ok(!inherits(run(list(EOO_km2 = numeric(0), AOO_km2 = 12)), "try-error"),
   "zero-length previous EOO degrades to NA")

cat(sprintf("\n[test_range_delta_null] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
