#!/usr/bin/env Rscript
# protected_area is a PIPE-JOINED multi-value column and must be split before
# it is tabulated.
#
# One occurrence can sit inside a national park that is itself inside a larger
# reserve, and 02e_wdpa.R records every match as "Park A | Reserve B". Counting
# that string whole makes it a category distinct from "Park A" and "Reserve B",
# so the narrative's protected-area list fragments into compound entries.
#
# This was mostly latent while WDPA geometries were cleaned with
# erase_overlaps = TRUE, which dissolved overlaps and left at most one match per
# record. CONFIG$spatial$wdpa_erase_overlaps now defaults to FALSE (it was the
# single largest runtime cost -- ~18 hours for four species; Reviewer 1,
# Ecological Informatics, 2026-09), so multi-match cells are normal and the
# split is required, not optional.
#
# 03c_reports_indigenous.R already split on the same separator for
# n_distinct_protected_areas, so the two disagreed until now.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R")
}))
# Pull the two tabulators out without running the module.
src <- readLines("R/10_canonical_narratives.R", warn = FALSE)
i0 <- grep("^\\.drop_unusable <- function", src)[1]
i1 <- grep("^\\.freq_table <- function", src)[1]
i2 <- grep("^\\.freq_table_multi <- function", src)[1]
i3 <- grep("^# =====", src); i3 <- i3[i3 > i2][1]
eval(parse(text = paste(src[i0:(i1 - 1)], collapse = "\n")))
eval(parse(text = paste(src[i1:(i2 - 1)], collapse = "\n")))
eval(parse(text = paste(src[i2:(i3 - 1)], collapse = "\n")))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

cat("[test_pa_multivalue]\n")

# Five records: three in a park, two of which are also in a nested reserve.
cells <- c("Park A",
           "Park A | Reserve B",
           "Park A | Reserve B",
           "Reserve B",
           "Park C")

old <- .freq_table(cells)
new <- .freq_table_multi(cells)

ok(nrow(old) == 4 && "Park A | Reserve B" %in% old$value,
   "unsplit tabulation invents a compound category (the defect)")
ok(setequal(new$value, c("Park A", "Reserve B", "Park C")),
   "split tabulation yields only real protected-area names")
ok(new$n[new$value == "Park A"] == 3,
   "a record in two areas counts under both: Park A = 3")
ok(new$n[new$value == "Reserve B"] == 3, "Reserve B = 3")
ok(new$n[new$value == "Park C"] == 1,   "Park C = 1")

# Totals may legitimately exceed the record count -- that is the point.
ok(sum(new$n) > length(cells),
   "counts sum above the record total, because areas nest")

# Agreement with the report module, which splits on the same separator.
n_distinct_report <- length(unique(trimws(unlist(strsplit(cells, "\\s*\\|\\s*")))))
ok(nrow(new) == n_distinct_report,
   "narrative and report module now agree on the distinct-area count")
ok(nrow(old) != n_distinct_report,
   "they disagreed before the split (what this fixes)")

# Ordering and separator tolerance.
ok(identical(new$value[1], "Park A") || identical(new$value[1], "Reserve B"),
   "ordered by frequency, descending")
ok(setequal(.freq_table_multi(c("A|B", "A | B", "A  |  B"))$value, c("A", "B")),
   "tolerates spacing variation around the separator")

# Degenerate input.
ok(nrow(.freq_table_multi(character(0))) == 0, "empty input -> empty table")
ok(nrow(.freq_table_multi(c(NA_character_, ""))) == 0, "NA and blank -> empty table")
ok(nrow(.freq_table_multi("Solo Park")) == 1, "single value still works")
ok(setequal(.freq_table_multi(c("A | ", " | B"))$value, c("A", "B")),
   "empty segments around a separator are dropped")

cat(sprintf("\n[test_pa_multivalue] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
