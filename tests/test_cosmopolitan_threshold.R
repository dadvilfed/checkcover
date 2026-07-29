#!/usr/bin/env Rscript
# FIX 2 (Lucian, 2026-07): `n_continents > 1` was too permissive — one stray or
# unlabelled record was enough to call a species cosmopolitan. A continent now
# counts only if it holds >=5 records AND >=5% of the species' labelled records,
# and blank/NA labels are excluded outright.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)
suppressWarnings(suppressMessages(source("R/00_helpers.R")))

PASS <- 0L; FAIL <- 0L
ok <- function(l, c) { if (isTRUE(c)) { PASS <<- PASS + 1L; cat("  ok  ", l, "\n") }
                       else           { FAIL <<- FAIL + 1L; cat("  FAIL", l, "\n") } }
rep2 <- function(a, na, b, nb) c(rep(a, na), rep(b, nb))

cat("== real cases Lucian flagged (must NOT be cosmopolitan) ==\n")
# A. torrentium: Europe 3480 / Asia 4 -> Asia fails both bars (4 < 5, 0.1% < 5%)
ok("A. torrentium (Europe 3480 / Asia 4) -> 1",
   count_continents(rep2("Europe", 3480, "Asia", 4)) == 1L)
# O. pellucidus: Kentucky cave endemic, 1 stray European record
ok("O. pellucidus (N.America 25 / Europe 1) -> 1",
   count_continents(rep2("North America", 25, "Europe", 1)) == 1L)
# E. spinifer / suttoni: a BLANK label was counting as a second continent
ok("E. spinifer (Oceania 101 / blank 2) -> 1",
   count_continents(rep2("Oceania", 101, "", 2)) == 1L)
ok("E. suttoni (Oceania 60 / blank 5) -> 1",
   count_continents(rep2("Oceania", 60, NA_character_, 5)) == 1L)
# P. varicosus: 2 N.American records out of 18 -> 11% share but only 2 records
ok("P. varicosus (S.America 16 / N.America 2) -> 1",
   count_continents(rep2("South America", 16, "North America", 2)) == 1L)

cat("== genuine cosmopolitans (must STAY cosmopolitan) ==\n")
ok("P. leptodactylus (Europe 1178 / Asia 519) -> 2",
   count_continents(rep2("Europe", 1178, "Asia", 519)) == 2L)
ok("P. eichwaldi (Europe 11 / Asia 7) -> 2",
   count_continents(rep2("Europe", 11, "Asia", 7)) == 2L)
ok("P. pachypus (Asia 9 / Europe 6) -> 2",
   count_continents(rep2("Asia", 9, "Europe", 6)) == 2L)

cat("== threshold boundaries ==\n")
ok("exactly 5 records AND exactly 5% counts",
   count_continents(c(rep("Europe", 95), rep("Asia", 5))) == 2L)
ok("5 records but 4.8% share fails",
   count_continents(c(rep("Europe", 99), rep("Asia", 5))) == 1L)
ok("20% share but only 4 records fails",
   count_continents(c(rep("Europe", 16), rep("Asia", 4))) == 1L)

cat("== degenerate inputs ==\n")
ok("all blank -> 0",              count_continents(c("", "", NA)) == 0L)
ok("empty vector -> 0",           count_continents(character(0)) == 0L)
ok("no continent clears bar -> 1 (dominant kept, never 0)",
   count_continents(c("Europe", "Asia", "Africa")) == 1L)
ok("single continent -> 1",       count_continents(rep("Europe", 40)) == 1L)
ok("blanks excluded from share denominator",
   count_continents(c(rep("Europe", 50), rep("Asia", 50), rep("", 900))) == 2L)

cat(sprintf("\n[test_cosmopolitan_threshold] %d passed, %d failed\n", PASS, FAIL))
quit(status = if (FAIL > 0) 1 else 0)
