#!/usr/bin/env Rscript
# tools/compare_inputs.R: before the builder's first table (1.1) runs, compare
# it with the table 1.0 ran on. This pins that it matches records across the
# two header namings (county vs stateProvince), sees an added extinction claim,
# and tells whitespace, Unicode-form and case differences from real edits.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)
for (p in c("dplyr", "stringr", "stringi")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_compare_inputs] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
tmp <- file.path(tempdir(), "compare_inputs"); dir.create(tmp, showWarnings = FALSE)
put <- function(name, header, rows) {
  p <- file.path(tmp, name); con <- file(p, "wb")
  writeLines(enc2utf8(c(paste(header, collapse = "\t"), vapply(rows, paste, "", collapse = "\t"))),
             con, sep = "\n", useBytes = TRUE)
  close(con); p
}
a_nfc <- intToUtf8(c(0x53, 0x103, 0x6c, 0x61, 0x6a))            # Sălaj, composed
a_nfd <- intToUtf8(c(0x53, 0x61, 0x306, 0x6c, 0x61, 0x6a))      # Sălaj, decomposed
cols <- c("occurrenceID", "scientificName", "decimalLatitude", "decimalLongitude", "year",
          "establishmentMeans", "occurrenceOrigin", "sourceCitation", "contributor",
          "claimExtinction", "occurrenceRemarks", "continent", "country")
row <- function(id, sp, year = "2020", cite = "Smith 2020", contrib = "Ana Pop",
                claim = "", remarks = "", adm = "Bihor")
  c(id, sp, "46.5", "22.5", year, "indigenous", "native", cite, contrib, claim, remarks,
    "Europe", "Romania", adm)
old <- put("old.tsv", c(cols, "county"), list(
  row("R1", "Astacus astacus"),
  row("R2", "Astacus astacus", remarks = "dried out"),
  row("R3", "Astacus astacus", adm = a_nfc),
  row("R4", "Astacus astacus", cite = "Smith 2020"),
  row("R5", "Astacus astacus", contrib = "Ana Pop"),
  row("R6", "Pontastacus leptodactylus", year = "2019"),
  row("R8", "Faxonius limosus")))
new <- put("new.tsv", c(cols, "stateProvince"), list(
  row("R1", "Astacus astacus"),
  row("R2", "Astacus astacus", claim = "Extinct", remarks = "dried out"),
  row("R3", "Astacus astacus", adm = a_nfd),
  row("R4", "Astacus astacus", cite = "Smith  2020"),
  row("R5", "Astacus astacus", contrib = "ANA POP"),
  row("R6", "Pontastacus leptodactylus", year = "2021"),
  row("R7", "Pontastacus leptodactylus")))
taxa <- file.path(tmp, "taxa.tsv")
out <- suppressWarnings(system2(rscript, shQuote(c("tools/compare_inputs.R", old, new, taxa)),
                                stdout = TRUE, stderr = TRUE))
st <- attr(out, "status"); if (is.null(st)) st <- 0L
cat(paste0("    | ", out), sep = "\n")
line <- function(rx) out[grepl(rx, out)][1]
ok(st == 0L, "runs (exit 0)")
ok(grepl("only in old 1 \\(1 taxa\\) \\| only in new 1 \\(1 taxa\\) \\| in both 6", line("^records only")),
   "matches by occurrenceID across county/stateProvince headers; one added, one removed")
ok(grepl("^is_extinct +1 +1 +0 +0 +0 +1$", line("^is_extinct")), "an added extinction claim is seen")
ok(grepl("^extinction_remarks +1 ", line("^extinction_remarks")),
   "the claim brings its remarks into the fingerprint")
ok(grepl("^admin_1 +1 +1 +0 +1 +0 +0$", line("^admin_1")), "a decomposed letter is classed unicode-form")
ok(grepl("^citation +1 +1 +1 +0 +0 +0$", line("^citation ")), "a doubled space is classed whitespace")
ok(grepl("^contributor +1 +1 +0 +0 +1 +0$", line("^contributor")), "a case change is classed case")
ok(grepl("^year +1 +1 +0 +0 +0 +1$", line("^year")), "a changed year is a real edit")
ok(grepl("any difference: 2 \\(only an extinction claim: 0\\) \\| new taxa: 0 \\| taxa gone: 1", line("^taxa with")),
   "taxon totals: two changed, one gone")
tt <- read.delim(taxa, colClasses = "character")
ok(identical(sort(tt$package_id), c("Astacus_astacus", "Faxonius_limosus", "Pontastacus_leptodactylus")),
   "taxa.tsv lists every taxon with a difference, by package id")
ok(tt$added[tt$package_id == "Pontastacus_leptodactylus"] == "1" &&
   tt$removed[tt$package_id == "Faxonius_limosus"] == "1", "taxa.tsv counts added and removed records")
ok(!any(grepl("46.5|22.5|Smith|Ana Pop|Bihor", out)), "prints no values")

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_compare_inputs] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
