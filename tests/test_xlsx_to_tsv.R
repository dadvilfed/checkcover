#!/usr/bin/env Rscript
# tools/xlsx_to_tsv.R turns the emailed WoC export into the input table of
# SERVICE_CONTRACT section 2 without a spreadsheet. A spreadsheet's text save of
# the 2026-09-23 export changed 92 of 680 taxa (Windows-1252, letters lost as
# '?', quoting, decimal commas). This pins the bytes the converter writes.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("readxl", "openxlsx")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_xlsx_to_tsv] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
convert <- function(xlsx, out) {
  res <- suppressWarnings(system2(rscript, shQuote(c("tools/xlsx_to_tsv.R", xlsx, out)),
                                  stdout = TRUE, stderr = TRUE))
  st <- attr(res, "status"); if (is.null(st)) 0L else st
}
tmp <- file.path(tempdir(), "xlsx_to_tsv"); dir.create(tmp, showWarnings = FALSE)

# Built from code points, so this file's own encoding cannot matter.
salaj <- intToUtf8(c(0x53, 0x103, 0x6c, 0x61, 0x6a))                          # Sălaj
cite  <- paste0("Smith", intToUtf8(0x2019), "s \"crayfish\" 1990", intToUtf8(0x2013), "1995")
d <- data.frame(occurrenceID     = c("WA1", "WA2"),
                scientificName   = c("Astacus astacus", "Astacus astacus"),
                decimalLatitude  = c(46.63824, 45.123456789012),
                year             = c(2023, 1998),
                claimExtinction  = c(NA, "Extinct"),
                county           = c(salaj, "Bihor"),
                sourceCitation   = c(cite, NA),
                stringsAsFactors = FALSE)
xlsx <- file.path(tmp, "export.xlsx")
openxlsx::write.xlsx(d, xlsx)

out <- file.path(tmp, "out.tsv")
ok(convert(xlsx, out) == 0L, "converts a WoC-shaped xlsx (exit 0)")
bytes <- readBin(out, "raw", file.size(out))
ok(!identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf))), "no byte-order mark")
ok(!any(bytes == as.raw(0x0d)), "LF line endings, no CR")
lines <- readLines(out, encoding = "UTF-8", warn = FALSE)
ok(all(validUTF8(lines)), "every line is valid UTF-8")
ok(identical(lines[1], paste(names(d), collapse = "\t")), "header row as in the xlsx")
want2 <- paste("WA1", "Astacus astacus", "46.63824", "2023", "", salaj, cite, sep = "\t")
ok(identical(lines[2], want2),
   "a row: letters kept, quotes unquoted and undoubled, number as stored, empty cell empty")
ok(identical(strsplit(lines[3], "\t", fixed = TRUE)[[1]][3], "45.123456789012"),
   "a 12-decimal coordinate keeps every digit")
ok(identical(sub("\t[^\t]*$", "\t", lines[3]), paste("WA2", "Astacus astacus", "45.123456789012", "1998",
                                                     "Extinct", "Bihor", "", sep = "\t")),
   "a trailing empty cell is an empty last field")

ok(convert(xlsx, out) == 2L, "refuses to overwrite an existing file (exit 2)")

d_tab <- d; d_tab$sourceCitation[2] <- "a\tb"
xlsx_tab <- file.path(tmp, "tab.xlsx"); out_tab <- file.path(tmp, "tab.tsv")
openxlsx::write.xlsx(d_tab, xlsx_tab)
ok(convert(xlsx_tab, out_tab) == 1L && !file.exists(out_tab),
   "a value holding a tab is refused and nothing is written")

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_xlsx_to_tsv] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
