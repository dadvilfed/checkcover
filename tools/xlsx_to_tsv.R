#!/usr/bin/env Rscript
# The emailed WoC export (.xlsx) as the input table of SERVICE_CONTRACT
# section 2, without a spreadsheet in between.
#
# Saving the xlsx as "Text (Tab delimited)" from a spreadsheet looks harmless
# and is not. On the 2026-09-23 export it wrote Windows-1252 instead of UTF-8,
# and turned every letter that encoding lacks into '?' (S?laj, Mehedin?i). It
# also wrapped 2,536 fields in quotes with their inner quotes doubled, wrote
# decimal commas, and rounded one coordinate pair. 92 of 680 taxa then
# fingerprinted differently from this script's conversion of the same file.
#
# This writes every cell as the text the xlsx stores. For numbers, readxl's text
# is the stored value: all 124,830 coordinates of that export compared equal.
# The output is UTF-8 without BOM, tab-separated with no quoting, LF line
# endings, and an empty field for an empty cell. A value holding a tab or a line
# break cannot be written unquoted, so the conversion refuses it rather than
# shift a row's columns.
#
#   Rscript tools/xlsx_to_tsv.R <export.xlsx> <out.tsv>
#
# Prints counts only, never records: the export holds exact coordinates, so
# write the TSV into the run folder (/data/runs/<run_id>/input.tsv).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  cat("usage: Rscript tools/xlsx_to_tsv.R <export.xlsx> <out.tsv>\n"); quit(status = 2)
}
src <- args[1]; out <- args[2]
if (!file.exists(src)) { cat("not found:", src, "\n"); quit(status = 2) }
if (file.exists(out))  { cat("refusing to overwrite", out, "\n"); quit(status = 2) }
if (!requireNamespace("readxl", quietly = TRUE)) {
  cat("readxl is not installed: install.packages(\"readxl\")\n"); quit(status = 2)
}

sheets <- readxl::excel_sheets(src)
if (length(sheets) > 1L) {
  cat(sprintf("%d sheets (%s): reading the first, '%s'.\n", length(sheets),
              paste(sheets, collapse = ", "), sheets[1]))
}
d <- readxl::read_xlsx(src, sheet = 1, col_types = "text", na = character(0),
                       trim_ws = FALSE, .name_repair = "minimal")
d <- as.data.frame(d, stringsAsFactors = FALSE)

# A tab or a line break inside a value would shift the row's columns.
bad <- vapply(d, function(v) sum(grepl("[\t\r\n]", v)), integer(1))
if (any(names(d) == "") || any(grepl("[\t\r\n]", names(d)))) {
  cat("FAILED: a column name is empty or holds a tab or line break. Nothing written.\n")
  quit(status = 1)
}
if (any(bad > 0L)) {
  cat("FAILED: values holding a tab or a line break, which an unquoted TSV cannot carry:\n")
  for (n in names(bad)[bad > 0L]) cat(sprintf("  %-24s %d\n", n, bad[[n]]))
  cat("Nothing written. Ask WoC to clean them at the source.\n")
  quit(status = 1)
}

cells <- lapply(d, function(v) { v <- enc2utf8(v); v[is.na(v)] <- ""; v })
lines <- c(paste(enc2utf8(names(d)), collapse = "\t"),
           if (nrow(d)) do.call(paste, c(cells, sep = "\t")) else character(0))
con <- file(out, open = "wb")                 # binary: no CRLF translation on Windows
writeLines(lines, con, sep = "\n", useBytes = TRUE)
close(con)

back <- readLines(out, encoding = "bytes", warn = FALSE)
if (length(back) != nrow(d) + 1L || !all(validUTF8(back))) {
  unlink(out)
  cat("FAILED: the written file does not read back as", nrow(d) + 1L, "UTF-8 lines. Nothing kept.\n")
  quit(status = 1)
}

col_of <- function(...) { hit <- which(tolower(names(d)) %in% tolower(c(...))); if (length(hit)) hit[1] else NA }
sp <- col_of("scientificName", "Crayfish_scientific_name")
cl <- col_of("claimExtinction", "Claim_extinction")
cat(sprintf("sheet:    %s\n", sheets[1]))
cat(sprintf("records:  %d\n", nrow(d)))
cat(sprintf("columns:  %d\n", ncol(d)))
if (!is.na(sp)) cat(sprintf("taxa:     %d\n", length(unique(trimws(gsub("\\s+", " ", d[[sp]][!is.na(d[[sp]])]))))))
if (!is.na(cl)) cat(sprintf("claims:   %d\n", sum(!is.na(d[[cl]]) & nzchar(trimws(d[[cl]])))))
cat(sprintf("written:  %s (UTF-8, no BOM, tab-separated, no quoting, LF)\n", out))
