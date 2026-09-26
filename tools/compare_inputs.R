#!/usr/bin/env Rscript
# What differs between two input tables, the way cheCkOVER reads them.
#
# Before WoC's builder delivers its first table (1.1, the runner's first job),
# compare it with the table 1.0 ran on. 1.0 came from the emailed xlsx. Any
# text the builder writes differently reads as "changed", even with nothing
# changed in WoC, and the revision rebuilds the taxon. This finds that in
# advance: both tables go through the real ingest mapping (headers in either
# naming, e.g. county or stateProvince), are matched by occurrenceID, and every
# column that feeds the fingerprint is compared as the fingerprint sees it.
# A text difference is classed as:
#   whitespace     equal once runs of spaces are collapsed and the ends trimmed
#   unicode-form   equal once both are in Unicode NFC (e.g. a decomposed ă)
#   case           equal ignoring case
#   other          a real edit
#
#   Rscript tools/compare_inputs.R <old.tsv> <new.tsv> [taxa.tsv]
#
# Prints counts only, never values. The optional taxa.tsv lists each taxon
# with a difference: its package id, its record counts, and the columns that
# differ. Duplicate consolidation is not simulated, so a difference inside a
# duplicate group can be counted where the fingerprint would see none.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 2:3) {
  cat("usage: Rscript tools/compare_inputs.R <old.tsv> <new.tsv> [taxa.tsv]\n"); quit(status = 2)
}
for (f in args[1:2]) if (!file.exists(f)) { cat("not found:", f, "\n"); quit(status = 2) }

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
paths <- normalizePath(args[1:2])
out_taxa <- if (length(args) == 3L) args[3] else NULL
if (!is.null(out_taxa) && file.exists(out_taxa)) { cat("refusing to overwrite", out_taxa, "\n"); quit(status = 2) }
owd <- setwd(.root)
suppressWarnings(suppressMessages({
  library(dplyr); library(stringr)
  source("config.R")
  source("R/00_logging.R"); source("R/00_helpers.R")
  source("R/00_geo_canon.R"); source("R/00_dwc_fields.R"); source("R/01_ingest.R")
}))
log_info <- log_warn <- log_debug <- log_error <- function(...) invisible(NULL)

read_mapped <- function(p) {
  d <- suppressWarnings(map_woc_to_checkover(load_woc_data(p)))
  d$species <- normalize_species_name(d$species)
  d$package_id <- make_package_id(d$species)
  d$record_id <- as.character(d$record_id)
  d
}
a <- read_mapped(paths[1]); b <- read_mapped(paths[2])
setwd(owd)

cat(sprintf("old  %8d records  %4d taxa   %s\n", nrow(a), length(unique(a$package_id)), basename(paths[1])))
cat(sprintf("new  %8d records  %4d taxa   %s\n", nrow(b), length(unique(b$package_id)), basename(paths[2])))
for (w in list(list(a, "old"), list(b, "new"))) {
  d <- w[[1]]
  if (anyNA(d$record_id)) cat(sprintf("  %s: %d records have no occurrenceID and cannot be matched\n", w[[2]], sum(is.na(d$record_id))))
  if (anyDuplicated(na.omit(d$record_id))) cat(sprintf("  %s: occurrenceID repeats %d times; the first is used\n", w[[2]], sum(duplicated(na.omit(d$record_id)))))
}

ids_a <- unique(na.omit(a$record_id)); ids_b <- unique(na.omit(b$record_id))
only_a <- setdiff(ids_a, ids_b); only_b <- setdiff(ids_b, ids_a); both <- intersect(ids_a, ids_b)
A <- a[match(both, a$record_id), , drop = FALSE]; B <- b[match(both, b$record_id), , drop = FALSE]
moved <- A$package_id != B$package_id
cat(sprintf("\nrecords only in old %d (%d taxa) | only in new %d (%d taxa) | in both %d | moved to another taxon %d\n",
            length(only_a), length(unique(a$package_id[a$record_id %in% only_a])),
            length(only_b), length(unique(b$package_id[b$record_id %in% only_b])),
            length(both), sum(moved)))

# The input-side columns of the fingerprint (R/00_run_context.R), as mapped.
cols <- c("longitude", "latitude", "year", "is_extinct", "extinction_remarks",
          "is_type_locality", "population_status", "status", "occurrence_origin",
          "accuracy", "doi", "url", "citation", "contributor",
          "confidentiality_level", "is_sensitive", "country", "continents", "admin_1")
same <- function(x, y) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
ws   <- function(x) trimws(gsub("\\s+", " ", x))
kind <- function(x, y) {
  k <- rep("other", length(x))
  one_na <- is.na(x) != is.na(y)
  x2 <- ifelse(is.na(x), "", as.character(x)); y2 <- ifelse(is.na(y), "", as.character(y))
  k[!one_na & ws(x2) == ws(y2)] <- "whitespace"
  nfc <- !one_na & k == "other" & stringi::stri_trans_nfc(x2) == stringi::stri_trans_nfc(y2)
  k[nfc] <- "unicode-form"
  k[!one_na & k == "other" & tolower(x2) == tolower(y2)] <- "case"
  k
}

cat("\ncolumn                 records   taxa  whitespace unicode-form   case   other\n")
diff_cols <- setNames(vector("list", length(both)), NULL)
dif_any <- moved
cols_by_rec <- rep("", length(both))
for (col in cols) {
  x <- A[[col]]; y <- B[[col]]
  d <- !same(x, y)
  if (!any(d)) next
  dif_any <- dif_any | d
  cols_by_rec[d] <- paste0(cols_by_rec[d], ifelse(nzchar(cols_by_rec[d]), ",", ""), col)
  k <- if (is.character(x)) table(factor(kind(x[d], y[d]), c("whitespace", "unicode-form", "case", "other")))
       else c(whitespace = 0, `unicode-form` = 0, case = 0, other = sum(d))
  cat(sprintf("%-21s %8d %6d  %10d %12d %6d %7d\n", col, sum(d), length(unique(A$package_id[d])),
              k[["whitespace"]], k[["unicode-form"]], k[["case"]], k[["other"]]))
}

tax_changed <- unique(c(A$package_id[dif_any], B$package_id[moved],
                        a$package_id[a$record_id %in% only_a], b$package_id[b$record_id %in% only_b]))
claim_cols <- c("is_extinct", "extinction_remarks")
claim_only <- vapply(tax_changed, function(t) {
  r <- A$package_id == t & dif_any
  added_or_removed <- any(a$package_id[a$record_id %in% only_a] == t) || any(b$package_id[b$record_id %in% only_b] == t)
  !added_or_removed && !any(moved & (A$package_id == t | B$package_id == t)) &&
    all(unlist(strsplit(cols_by_rec[r], ",")) %in% claim_cols)
}, logical(1))
new_taxa <- setdiff(unique(b$package_id), unique(a$package_id))
gone_taxa <- setdiff(unique(a$package_id), unique(b$package_id))
cat(sprintf("\ntaxa with any difference: %d (only an extinction claim: %d) | new taxa: %d | taxa gone: %d\n",
            length(setdiff(tax_changed, c(new_taxa, gone_taxa))), sum(claim_only[!tax_changed %in% c(new_taxa, gone_taxa)]),
            length(new_taxa), length(gone_taxa)))

if (!is.null(out_taxa)) {
  rows <- lapply(sort(tax_changed), function(t) {
    r <- A$package_id == t & dif_any
    data.frame(package_id = t,
               records_old = sum(a$package_id == t), records_new = sum(b$package_id == t),
               added = sum(b$package_id[b$record_id %in% only_b] == t),
               removed = sum(a$package_id[a$record_id %in% only_a] == t),
               changed = sum(r),
               columns = paste(sort(unique(unlist(strsplit(cols_by_rec[r], ",")))), collapse = ","),
               stringsAsFactors = FALSE)
  })
  tt <- if (length(rows)) do.call(rbind, rows) else
    data.frame(package_id = character(), records_old = integer(), records_new = integer(),
               added = integer(), removed = integer(), changed = integer(), columns = character())
  con <- file(out_taxa, open = "wb")
  write.table(tt, con, sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  close(con)
  cat("taxa listed in", out_taxa, "\n")
}
