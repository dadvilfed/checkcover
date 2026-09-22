#!/usr/bin/env Rscript
# ============================================================================
# cheCkOVER — pick N taxa of mixed sizes, for timing runs (section 09)
# ============================================================================
# Reads an input table, counts records per taxon (by package id, with the same
# name rule cheCkOVER applies), and picks N taxa spread evenly over the range of
# record counts, from the smallest to the largest. Deterministic: the same
# table and N always give the same taxa.
#
# Prints the chosen ids, comma-separated, on the last line (for
# tools/service_trial.sh), after a table of id and record count. Counts and
# names only: no coordinates.
#
# Usage:
#   Rscript tools/pick_taxa.R <input.tsv> <N> [ids_out.txt]
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) { cat("Usage: Rscript tools/pick_taxa.R <input.tsv> <N> [ids_out.txt]\n"); quit(status = 64) }
input <- args[1]; n <- as.integer(args[2]); out <- if (length(args) >= 3) args[3] else NULL

root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), ".."))
suppressWarnings(suppressMessages(source(file.path(root, "R", "00_helpers.R"))))

hdr <- names(utils::read.delim(input, sep = "\t", nrows = 1, check.names = FALSE, quote = ""))
col <- intersect(c("scientificName", "Crayfish_scientific_name"), hdr)[1]
if (is.na(col)) stop("No scientificName (or Crayfish_scientific_name) column in ", input)
names_raw <- utils::read.delim(input, sep = "\t", quote = "", colClasses = "character",
                               na.strings = c("", "NA", "N/A"), check.names = FALSE)[[col]]
names_raw <- names_raw[!is.na(names_raw) & nzchar(trimws(names_raw))]

ids <- make_package_id(normalize_species_name(trimws(names_raw)))
tab <- sort(table(ids))
if (n > length(tab)) stop(sprintf("Asked for %d taxa; the table has %d.", n, length(tab)))

pos <- if (n == 1L) ceiling(length(tab) / 2) else unique(round(seq(1, length(tab), length.out = n)))
pick <- tab[pos]

cat(sprintf("%d taxa in %s; picked %d, spread over record counts %d to %d:\n",
            length(tab), basename(input), length(pick), min(pick), max(pick)))
for (i in seq_along(pick)) cat(sprintf("  %-45s %7d records\n", names(pick)[i], pick[[i]]))
cat(sprintf("  total %d records\n\n", sum(pick)))
line <- paste(names(pick), collapse = ",")
if (!is.null(out)) { writeLines(names(pick), out); cat(sprintf("ids written to %s\n", out)) }
cat(line, "\n", sep = "")
