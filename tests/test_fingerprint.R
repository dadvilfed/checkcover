#!/usr/bin/env Rscript
# Change-detection fingerprint must be row-order-independent even when the
# DwC-aligned template omits occurrenceID (record_id all NA) — otherwise sparse
# versioning spuriously reprocesses or mis-matches on the v1.0→v1.1 transition.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("digest", quietly = TRUE)) {
  cat("[test_fingerprint] SKIP (digest not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages(source("R/00_run_context.R")))

# New-template data: record_id all NA. Same content, shuffled rows.
d1 <- data.frame(record_id = NA_character_, longitude = c(22,23,24), latitude = c(46,47,48),
                 year = c(1990,1995,2000), is_extinct = c(FALSE,FALSE,TRUE),
                 population_status = "indigenous", accuracy = "exact", stringsAsFactors = FALSE)
d2 <- d1[c(3,1,2), ]
fp1 <- compute_species_fingerprint(d1); fp2 <- compute_species_fingerprint(d2)

# A real change (an extinction claim) MUST change the fingerprint.
d3 <- d1; d3$is_extinct <- c(FALSE,FALSE,FALSE)
fp3 <- compute_species_fingerprint(d3)

# Legacy data with real record_ids stays order-independent (sorted by record_id).
e1 <- data.frame(record_id = c("A","B","C"), longitude = c(22,23,24), latitude = c(46,47,48),
                 year = c(1990,1995,2000), is_extinct = FALSE, population_status = "indigenous",
                 accuracy = "exact", stringsAsFactors = FALSE)
e2 <- e1[c(3,2,1), ]

# Occurrence origin is `status` without the WoRMS join and `status.x` with it.
# Either way a native<->alien correction MUST change the fingerprint, and the
# two spellings of the same data must fingerprint identically.
s1 <- transform(e1, status = c("native", "native", "native"))
s2 <- transform(e1, status = c("native", "alien",  "native"))
s3 <- transform(e1, status.x = c("native", "native", "native"))
origin_ok <- !identical(compute_species_fingerprint(s1), compute_species_fingerprint(s2)) &&
             identical(compute_species_fingerprint(s1), compute_species_fingerprint(s3))

pass <- identical(fp1, fp2) && !identical(fp1, fp3) &&
        identical(compute_species_fingerprint(e1), compute_species_fingerprint(e2)) &&
        origin_ok

cat(sprintf("[test_fingerprint] NA-id order-independent:%s  detects-change:%s  legacy-ok:%s  origin:%s -> %s\n",
            identical(fp1, fp2), !identical(fp1, fp3),
            identical(compute_species_fingerprint(e1), compute_species_fingerprint(e2)),
            origin_ok, if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
