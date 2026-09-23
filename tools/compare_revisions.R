#!/usr/bin/env Rscript
# ============================================================================
# cheCkOVER — compare two revision folders (UVT server setup, section 08)
# ============================================================================
# Lists every difference between two builds of the same revision, e.g. the
# demo run on the host and in the container:
#
#   * files present on one side only
#   * files that differ, and for each one HOW:
#       - JSON: which keys differ, with both values (maps/ excepted, below)
#       - map layers: vertex counts and the largest coordinate difference only
#       - text (md, txt, csv, tsv, bib, cff, kml): how many lines differ, and
#         the first few, side by side
#   * differences expected by design (dates, run id, code version) are listed
#     separately from the rest
#
# Map geometries are never printed: an EOO hull's corners are exact record
# locations. Everything else in a revision folder is coordinate-free by rule.
#
# Usage:
#   Rscript tools/compare_revisions.R <revision_A> <revision_B> [report.md]
# Exit code: 0 identical, 1 only expected differences, 2 other differences.
# ============================================================================

suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: Rscript tools/compare_revisions.R <revision_A> <revision_B> [report.md]\n")
  quit(status = 64)
}
A <- normalizePath(args[1], mustWork = TRUE); B <- normalizePath(args[2], mustWork = TRUE)
REPORT <- if (length(args) >= 3) args[3] else NULL

# Keys and text that legitimately differ between two runs of the same revision.
VOLATILE_KEYS <- c("generated", "generated_date", "generated_utc", "generated_at",
                   "processing_date", "script_version", "run_id", "code_version",
                   "audited_at", "checked_at", "updated_at", "started_at", "version_dir",
                   "date-released", "timestamp", "script_run_time")
VOLATILE_LINE <- "(Generated|generated|date-released|version: \"[0-9]{4}-|[0-9]{4}-[0-9]{2}-[0-9]{2})"

# Dates and timestamps replaced by a placeholder. Two values or lines that are
# equal once masked differ only in when they were written, which two runs of
# the same revision on different days always do. Proven per value, not assumed.
mask_dates <- function(s) {
  s <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}([T ][0-9]{2}:[0-9]{2}(:[0-9]{2})?(\\.[0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?)?",
            "<date>", s, perl = TRUE)
  gsub("[0-9]{8}_[0-9]{6}", "<stamp>", s, perl = TRUE)
}

rel_files <- function(d) sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE))
fa <- rel_files(A); fb <- rel_files(B)
only_a <- setdiff(fa, fb); only_b <- setdiff(fb, fa); both <- intersect(fa, fb)

md5a <- tools::md5sum(file.path(A, both)); md5b <- tools::md5sum(file.path(B, both))
differ <- both[unname(md5a) != unname(md5b)]

flatten <- function(x, prefix = "") {
  if (is.list(x) && length(x)) {
    nm <- names(x); if (is.null(nm)) nm <- as.character(seq_along(x))
    unlist(lapply(seq_along(x), function(i)
      flatten(x[[i]], if (nzchar(prefix)) paste0(prefix, ".", nm[i]) else nm[i])), use.names = TRUE)
  } else {
    v <- if (is.null(x) || !length(x)) "null" else paste(as.character(x), collapse = ",")
    stats::setNames(v, if (nzchar(prefix)) prefix else "(root)")
  }
}
leaf <- function(path) sub("^.*\\.", "", path)
short <- function(s, n = 70) ifelse(nchar(s) > n, paste0(substr(s, 1, n - 3), "..."), s)

coords_of <- function(g) {           # every number under "coordinates", in order
  out <- numeric(0)
  walk <- function(x, inside = FALSE) {
    if (is.list(x)) { nm <- names(x); for (i in seq_along(x))
      walk(x[[i]], inside || (!is.null(nm) && identical(nm[i], "coordinates"))) }
    else if (inside && is.numeric(x)) out <<- c(out, x)
  }
  walk(g); out
}

lines_out <- character(0)
say <- function(...) lines_out <<- c(lines_out, sprintf(...))
expected_only <- character(0); substantive <- character(0)

for (f in differ) {
  pa <- file.path(A, f); pb <- file.path(B, f)
  ext <- tolower(tools::file_ext(f)); in_maps <- grepl("(^|/)maps/", f)

  if (in_maps && ext %in% c("geojson", "json")) {
    ca <- coords_of(fromJSON(pa, simplifyVector = FALSE)); cb <- coords_of(fromJSON(pb, simplifyVector = FALSE))
    msg <- if (length(ca) != length(cb)) sprintf("%d vs %d coordinate values (geometry differs)", length(ca), length(cb))
           else sprintf("same %d coordinate values; largest difference %.3g degrees", length(ca), max(abs(ca - cb)))
    kind <- "substantive"
    if (length(ca) == length(cb)) {
      # identical geometry: the difference is in the properties
      pa_p <- flatten(lapply(fromJSON(pa, simplifyVector = FALSE)$features, `[[`, "properties"))
      pb_p <- flatten(lapply(fromJSON(pb, simplifyVector = FALSE)$features, `[[`, "properties"))
      if (max(abs(ca - cb)) == 0 && identical(pa_p, pb_p)) { kind <- "expected"; msg <- paste(msg, "(formatting only)") }
    }
    say("- `%s` (map): %s", f, msg)
    if (kind == "expected") expected_only <- c(expected_only, f) else substantive <- c(substantive, f)
    next
  }

  if (ext %in% c("json", "geojson")) {
    ja <- tryCatch(flatten(fromJSON(pa, simplifyVector = FALSE)), error = function(e) NULL)
    jb <- tryCatch(flatten(fromJSON(pb, simplifyVector = FALSE)), error = function(e) NULL)
    if (is.null(ja) || is.null(jb)) { say("- `%s`: not valid JSON on one side", f); substantive <- c(substantive, f); next }
    keys <- union(names(ja), names(jb))
    va <- unname(ja[keys]); vb <- unname(jb[keys])
    is_diff <- is.na(va) | is.na(vb) | va != vb
    is_diff[is.na(is_diff)] <- TRUE
    dk <- keys[is_diff]
    only_dates <- dk[!is.na(ja[dk]) & !is.na(jb[dk]) & mask_dates(ja[dk]) == mask_dates(jb[dk])]
    vol <- unique(c(dk[leaf(dk) %in% VOLATILE_KEYS], only_dates)); real <- setdiff(dk, vol)
    say("- `%s`: %d key(s) differ%s", f, length(dk),
        if (length(vol)) sprintf(", %d of them only in dates/run identity (%s)", length(vol),
                                 paste(unique(leaf(vol)), collapse = ", ")) else "")
    for (k in head(real, 12)) say("    - `%s`: `%s` vs `%s`", k, short(ja[k] %||% "(absent)"), short(jb[k] %||% "(absent)"))
    if (length(real) > 12) say("    - ... and %d more", length(real) - 12)
    if (length(real)) substantive <- c(substantive, f) else expected_only <- c(expected_only, f)
    next
  }

  la <- readLines(pa, warn = FALSE, encoding = "UTF-8"); lb <- readLines(pb, warn = FALSE, encoding = "UTF-8")
  n <- max(length(la), length(lb)); la <- c(la, rep("", n - length(la))); lb <- c(lb, rep("", n - length(lb)))
  d <- which(la != lb)
  vol <- d[mask_dates(la[d]) == mask_dates(lb[d])]
  real <- setdiff(d, vol)
  if (f %in% c("file_manifest.csv") || grepl("(^|/)file_manifest\\.csv$", f)) {
    say("- `%s`: %d line(s) differ (checksums of the files above)", f, length(d))
    expected_only <- c(expected_only, f); next
  }
  say("- `%s`: %d line(s) differ%s", f, length(d),
      if (length(vol)) sprintf(", %d of them only in dates", length(vol)) else "")
  if (!in_maps) for (i in head(real, 5)) say("    - line %d: `%s` vs `%s`", i, short(la[i]), short(lb[i]))
  if (in_maps && length(real)) {
    # Map lines can hold coordinates, so they are shown with every digit
    # masked: the SHAPE of the difference (a tag, an attribute, the number of
    # decimals) is visible, a location is not.
    digits_only <- real[gsub("[0-9]", "#", la[real]) == gsub("[0-9]", "#", lb[real])]
    say("    - %d line(s) differ in more than dates; %d of them only in digits (map lines shown with digits masked)",
        length(real), length(digits_only))
    for (i in head(setdiff(real, digits_only), 3))
      say("    - line %d: `%s` vs `%s`", i, short(gsub("[0-9]", "#", la[i]), 110), short(gsub("[0-9]", "#", lb[i]), 110))
    for (i in head(digits_only, 2))
      say("    - line %d (digits only): `%s` vs `%s`", i, short(gsub("[0-9]", "#", la[i]), 110), short(gsub("[0-9]", "#", lb[i]), 110))
  }
  if (length(real)) substantive <- c(substantive, f) else expected_only <- c(expected_only, f)
}

hdr <- c(
  sprintf("# Revision comparison"),
  "",
  sprintf("- A: `%s`", A), sprintf("- B: `%s`", B),
  sprintf("- files: %d in A, %d in B, %d in both", length(fa), length(fb), length(both)),
  sprintf("- identical: %d", length(both) - length(differ)),
  sprintf("- differ only as expected (dates, run id, code version, checksums): %d", length(expected_only)),
  sprintf("- differ otherwise: %d", length(substantive)),
  sprintf("- only in A: %d, only in B: %d", length(only_a), length(only_b)),
  "")
body <- c(
  if (length(only_a)) c("## Only in A", "", paste0("- `", only_a, "`"), "") else NULL,
  if (length(only_b)) c("## Only in B", "", paste0("- `", only_b, "`"), "") else NULL,
  if (length(differ)) c("## Files that differ", "", lines_out, "") else NULL)

out <- c(hdr, body)
cat(out, sep = "\n")
if (!is.null(REPORT)) { writeLines(enc2utf8(out), REPORT, useBytes = TRUE); cat(sprintf("\nReport written to %s\n", REPORT)) }

quit(status = if (!length(differ) && !length(only_a) && !length(only_b)) 0L
              else if (!length(substantive) && !length(only_a) && !length(only_b)) 1L else 2L)
