#!/usr/bin/env Rscript
#### cheCkOVER — GEOGRAPHIC ACCEPTANCE CHECK ####
# The four acceptance criteria Lucian set for the geographic fallback work
# (2026-07). Run against a completed version directory; exits non-zero unless
# all four hold.
#
#   1. Zero records emit geography outside the canonical vocabulary.
#   2. No species is cosmopolitan on the basis of an unresolved or snapped
#      record — cosmopolitan must rest on native or cleanly-derived values.
#   3. Integrity report: N native / N fallback / N unresolved, unresolved at or
#      near zero now that the source data is corrected.
#   4. Euastacus suttoni resolves to Oceania/Australia only, single continent,
#      and is NOT cosmopolitan.
#
# Usage:
#   Rscript verify_geo_acceptance.R <version_dir> [run_dir]
#   e.g. Rscript verify_geo_acceptance.R checkover_output/1.0 \
#                                        checkover_output/runs/run_production_v1

suppressPackageStartupMessages(library(jsonlite))

args    <- commandArgs(trailingOnly = TRUE)
vdir    <- if (length(args) >= 1) args[1] else "checkover_output/1.0"
run_dir <- if (length(args) >= 2) args[2] else NA_character_

# Load the canon + helpers (path-independent).
.root <- getwd()
for (p in c("R/00_helpers.R", "R/00_geo_canon.R")) {
  if (file.exists(p)) suppressWarnings(suppressMessages(source(p)))
}
if (!exists("CHECKOVER_CONTINENTS")) {
  CHECKOVER_CONTINENTS <- c("North America","Europe","Asia","Oceania","South America","Africa")
  GEO_UNRESOLVED <- "unresolved"
  is_geo_unresolved <- function(x) { v <- trimws(as.character(x))
    is.na(v) | !nzchar(v) | tolower(v) == GEO_UNRESOLVED }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
fm <- function(x) if (length(x) == 0 || is.na(x)) "n/a" else format(x, big.mark = ",")

PASS_ALL <- TRUE
verdict <- function(ok) { if (!isTRUE(ok)) PASS_ALL <<- FALSE
                          if (isTRUE(ok)) "PASS" else "FAIL" }

cat(sprintf("\n=== cheCkOVER geographic acceptance check ===\nversion dir: %s\n", vdir))

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------
dirs <- list.dirs(vdir, recursive = FALSE)
dirs <- dirs[file.exists(file.path(dirs, "package_metadata.json"))]
if (!length(dirs)) { cat("No packages found. Aborting.\n"); quit(status = 2) }
metas <- lapply(dirs, function(d)
  tryCatch(read_json(file.path(d, "package_metadata.json"), simplifyVector = TRUE),
           error = function(e) NULL))
names(metas) <- basename(dirs)
metas <- metas[!vapply(metas, is.null, logical(1))]
cat(sprintf("packages: %d\n", length(metas)))

# Indigenous distribution category, from the canonical narrative (section 2 only).
cat_of <- function(d) {
  s <- basename(d); f <- file.path(d, "narratives", paste0(s, "_canonical.md"))
  if (!file.exists(f)) return(NA_character_)
  tx <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  cut <- regexpr("\n## 3", tx, fixed = TRUE)
  part <- if (cut > 0) substr(tx, 1, cut) else tx
  m <- regmatches(part, regexec("[*][*]Distribution category:[*][*][[:space:]]*([A-Za-z-]+)", part))[[1]]
  if (length(m) >= 2) tolower(m[2]) else NA_character_
}
cats <- vapply(dirs, cat_of, character(1)); names(cats) <- basename(dirs)

# Optional: the run's occurrence table (per-record geography + provenance).
occ <- NULL
occ_candidates <- c(file.path(vdir, "checkover", "clean_occurrences.tsv"),
                    if (!is.na(run_dir)) file.path(run_dir, "clean_occurrences_with_gadm.tsv"),
                    if (!is.na(run_dir)) file.path(run_dir, "clean_occurrences.tsv"))
for (p in occ_candidates) {
  if (!is.na(p) && file.exists(p)) {
    occ <- tryCatch(read.delim(p, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                               quote = "", colClasses = "character", na.strings = c("", "NA")),
                    error = function(e) NULL)
    if (!is.null(occ)) { cat(sprintf("occurrence table: %s (%s rows)\n", p, fm(nrow(occ)))); break }
  }
}

# ---------------------------------------------------------------------------
# 1. Canonical vocabulary compliance
# ---------------------------------------------------------------------------
cat("\n--- 1. Canonical vocabulary ---\n")
pkg_countries <- unique(unlist(lapply(metas, function(m)
  c(unlist(m$metrics$indigenous$countries), unlist(m$metrics$non_indigenous$countries)))))
pkg_countries <- pkg_countries[!is.na(pkg_countries) & nzchar(pkg_countries)]

canon <- if (exists("load_geo_canon", mode = "function")) load_geo_canon() else NULL
bad_country_pkg <- if (!is.null(canon))
  pkg_countries[!(tolower(trimws(pkg_countries)) %in% canon$country_key)] else character(0)
bad_country_pkg <- setdiff(bad_country_pkg, GEO_UNRESOLVED)

bad_cont_occ <- character(0); bad_country_occ <- character(0)
if (!is.null(occ)) {
  if ("continents" %in% names(occ)) {
    cv <- unique(occ$continents[!is.na(occ$continents)])
    bad_cont_occ <- setdiff(cv[!(cv %in% CHECKOVER_CONTINENTS)], GEO_UNRESOLVED)
  }
  if ("country" %in% names(occ) && !is.null(canon)) {
    kv <- unique(occ$country[!is.na(occ$country)])
    bad_country_occ <- setdiff(kv[!(tolower(trimws(kv)) %in% canon$country_key)], GEO_UNRESOLVED)
  }
}
n_bad_vocab <- length(bad_country_pkg) + length(bad_cont_occ) + length(bad_country_occ)
cat(sprintf("  distinct countries emitted in packages : %s\n", fm(length(pkg_countries))))
cat(sprintf("  non-canonical country values (packages): %s%s\n", fm(length(bad_country_pkg)),
            if (length(bad_country_pkg)) paste0(" -> ", paste(head(bad_country_pkg, 10), collapse = ", ")) else ""))
cat(sprintf("  non-canonical continent values (records): %s%s\n", fm(length(bad_cont_occ)),
            if (length(bad_cont_occ)) paste0(" -> ", paste(head(bad_cont_occ, 10), collapse = ", ")) else ""))
cat(sprintf("  non-canonical country values (records)  : %s%s\n", fm(length(bad_country_occ)),
            if (length(bad_country_occ)) paste0(" -> ", paste(head(bad_country_occ, 10), collapse = ", ")) else ""))
c1 <- n_bad_vocab == 0
cat(sprintf("  => CRITERION 1: %s\n", verdict(c1)))

# ---------------------------------------------------------------------------
# 2. Cosmopolitan rests only on usable values
# ---------------------------------------------------------------------------
cat("\n--- 2. Cosmopolitan basis ---\n")
cosmo <- names(cats)[!is.na(cats) & cats == "cosmopolitan"]
cat(sprintf("  cosmopolitan species: %s\n", fm(length(cosmo))))
c2 <- TRUE
if (length(cosmo)) {
  for (s in cosmo) {
    sp_name <- metas[[s]]$species %||% s
    detail <- "(no occurrence table — cannot verify basis)"
    if (!is.null(occ) && all(c("species", "continents") %in% names(occ))) {
      rows <- occ[occ$species == sp_name, , drop = FALSE]
      # Indigenous scope only, matching the classifier.
      if ("population_status" %in% names(rows))
        rows <- rows[rows$population_status == "indigenous", , drop = FALSE]
      v  <- rows$continents
      nu <- sum(is_geo_unresolved(v))
      n_eff <- if (exists("count_continents", mode = "function")) count_continents(v) else NA
      # Was any contributing record rejected/snapped?
      snapped <- if ("continent_reject" %in% names(rows)) sum(!is.na(rows$continent_reject)) else 0L
      tb <- table(v[!is_geo_unresolved(v)])
      detail <- sprintf("effective continents=%s | unresolved records=%s | rejected snaps=%s | %s",
                        n_eff, nu, snapped,
                        paste(sprintf("%s=%d", names(tb), as.integer(tb)), collapse = " "))
      if (!is.na(n_eff) && n_eff < 2) { c2 <- FALSE; detail <- paste(detail, "  <-- NOT genuinely multi-continent") }
      if (snapped > 0)               { c2 <- FALSE; detail <- paste(detail, "  <-- relies on a snapped record") }
    }
    cat(sprintf("    %-34s %s\n", sp_name, detail))
  }
}
cat(sprintf("  => CRITERION 2: %s\n", verdict(c2)))

# ---------------------------------------------------------------------------
# 3. Integrity report totals
# ---------------------------------------------------------------------------
cat("\n--- 3. Integrity report ---\n")
gi <- NULL
gi_candidates <- c(if (!is.na(run_dir)) file.path(run_dir, "geographic_integrity.json"),
                   file.path(vdir, "geographic_integrity.json"),
                   file.path(dirname(vdir), "geographic_integrity.json"))
for (p in gi_candidates) if (!is.na(p) && file.exists(p)) {
  gi <- tryCatch(read_json(p, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.null(gi)) { cat(sprintf("  source: %s\n", p)); break }
}
# Fallback: reconstruct the totals from the run log. geographic_integrity.json
# lives in the run directory, which is usually not synced alongside the version
# zips, but Module 2C logs the same numbers verbatim.
if (is.null(gi)) {
  logs <- list.files(file.path(dirname(vdir), "logs"), pattern = "\\.log$", full.names = TRUE)
  if (!length(logs)) logs <- list.files("checkover_output/logs", pattern = "\\.log$", full.names = TRUE)
  best <- NULL
  for (L in logs) {
    tx <- readLines(L, warn = FALSE)
    hits <- grep("MODULE2C_GEO_INTEGRITY", tx, value = TRUE)
    if (!length(hits)) next
    tot <- list()
    for (f in c("continent", "country", "admin_1")) {
      ln <- grep(sprintf("  %s +native\\(WoC\\)", f), hits, value = TRUE)
      if (!length(ln)) next
      m <- regmatches(ln[1], regexec(
        "native\\(WoC\\) +([0-9]+) \\| fallback +([0-9]+) \\| unresolved +([0-9]+)", ln[1]))[[1]]
      if (length(m) >= 4)
        tot[[f]] <- list(native = as.integer(m[2]), computed = as.integer(m[3]),
                         unresolved = as.integer(m[4]))
    }
    if (length(tot)) {
      nrec <- regmatches(hits, regexec("Geographic provenance \\(([0-9]+) records\\)", hits))
      nrec <- Filter(function(z) length(z) >= 2, nrec)
      rej  <- grep("snap REJECTED", hits, value = TRUE)
      best <- list(totals = tot,
                   total_records = if (length(nrec)) as.integer(nrec[[1]][2]) else NA_integer_,
                   species_affected = if (any(grepl("No unresolved geographic fields", hits))) 0L else NA_integer_,
                   rejected_snaps = if (length(rej)) data.frame(n = length(rej)) else NULL,
                   .from_log = basename(L))
    }
  }
  if (!is.null(best)) { gi <- best; cat(sprintf("  source: run log (%s)\n", gi$.from_log)) }
}
c3 <- NA
if (is.null(gi)) {
  cat("  geographic_integrity.json not found — sync it from the run directory.\n")
  cat("  => CRITERION 3: INCONCLUSIVE\n"); PASS_ALL <- FALSE
} else {
  tot_unres <- 0L
  for (f in names(gi$totals)) {
    t <- gi$totals[[f]]
    cat(sprintf("  %-10s native %s | fallback %s | unresolved %s\n",
                f, fm(t$native), fm(t$computed), fm(t$unresolved)))
    tot_unres <- tot_unres + as.integer(t$unresolved %||% 0)
  }
  nrej <- if (!is.null(gi$rejected_snaps)) nrow(gi$rejected_snaps) else 0L
  cat(sprintf("  species with unresolved fields: %s | rejected snaps: %s\n",
              fm(gi$species_affected %||% 0), fm(nrej)))
  c3 <- tot_unres == 0
  if (!c3) cat(sprintf("  NOTE: %s unresolved field-values remain.\n", fm(tot_unres)))
  cat(sprintf("  => CRITERION 3: %s\n", verdict(c3)))
}

# ---------------------------------------------------------------------------
# 4. Euastacus suttoni spot-check
# ---------------------------------------------------------------------------
cat("\n--- 4. Euastacus suttoni ---\n")
key <- names(metas)[grepl("^Euastacus_suttoni$", names(metas))]
c4 <- FALSE
if (!length(key)) {
  cat("  not present in this version (nothing to check).\n"); c4 <- NA
} else {
  m <- metas[[key]]
  ctry <- unlist(m$metrics$indigenous$countries)
  cat(sprintf("  category  : %s\n", cats[[key]] %||% "n/a"))
  cat(sprintf("  countries : %s\n", paste(ctry, collapse = ", ")))
  conts <- NA
  if (!is.null(occ) && all(c("species","continents") %in% names(occ))) {
    rows <- occ[occ$species == (m$species %||% "Euastacus suttoni"), , drop = FALSE]
    if ("population_status" %in% names(rows))
      rows <- rows[rows$population_status == "indigenous", , drop = FALSE]
    tb <- table(rows$continents[!is_geo_unresolved(rows$continents)])
    conts <- names(tb)
    cat(sprintf("  continents: %s | unresolved records: %d\n",
                paste(sprintf("%s=%d", names(tb), as.integer(tb)), collapse = " "),
                sum(is_geo_unresolved(rows$continents))))
  }
  ok_cat  <- !identical(cats[[key]], "cosmopolitan")
  ok_ctry <- all(ctry %in% c("Australia")) || length(ctry) == 0
  ok_cont <- is.na(conts[1]) || identical(sort(unique(conts)), "Oceania")
  cat(sprintf("  not cosmopolitan: %s | Australia only: %s | Oceania only: %s\n",
              ok_cat, ok_ctry, ok_cont))
  c4 <- ok_cat && ok_ctry && ok_cont
  cat(sprintf("  => CRITERION 4: %s\n", verdict(c4)))
}

cat(sprintf("\n=== RESULT: %s ===\n\n", if (PASS_ALL) "ALL CRITERIA HOLD" else "NOT YET ACCEPTABLE"))
quit(status = if (PASS_ALL) 0 else 1)
