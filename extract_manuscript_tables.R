#!/usr/bin/env Rscript
#### cheCkOVER — MANUSCRIPT TABLES (Table 2 / Table 3 rebuild + examples) ####
# Second-round extraction for the manuscript rewrite (Lucian, 2026-07).
# Aggregated counts and summary statistics only — no raw coordinates.
#
# Usage:
#   Rscript extract_manuscript_tables.R [base_dir] [version] [raw_input_tsv]
#   e.g. Rscript extract_manuscript_tables.R checkover_output 1.0 WoC_1_0.tsv
#
# Writes <base_dir>/manuscript_tables.{md,json,tsv}

suppressPackageStartupMessages(library(jsonlite))
# Canonical vocabulary, so the country-canon check below can run. Optional: the
# script still works without it, just without the canon comparison.
for (.p in c("R/00_geo_canon.R", "00_geo_canon.R"))
  if (file.exists(.p)) { suppressWarnings(suppressMessages(source(.p))); break }

args      <- commandArgs(trailingOnly = TRUE)
base_dir  <- if (length(args) >= 1) args[1] else "checkover_output"
VER       <- if (length(args) >= 2) args[2] else "1.0"
RAW_TSV   <- if (length(args) >= 3) args[3] else "WoC_1_0.tsv"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
.n     <- function(x) suppressWarnings(as.numeric(x))
fm     <- function(x, d = 0) if (length(x) == 0 || is.na(x)) "n/a" else
            formatC(x, format = "f", big.mark = ",", digits = d)

# ---------------------------------------------------------------------------
# Per-species table from the shipped packages
# ---------------------------------------------------------------------------
cat_from_narrative <- function(d, scope) {
  s <- basename(d)
  f <- file.path(d, "narratives", paste0(s, "_canonical.md"))
  if (!file.exists(f)) return(NA_character_)
  tx <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  cut <- regexpr("\n## 3", tx, fixed = TRUE)
  part <- if (cut > 0) {
    if (scope == "indigenous") substr(tx, 1, cut) else substr(tx, cut, nchar(tx))
  } else if (scope == "indigenous") tx else ""
  m <- regmatches(part, regexec("[*][*]Distribution category:[*][*][[:space:]]*([A-Za-z-]+)", part))[[1]]
  if (length(m) >= 2) tolower(m[2]) else NA_character_
}

vdir <- file.path(base_dir, VER)
dirs <- list.dirs(vdir, recursive = FALSE)
dirs <- dirs[file.exists(file.path(dirs, "package_metadata.json"))]
cat(sprintf("Reading %d packages from %s ...\n", length(dirs), vdir))

rows <- lapply(dirs, function(d) {
  m <- tryCatch(read_json(file.path(d, "package_metadata.json"), simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  i <- m$metrics$indigenous; nn <- m$metrics$non_indigenous
  data.frame(
    species     = m$species %||% basename(d),
    cat_ind     = cat_from_narrative(d, "indigenous"),
    cat_non     = cat_from_narrative(d, "non_indigenous"),
    rec_ind     = .n(i$records %||% 0),   eoo_ind = .n(i$EOO_km2 %||% NA), aoo_ind = .n(i$AOO_km2 %||% NA),
    n_countries = .n(i$countries_count %||% 0),
    countries     = paste(unlist(i$countries),  collapse = "; "),
    countries_non = paste(unlist(nn$countries), collapse = "; "),
    rec_non     = .n(nn$records %||% 0),  eoo_non = .n(nn$EOO_km2 %||% NA), aoo_non = .n(nn$AOO_km2 %||% NA),
    stringsAsFactors = FALSE)
})
df <- do.call(rbind, Filter(Negate(is.null), rows))

# ---------------------------------------------------------------------------
# Continent count per species: map the ENRICHED country list through a
# country -> continent lookup built from the source table. package_metadata
# carries countries but not continents, so this is the closest we can get
# without the (unsynced) per-species report JSONs.
# ---------------------------------------------------------------------------
n_continents <- rep(NA_real_, nrow(df))
if (file.exists(RAW_TSV)) {
  raw <- read.delim(RAW_TSV, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                    quote = "", na.strings = c("", "NA"), colClasses = "character")
  if (all(c("country", "continent") %in% names(raw))) {
    cc <- unique(raw[!is.na(raw$country) & !is.na(raw$continent), c("country", "continent")])
    # The source uses both "Australia" and "Oceania"; treat as one continent.
    cc$continent[cc$continent == "Australia"] <- "Oceania"
    lut <- setNames(cc$continent, cc$country)
    n_continents <- vapply(strsplit(df$countries, ";\\s*"), function(v) {
      v <- trimws(v); v <- v[nzchar(v)]
      if (!length(v)) return(NA_real_)
      as.numeric(length(unique(na.omit(unname(lut[v])))))
    }, numeric(1))
  }
}
df$n_continents <- n_continents

# ---------------------------------------------------------------------------
# Summary helpers
# ---------------------------------------------------------------------------
summ <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(list(n = 0, median = NA, min = NA, max = NA, q1 = NA, q3 = NA))
  list(n = length(x), median = stats::median(x), min = min(x), max = max(x),
       q1 = unname(stats::quantile(x, .25)), q3 = unname(stats::quantile(x, .75)))
}
row_line <- function(lbl, s, d = 0)
  sprintf("| %s | %s | %s | %s – %s | %s – %s |", lbl, fm(s$n), fm(s$median, d),
          fm(s$min, d), fm(s$max, d), fm(s$q1, d), fm(s$q3, d))

IND_CATS <- c("endemic", "regional", "cosmopolitan")
NON_CATS <- c("local", "widespread")

# ---------------------------------------------------------------------------
# 1. TOP INVADERS
# ---------------------------------------------------------------------------
TOP3 <- c("Procambarus clarkii", "Faxonius limosus", "Pacifastacus leniusculus")
top <- df[match(TOP3, df$species), c("species", "rec_non")]
total_non <- sum(df$rec_non, na.rm = TRUE)
top_sum   <- sum(top$rec_non, na.rm = TRUE)

# Cross-check the alien denominator against clean_occurrences.tsv when it ships
# with the version. Previously this comparison value was HARDCODED (55,022),
# so a new run reported a phantom 72-record gap against its own correct total.
occ_non <- NA_integer_
occ_p <- file.path(base_dir, VER, "checkover", "clean_occurrences.tsv")
if (file.exists(occ_p)) {
  oc <- tryCatch(read.delim(occ_p, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                            quote = "", colClasses = "character", na.strings = c("", "NA")),
                 error = function(e) NULL)
  if (!is.null(oc) && "population_status" %in% names(oc))
    occ_non <- sum(oc$population_status == "non-indigenous", na.rm = TRUE)
}

# ---------------------------------------------------------------------------
# 2. REPRESENTATIVE ("textbook") EXAMPLES — closest to the category centre
# ---------------------------------------------------------------------------
# min_countries guards against two artifacts documented in the caveats section:
#   * countries_count == 0 (country enrichment dropped USA/RUS - antimeridian)
#   * "cosmopolitan" species whose second continent is a single stray record or a
#     blank label; a genuinely cosmopolitan taxon spans >1 country.
# Species whose multi-continent status rests on a handful of stray or unlabelled
# records (see caveat B). Excluded from EXAMPLE selection only — they remain in
# all the summary statistics above.
ARTIFACT_COSMO <- c("Austropotamobius torrentium", "Orconectes pellucidus",
                    "Parastacus varicosus", "Euastacus spinifer", "Euastacus suttoni")

pick_examples <- function(cat, k = 3, min_rec = 5, min_countries = 1, exclude = character(0)) {
  d <- df[!is.na(df$cat_ind) & df$cat_ind == cat &
          !is.na(df$eoo_ind) & !is.na(df$aoo_ind) &
          df$eoo_ind > 0 & df$aoo_ind > 0 & df$rec_ind >= min_rec &
          !is.na(df$n_countries) & df$n_countries >= min_countries &
          !(df$species %in% exclude), , drop = FALSE]
  if (!nrow(d)) return(d)
  # Distance to the category median on a log scale (EOO/AOO/records span orders
  # of magnitude), so "typical" means central rather than merely small.
  ctr <- function(v) log10(stats::median(v, na.rm = TRUE))
  score <- abs(log10(d$eoo_ind) - ctr(d$eoo_ind)) +
           abs(log10(d$aoo_ind) - ctr(d$aoo_ind)) +
           abs(log10(d$rec_ind) - ctr(d$rec_ind))
  d[order(score), ][seq_len(min(k, nrow(d))), ]
}
ex <- list(
  endemic      = pick_examples("endemic",      min_countries = 1),
  regional     = pick_examples("regional",     min_countries = 2),
  cosmopolitan = pick_examples("cosmopolitan", min_countries = 2, exclude = ARTIFACT_COSMO)
)

# --- data-quality diagnostics feeding the caveats section ---
# The country GAP only matters for species that actually have an indigenous
# range. A non-indigenous-only species legitimately has 0 indigenous countries,
# so counting it as a "missing country" made the caveat cry wolf once the
# WoC-native geography fix had already resolved the real gap (406 -> 2, and both
# of those 2 have zero indigenous records).
n_zero_ctry <- sum(df$n_countries == 0 & df$rec_ind > 0, na.rm = TRUE)
pkg_ctry <- unique(trimws(unlist(strsplit(df$countries[nzchar(df$countries)], ";\\s*"))))
pkg_ctry <- pkg_ctry[nzchar(pkg_ctry)]
# Countries across the whole cohort (indigenous + non-indigenous), for a
# like-for-like comparison with the source table (which is not scope-split).
all_ctry_raw <- c(df$countries, df$countries_non)
all_ctry <- unique(trimws(unlist(strsplit(all_ctry_raw[nzchar(all_ctry_raw)], ";\\s*"))))
all_ctry <- all_ctry[nzchar(all_ctry)]
src_ctry <- NA_integer_
if (exists("raw") && "country" %in% names(raw))
  # NB: nzchar(NA) is TRUE, so filtering on nzchar() alone counts NA as a
  # distinct country and overstates the source total by one.
  src_ctry <- length(unique(raw$country[!is.na(raw$country) & nzchar(raw$country)]))
n_eoo_zero <- sum(df$eoo_ind == 0, na.rm = TRUE)

# ---------------------------------------------------------------------------
# EMIT
# ---------------------------------------------------------------------------
L <- c(); add <- function(...) L <<- c(L, sprintf(...))

add("# cheCkOVER — manuscript tables (v%s)", VER)
add("")
add("Generated %s from `%s`. Indigenous metrics unless stated. Counts only.",
    as.character(Sys.Date()), vdir)
add("")

add("## 1. Top invaders — exact share")
add("")
add("| Species | Non-indigenous records |")
add("|---|---|")
for (i in seq_len(nrow(top))) add("| *%s* | %s |", top$species[i], fm(top$rec_non[i]))
add("| **Combined (3 species)** | **%s** |", fm(top_sum))
add("| Total non-indigenous records | %s |", fm(total_non))
add("| **Share of all non-indigenous records** | **%.1f%%** |", 100 * top_sum / total_non)
add("")

add("## 4. EOO / AOO by indigenous category (Table 2 rebuild)")
add("")
add("| Category | n | Median | Range (min – max) | IQR (Q1 – Q3) |")
add("|---|---|---|---|---|")
for (k in IND_CATS) add(row_line(sprintf("%s — EOO km2", k), summ(df$eoo_ind[df$cat_ind %in% k])))
for (k in IND_CATS) add(row_line(sprintf("%s — AOO km2", k), summ(df$aoo_ind[df$cat_ind %in% k])))
add("")
for (k in IND_CATS) {
  tot <- sum(df$cat_ind %in% k); have <- sum(df$cat_ind %in% k & !is.na(df$eoo_ind))
  add("- **%s**: %s of %s species have a computable EOO (%s have <3 records, so EOO is NA by definition).",
      k, fm(have), fm(tot), fm(tot - have))
}
add("")

add("## 5. EOO / AOO by non-indigenous category (Table 3 rebuild)")
add("")
add("| Category | n | Median | Range (min – max) | IQR (Q1 – Q3) |")
add("|---|---|---|---|---|")
for (k in NON_CATS) add(row_line(sprintf("%s — EOO km2", k), summ(df$eoo_non[df$cat_non %in% k])))
for (k in NON_CATS) add(row_line(sprintf("%s — AOO km2", k), summ(df$aoo_non[df$cat_non %in% k])))
add("")

add("## 2. Representative species examples")
add("")
add("Chosen as closest to each category's median on EOO, AOO and record count")
add("(log scale), restricted to species with >=%d indigenous records and a", 5L)
add("computable EOO — i.e. deliberately central, not outliers.")
add("")
add("| Category | Species | Indigenous records | EOO km2 | AOO km2 | Countries | Continents |")
add("|---|---|---|---|---|---|---|")
for (k in IND_CATS) {
  d <- ex[[k]]
  if (!nrow(d)) { add("| %s | *(none meeting criteria)* | | | | | |", k); next }
  for (i in seq_len(nrow(d))) {
    extra <- if (k == "endemic") d$countries[i] else fm(d$n_countries[i])
    add("| %s | *%s* | %s | %s | %s | %s | %s |", k, d$species[i], fm(d$rec_ind[i]),
        fm(d$eoo_ind[i]), fm(d$aoo_ind[i]),
        if (k == "endemic") substr(extra, 1, 40) else extra,
        fm(d$n_continents[i]))
  }
}
add("")

add("## 3. Indigenous record counts by category")
add("")
add("| Category | n | Median | Range (min – max) | IQR (Q1 – Q3) |")
add("|---|---|---|---|---|")
for (k in IND_CATS) add(row_line(k, summ(df$rec_ind[df$cat_ind %in% k])))
add("")

add("## 6. Alien denominator confirmation")
add("")
add("| Source | Non-indigenous records |")
add("|---|---|")
add("| Sum of `metrics.non_indigenous.records` across all v%s packages | %s |", VER, fm(total_non))
if (!is.na(occ_non)) {
  add("| `population_status == \"non-indigenous\"` in clean_occurrences.tsv | %s |", fm(occ_non))
  add("")
  add(if (isTRUE(total_non == occ_non))
        sprintf("**Confirmed** — both sources agree exactly, so **%s** is the denominator for all alien percentages.", fm(total_non))
      else
        sprintf("**Discrepancy of %s records** between the two sources — resolve before publishing percentages.",
                fm(abs(total_non - occ_non))))
} else {
  add("")
  add(paste("`clean_occurrences.tsv` is not present in this version directory, so only the",
            "package-derived total is available. Use **%s** as the denominator."))
  add(sprintf("Denominator: **%s** (sum across packages).", fm(total_non)))
}
add("")

# Caveats are COMPUTED from the current run, never hardcoded. An earlier version
# carried literal numbers from a previous extraction (cosmopolitan "n=11", a
# fixed 55,022 denominator), which then contradicted the computed tables above
# and made the two documents look like they came from different runs.
n_cosmo <- sum(df$cat_ind %in% "cosmopolitan")
caveats <- character(0)

add("## Data-quality notes")
add("")
if (n_zero_ctry > 0) {
  add("**A. Country enrichment — %s species with an indigenous range still lack a country.**",
      fm(n_zero_ctry))
  add("")
  add("Packages carry %s distinct countries cohort-wide against %s in the source table.",
      fm(length(all_ctry)), fm(src_ctry))
  add("If more than a handful, check the run log for an antimeridian drop line")
  add("(`sanitize_spatial_layer(...): dropped ... USA, RUS, ...`).")
} else {
  add("**A. Country enrichment is complete.** Every species with an indigenous range")
  add("carries at least one country. Cohort-wide the packages cover **%s** distinct countries",
      fm(length(all_ctry)))
  add("against %s in the source table. The %s native-range-only figure in the summary is a",
      fm(src_ctry), fm(length(pkg_ctry)))
  add("narrower slice, not a shortfall. Country counts are safe to publish.")
  # Name the source values that are not in the canon rather than assuming a
  # reason for the difference. Anything listed here is present in the raw export
  # but absent from WoC_canonical_country_continent.tsv — either a value that
  # needs adding to the canon, or a corrupted row (a shifted column can put a
  # continent name in the country field). The pipeline output is unaffected:
  # criterion 1 of the acceptance check confirms zero non-canonical values
  # actually reach the packages.
  if (exists("raw") && "country" %in% names(raw) && !is.null(canon_tbl <- tryCatch(load_geo_canon(), error = function(e) NULL))) {
    src_vals <- unique(raw$country[!is.na(raw$country) & nzchar(raw$country)])
    off <- src_vals[!(tolower(trimws(src_vals)) %in% canon_tbl$country_key)]
    if (length(off) > 0) {
      add("")
      add("Source values not in the canon (%s): %s. These do not reach the packages —",
          fm(length(off)), paste(sprintf("`%s`", off), collapse = ", "))
      add("ingest rejects a row whose geography block is inconsistent — but they are worth")
      add("checking: each is either a value to add to the canon or a corrupted row.")
    }
  }
}
add("")

# B — cosmopolitan composition, listed from the actual data, with an automatic
# check for the coordinate artefact that survives the >=5-record threshold: a
# species whose country-based continent count is 1 but is classified cosmopolitan
# has out-of-range coordinates placing >=5 records on a second continent.
add("**B. Cosmopolitan class (n=%s).**", fm(n_cosmo))
add("")
if (n_cosmo > 0) {
  cos <- df[df$cat_ind %in% "cosmopolitan", ]
  cos <- cos[order(-cos$rec_ind), ]
  cos$suspect <- !is.na(cos$n_continents) & cos$n_continents <= 1
  add("| Species | Indigenous records | Countries | Continents (by country) | Flag |")
  add("|---|---|---|---|---|")
  for (i in seq_len(nrow(cos)))
    add("| *%s* | %s | %s | %s | %s |", cos$species[i], fm(cos$rec_ind[i]),
        fm(cos$n_countries[i]), fm(cos$n_continents[i]),
        if (isTRUE(cos$suspect[i])) "**suspect coords**" else "genuine")
  add("")
  add("A continent counts only if it holds >=5 records AND >=5%% of the species' labelled")
  add("indigenous records, blank/NA excluded — so a single stray or unlabelled record can")
  add("no longer make a species cosmopolitan.")
  if (any(cos$suspect)) {
    sname <- paste(sprintf("*%s*", cos$species[cos$suspect]), collapse = ", ")
    add("")
    add("**Flagged: %s.** The country-based continent count is 1, yet the classifier saw >1", sname)
    add("continent — meaning >=5 records carry coordinates that fall on a second continent")
    add("(e.g. a dropped digit in the longitude). This is a **source coordinate error**, not a")
    add("classification bug; the affected records also inflate the species' EOO. Recommend")
    add("correcting the coordinates in WoC before publishing the cosmopolitan row.")
  }
} else {
  add("No species qualifies as cosmopolitan under the current threshold.")
}
add("")

if (n_eoo_zero > 0) {
  add("**C.** %s species have `EOO = 0` (>=3 records but collinear/coincident, so the convex", fm(n_eoo_zero))
  add("hull has zero area). Legitimate but degenerate — it sets the minimum of the EOO ranges.")
  add("")
}

writeLines(L, file.path(base_dir, "manuscript_tables.md"), useBytes = TRUE)

out <- list(
  version = VER, generated = as.character(Sys.Date()),
  top_invaders = list(species = top$species, records = top$rec_non,
                      combined = top_sum, total_non_indigenous = total_non,
                      share_pct = round(100 * top_sum / total_non, 2)),
  eoo_aoo_indigenous = lapply(setNames(IND_CATS, IND_CATS), function(k)
    list(EOO = summ(df$eoo_ind[df$cat_ind %in% k]), AOO = summ(df$aoo_ind[df$cat_ind %in% k]),
         records = summ(df$rec_ind[df$cat_ind %in% k]))),
  eoo_aoo_non_indigenous = lapply(setNames(NON_CATS, NON_CATS), function(k)
    list(EOO = summ(df$eoo_non[df$cat_non %in% k]), AOO = summ(df$aoo_non[df$cat_non %in% k]))),
  examples = lapply(ex, function(d)
    d[, c("species","rec_ind","eoo_ind","aoo_ind","n_countries","n_continents","countries")]),
  denominator_check = list(sum_packages = total_non, clean_occurrences = occ_non)
)
write_json(out, file.path(base_dir, "manuscript_tables.json"), pretty = TRUE,
           auto_unbox = TRUE, na = "null")
write.table(df, file.path(base_dir, "manuscript_tables.tsv"), sep = "\t",
            row.names = FALSE, quote = TRUE)

cat(paste(L, collapse = "\n"), "\n")
cat(sprintf("\nWrote %s/manuscript_tables.{md,json,tsv}\n", base_dir))
