#!/usr/bin/env Rscript
#### cheCkOVER — MANUSCRIPT SUMMARY EXTRACTION ####
# Aggregated summaries for the cheCkOVER manuscript revision (Lucian, 2026-07).
# Raw coordinates are never emitted — counts only.
#
# Usage:
#   Rscript extract_manuscript_summaries.R [base_dir] [baseline_version] [current_version]
#   e.g. Rscript extract_manuscript_summaries.R checkover_output 1.0 1.1
#
# base_dir must contain <version>/ package folders, each with
# package_metadata.json + narratives/, and <version>/checkover/clean_occurrences.tsv.
#
# Writes, next to base_dir:
#   manuscript_summary.md    human-readable report (paste-able into email)
#   manuscript_summary.json  machine-readable
#   manuscript_summary.tsv   flat key/value table

suppressPackageStartupMessages(library(jsonlite))

args     <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else "checkover_output"
V_BASE   <- if (length(args) >= 2) args[2] else "1.0"
V_CURR   <- if (length(args) >= 3) args[3] else "1.1"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
.num   <- function(x) suppressWarnings(as.numeric(x))
pct    <- function(a, b) if (isTRUE(b > 0)) round(100 * a / b, 2) else NA_real_

# ---------------------------------------------------------------------------
# Load a version's packages
# ---------------------------------------------------------------------------
load_version <- function(v) {
  vdir <- file.path(base_dir, v)
  if (!dir.exists(vdir)) stop("Version dir not found: ", vdir)
  dirs <- list.dirs(vdir, recursive = FALSE)
  dirs <- dirs[file.exists(file.path(dirs, "package_metadata.json"))]
  metas <- lapply(dirs, function(d)
    tryCatch(jsonlite::read_json(file.path(d, "package_metadata.json"), simplifyVector = TRUE),
             error = function(e) NULL))
  names(metas) <- basename(dirs)
  metas[!vapply(metas, is.null, logical(1))]
}

# Distribution category lives in the canonical narrative. NOTE there are TWO
# different classifiers, each with its own vocabulary:
#   Section 2.1 (indigenous)     -> endemic / regional / cosmopolitan
#   Section 3.1 (non-indigenous) -> local / widespread
# Scenario-3 species carry BOTH lines, so we must split the document at
# "## 3." before matching, otherwise a non-indigenous-only species reports
# "widespread" as if it were an indigenous category.
read_dist_category <- function(v, sp_clean, scope = c("indigenous", "non_indigenous")) {
  scope <- match.arg(scope)
  f <- file.path(base_dir, v, sp_clean, "narratives", paste0(sp_clean, "_canonical.md"))
  if (!file.exists(f)) return(NA_character_)
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  cut <- regexpr("\n## 3\\.", txt)
  part <- if (cut > 0) {
    if (scope == "indigenous") substr(txt, 1, cut) else substr(txt, cut, nchar(txt))
  } else if (scope == "indigenous") txt else ""
  m <- regmatches(part, regexec("\\*\\*Distribution category:\\*\\*\\s*([A-Za-z-]+)", part))[[1]]
  if (length(m) >= 2) tolower(m[2]) else NA_character_
}

cat(sprintf("Reading %s / %s ...\n", V_BASE, V_CURR))
base_meta <- load_version(V_BASE)
curr_meta <- tryCatch(load_version(V_CURR), error = function(e) list())

occ_path <- file.path(base_dir, V_BASE, "checkover", "clean_occurrences.tsv")
occ <- if (file.exists(occ_path)) {
  read.delim(occ_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
             quote = "", na.strings = c("", "NA"), colClasses = "character")
} else NULL

# ---------------------------------------------------------------------------
# 1. BASE VALIDATION FIGURES
# ---------------------------------------------------------------------------
# Ingest counts come from the run log (authoritative parsed row counts).
# Several runs share the logs/ directory (v1.0 full-cohort, then sparse v1.1/v1.2
# runs covering a handful of species). Always take the FULL-COHORT figures, i.e.
# the maximum across logs — otherwise a later sparse run's numbers get reported.
find_log_counts <- function() {
  logs <- list.files(file.path(base_dir, "logs"), pattern = "\\.log$", full.names = TRUE)
  raw <- mapped <- NA_integer_
  for (L in logs) {
    tx <- readLines(L, warn = FALSE)
    h1 <- Filter(function(z) length(z) >= 2, regmatches(tx, regexec("Loaded ([0-9]+) raw records", tx)))
    h2 <- Filter(function(z) length(z) >= 3,
                 regmatches(tx, regexec("Mapped ([0-9]+) valid records from ([0-9]+) input records", tx)))
    if (length(h1)) raw    <- max(raw,    as.integer(h1[[1]][2]), na.rm = TRUE)
    if (length(h2)) mapped <- max(mapped, as.integer(h2[[1]][2]), na.rm = TRUE)
  }
  list(raw = raw, mapped = mapped)
}
lg        <- find_log_counts()
n_raw     <- lg$raw
n_mapped  <- lg$mapped                      # survived field/coordinate validation
n_valid   <- if (!is.null(occ)) nrow(occ) else NA_integer_
n_invalid <- if (!is.na(n_raw) && !is.na(n_mapped)) n_raw - n_mapped else NA_integer_
n_dupes   <- if (!is.na(n_mapped) && !is.na(n_valid)) n_mapped - n_valid else NA_integer_
n_species <- length(base_meta)

# ---------------------------------------------------------------------------
# 2. DISTRIBUTIONAL CATEGORY (indigenous range)
# ---------------------------------------------------------------------------
cats <- vapply(names(base_meta), function(s) read_dist_category(V_BASE, s, "indigenous"), character(1))
n_no_indigenous <- sum(is.na(cats))          # non-indigenous-only species
cats <- cats[!is.na(cats)]
cat_tab <- table(factor(cats, levels = c("endemic", "regional", "cosmopolitan")))
cat_other <- sum(!cats %in% c("endemic", "regional", "cosmopolitan"))
# Non-indigenous classifier (separate vocabulary), reported for completeness
ncats <- vapply(names(base_meta), function(s) read_dist_category(V_BASE, s, "non_indigenous"), character(1))
ncat_tab <- table(factor(ncats[!is.na(ncats)], levels = c("local", "widespread")))

# Species with >=5 distinct indigenous localities (clustering eligibility)
n_ge5 <- NA_integer_; loc_per_sp <- integer(0)
if (!is.null(occ) && all(c("species","longitude","latitude","population_status") %in% names(occ))) {
  ind <- occ[occ$population_status == "indigenous", , drop = FALSE]
  key <- paste(ind$species, ind$longitude, ind$latitude)
  loc_per_sp <- tapply(key, ind$species, function(k) length(unique(k)))
  n_ge5 <- sum(loc_per_sp >= 5, na.rm = TRUE)
}

# ---------------------------------------------------------------------------
# 3. POPULATION ORIGIN BREAKDOWN
# ---------------------------------------------------------------------------
orig <- list()
if (!is.null(occ)) {
  ps <- occ$population_status; oo <- occ$occurrence_origin
  orig$records_by_status <- as.list(table(ps))
  orig$records_by_origin <- as.list(table(oo))
  orig$species_indigenous     <- length(unique(occ$species[ps == "indigenous"]))
  orig$species_non_indigenous <- length(unique(occ$species[ps == "non-indigenous"]))
  orig$species_both <- length(intersect(unique(occ$species[ps == "indigenous"]),
                                        unique(occ$species[ps == "non-indigenous"])))
}

# ---------------------------------------------------------------------------
# 4. VERNACULAR / TYPE LOCALITY / ENRICHMENT COVERAGE
# ---------------------------------------------------------------------------
# Language tags follow the GBIF trailing-code convention: an untagged name
# inherits the next tagged code (same rule the narrative generator uses).
langs_of <- function(v) {
  if (is.null(v) || is.na(v) || !nzchar(v)) return(character(0))
  e <- trimws(strsplit(v, "\\s*\\|\\s*")[[1]]); e <- e[nzchar(e)]
  code <- rep(NA_character_, length(e))
  for (i in seq_along(e)) if (grepl("\\(([a-z]{2,3})\\)$", e[i]))
    code[i] <- sub(".*\\(([a-z]{2,3})\\)$", "\\1", e[i])
  nx <- NA_character_
  for (i in rev(seq_along(code))) if (!is.na(code[i])) nx <- code[i] else code[i] <- nx
  code[is.na(code)] <- "eng"
  code
}
vern_names <- 0L; vern_langs <- character(0); n_with_vern <- 0L
for (m in base_meta) {
  v <- m$vernacular_names
  if (is.null(v) || is.na(v) || !nzchar(v)) next
  n_with_vern <- n_with_vern + 1L
  parts <- trimws(strsplit(v, "\\s*\\|\\s*")[[1]]); parts <- parts[nzchar(parts)]
  vern_names <- vern_names + length(parts)
  vern_langs <- c(vern_langs, langs_of(v))
}
n_type_loc <- sum(vapply(base_meta, function(m) isTRUE(m$metrics$type_locality_present), logical(1)))
# Countries come in two flavours and the two extraction scripts previously
# disagreed (100 vs 62) purely because one counted both population scopes and
# the other only the native range. Report BOTH, explicitly labelled.
ind_countries <- unique(unlist(lapply(base_meta, function(m) unlist(m$metrics$indigenous$countries))))
ind_countries <- ind_countries[!is.na(ind_countries) & nzchar(ind_countries)]
all_countries <- unique(unlist(lapply(base_meta, function(m)
  c(unlist(m$metrics$indigenous$countries), unlist(m$metrics$non_indigenous$countries)))))
all_countries <- all_countries[!is.na(all_countries) & nzchar(all_countries)]

# ---------------------------------------------------------------------------
# 5. SPATIAL CLUSTERING (fragmentation)
# ---------------------------------------------------------------------------
clusters <- vapply(base_meta, function(m) {
  # Key renamed fragmentation_clusters -> n_clusters (2026-07); read both so the
  # script works against packages from either side of the rename.
  x <- m$metrics$indigenous$n_clusters %||% m$metrics$indigenous$fragmentation_clusters
  if (is.null(x) || length(x) == 0) NA_real_ else .num(x)[1]
}, numeric(1))
# Packages now ship clustering_status alongside the count, which makes them
# self-describing: n_clusters == 0 alone cannot distinguish "computed, single
# cluster" from "never computed". Prefer the packages (the published artifact)
# so every figure below comes from ONE source; fall back to the run log only for
# packages produced before the status field existed.
cl_status <- vapply(base_meta, function(m)
  as.character(m$metrics$indigenous$clustering_status %||% NA_character_), character(1))
have_status <- any(!is.na(cl_status))
cl_detected <- clusters[!is.na(clusters) & clusters > 1]
frag_log <- local({
  logs <- list.files(file.path(base_dir, "logs"), pattern = "\\.log$", full.names = TRUE)
  g <- function(tx, pat) {
    h <- Filter(function(z) length(z) >= 2, regmatches(tx, regexec(pat, tx)))
    if (length(h)) suppressWarnings(as.integer(h[[1]][2])) else NA_integer_
  }
  res <- list(analysed = NA_integer_, detected = NA_integer_,
              none_detected = NA_integer_, not_computed = NA_integer_)
  # Keep the FULL-COHORT run (largest 'analysed'); later sparse runs cover only
  # the handful of reprocessed species and would otherwise overwrite these.
  best <- -1L
  for (L in logs) {
    tx <- readLines(L, warn = FALSE)
    a  <- g(tx, "Analyzing fragmentation for ([0-9]+) species")
    if (is.na(a) || a <= best) next
    best <- a
    d  <- g(tx, "Detected: ([0-9]+) species")
    nd <- g(tx, "None detected: ([0-9]+) species")   # logged as "NA species" => 0
    nc <- g(tx, "Not computed: ([0-9]+) species")
    res$analysed      <- a
    res$detected      <- d
    res$none_detected <- if (is.na(nd) && !is.na(d)) 0L else nd
    res$not_computed  <- nc
  }
  res
})
# ONE consistent set. With clustering_status present everything is derived from
# the packages, so analysed == multi + single by construction and the totals can
# never contradict each other (the 484-vs-489 impossibility in the 2026-07 run
# came from mixing a package-derived count with a log-derived one).
if (have_status) {
  n_multi      <- sum(cl_status == "detected",      na.rm = TRUE)
  n_single     <- sum(cl_status == "none_detected", na.rm = TRUE)
  n_notcomp    <- sum(!(cl_status %in% c("detected", "none_detected")), na.rm = TRUE)
  n_analysed   <- n_multi + n_single
  clust_source <- "packages (clustering_status)"
} else {
  n_multi      <- frag_log$detected      %||% length(cl_detected)
  n_single     <- frag_log$none_detected %||% 0L
  n_notcomp    <- frag_log$not_computed  %||% NA_integer_
  n_analysed   <- n_multi + n_single
  clust_source <- "run log (packages predate clustering_status)"
}
cl_analysed <- cl_detected   # cluster-count distribution: detected species only

# ---------------------------------------------------------------------------
# 6. v1.0 -> v1.1 DELTA
# ---------------------------------------------------------------------------
# One row per species PER SCOPE. Invaders (e.g. Procambarus clarkii) change in
# their non-indigenous block while their indigenous block is static, so reporting
# indigenous only would show "no change" for exactly the species of interest.
delta_row <- function(sp, pop) {
  b <- base_meta[[sp]]; c_ <- curr_meta[[sp]]
  if (is.null(b) || is.null(c_)) return(NULL)
  g <- function(m, f) .num(m$metrics[[pop]][[f]] %||% NA)
  rb <- g(b,"records"); rc <- g(c_,"records")
  if (isTRUE(rb == 0) && isTRUE(rc == 0)) return(NULL)   # scope absent for this species
  ab <- g(b,"AOO_km2"); ac <- g(c_,"AOO_km2")
  eb <- g(b,"EOO_km2"); ec <- g(c_,"EOO_km2")
  xb <- g(b,"extinctions_count"); xc <- g(c_,"extinctions_count")
  data.frame(
    species              = b$species %||% sp,
    scope                = if (pop == "indigenous") "indigenous" else "non-indigenous",
    status               = c_$status %||% "Extant",
    records_prev         = rb, records_curr = rc,
    record_delta         = rc - rb,
    new_extinctions      = ifelse(is.na(xc), NA, xc - ifelse(is.na(xb), 0, xb)),
    AOO_prev = ab, AOO_curr = ac,
    AOO_pct  = ifelse(!is.na(ab) & ab > 0, round((ac - ab) / ab * 100, 1), NA),
    EOO_prev = eb, EOO_curr = ec,
    trend    = c_$metrics[[pop]]$trend_vs_previous %||% NA_character_,
    stringsAsFactors = FALSE)
}
delta_all <- do.call(rbind, Filter(Negate(is.null),
  unlist(lapply(names(curr_meta), function(s)
    list(delta_row(s, "indigenous"), delta_row(s, "non_indigenous"))), recursive = FALSE)))

# ---------------------------------------------------------------------------
# EMIT
# ---------------------------------------------------------------------------
fm <- function(x) if (is.na(x)) "n/a" else format(x, big.mark = ",", scientific = FALSE)
L <- c()
add <- function(...) L <<- c(L, sprintf(...))

add("# cheCkOVER — manuscript summary extraction")
add("")
add("Source: `%s`  |  baseline **v%s**, current **v%s**  |  generated %s",
    base_dir, V_BASE, V_CURR, as.character(Sys.Date()))
add("Counts only — no raw coordinates.")
add("")
add("## 1. v%s base validation figures", V_BASE)
add("")
add("| Metric | Value |")
add("|---|---|")
add("| Total raw records | %s |", fm(n_raw))
add("| Validated records | %s (%s%% of raw) |", fm(n_valid), pct(n_valid, n_raw))
add("| Removed — coordinate/field validation | %s (%s%%) |", fm(n_invalid), pct(n_invalid, n_raw))
add("| Removed — duplicates | %s (%s%%) |", fm(n_dupes), pct(n_dupes, n_raw))
add("| Total removed | %s (%s%%) |", fm(n_invalid + n_dupes), pct(n_invalid + n_dupes, n_raw))
add("| Total species | %s |", fm(n_species))
add("")
add("## 2. Species by distributional category (indigenous range)")
add("")
add("| Category | Species |")
add("|---|---|")
for (k in names(cat_tab)) add("| %s | %s |", k, fm(as.integer(cat_tab[[k]])))
if (cat_other > 0) add("| other/unresolved | %s |", fm(cat_other))
add("| **total with an indigenous range** | %s |", fm(sum(cat_tab) + cat_other))
add("")
add("%s species have no indigenous range (non-indigenous only) and therefore carry", fm(n_no_indigenous))
add("no indigenous category; %s + %s = %s.", fm(sum(cat_tab) + cat_other), fm(n_no_indigenous), fm(n_species))
add("")
add("The non-indigenous classifier uses a separate vocabulary (reported for completeness):")
add("")
add("| Non-indigenous category | Species |")
add("|---|---|")
for (k in names(ncat_tab)) add("| %s | %s |", k, fm(as.integer(ncat_tab[[k]])))
add("")
add("Eligible for spatial clustering (>=5 valid indigenous coordinates): **%s** computed,",
    fm(n_analysed))
add("**%s** not computed. Full breakdown in section 5 — this line uses the same", fm(n_notcomp))
add("(%s) source, so the two sections cannot disagree.", clust_source)
add("(An independent count of species with >=5 distinct indigenous localities gives %s.)", fm(n_ge5))
add("")
add("## 3. Population origin breakdown")
add("")
add("| Population origin | Species | Records |")
add("|---|---|---|")
add("| indigenous | %s | %s |", fm(orig$species_indigenous), fm(as.integer(orig$records_by_status[["indigenous"]] %||% 0)))
add("| non-indigenous | %s | %s |", fm(orig$species_non_indigenous), fm(as.integer(orig$records_by_status[["non-indigenous"]] %||% 0)))
add("")
add("(%s species have BOTH indigenous and non-indigenous records, so the species", fm(orig$species_both))
add("columns sum to more than the %s-species cohort.)", fm(n_species))
add("")
add("| Origin sub-category | Records |")
add("|---|---|")
for (k in c("type locality","native","invasive","introduced","cryptogenic")) {
  vv <- orig$records_by_origin[[k]]
  if (!is.null(vv)) add("| %s | %s |", k, fm(as.integer(vv)))
}
add("")
add("## 4. Vernacular / type locality / enrichment coverage")
add("")
add("| Metric | Value |")
add("|---|---|")
add("| Vernacular names retrieved | %s |", fm(vern_names))
add("| Distinct languages | %s |", fm(length(unique(vern_langs))))
add("| Species with >=1 vernacular name | %s |", fm(n_with_vern))
add("| Species with type locality identified | %s |", fm(n_type_loc))
add("| Countries covered — all records (indigenous + non-indigenous) | %s |", fm(length(all_countries)))
add("| Countries covered — native ranges only | %s |", fm(length(ind_countries)))
add("")
add("## 5. Spatial clustering output")
add("")
add("Source: **%s**. All rows below come from that one source, so", clust_source)
add("computed = multiple + single by construction.")
add("")
add("| Metric | Value |")
add("|---|---|")
add("| Species in cohort | %s |", fm(n_species))
add("| Clustering computed (>=5 valid coordinates) | %s |", fm(n_analysed))
add("| — of which multiple clusters (>1) | %s |", fm(n_multi))
add("| — of which single cluster | %s |", fm(n_single))
add("| Not computed (<5 valid coordinates) | %s |", fm(n_notcomp))
if (length(cl_analysed) > 0) {
  add("| Cluster count (detected only) — min / median / max | %s / %s / %s |",
      fm(min(cl_analysed)), fm(stats::median(cl_analysed)), fm(max(cl_analysed)))
}
add("")
if (isTRUE(n_single == 0))
  add("Note: the single-cluster class is empty — every species with enough valid coordinates resolved into >1 cluster.")
add("")
add("## 6. v%s -> v%s delta", V_BASE, V_CURR)
add("")
if (!is.null(delta_all) && nrow(delta_all) > 0) {
  add("All reprocessed species, one row per population scope (%d rows / %d species):",
      nrow(delta_all), length(unique(delta_all$species)))
  add("")
  add("| Species | Scope | Status | Records prev->curr | New extinctions | AOO prev->curr (%%) | EOO prev->curr | Trend |")
  add("|---|---|---|---|---|---|---|---|")
  d <- delta_all[order(-abs(delta_all$record_delta)), ]
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    add("| *%s* | %s | %s | %s -> %s (%s%s) | %s | %s -> %s (%s%%) | %s -> %s | %s |",
        r$species, r$scope, r$status, fm(r$records_prev), fm(r$records_curr),
        ifelse(r$record_delta >= 0, "+", ""), fm(r$record_delta),
        fm(r$new_extinctions), fm(r$AOO_prev), fm(r$AOO_curr),
        ifelse(is.na(r$AOO_pct), "n/a", sprintf("%+.1f", r$AOO_pct)),
        fm(r$EOO_prev), fm(r$EOO_curr), r$trend %||% "n/a")
  }
  add("")
  add("Note: **no new presence records** were added between v%s and v%s — the only input", V_BASE, V_CURR)
  add("change was the extinction claims. Across all species/scopes that is **%d distinct",
      sum(delta_all$new_extinctions, na.rm = TRUE))
  add("extirpated localities** (extinctions_count counts unique coordinates, not rows).")
  add("Every record delta is therefore negative: records leave the active set via the")
  add("extinction flag plus the 500 m backward suppression mask. EOO is unchanged wherever")
  add("the extirpated points sat inside the convex hull, which is why AOO moves but EOO")
  add("often does not.")
} else add("*No overlapping species between the two versions.*")
add("")

writeLines(L, file.path(base_dir, "manuscript_summary.md"), useBytes = TRUE)

out <- list(
  generated = as.character(Sys.Date()), base_dir = base_dir,
  baseline_version = V_BASE, current_version = V_CURR,
  validation = list(raw_records = n_raw, validated_records = n_valid,
                    removed_validation = n_invalid, removed_duplicates = n_dupes,
                    validated_pct = pct(n_valid, n_raw), total_species = n_species),
  distribution_category = as.list(cat_tab),
  species_ge5_indigenous_localities = n_ge5,
  population_origin = orig,
  coverage = list(vernacular_names = vern_names, languages = length(unique(vern_langs)),
                  species_with_vernacular = n_with_vern,
                  species_with_type_locality = n_type_loc,
                  countries = length(all_countries)),
  clustering = list(species_analysed = length(cl_analysed), multiple_clusters = n_multi,
                    single_cluster = n_single,
                    min = if (length(cl_analysed)) min(cl_analysed) else NA,
                    median = if (length(cl_analysed)) stats::median(cl_analysed) else NA,
                    max = if (length(cl_analysed)) max(cl_analysed) else NA),
  delta = delta_all
)
jsonlite::write_json(out, file.path(base_dir, "manuscript_summary.json"),
                     pretty = TRUE, auto_unbox = TRUE, na = "null")

flat <- rbind(
  data.frame(section = "validation", metric = c("raw_records","validated_records",
             "removed_validation","removed_duplicates","total_species"),
             value = c(n_raw, n_valid, n_invalid, n_dupes, n_species)),
  data.frame(section = "distribution_category", metric = names(cat_tab), value = as.integer(cat_tab)),
  data.frame(section = "clustering", metric = c("species_analysed","multiple_clusters","single_cluster"),
             value = c(length(cl_analysed), n_multi, n_single))
)
write.table(flat, file.path(base_dir, "manuscript_summary.tsv"),
            sep = "\t", row.names = FALSE, quote = FALSE)

cat(paste(L, collapse = "\n"), "\n")
cat(sprintf("\nWrote: %s/manuscript_summary.{md,json,tsv}\n", base_dir))
