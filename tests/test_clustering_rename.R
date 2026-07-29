#!/usr/bin/env Rscript
# "fragmentation" is reserved for checKOLOGY. In cheCkOVER every user-facing
# label, output string and JSON key must say "spatial clustering" instead
# (Lucian, 2026-07, task A). Values and thresholds are unchanged.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

suppressWarnings(suppressMessages({
  source("R/00_helpers.R"); source("R/00_dwc_fields.R"); source("R/10_canonical_narratives.R")
}))

PASS <- 0L; FAIL <- 0L
ok <- function(l, c) { if (isTRUE(c)) { PASS <<- PASS + 1L; cat("  ok  ", l, "\n") }
                       else           { FAIL <<- FAIL + 1L; cat("  FAIL", l, "\n") } }

iso <- .iso639_language_map()
mk_report <- function(block_name) {
  r <- list(
    species = "Testus astacus",
    taxonomy = list(higher_taxonomy = "Decapoda > Astacidae"),
    metrics = list(n_records = 40L, eoo_km2 = 5000, aoo_km2 = 100, iucn_category = "regional"),
    temporal = list(year_min = 1990, year_max = 2010, pct_post_2000 = 50),
    counts = list(n_countries = 1L, n_hydrobasins = 2L, n_named_basins = 2L, n_extinctions = 0L),
    conservation = list(n_protected_records = 2L, protection_percentage = 5))
  r[[block_name]] <- list(computed = TRUE, status = "detected",
                          n_clusters = 4L, cluster_sizes = "20/10/6/4")
  r
}
sp_ind <- data.frame(species = "Testus astacus", country = "Romania", admin_1 = "Bihor",
                     hydrobasin = c("L8:1", "L8:2"), freshwater_ecoregion = "101",
                     ecoregion = "Carpathian", protected_area = c("Apuseni", ""),
                     year = c(1990, 2005), accuracy = "exact", doi = "", citation = "",
                     temporal_status = "active", is_type_locality = FALSE,
                     stringsAsFactors = FALSE)

build <- function(rep) .build_canonical_narrative(
  sp = "Testus astacus", scenario = 1, vern_str = NA_character_,
  sp_ind = sp_ind, sp_nind = NULL, ind_report = rep, nind_report = NULL,
  iso_lang = iso, feow_map = NULL, hydrobasin_names = NULL,
  checkover_version = "1.1", output_version = "v1.1", snapshot_date = "2026-07-20",
  prev_reports_dir = NULL, sp_clean = "Testus_astacus", module = "TEST", ctx = NULL)

md <- build(mk_report("spatial_clustering"))$full_markdown

cat("== new labels present ==\n")
ok("section header renamed",   grepl("### 2.3 Spatial Clustering (Indigenous Range Only)", md, fixed = TRUE))
ok("signal field renamed",     grepl("**Spatial clustering signal:** detected", md, fixed = TRUE))
ok("new Para-3 wording",       grepl("Spatial analysis identifies 4 distinct occurrence clusters within the native range.", md, fixed = TRUE))
ok("new caveat wording",       grepl("conservative descriptive signal of spatial structure", md, fixed = TRUE))
ok("caveat still disclaims fragmentation", grepl("connectivity, fragmentation, or Red List assessment", md, fixed = TRUE))
ok("cluster count unchanged",  grepl("**Number of clusters:** 4", md, fixed = TRUE))

cat("== old labels gone ==\n")
ok("no 'Fragmentation Assessment' header", !grepl("Fragmentation Assessment", md, fixed = TRUE))
ok("no 'Fragmentation signal'",            !grepl("Fragmentation signal", md, fixed = TRUE))
ok("no 'reveals **fragmentation**'",       !grepl("reveals **fragmentation**", md, fixed = TRUE))
ok("no 'spatially disjunct'",              !grepl("spatially disjunct", md, fixed = TRUE))

cat("== back-compat: old report key still read ==\n")
md_old <- build(mk_report("fragmentation"))$full_markdown
ok("old `fragmentation` block still renders", grepl("**Spatial clustering signal:** detected", md_old, fixed = TRUE))

cat(sprintf("\n[test_clustering_rename] %d passed, %d failed\n", PASS, FAIL))
quit(status = if (FAIL > 0) 1 else 0)
