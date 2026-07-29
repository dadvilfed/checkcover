#!/usr/bin/env Rscript
# End-to-end: generate a narrative from a synthetic canonical report and confirm
# the consistency audit reports ZERO mismatches — i.e. the narrative is a
# faithful formatter of package_metadata.json (Lucian bugs 0/1/2/3 + extinctions).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

# audit_packages.R lives in tests/ (repo reorganised 2026-07); accept either.
.audit <- if (file.exists("tests/audit_packages.R")) "tests/audit_packages.R" else "audit_packages.R"
suppressWarnings(suppressMessages({
  source("R/00_helpers.R"); source("R/00_dwc_fields.R")
  source("R/10_canonical_narratives.R")
  source(.audit)
}))

iso <- .iso639_language_map()

ind_report <- list(
  species = "Testus astacus",
  taxonomy = list(higher_taxonomy = "Decapoda > Astacoidea > Astacidae"),
  metrics = list(n_records = 3L, eoo_km2 = 5000, aoo_km2 = 100,
                 iucn_category = "regional", hydrobasins_level = 8L),
  temporal = list(year_min = 1990, year_max = 2010, pct_post_2000 = 33.3),
  counts = list(n_countries = 1L, n_hydrobasins = 2L, n_named_basins = 2L, n_extinctions = 3L),
  conservation = list(n_protected_records = 2L, protection_percentage = 66.7),
  spatial_clustering = list(computed = FALSE, status = "not_detected"),
  type_locality_present = FALSE
)

sp_ind <- data.frame(
  species = rep("Testus astacus", 3),
  country = "Romania", admin_1 = c("Bihor","Cluj","Bihor"),
  hydrobasin = c("L8:1","L8:2","L8:1"),
  freshwater_ecoregion = c("101","101","102"),
  ecoregion = c("Carpathian montane","Carpathian montane","Pannonian"),
  protected_area = c("Apuseni","", "Apuseni"),
  year = c(1990, 2005, 2010), accuracy = c("exact","exact","locality"),
  doi = c("10.1/x","10.1/x",""), citation = c("Ion 2024","Ion 2024",""),
  temporal_status = "active", is_type_locality = FALSE, stringsAsFactors = FALSE
)

can <- .build_canonical_narrative(
  sp = "Testus astacus", scenario = 1, vern_str = "rac de râu(ron) | noble crayfish",
  sp_ind = sp_ind, sp_nind = NULL, ind_report = ind_report, nind_report = NULL,
  iso_lang = iso, feow_map = NULL, hydrobasin_names = NULL,
  checkover_version = "1.1", output_version = "v1.1",
  snapshot_date = "2026-07-11", prev_reports_dir = NULL,
  sp_clean = "Testus_astacus", module = "TEST", ctx = NULL
)

tmp <- file.path(tempdir(), "auditpkg_it"); unlink(tmp, recursive = TRUE)
d <- file.path(tmp, "Testus_astacus"); dir.create(file.path(d, "narratives"), recursive = TRUE); dir.create(file.path(d, "maps"))
writeLines(can$full_markdown, file.path(d, "narratives", "Testus_astacus_canonical.md"), useBytes = TRUE)
writeLines(can$formal_narrative_text, file.path(d, "narratives", "Testus_astacus_narrative.txt"), useBytes = TRUE)
writeLines("{}", file.path(d, "maps", "Testus_astacus_AOO.geojson"))
writeLines("{}", file.path(d, "maps", "Testus_astacus_basins.geojson"))

pkg_meta <- list(
  species = "Testus astacus", package_id = "Testus_astacus",
  snapshot = list(version = "1.1", date = "2026-07-11", is_baseline = FALSE),
  scenario = 1, status = "Extant",
  metrics = list(
    indigenous = list(AOO_km2 = 100L, EOO_km2 = 5000L, records = 3L, basins_count = 2L,
                      countries_count = 1L, protected_areas_count = 1L, n_clusters = 0L,
                      records_post_2000_pct = 33.3, records_in_protected_areas_pct = 66.7,
                      extinctions_count = 3L, trend_vs_previous = NA),
    non_indigenous = list(AOO_km2 = 0L, EOO_km2 = NA, records = 0L, basins_count = 0L, extinctions_count = 0L)
  )
)
jsonlite::write_json(pkg_meta, file.path(d, "package_metadata.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")

cons  <- audit_narrative_metadata_consistency(tmp)
integ <- audit_folder_integrity(tmp)
cat(sprintf("[test_narrative_integration] consistency mismatches: %d ; integrity flags: %d\n",
            cons$n_mismatched, integ$n_flagged))
for (sp in names(cons$mismatches))  for (p in cons$mismatches[[sp]]) cat("  cons* ", p, "\n")
for (sp in names(integ$flags))      for (p in integ$flags[[sp]])     cat("  integ*", p, "\n")

fail <- cons$n_mismatched > 0 || integ$n_flagged > 0
cat(sprintf("[test_narrative_integration] %s\n", if (fail) "FAIL" else "PASS"))
quit(status = if (fail) 1 else 0)
