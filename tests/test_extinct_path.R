#!/usr/bin/env Rscript
# Zero-active (total extinction) terminal path: a Cambarus veitchorum-style
# species (1 record, 1968, now extinct) must produce a VALID minimal narrative
# with the data-state disclaimer, and pass both audits (Lucian email 1 + bug 2).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

# audit_packages.R lives in tests/ (repo reorganised 2026-07); accept either.
.audit <- if (file.exists("tests/audit_packages.R")) "tests/audit_packages.R" else "audit_packages.R"
suppressWarnings(suppressMessages({
  source("R/00_helpers.R"); source("R/00_dwc_fields.R")
  source("R/10_canonical_narratives.R"); source(.audit)
}))
iso <- .iso639_language_map()

ind_report <- list(
  species="Cambarus veitchorum", status="Extinct", terminal_state="zero_active",
  taxonomy=list(higher_taxonomy="Decapoda > Astacoidea > Cambaridae"),
  metrics=list(n_records=0L, eoo_km2=NA, aoo_km2=0, iucn_category="Extinct"),
  temporal=list(year_min=1968, year_max=1968, pct_post_2000=NA),
  counts=list(n_countries=0L, n_hydrobasins=0L, n_extinctions=1L),
  conservation=list(n_protected_records=0L, protection_percentage=NA),
  extinction_year=1968, integrity_flag="MAX",
  disclaimer=CHECKOVER_EXTINCTION_DISCLAIMER, type_locality_present=FALSE)

can <- .build_canonical_narrative("Cambarus veitchorum", 1, "Veitch's crayfish",
  NULL, NULL, ind_report, NULL, iso, NULL, NULL, "1.1", "v1.1",
  "2026-07-11", NULL, "Cambarus_veitchorum", "TEST", NULL)

tmp <- file.path(tempdir(),"extpkg_it"); unlink(tmp, recursive=TRUE)
d <- file.path(tmp,"Cambarus_veitchorum"); dir.create(file.path(d,"narratives"), recursive=TRUE); dir.create(file.path(d,"maps"))
writeLines(can$full_markdown, file.path(d,"narratives","Cambarus_veitchorum_canonical.md"), useBytes=TRUE)
writeLines(can$formal_narrative_text, file.path(d,"narratives","Cambarus_veitchorum_narrative.txt"), useBytes=TRUE)
# Extinct package: a "former range" layer is allowed, but NO current basins layer.
writeLines("{}", file.path(d,"maps","Cambarus_veitchorum_former_range.geojson"))

pkg <- list(species="Cambarus veitchorum", status="Extinct",
  snapshot=list(version="1.1", is_baseline=FALSE),
  data_disclaimer=CHECKOVER_EXTINCTION_DISCLAIMER, integrity_flag="MAX",
  metrics=list(indigenous=list(AOO_km2=0L, EOO_km2=NA, records=0L, basins_count=0L, extinctions_count=1L),
               non_indigenous=list(AOO_km2=0L, EOO_km2=NA, records=0L, basins_count=0L, extinctions_count=0L)))
jsonlite::write_json(pkg, file.path(d,"package_metadata.json"), pretty=TRUE, auto_unbox=TRUE, na="null")

cons <- audit_narrative_metadata_consistency(tmp)
integ <- audit_folder_integrity(tmp)
cat(sprintf("[test_extinct_path] consistency mismatches: %d ; integrity flags: %d\n", cons$n_mismatched, integ$n_flagged))
for (sp in names(cons$mismatches)) for (p in cons$mismatches[[sp]]) cat("  cons* ",p,"\n")
for (sp in names(integ$flags))     for (p in integ$flags[[sp]])     cat("  integ*",p,"\n")

fail <- cons$n_mismatched > 0 || integ$n_flagged > 0 ||
        !grepl("Extinct", can$full_markdown) || !grepl("field verification", can$formal_narrative_text) ||
        !grepl("0 km", can$full_markdown) || !grepl("undefined", can$full_markdown)
cat(sprintf("[test_extinct_path] %s\n", if (fail) "FAIL" else "PASS"))
quit(status = if (fail) 1 else 0)
