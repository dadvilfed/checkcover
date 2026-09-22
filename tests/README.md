# cheCkOVER tests

Fast, dependency-light regression tests for the 2026-06 fixes (Lucian's
New Orleans review). They exercise the real pipeline functions where practical
and drive the audit gate on freshly generated packages.

## Run everything

```bash
Rscript tests/run_all.R
```

Runs each `test_*.R` in its own R process and prints a PASS/FAIL summary
(exit non-zero if any fail). Runnable from any working directory — each test
resolves the project root from its own path.

## Run one

```bash
Rscript tests/test_unit.R
```

## What each file covers

| File | Covers | Deps |
|------|--------|------|
| `test_unit.R` | Vernacular GBIF trailing-code inheritance; DwC input/output field maps; DwC export crosswalk (`establishmentMeans`/`degreeOfEstablishment`/`basisOfRecord`/`dynamicProperties`); single-source trend rule; zero-active helper | base R only |
| `test_classifier.R` | `<3 active records → endemic` with `EOO = NA` (real `calculate_indigenous_metrics`) | `dplyr` |
| `test_dwc_ingest.R` | New DwC template header → internal columns, comma-decimals, `establishmentMeans`/`occurrenceOrigin` kept separate, voucher fields, derived `basisOfRecord` (real `map_woc_to_checkover`) | `dplyr`, `stringr` |
| `test_narrative_integration.R` | A freshly generated narrative is numerically consistent with `package_metadata.json` — zero audit mismatches (bugs 0/1/2/3 + extinctions) | base R only |
| `test_extinct_path.R` | Zero-active terminal narrative: extirpation statement, AOO=0/EOO=NA, mandatory data-state disclaimer, no basins layer; passes both audits | base R only |
| `test_coordinate_free.R` | A revision folder carries no coordinates: the audit's coordinate scan flags binaries, coordinate columns/keys, lat/lon pairs and map points, and does not flag DOIs or range polygons; `records_used.tsv` has exactly `record_id, species, state` | `jsonlite` |
| `test_run_file.R` | Service mode: the `CHECKOVER_RUN` run file overrides `config.R` and a bad one is refused with every problem in `preflight.json`; exit codes 0/1/2 and `status.json` from real child R processes; output root holds revisions only, a state dir belongs to one output root; the temporal history rolls back after a run that did not complete | `jsonlite` |
| `test_version_order.R` | Revisions compare by numeric component: builds 1.2 → 1.9 → 1.10 → 1.11 → 1.100 with real change detection; 1.100 is latest, each revision's predecessor and every taxon's source are right; the preflight refuses a number that is not after the latest | `digest`, `jsonlite`, `readr` |
| `test_citation.R` | The cheCkOVER reference is exactly the agreed text (code points included), rebuilt from its fields, and carried as preferred-citation in the CFF Module 7 writes, in package_metadata.json and in the READMEs | `dplyr`, `jsonlite`, `glue` |
| `test_github_packages.R` | GitHub-only packages are named once (`GITHUB_PACKAGES` in config.R): required ones pinned to a commit, the Dockerfile installs exactly that spec, and no code, message or instruction names another repository | base R only |
| `test_species_scope.R` | `species_scope`: approved taxa reprocessed, others `deferred`/`deferred_new`; across two runs a deferred change is still seen (source fingerprint kept) | `digest`, `jsonlite`, `readr` |

## Notes

- **`sf` is intentionally avoided.** Loading `sf` through the packaged Rscript
  segfaults on this Windows/GDAL setup. The classifier test only uses `<3`-point
  and undefined-EOO paths, which never call `sf`.
- These tests do **not** replace the full-pipeline rerun (which needs the real
  spatial layers). They guard the pure/logic layer that carried Lucian's bugs.
- The cohort regression gate for real output is `Rscript audit_packages.R <version_dir>`.
