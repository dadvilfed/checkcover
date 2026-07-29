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

## Notes

- **`sf` is intentionally avoided.** Loading `sf` through the packaged Rscript
  segfaults on this Windows/GDAL setup. The classifier test only uses `<3`-point
  and undefined-EOO paths, which never call `sf`.
- These tests do **not** replace the full-pipeline rerun (which needs the real
  spatial layers). They guard the pure/logic layer that carried Lucian's bugs.
- The cohort regression gate for real output is `Rscript audit_packages.R <version_dir>`.
