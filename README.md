# cheCkOVER

**A reproducible framework for turning curated biodiversity occurrence records
into publication-ready, versioned species packages.**

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.2.0-blue.svg)](https://www.r-project.org/)

cheCkOVER ingests occurrence records, enriches them with spatial context,
computes distribution metrics, and emits a self-contained package per species —
maps, metrics, a geo-narrative, and a bibliography — with a per-run audit trail.
It was built for the [World of Crayfish](https://world.crayfish.ro) database
(~124k records, 676 species) but is not crayfish-specific.

---

## Table of contents

- [What it produces](#what-it-produces)
- [Design principles](#design-principles)
- [Installation](#installation)
- [Reference data](#reference-data)
- [Running the pipeline](#running-the-pipeline)
- [Versioning and change detection](#versioning-and-change-detection)
- [Verification](#verification)
- [Module map](#module-map)
- [Conventions that matter](#conventions-that-matter)
- [Data availability](#data-availability)
- [Citation](#citation)
- [License](#license)

---

## What it produces

For every species, under `<root>/<version>/<Species_name>/`:

```
Astacus_astacus/
├── package_metadata.json      metrics, provenance, snapshot, contents
├── maps/                      EOO, AOO, HydroBASINS (GeoJSON + KML)
├── narratives/                canonical .md, formal .txt, structured .json
├── citations/                 BibTeX, CSV, JSON, CITATION.cff
├── file_manifest.csv          sizes + MD5 per file
└── README.md
```

Run-level scaffolding lives in `<root>/<version>/checkover/` (manifest,
fingerprints, index) — deliberately outside the species namespace so a
consuming platform can treat every `Genus_species/` folder as a drop-in unit.

---

## Design principles

**One source of truth for every number.** `package_metadata.json`'s `metrics`
object is canonical. The narrative generator is a *pure formatter*: it never
recomputes a count, percentage or year. If a number appears in a narrative it
came from that object, so the prose and the JSON cannot drift apart.

**Metrics describe the active set.** Records flagged extinct, and records
suppressed by the extinction mask, are excluded from every metric — AOO, EOO,
counts, percentages, basins, protected areas. Maps are drawn from the same
active set, so a map can never show range that the metrics have already
retired.

**Missing data must never change a conclusion.** Geography that cannot be
resolved is written as the explicit sentinel `unresolved` — never a guess or a
placeholder — and is excluded from all counts and from category assignment.

**The source database wins.** Where the input already supplies country,
continent or subnational unit, cheCkOVER uses it verbatim and never recomputes
or overwrites it. Spatial lookup runs only to fill genuine gaps.

**Extinction is a data state, not a verdict.** A species reduced to zero active
occurrences is reported as `Extinct` with a mandatory disclaimer stating that
this reflects the state of the data and warrants field verification — never as
a biological determination.

---

## Installation

```r
install.packages(c(
  "sf", "dplyr", "tidyr", "stringr", "jsonlite", "readr", "digest",
  "rnaturalearth", "rnaturalearthdata", "geodata", "wdpar", "worrms",
  "lwgeom", "units", "glue", "progress", "future", "future.apply"
))
```

R ≥ 4.2 is recommended (developed and verified on 4.5.2). `sf` needs system
GDAL/GEOS/PROJ.

**Resources.** A full 124k-record / 676-species run needs roughly **32 GB RAM**
and takes **~3 hours** on 8 cores; peak RSS observed is ~22–32 GB. It will not
run on a typical laptop. Lower `CONFIG$memory$batch_size` for smaller machines,
or run a subset.

---

## Reference data

Not redistributed here — licences belong to the providers. Download and place
under `spatial_data/`:

| Layer | Source | Used for |
|---|---|---|
| HydroBASINS L6/L8/L10 | [HydroSHEDS](https://www.hydrosheds.org/products/hydrobasins) | basin assignment |
| FEOW | [feow.org](https://www.feow.org/) | freshwater ecoregions |
| WDPA | [Protected Planet](https://www.protectedplanet.net/) | protected-area overlap |
| GADM v4.1 | [gadm.org](https://gadm.org) | country / admin-1 fallback |
| Natural Earth | via `rnaturalearth` | continents, admin-0 fallback |
| TEOW | WWF | terrestrial ecoregions |

Two small lookups ship with the repo:
`WoC_canonical_country_continent.tsv` (canonical geographic vocabulary) and
`ecoregions_list.tsv`. The HydroBASINS name table (`Table_S3.tsv`, 39 MB) is
distributed with the paper's supplementary material.

---

## Running the pipeline

Edit `config.R`:

```r
CONFIG$input_file        <- "WoC_1_0.tsv"   # occurrence export
CONFIG$framework_version <- "1.0"           # drives <root>/<version>/
CONFIG$root_output_dir   <- "checkover_output"
```

Then:

```bash
Rscript checkcover_main.R
```

The run refuses to start if `framework_version` already has output on disk —
the usual guard against silently overwriting a published version. Archive or
bump the version.

### Input format

A tab-separated export with Darwin Core-aligned headers. Required:
`scientificName`, `decimalLatitude`, `decimalLongitude`, `year`,
`establishmentMeans`. Optional but used when present: `occurrenceOrigin`,
`claimExtinction`, `continent`, `country`, `county`, `catalogNumber`,
`institutionCode`, `confidentialityLevel`, `contributor`, `occurrenceRemarks`,
citation fields. Legacy WoC headers are still accepted via an alias table
(`R/00_dwc_fields.R`).

> **Note on delimiters.** Free-text fields containing an unescaped tab or
> newline will shift or split a row. cheCkOVER detects the resulting nonsense
> geography and routes those records to the fallback, but it is far better to
> escape them at export time.

---

## Versioning and change detection

Runs are **sparse**. Each species is fingerprinted (SHA-256 over its
comparison columns); a species whose fingerprint is unchanged since the
previous version is not reprocessed — the manifest points at the version where
its artifacts already live. Only `new` and `reprocessed` species are rebuilt.

This makes an incremental version cheap: the v1.0 → v1.1 run in the reference
dataset reprocessed 16 of 676 species. `<version>/checkover/manifest.json` is
the consumer-facing record of what lives where.

---

## Verification

Three independent gates, all runnable standalone:

```bash
Rscript tests/run_all.R                              # unit + regression suite
Rscript tests/audit_packages.R checkover_output/1.0  # per-package integrity
Rscript verify_geo_acceptance.R checkover_output/1.0 # geographic acceptance
```

**`tests/run_all.R`** — 14 files covering the classifier, extinction handling,
the geographic fallback, vocabulary, Darwin Core mapping, fingerprinting, basin
resolution and narrative consistency. Each runs in its own process.

**`tests/audit_packages.R`** — for every species package, asserts the expected
artifacts exist and are non-empty, and that every headline number in the
narrative equals the corresponding field in `package_metadata.json`. Exits
non-zero on any mismatch. Run it before publishing anything.

**`verify_geo_acceptance.R`** — checks that no emitted geography falls outside
the canonical vocabulary, that no species is classified on unresolved or
snapped values, and reports native / fallback / unresolved counts per field.

Extraction helpers for reporting: `extract_manuscript_summaries.R` and
`extract_manuscript_tables.R` produce aggregate summary tables (counts only, no
coordinates) as `.md`, `.json` and `.tsv`.

---

## Module map

| Stage | Module | Does |
|---|---|---|
| 0 | `00_helpers`, `00_logging`, `00_run_context` | shared utilities, logging, run context + fingerprinting |
| 0 | `00_geo_canon` | canonical geographic vocabulary; `unresolved` sentinel |
| 0 | `00_dwc_fields` | Darwin Core ↔ internal field mapping (exposed layer only) |
| 0 | `00_spatial_sanitize` | geometry repair; antimeridian-safe extent audit |
| 1 | `01_ingest` | load, validate, map to internal schema |
| 1 | `01e_change_detection` | per-species fingerprints → active set |
| 1 | `01b_vernacular`, `01c_split`, `01d_*` | common names, population split, clustering, scenarios |
| 2 | `02a_continents`, `02b_gadm` | continent / country / admin-1 — **native first, lookup only for gaps** |
| 2 | `02c_geo_integrity` | per-run provenance report: native / fallback / unresolved |
| 2 | `02c_teow`, `02d_feow`, `02e_wdpa`, `02f_hydrobasins` | ecoregions, protected areas, basins |
| 3–4 | `03a`/`04a` metrics, `03b`/`04b` enrich, `03c`/`04c` reports | per-population metrics and per-species report JSON |
| 5 | `05_merge_scenario3` | merge species with both native and introduced populations |
| 7–9 | `07_citations`, `08_maps`, `09_package_export` | bibliographies, map layers, package assembly |
| 10 | `10_canonical_narratives` | geo-narrative (canonical .md, .txt, .json) |
| 11–13 | `11_temporal_delta`, `12_temporal_outputs`, `13_temporal_pipeline` | version-over-version change detection and temporal maps |

Files marked `_DEPRECATED` are retained for provenance and are not sourced.

---

## Conventions that matter

These are decisions, not accidents — changing one changes published numbers.

| Rule | Value |
|---|---|
| EOO | convex hull; **undefined (`NA`, never 0)** below 3 unique points |
| Distribution category | `< 3` active records ⇒ **endemic** (short-range), regardless of EOO |
| Cosmopolitan | a continent counts only with **≥ 5 records and ≥ 5 %** of the species' labelled records; blanks excluded |
| Non-indigenous category | `< 3` records ⇒ **local**, not widespread |
| post-2000 | strictly `year > 2000` |
| Extinction mask | records within **500 m** of an extinction event and predating it are suppressed |
| Zero active records | terminal state: `status = Extinct`, AOO 0, EOO `NA`, no basins, mandatory disclaimer |
| Nearest-land snap | capped at **100 km**, and rejected if the continent appears in no other record of that species |
| Continent vocabulary | exactly six values; `Australia` is a country ⇒ `Oceania` |
| Spatial clustering | descriptive signal only — the term *fragmentation* is reserved for downstream connectivity work |

Darwin Core naming applies to the **exposed layer only** — output properties,
JSON keys, export schema. Internal column names and values are unchanged, and
`dwc:`/`dcterms:` prefixes appear only in a DwC-Archive `meta.xml`.

---

## Data availability

This repository contains **code only**. The occurrence records are not included:
a majority of them carry a confidentiality flag, and their exact coordinates are
withheld by design — the export module strips coordinates and confidentiality
fields from every published package. Request data through
[World of Crayfish](https://world.crayfish.ro) under its access terms.

---

## Citation

If you use cheCkOVER, please cite the software and the underlying data:

```bibtex
@software{checkover,
  title  = {cheCkOVER: a reproducible framework for versioned biodiversity
            occurrence packages},
  author = {Pârvulescu, Lucian and collaborators},
  year   = {2026},
  url    = {https://github.com/<owner>/<repo>},
  note   = {Version 1.0}
}
```

**Underlying data** — Ion, M. C. et al. (2024). World of Crayfish™: a web
platform towards real-time global mapping of freshwater crayfish and their
pathogens. *PeerJ* 12:e18229. <https://doi.org/10.7717/peerj.18229>

**Reference layers** — Natural Earth; GADM v4.1; Olson et al. (2001) *BioScience*
51:933–938 (TEOW); Abell et al. (2008) *BioScience* 58:403–414 (FEOW);
UNEP-WCMC & IUCN (2026) Protected Planet; Lehner & Grill (2013)
*Hydrological Processes* 27:2171–2186 (HydroBASINS); WoRMS Editorial Board.

---

## License

Code released under **CC BY 4.0**. Reference layers and occurrence data remain
under their own licences.
