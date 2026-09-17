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
- [Try it on the demo dataset first](#try-it-on-the-demo-dataset-first)
- [Running the pipeline](#running-the-pipeline)
  - [Configuration reference](#configuration-reference)
  - [Parallelisation](#parallelisation)
- [Versioning and change detection](#versioning-and-change-detection)
- [Troubleshooting](#troubleshooting)
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

The run directory also carries an audit of what was discarded during ingest:

```
ingest_validation_report.tsv   count and reason for every removed record
ingest_dropped_records.tsv     identifiers of those records (no coordinates)
```

Spatial outputs are **GeoJSON and KML**. KMZ is not produced.

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

cheCkOVER is a set of scripts, not an R package. There is nothing to
`install.packages()` — you clone the repository and run the entry-point script
from inside it.

**1. Clone the repository.**

```bash
git clone https://github.com/dadvilfed/checkcover.git
cd checkcover
```

**2. Install the CRAN dependencies.** From an R session opened *in that
directory*:

```r
source("config.R")
install.packages(REQUIRED_PACKAGES)
```

`REQUIRED_PACKAGES` in `config.R` is the authoritative list — installing from it
cannot drift out of step with what the code loads.

**3. Install the packages that are not on CRAN.** `install.packages()` cannot
fetch these and will fail with "package is not available":

```r
install.packages("remotes")
remotes::install_github("jeffreyhanson/ecoregions")
```

`ecoregions` supplies the TEOW terrestrial-ecoregion polygons used by Module 2C.
It is **required** — unlike FEOW, TEOW has no local-file alternative.

Optionally, `remotes::install_github("mhpob/feowR")` provides an alternative
source for freshwater ecoregions; see `CONFIG$spatial$feow_source` below. The
default (`"local"`) does not need it.

**4. Check your setup before running anything.**

```r
source("config.R"); source("R/00_logging.R"); source("R/00_helpers.R")
source("R/00_dwc_fields.R"); source("R/00_preflight.R")
preflight_check(CONFIG, strict = FALSE)
```

This verifies every input, lookup table, reference layer, package and output
path, and reports **everything** that is missing in one pass. The full pipeline
runs the same check automatically and refuses to start if anything blocking is
absent, so a broken setup costs one run to diagnose rather than one run per
missing file.

R ≥ 4.2 is recommended (developed and verified on 4.5.2). `sf` needs system
GDAL/GEOS/PROJ. On Ubuntu: `apt install libgdal-dev libgeos-dev libproj-dev
libudunits2-dev`. The `Dockerfile` in this repository provides a working stack
if you would rather not build one.

### Resources and runtime

A full 124k-record / 676-species run needs roughly **32 GB RAM**; peak RSS
observed is ~22–32 GB. It will not run on a typical laptop. Lower
`CONFIG$memory$batch_size` for smaller machines, or run a subset.

**Runtime is dominated by the protected-area step.** Earlier versions of this
README quoted ~3 hours for a full run; that figure predated a change in how
WDPA geometries were cleaned and was not achievable with the shipped settings.
`wdpar::wdpa_clean()` defaults `erase_overlaps = TRUE`, an operation the wdpar
authors themselves recommend disabling for larger datasets, and with it enabled
a **four-species** dataset can take the better part of a day.

`CONFIG$spatial$wdpa_erase_overlaps` now exposes it and defaults to `FALSE`.
With that default, a full production run took **~29 hours** end to end
(2026-08, 677 species, 8 cores), of which ~24 hours was HydroBASINS assignment
and ~1.7 hours WDPA. Budget accordingly: this is an overnight job, not a
coffee-break one. A four-species demo run takes minutes.

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

Offline lookups ship with the repo: `WoC_canonical_country_continent.tsv`
(canonical geographic vocabulary), `Table_S3.tsv` (HydroBASINS names, 39 MB),
`(Table_S4)ecoregions_list.tsv` (freshwater ecoregion names) and
`(Table_S2)vernacular_names_wide.tsv` (curated common names). The last two also
serve as manuscript supplements, hence the prefixed filenames — set
`CONFIG$dictionaries$feow` and `CONFIG$vernaculars$path` to match them.

---

## Try it on the demo dataset first

A 519-record, 4-species extract from the Ponto-Caspian crayfish data ships in
`demo_data/WoC_demo_Pontastacus.tsv` (all records are `confidentialityLevel 0`,
i.e. public). It exercises every branch — both population streams, a type
locality, an extinction claim — and runs in minutes.

```r
CONFIG$input_file        <- "demo_data/WoC_demo_Pontastacus.tsv"
CONFIG$framework_version <- "0.1"
```

```bash
Rscript checkcover_main.R
```

Expected: 519 records in, 15 removed as duplicates, **504 retained across 4
species** — 252 indigenous, 252 non-indigenous. The reference layers are still
required, so run `preflight_check()` first.

---

## Running the pipeline

Edit `config.R`, then `Rscript checkcover_main.R`.

The run refuses to start if `framework_version` already has output on disk —
the usual guard against silently overwriting a published version. Archive or
bump the version.

### Configuration reference

`config.R` is commented throughout; this is the summary. Everything not listed
has a working default.

**Paths and identity**

| Setting | Default | What it does |
|---|---|---|
| `input_file` | `"WoC_1_1.tsv"` | Occurrence export to process. |
| `root_output_dir` | `"checkover_output"` | Everything is written under here. |
| `framework_version` | `"1.1"` | Output folder `<root>/<version>/`. Must match `^\d+\.\d+$`. |
| `version` | `"production"` | Run id. **Change it to force a re-ingest** — reusing it resumes from cached data. |

**Reference layers** (`CONFIG$spatial`)

| Setting | Default | What it does |
|---|---|---|
| `hydro_dir` | `"spatial_data/hydrobasins"` | Where you unpacked HydroBASINS. |
| `hydro_files` | `lev06/08/10` | Filenames per level; all three required. |
| `hydro_bbox` | `50` | Bounding-box expansion (km) when cropping basins. |
| `feow_source` | `"local"` | `"local"` (your shapefile), `"feowR"` (the package), or `"auto"`. |
| `feow_path` | `spatial_data/feow/...` | Used when `feow_source` is `"local"`. |
| `wdpa_km` | `2` | Buffer (km) around protected areas. |
| `wdpa_erase_overlaps` | `FALSE` | Passed to `wdpa_clean()`. **The single largest runtime lever** — see Resources. |
| `gadm_version`, `ne_scale` | `"4.1"`, `"medium"` | GADM release and Natural Earth resolution. |

**Lookup tables**

| Setting | Default |
|---|---|
| `vernaculars$path` | `"(Table_S2)vernacular_names_wide.tsv"` |
| `dictionaries$feow` | `"(Table_S4)ecoregions_list.tsv"` |
| `dictionaries$hydrobasins` | `"Table_S3.tsv"` |

The parenthesised prefixes are part of the filenames — these tables double as
manuscript supplements.

**Analysis**

| Setting | Default | What it does |
|---|---|---|
| `clustering$method` | `"basin"` | `"basin"` connects records sharing a HydroBASINS unit; `"euclidean"` is the distance-only fallback. |
| `clustering$threshold_km` | `10` | Separation above which records in *different* basins are separate clusters. **Provisional** — see Conventions. |
| `clustering$linkage` | `"single"` | Required for `"basin"`, not merely preferred — see Conventions. |
| `temporal$enabled` | `TRUE` | Per-species versioned temporal tracking (Modules 11–13). |
| `temporal$major_bump` | `FALSE` | Force v1.x → v2.0 instead of v1.x+1. |
| `force_reprocess` | `FALSE` | `TRUE`, or a vector of species, to rebuild despite unchanged data. See below. |

**Resources**

| Setting | Default | What it does |
|---|---|---|
| `memory$batch_size` | `5` | Species per batch. Lower it on smaller machines. |
| `memory$max_worker_memory` | `1500` | MB per worker. |
| `reporting$formats` | `c("geojson","kml")` | Map output formats. KMZ is **not** produced. |
| `parallel$force_sequential` | `TRUE` | See Parallelisation. |
| `reporting$parallel_maps` | `FALSE` | **Inert.** See Parallelisation. |

### Parallelisation

**cheCkOVER does not run in parallel.** Every run is sequential on every
platform, regardless of `parallel$force_sequential`, `parallel$workers` or
`reporting$parallel_maps` — nothing reads those settings.

`R/08_maps_parallel.R` exists but is never sourced and never called. It is an
unfinished experiment, kept deliberately because the problem is still open.
Each worker needed its own copy of the HydroBASINS layers, so memory cost scaled
with worker count instead of being amortised; runs died with allocation
failures, and the configurations that survived were not meaningfully faster,
because the work is dominated by geometry operations that were already
memory-bound. Making it work needs the reference layers shared rather than
duplicated, which is a larger change than parallelising the loop.

Treat parallel execution as future work, not a feature.

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

### Forcing a reprocess

Fingerprints cover the **input data**, so a code-only change — a new output
property, a geometry fix, a renamed JSON key — is invisible to change
detection. Every species comes out `unchanged`, keeps inheriting the previous
version's artifacts, and the fix never reaches the output. `force_reprocess`
in `config.R` is the override:

```r
CONFIG$force_reprocess <- FALSE                  # normal sparse versioning
CONFIG$force_reprocess <- TRUE                   # rebuild every species
CONFIG$force_reprocess <- c("Cherax destructor") # rebuild only these
```

The vector form accepts either the display name or the package id
(`Cherax_destructor`); names matching no species in the run are reported rather
than silently ignored.

Forced species are recorded as `reprocessed` — never `new`, which would drop
`prior_source_version` and cost the temporal delta — with a `change_summary`
stating the reprocess was forced, so a manifest full of reprocessed species is
never mistaken for that many real data changes. The temporal pipeline is
unaffected: Phase 5C runs its own data comparison and still skips species whose
occurrences are identical, so no spurious per-species versions are created.

Use it for code-only changes, then set it back to `FALSE`. Data changes need no
override.

---

## Troubleshooting

Run `preflight_check(CONFIG, strict = FALSE)` first — it catches most of these
before anything is processed.

**`package 'ecoregions' is not available`** — it is not on CRAN. Install it with
`remotes::install_github("jeffreyhanson/ecoregions")`. It is required: TEOW has
no local-file alternative.

**`Vernacular file path is invalid`** — `CONFIG$vernaculars$path` must name a
file that exists. The shipped table is `(Table_S2)vernacular_names_wide.tsv`;
the parenthesised prefix is part of the filename.

**The run produced empty outputs and no obvious error** — almost always a
missing `establishmentMeans`. cheCkOVER splits occurrences into indigenous and
non-indigenous streams, and a record with neither value enters neither. The run
now prints how many records this affects; if it is all of them, nothing
meaningful is produced. GBIF exports frequently carry the column but leave it
empty. `occurrenceOrigin` (native / type locality / introduced / invasive /
cryptogenic) is a **different field** and is not a substitute.

**Records disappeared between input and output** — read
`ingest_validation_report.tsv` in the run directory. It gives the count and
reason for every removal, and `ingest_dropped_records.tsv` lists the
identifiers. Note that de-duplication keys on species + coordinates + year, so
two records of the same occurrence from different sources collapse to one.

**Some species have no taxonomic hierarchy** — WoRMS could not resolve those
names; the run reports which. Occurrences are still processed and still get
metrics. The usual cause is an authorship string left in the name field (common
in GBIF exports: `Astacus astacus (Linnaeus, 1758)` rather than
`Astacus astacus`).

**It has been running for hours** — check `CONFIG$spatial$wdpa_erase_overlaps`
is `FALSE`, and see Resources for what a realistic runtime looks like. This is
an overnight job at full scale.

**`framework_version already has output on disk`** — deliberate. Bump
`CONFIG$framework_version` or archive the existing folder.

**Nothing was reprocessed even though I changed the code** — change detection
fingerprints the *data*, not the code. Set `CONFIG$force_reprocess <- TRUE`, or
name specific species. See Forcing a reprocess.

---

## Verification

Two gates ship with the repository, both runnable standalone:

```bash
Rscript tests/run_all.R                              # unit + regression suite
Rscript tests/audit_packages.R checkover_output/1.0  # per-package integrity
```

**`tests/run_all.R`** — 14 files covering the classifier, extinction handling,
the geographic fallback, vocabulary, Darwin Core mapping, fingerprinting, basin
resolution and narrative consistency. Each runs in its own process.

**`tests/audit_packages.R`** — for every species package, asserts the expected
artifacts exist and are non-empty, and that every headline number in the
narrative equals the corresponding field in `package_metadata.json`. Exits
non-zero on any mismatch. Run it before publishing anything.

A third check runs inside the pipeline itself: Module 2C writes
`geographic_integrity.{json,tsv}` to the run directory each run, recording how
many records took their geography from the source database, how many were
derived from coordinates, and how many could not be resolved — broken down by
species, so gaps surface in the log immediately rather than in a later figure.

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
| AOO | occupied cells on a **true equal-area 2 × 2 km lattice** (EPSG:6933), 4 km² each |
| Extinction mask | records within **500 m geodesic** of an extinction event and predating it are suppressed |
| Zero active records | terminal state: `status = Extinct`, AOO 0, EOO `NA`, no basins, mandatory disclaimer |
| Nearest-land snap | capped at **100 km**, and rejected if the continent appears in no other record of that species |
| Continent vocabulary | exactly six values; `Australia` is a country ⇒ `Oceania` |
| Spatial clustering | descriptive signal only — the term *fragmentation* is reserved for downstream connectivity work |
| Clustering | **basin-aware**: records sharing a HydroBASINS unit are connected regardless of distance; different units are separate unless within the threshold |
| Clustering basin level | the level already assigned per category — L10 endemic, L08 regional, L06 cosmopolitan |
| Clustering threshold | **provisional at 10 km**, for the between-basin rule only — a configuration choice, not a workflow property; recorded in every output |
| De-duplication | species + coordinates + year. **All** source citations of the collapsed group are retained in `citation_all` |

> **Clustering results before v1.3 are not usable.** Up to and including v1.2 the
> cut height was the *mean pairwise distance of the points themselves*. With
> complete linkage the root merge sits at the maximum pairwise distance, and the
> mean is always below it, so a single cluster was unreachable by construction:
> every species with enough coordinates scored more than one cluster (494 of 494
> in v1.2), and the count tracked sample size rather than spatial structure.
> The threshold is now absolute and configurable via
> `CONFIG$clustering$threshold_km`, but the default is a placeholder pending an
> ecologically justified value. Do not cite cluster counts until it is settled.

**Clustering output keys changed with that fix.** The `spatial_clustering`
block no longer carries `mean_threshold_km`. It was a single key holding two
different things, and it was only ever accurate because the cut height happened
to *be* the mean pairwise distance — the defect above. It is replaced by:

| Key | Meaning |
|---|---|
| `threshold_km` | the absolute cut height the clusters were computed under |
| `mean_pairwise_distance_km` | a descriptive statistic of the points; **not** the threshold |

**Basin map features.** Every feature in `*_basins.geojson` (and every KML
Placemark, in ExtendedData) carries:

| Property | Example | Meaning |
|---|---|---|
| `HB_LABEL` | `"2100513510"` | HydroBASINS id of the polygon |
| `basin_name` | `"Danube"` | root of the naming hierarchy |
| `basin_name_fine` | `"Tisza - Crișul Repede"` | the specific water; exactly the string the narrative prints |
| `MAIN_BAS` | `"2100008490"` | outlet basin of the river system the polygon drains to |
| `NEXT_DOWN` | `"2100514090"` | immediately downstream basin; `"0"` at an outlet |
| `status` | `"Native"` | `Native` or `Introduced` |

All ids are exact decimal strings. The source stores them as doubles, which R
would otherwise write as `"3.1e+09"` for a round id. The KML Placemark label is
`basin_name_fine`, because the root name labels every polygon of a
restricted-range endemic identically.

About one in five level-8 polygons has no name of its own. Walking `NEXT_DOWN`
to the first named downstream basin names more of them than jumping to the
outlet via `MAIN_BAS` (for *Procambarus clarkii*, 81 vs 55 of 113 anonymous
polygons), but beyond the first hop it needs the downstream basins' own
`NEXT_DOWN`, which is not on the species' features. Some systems stay
anonymous regardless: they are unnamed in the source.

The HydroBASINS cache is validated for these columns, not just for feature
count, so a cache written before they existed is rebuilt rather than silently
reused.

Consumers reading `mean_threshold_km` (the World of Crayfish species pages do)
need updating. An earlier rename in the same block — `fragmentation_clusters` →
`n_clusters`, and the `fragmentation` block → `spatial_clustering` — is already
in effect; readers accept both spellings for one release.

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

Two different things are licensed two different ways:

| What | Licence |
|---|---|
| **This software** — everything in `R/`, `tests/`, and the root scripts | **GPL-3.0-or-later** (see [`LICENSE`](LICENSE)) |
| **Data packages produced by running it** — the per-species metrics, narratives, maps and citations | **CC BY 4.0**, recorded in each package's `provenance.license` |

The split is deliberate: the pipeline is copyleft so improvements stay open,
while the biodiversity products it generates are permissively licensed so they
can be reused and cited freely.

Reference layers (Natural Earth, GADM, HydroBASINS, FEOW, TEOW, WDPA) and the
underlying occurrence records remain under their own licences and are not
redistributed here.
