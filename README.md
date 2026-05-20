---
editor_options:
  markdown:
    wrap: 72
output:
  word_document: default
  pdf_document: default
---

# cheCkOVER: Biodiversity Occurrence Framework

**Unlocking biodiversity occurrence data for artificial intelligence and
conservation science**

[![License: CC BY
4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![R
Version](https://img.shields.io/badge/R-%E2%89%A5%204.3.0-blue.svg)](https://www.r-project.org/)

------------------------------------------------------------------------

## 📋 Table of Contents

-   [Overview](#overview)
-   [Features](#features)
-   [Architecture & Versioning](#architecture--versioning)
-   [System Requirements](#system-requirements)
-   [Installation](#installation)
-   [Quick Start](#quick-start)
-   [Configuration](#configuration)
-   [Pipeline Phases](#pipeline-phases)
-   [Module Overview](#module-overview)
-   [Helper Scripts](#helper-scripts)
-   [Usage Examples](#usage-examples)
-   [Output Structure](#output-structure)
-   [Canonical Narrative Template](#canonical-narrative-template)
-   [Data Sources to Cite](#data-sources-to-cite)
-   [License](#license)

------------------------------------------------------------------------

## 🎯 Overview {#overview}

cheCkOVER is a modular R framework for processing, enriching, and analyzing
freshwater biodiversity occurrence data. It transforms raw species occurrence
records into AI-ready, publication-grade per-species data packages with:

-   **Spatial enrichment** — continents, countries, terrestrial and freshwater
    ecoregions, protected areas, hydrographic basins
-   **Taxonomic validation** — via WoRMS with retry/backoff for transient
    network failures
-   **Distributional metrics** — EOO, AOO, fragmentation analysis, IUCN B1
    range-based categorization
-   **Extinction-aware metric computation** — 500 m geodesic mask around
    documented extinctions, applied upstream of all metrics
-   **Canonical geo-narratives** — automated ecological summaries following
    the File_S1 template
-   **Citation management** — APA + DOI bibliographies, BibTeX, CITATION.cff
-   **Sparse incremental versioning** — only species whose data actually
    changed are reprocessed between releases; unchanged species inherit
    artifacts from the prior version via a manifest

**Designed for:** Conservation biologists, ecologists, biodiversity data
scientists, IUCN assessors

**Primary use case:** World of Crayfish database (120 k+ occurrence records,
670+ species, three population scenarios)

------------------------------------------------------------------------

## ✨ Features {#features}

### Core Capabilities

-   ✅ **Modular architecture** — Run individual modules or the full pipeline
-   ✅ **Sparse-versioned output** — Only changed/new species packaged in
    incremental runs; baseline runs package everything
-   ✅ **Fingerprint-based change detection** — Per-species SHA-256 over a
    fixed set of comparison columns; reader-invariant
-   ✅ **Extinction masking** — Records flagged `is_extinct=TRUE` become
    `temporal_status="extinct"`; records within 500 m become `"suppressed"`;
    only `"active"` records contribute to metrics
-   ✅ **Memory-safe** — Processes large datasets via batching
-   ✅ **Parallel processing** — Optional for faster execution
-   ✅ **Comprehensive logging** — Debug-friendly, timestamped logs
-   ✅ **Cached computations** — Reuses spatial layers and API results
-   ✅ **Scenario-aware processing** — Handles indigenous, non-indigenous,
    and mixed populations
-   ✅ **Framework-version guard** — Pipeline refuses to start if the
    target version already has published output (prevents accidental
    overwrites / mislabeled runs)

### Enrichment Modules

| Module | Description | Data Source |
|----|----|----|
| **1. Ingest & Clean** | Load, validate, and standardize occurrence data; resolve taxonomy via WoRMS | User .tsv file |
| **1B. Vernacular Names** | Add common names in multiple languages | TSV dictionary |
| **1C. Split** | Split records by population type (indigenous / non-indigenous) | Computed |
| **1D. Scenario Detection** | Classify each species by population scenario (1 / 2 / 3) | Computed |
| **1E. Change Detection** | Compute per-species fingerprints, compare against prior version manifests, populate `active_species` | Computed |
| **2A. Continents** | Tag records by continent | Natural Earth |
| **2B. GADM** | Add country and admin-1 boundaries | GADM v4.1 |
| **2C. TEOW** | Link to terrestrial ecoregions | WWF TEOW |
| **2D. FEOW** | Link to freshwater ecoregions | WWF FEOW |
| **2E. WDPA** | Identify protected area overlap | WDPA |
| **2F. HydroBASINS** | Assign hydrographic basins | HydroSHEDS |
| **3A/3C. Metrics & Reports (Indigenous)** | EOO, AOO, IUCN B1 category, distributional category, per-species JSON | Computed |
| **3B. Spatial Enrichment (Indigenous)** | Orchestrates 2C–2F on indigenous branch | Computed |
| **4A/4C. Metrics & Reports (Non-Indigenous)** | EOO, AOO, NI category, per-species JSON | Computed |
| **4B. Spatial Enrichment (Non-Indigenous)** | Orchestrates 2C–2F on non-indigenous branch | Computed |
| **5. Merge Scenario 3** | Merge reports for species with both population types | Compiled |
| **7. Citations** | Extract bibliographies in APA, BibTeX, CSV, CITATION.cff | User data |
| **8. Maps** | EOO/AOO/basin maps (GeoJSON, KML) with type locality markers | Computed |
| **9. Package Export** | Per-species package directories + version manifest | Compiled |
| **10. Canonical Narratives** | Generate File_S1 compliant geo-narratives (markdown + JSON + TXT) | Compiled |

------------------------------------------------------------------------

## 🏗️ Architecture & Versioning {#architecture--versioning}

cheCkOVER uses **sparse incremental versioning**: each new release of the
underlying database produces a new `framework_version`. Only species whose
data actually changed are reprocessed; unchanged species are referenced from
prior versions via a manifest. This makes incremental releases proportional
to the number of *changed* species rather than the cohort size.

### Versioning conventions

-   `framework_version` (e.g. `"1.0"`, `"1.1"`, `"2.0"`) labels each
    published release. Set in `config.R`. **Must be bumped manually**
    between database refreshes.
-   `run_id` (e.g. `"production"`, `"mock_v1"`) labels working directories
    under `runs/`. Cosmetic — does not affect the published output path.
-   Version folders on disk match the regex `^\d+\.\d+$`. Minor bumps
    (`1.0 → 1.1 → 1.2`) signal data refreshes; major bumps
    (`1.x → 2.0`) signal methodology changes.

### Outcome model

Phase 1.5 classifies each species into one of three outcomes:

| Outcome | Meaning | Where artifacts live |
|---|---|---|
| `new` | Species not present in any prior manifest | `<root>/<v>/<species>/` |
| `reprocessed` | Fingerprint differs from the most recent prior occurrence | `<root>/<v>/<species>/` |
| `unchanged` | Fingerprint identical to the prior source version | Inherited from `<root>/<source_version>/<species>/` via manifest |

The fingerprint covers 15 columns including coordinates, year, extinction
flags, DOI, citation, contributor — so legitimate metadata corrections
(e.g. DOI backfills) correctly trigger `reprocessed` even when spatial
data is unchanged.

### Manifest

Each version writes `<root>/<v>/checkover/manifest.json` describing the
full cohort. For unchanged species, the manifest's `source_version` field
points back to the version where artifacts actually live. Frontend consumers
walk the manifest chain to resolve the latest snapshot per species.

### Extinction masking

Records with `is_extinct=TRUE` (and any record within 500 m geodesic
of an extinction) are excluded from AOO, EOO, country counts, basin counts,
and protected-area counts. The masking is applied once after Phase 1.5 and
propagates through all downstream metrics modules. The `extinctions_count`
field counts distinct extinction localities and is computed separately from
the unmasked record set.

------------------------------------------------------------------------

## 💻 System Requirements {#system-requirements}

### Minimum (testing with mock data)

-   **OS:** Windows 10/11, macOS 10.15+, or Linux
-   **RAM:** 8 GB
-   **Storage:** 10 GB free space
-   **R Version:** ≥ 4.3.0

### Recommended (full 120 k+ record database)

-   **OS:** Linux (Ubuntu 22.04+ or CentOS 7+)
-   **RAM:** 64 GB+ (peak usage during enrichment ~25 GB RSS)
-   **Storage:** 50 GB free space
-   **CPU:** 8+ cores
-   **R Version:** ≥ 4.3.0

### External Dependencies

-   **Spatial data files** (not included, download separately):
    -   HydroBASINS Level 6 / 8 / 10 shapefiles
        ([download](https://www.hydrosheds.org/products/hydrobasins))
    -   FEOW shapefile
        ([download](https://www.feow.org/))
    -   WDPA shapefiles (downloaded automatically by Module 2E via
        `wdpar` for required countries)

-   **R packages:** `sf`, `dplyr`, `tidyr`, `readr`, `jsonlite`,
    `rnaturalearth`, `worrms`, `wdpar`, `geodata`, `digest`, `future`,
    `future.apply`, `progress`, `glue`

------------------------------------------------------------------------

## 📦 Installation {#installation}

### 1. Clone or Download Repository

``` bash
# Option A: Git clone
git clone https://github.com/yourusername/checkover.git
cd checkover

# Option B: Download ZIP
# Extract to your working directory
```

### 2. Install R Packages

The framework will auto-install missing packages, but you can pre-install:

``` r
required_packages <- c(
  "sf", "dplyr", "tidyr", "readr", "jsonlite", "rnaturalearth",
  "worrms", "wdpar", "geodata", "digest",
  "future", "future.apply", "progress", "glue"
)
install.packages(required_packages)
```

### 3. Download Spatial Data

``` bash
mkdir -p spatial_data/hydrobasins spatial_data/feow

# HydroBASINS — global, levels 6 / 8 / 10
cd spatial_data/hydrobasins
wget https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_lev10_v1c.zip
unzip hybas_lev10_v1c.zip
# Repeat for levels 6 and 8

# FEOW — follow instructions at feow.org
# Place feow_hydrosheds.shp (+ .dbf, .shx, .prj) under spatial_data/feow/
```

### 4. Prepare Input Data

Place your WoC export (or other source TSV) at the project root and point
`CONFIG$input_file` to it.

------------------------------------------------------------------------

## 🚀 Quick Start {#quick-start}

``` r
# 1. Open R/RStudio and set working directory
setwd("path/to/checkover")

# 2. Edit config.R (set framework_version + input_file)

# 3. Run the pipeline
source("checkcover_main.R")
```

The script will:

-   Load configuration from `config.R`
-   Refuse to start if the target `framework_version` already has output
    on disk (delete the dir first or bump the version to override)
-   Initialize logging
-   Run all pipeline phases
-   Save outputs to `checkover_output/<framework_version>/` (sparse) plus
    `checkover_output/runs/<run_id>/` (working files)

------------------------------------------------------------------------

## ⚙️ Configuration {#configuration}

Edit `config.R` to customize the analysis.

### Basic Settings

``` r
CONFIG <- list(
  # Input / Output
  input_file = "WoC_full_export.tsv",
  root_output_dir = "checkover_output",
  version = "production",        # run_id (cosmetic — chooses runs/<X>/ name)
  framework_version = "1.0",     # output version — BUMP for each new database

  # Taxonomy
  taxonomy = list(
    resolve = TRUE                # Use WoRMS API
  ),

  # Vernacular Names
  vernaculars = list(
    source = "file",              # "itis" or "file"
    path = "vernacular_names_wide.tsv"
  )
)
```

> ⚠️ **Forget to bump `framework_version`?** The pipeline checks for
> existing output at the target version path and aborts in seconds with a
> clear error rather than producing a mislabeled multi-hour run.

### Memory Management

``` r
CONFIG$memory <- list(
  max_worker_memory = 1500,      # MB per worker
  aggressive_gc = TRUE,
  batch_size = 50,
  use_disk_cache = TRUE
)
```

### Parallelization

``` r
CONFIG$parallel <- list(
  workers = "auto",
  force_sequential = TRUE        # FALSE to enable parallel
)
```

### Spatial Data Paths

``` r
CONFIG$spatial <- list(
  hydro_dir = "spatial_data/hydrobasins",
  hydro_files = list(
    "6"  = "hybas_lev06_v1c.shp",
    "8"  = "hybas_lev08_v1c.shp",
    "10" = "hybas_lev10_v1c.shp"
  ),
  feow_source = "local",
  feow_path = "spatial_data/feow/feow_hydrosheds.shp"
)
```

------------------------------------------------------------------------

## 🔁 Pipeline Phases {#pipeline-phases}

```
Phase 1     Ingest & clean (Module 1)
            Vernacular names (Module 1B)
            Pre-split spatial enrichment: continents + GADM (2A, 2B)
            Split by population type (Module 1C)
            Scenario detection (Module 1D)

Phase 1.5   Change detection (Module 1E)
            ├─ Compute per-species fingerprints
            ├─ Compare against prior version manifests
            └─ Populate ctx$active_species (= new + reprocessed)

            ── Extinction masking applied here (500 m geodesic) ──

Phase 2     Indigenous branch: metrics → fragmentation → spatial enrichment
                              (2C–2F) → reports (3A, 3C)
Phase 3     Non-indigenous branch: metrics → spatial enrichment (2C–2F)
                                  → reports (4A, 4C)
Phase 4     Merge Scenario 3 species (Module 5)
Phase 5A    Canonical narrative generation (Module 10)
Phase 5B    Map generation (Module 8)
Phase 6     Citation management (Module 7)
Phase 7     Sparse package export + manifest writing (Module 9)
```

Phases 2–8 operate on `ctx$active_species` only. Unchanged species exit
the pipeline at Phase 1.5 and are referenced from prior versions via the
manifest written in Phase 7.

------------------------------------------------------------------------

## 📚 Module Overview {#module-overview}

### `R/00_run_context.R` — RunContext + Fingerprinting

The orchestration backbone for sparse versioning.

-   `RunContext_init(config, run_id)` — Build the context object passed
    through Module 9
-   `compute_species_fingerprint(sp_data)` — Reader-invariant SHA-256 hash
    of a per-species data slice
-   `resolve_species_path(ctx, sp, ...)` — Cross-version path lookup via
    manifest
-   `list_prior_versions(root)` — Numeric-descending discovery of prior
    version folders

### Module 1: Data Ingestion (`R/01_ingest.R`)

Loads the source TSV, validates records (year range, coordinates), resolves
taxonomy via WoRMS with 3-attempt retry + 2 s/4 s backoff on network errors.
Sanitizes API error strings (newlines/tabs stripped) so transient outages
never corrupt the output TSV.

**Outputs:** `clean_occurrences.tsv`, `taxonomy_mapping_full.tsv`,
`type_localities.tsv`

### Module 1E: Change Detection (`R/01e_change_detection.R`)

Reads `clean_occurrences.tsv` from `<v>/checkover/`, fingerprints each
species, walks prior manifests in numeric-descending order to find the
most recent occurrence, and classifies each species as `new`,
`reprocessed`, or `unchanged`. Populates `ctx$active_species` and
`ctx$species_outcomes`. Writes per-species fingerprint files to
`<v>/checkover/fingerprints/<species>.json` for audit.

### Module 2F: HydroBASINS (Memory-Optimized)

Branch-aware caching: indigenous and non-indigenous data are cached
separately so re-runs reuse prior assignments cleanly.

### Module 8: Map Generation (Scenario-Aware)

-   Scenario 1: Indigenous populations only (orange basins)
-   Scenario 2: Non-indigenous populations only (purple basins)
-   Scenario 3: Both population types (mixed coloring per basin)
-   Type locality markers (red 2 km × 2 km squares)
-   EOO convex hulls + AOO grid cells (yellow)
-   Bbox crop skipped when global (> 90° wide) to avoid antimeridian
    geometry corruption

### Module 9: Package Export & Manifest (`R/09_package_export.R`)

Writes per-species packages to `<root>/<v>/<species>/` with the layout
described in [Output Structure](#output-structure). Only species in
`ctx$active_species` are physically packaged at this version; unchanged
species are referenced from prior versions in the manifest.

Writes `<root>/<v>/checkover/manifest.json` with the full cohort and per-
species outcomes (`new` / `reprocessed` / `unchanged`), fingerprints, and
`source_version` pointers.

### Module 10: Canonical Narrative Generation

Renders the File_S1 5-section template per species. Section 4.6
("Processing framework provenance") includes processing date and data
snapshot date only.

------------------------------------------------------------------------

## 🛠️ Helper Scripts {#helper-scripts}

Three standalone scripts run from the project root.

### `production_stats.R`

Extracts paper-quality aggregate statistics for a completed version:
cohort + outcomes from the manifest, validated record counts, scenario
mix, IUCN B1 categorization (derived from EOO if not directly stored),
NI breakdown, HydroBASINS coverage, runtime from log timestamps.

``` bash
Rscript production_stats.R checkover_output 1.0 \
  checkover_output/logs/checkover_<timestamp>.log > stats_v1.0.md
```

Hardware specs (CPU / RAM / OS / R version) are filled in manually at the
bottom of the output.

### `build_s7_posthoc.R`

Reconstructs `supplement_S7.tsv` (per-species inter-version delta log)
from two completed version directories. Reads manifests + per-species
`package_metadata.json` + `clean_occurrences.tsv` for both versions and
emits one row per species in the cohort with columns:

```
species_name | outcome | source_version | delta_EOO_km2 | delta_AOO_km2 |
delta_country_count | delta_basin_count | fragmentation_transition |
loss_hotspot_flagged | n_records_added | n_records_retired
```

``` bash
Rscript build_s7_posthoc.R checkover_output 1.1 1.0
```

### `refingerprint_v10.R`

One-shot utility for recomputing a baseline version's fingerprints under
updated canonicalization. Reads the version's `clean_occurrences.tsv` with
the current reader, recomputes each species' fingerprint, and rewrites
the version's `fingerprints/<species>.json` files and the inline
fingerprints in `manifest.json` (with a `.bak` backup).

Only needed if the canonicalization or reader changes after a baseline has
been published.

``` bash
Rscript refingerprint_v10.R checkover_output 1.0
```

------------------------------------------------------------------------

## 💡 Usage Examples {#usage-examples}

### Example 1: Quick Test with Mock Data

``` r
# 1. Edit config.R
CONFIG$version <- "test_run_v1"
CONFIG$framework_version <- "1.0"
CONFIG$input_file <- "mock_database.tsv"
CONFIG$memory$batch_size <- 10

# 2. Run
source("checkcover_main.R")

# 3. Check results
list.files("checkover_output/1.0/")
```

### Example 2: Production Baseline (v1.0)

``` bash
ssh user@yourserver.com
cd /path/to/checkover

# Edit config: framework_version = "1.0", input_file = "WoC_full.tsv"
nano config.R

# Run in background
nohup Rscript checkcover_main.R > analysis_v1_0.log 2>&1 &

# Monitor
tail -f checkover_output/logs/checkover_*.log
```

### Example 3: Incremental Release (v1.1)

``` bash
# After a database refresh, bump framework_version
sed -i 's/framework_version = "1.0"/framework_version = "1.1"/' config.R

# Point to the new input
sed -i 's|input_file = "WoC_full.tsv"|input_file = "WoC_full_v1_1.tsv"|' config.R

# Run — Phase 1.5 will detect which species actually changed
nohup Rscript checkcover_main.R > analysis_v1_1.log 2>&1 &

# Watch the cutoff:
grep "Active species cutoff" checkover_output/logs/checkover_*.log
# Expected output:
# Active species cutoff applied: 208 of 676 species will be processed
# downstream. Skipped: 468 (unchanged since prior snapshot).
```

### Example 4: Generate Paper Stats & S7 after delivery

``` bash
Rscript production_stats.R checkover_output 1.0 \
  checkover_output/logs/checkover_<v1.0_log>.log > stats_v1.0.md

Rscript production_stats.R checkover_output 1.1 \
  checkover_output/logs/checkover_<v1.1_log>.log > stats_v1.1.md

Rscript build_s7_posthoc.R checkover_output 1.1 1.0
# Output: checkover_output/1.1/checkover/supplement_S7.tsv
```

------------------------------------------------------------------------

## 📂 Output Structure {#output-structure}

After a baseline (`v1.0`) and incremental (`v1.1`) run:

```
checkover_output/
├── 1.0/                                       # ← v1.0 published output
│   ├── Astacus_astacus/
│   │   ├── maps/
│   │   │   ├── Astacus_astacus_AOO.geojson
│   │   │   ├── Astacus_astacus_AOO.kml
│   │   │   ├── Astacus_astacus_basins.geojson
│   │   │   ├── Astacus_astacus_basins.kml
│   │   │   ├── Astacus_astacus_EOO.geojson
│   │   │   └── Astacus_astacus_EOO.kml
│   │   ├── narratives/
│   │   │   ├── Astacus_astacus_canonical.md
│   │   │   ├── Astacus_astacus_narrative.json
│   │   │   └── Astacus_astacus_narrative.txt
│   │   ├── citations/
│   │   │   ├── Astacus_astacus_bibliography.bib
│   │   │   ├── Astacus_astacus_bibliography.csv
│   │   │   ├── Astacus_astacus_bibliography.json
│   │   │   └── Astacus_astacus_CITATION.cff
│   │   ├── file_manifest.csv
│   │   ├── README.md
│   │   └── package_metadata.json
│   ├── Procambarus_clarkii/
│   │   └── (same structure)
│   ├── ... (one folder per species in the v1.0 cohort)
│   └── checkover/                             # ← internal scaffolding
│       ├── manifest.json                      #   cohort + outcomes
│       ├── fingerprints/
│       │   └── <species>.json                 #   audit deposit
│       ├── clean_occurrences.tsv              #   the input snapshot
│       ├── INDEX.md                           #   human-readable index
│       └── packaging_summary.json
│
├── 1.1/                                       # ← v1.1 sparse output
│   ├── Astacus_astacus/                       #   only changed/new species
│   ├── Faxonius_limosus/                      #   appear here
│   ├── ... (only ~208 of 676 species)
│   └── checkover/
│       ├── manifest.json                      #   468 species reference v1.0
│       ├── fingerprints/                      #   computed for all 676
│       ├── clean_occurrences.tsv
│       ├── supplement_S7.tsv                  #   if build_s7_posthoc.R ran
│       ├── INDEX.md
│       └── packaging_summary.json
│
├── runs/                                      # ← working directories
│   └── <run_id>/
│       ├── cache/                             #   spatial layer caches
│       ├── clean_occurrences.tsv
│       ├── clean_occurrences_with_*.tsv       #   intermediate enrichment
│       ├── indigenous_metrics.tsv
│       ├── non_indigenous_metrics.tsv
│       ├── species_scenarios_summary.json
│       ├── scenario_table.rds
│       ├── reports/
│       │   ├── indigenous/<species>.json
│       │   ├── non_indigenous/<species>.json
│       │   └── merged_scenario3/<species>_merged.json
│       ├── narratives/
│       ├── narratives_canonical/
│       ├── citations/
│       └── maps/
│
└── logs/
    └── checkover_<timestamp>.log
```

### Notes

-   The per-species folder shape inside `<v>/<species>/` is byte-stable
    across versions — frontends can hard-code the layout.
-   `<v>/checkover/` is internal scaffolding. The version regex
    (`^\d+\.\d+$`) ensures consumers ignore it.
-   `runs/<run_id>/` holds working files. Safe to archive or delete
    after a successful run completes.

------------------------------------------------------------------------

## 📄 Canonical Narrative Template {#canonical-narrative-template}

Module 10 produces a 5-section canonical narrative per species.

### Section 1: Taxonomic Identity
- Scientific name (italicized)
- **Higher taxonomy:** Order > Infraorder > Superfamily > Family
- Common names
- Type locality (if available)

### Section 2: Indigenous Range Overview
- **Distribution category:** endemic / regional / cosmopolitan
- **EOO / AOO** with km² units
- **Countries** (sorted by record count)
- **Subnational administrative units** (GADM Level 1)
- **Hydrographic basins** with names
- **Protected area coverage** (count + percentage)
- **Biogeographic context:** TEOW and FEOW ecoregions
- **Temporal context:** year range, post-2000 percentage
- **Range dynamics** (if extinctions present): historical vs. current EOO/AOO
- **Fragmentation assessment** (endemic / regional species only)

### Section 3: Non-Indigenous Range Overview
- Status and category (local / widespread)
- EOO / AOO metrics (informative; not for IUCN)
- Countries, basins, temporal coverage
- First record year

### Section 4: Data Quality, Traceability, and Provenance
- **Data summary:** total records, indigenous / non-indigenous breakdown
- **Data quality score:** 0–100 composite (spatial accuracy 40 %, temporal
  precision 25 %, source reliability 25 %, recency 10 %)
- **High-accuracy records** percentage
- **Bibliographic coverage:** total references, DOI-linked percentage
- **Raw data provenance:** World of Crayfish® citation (Ion et al. 2024)
- **Processing framework provenance:** processing date, data snapshot date
- **Interpretation note:** disclaimer about non-IUCN status

### Section 5: Formal Narrative Summary (300–500 words)

Human-readable prose in 5 paragraphs:
1. Taxonomic identity with higher taxonomy and distribution category
2. Indigenous range with countries, basins, ecoregions, temporal coverage
3. Conservation context (protection, fragmentation, extinctions)
4. Non-indigenous populations (if applicable)
5. Data provenance with quality metrics and IUCN disclaimer

------------------------------------------------------------------------

## 📖 Data Sources to Cite {#data-sources-to-cite}

-   **Natural Earth:** Made with Natural Earth.
    <https://www.naturalearthdata.com>
-   **GADM:** GADM database v4.1. <https://gadm.org>
-   **TEOW:** Olson, D.M., et al. (2001). Terrestrial Ecoregions of the
    World. *BioScience* 51(11):933-938.
-   **FEOW:** Abell, R., et al. (2008). Freshwater Ecoregions of the
    World. *BioScience* 58(5):403-414.
-   **WDPA:** UNEP-WCMC and IUCN (2025), Protected Planet.
    <https://www.protectedplanet.net>
-   **HydroBASINS:** Lehner, B. & Grill, G. (2013). Global river
    hydrography and network routing. *Hydrological Processes*
    27(15):2171-2186.
-   **WoRMS:** WoRMS Editorial Board (2025). World Register of Marine
    Species. <https://www.marinespecies.org>
-   **World of Crayfish:** Ion, M.C., et al. (2024). World of Crayfish™
    database.

------------------------------------------------------------------------

## 📄 License {#license}

This project is licensed under **Creative Commons Attribution 4.0
International (CC BY 4.0)**.

You are free to:
- **Share** — copy and redistribute the material
- **Adapt** — remix, transform, and build upon the material

Under the following terms:
- **Attribution** — You must give appropriate credit

See [LICENSE](LICENSE) file for details.

------------------------------------------------------------------------

## 🔄 Version History

### v1.1 — Sparse incremental processing (May 2026)

-   Sparse-versioned output layout (`<root>/<v>/<species>/`)
-   Per-species change detection via reader-invariant fingerprints
-   Per-version manifest with cohort outcomes + `source_version` chaining
-   Extinction masking (500 m geodesic suppression) applied upstream of
    metrics
-   Framework-version guard prevents accidental overwrites
-   WoRMS ingest hardened with retry/backoff + error sanitization
-   FEOW bbox crop skipped at global scale to prevent antimeridian
    geometry corruption
-   Helper scripts: `production_stats.R`, `build_s7_posthoc.R`,
    `refingerprint_v10.R`

### Roadmap

-   [ ] Web interface for interactive exploration
-   [ ] Integration with GBIF API
-   [ ] Automated IUCN Red List assessment templates
-   [ ] Docker containerization

------------------------------------------------------------------------

**Last Updated:** 2026-05-20\
**Documentation Version:** 1.1.0
