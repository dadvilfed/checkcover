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
Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)

------------------------------------------------------------------------

## 📋 Table of Contents

-   [Overview](#overview)
-   [Features](#features)
-   [System Requirements](#system-requirements)
-   [Installation](#installation)
-   [Quick Start](#quick-start)
-   [Configuration](#configuration)
-   [Module Overview](#module-overview)
-   [Usage Examples](#usage-examples)
-   [Output Structure](#output-structure)
-   [Canonical Narrative Template](#canonical-narrative-template)
-   [License](#license)

------------------------------------------------------------------------

## 🎯 Overview {#overview}

cheCkOVER is a comprehensive R framework for processing, enriching, and
analyzing biodiversity occurrence data. It transforms raw species
occurrence records into AI-ready datasets with:

-   **Spatial enrichment** (continents, countries, ecoregions, protected
    areas, hydrographic basins)
-   **Taxonomic validation** (via WoRMS)
-   **Distribution metrics** (EOO, AOO, fragmentation analysis)
-   **Conservation narratives** (automated ecological summaries following File_S1 template)
-   **Citation management** (BibTeX, JSON, CITATION.cff)
-   **Exportable species packages** (privacy-filtered,
    publication-ready)

**Designed for:** Conservation biologists, ecologists, biodiversity data
scientists, IUCN assessors

**Primary use case:** World of Crayfish database (100k+ occurrence
records, 700+ species)

------------------------------------------------------------------------

## ✨ Features {#features}

### Core Capabilities

-   ✅ **Modular architecture** - Run individual modules or the full
    pipeline
-   ✅ **Resume capability** - Interruptions won't lose progress
-   ✅ **Memory-safe** - Processes large datasets via batching
-   ✅ **Parallel processing** - Optional for faster execution
-   ✅ **Comprehensive logging** - Debug-friendly, timestamped logs
-   ✅ **Progress tracking** - Visual progress bars for long operations
-   ✅ **Cached computations** - Reuses spatial layers and API results
-   ✅ **Scenario-aware processing** - Handles indigenous, non-indigenous, and mixed populations

### Enrichment Modules

| Module | Description | Data Source |
|----|----|----|
| **1. Ingest & Clean** | Load, validate, and standardize occurrence data | User .tsv file |
| **1B. Vernacular Names** | Add common names in multiple languages | ITIS API or manual dictionary |
| **1C. Distribution Metrics** | Calculate EOO/AOO (historical vs current) | Computed |
| **1D. Fragmentation** | Detect spatial fragmentation patterns | Computed |
| **1D. Scenario Detection** | Classify species by population type (indigenous/non-indigenous/both) | Computed |
| **2A. Continents** | Tag records by continent | Natural Earth |
| **2B. GADM** | Add country and admin-1 boundaries | GADM v4.1 |
| **2C. TEOW** | Link to terrestrial ecoregions | WWF TEOW |
| **2D. FEOW** | Link to freshwater ecoregions | WWF FEOW |
| **2E. WDPA** | Identify protected area overlap | WDPA |
| **2F. HydroBASINS** | Assign hydrographic basins | HydroSHEDS |
| **3. Reports (Indigenous)** | Build species-level JSON reports for native populations | Compiled |
| **4. Reports (Non-Indigenous)** | Build species-level JSON reports for introduced populations | Compiled |
| **5. Merge Scenario 3** | Merge reports for species with both population types | Compiled |
| **6. Narratives** | Generate summaries | Compiled |
| **7. Citations** | Extract and format bibliographies | User data |
| **8. Maps** | Generate EOO/AOO/basin maps (GeoJSON, KML) with type locality markers | Computed |
| **9. Package Export** | Package data for publication | Compiled |
| **10. Canonical Narratives** | Generate File_S1 compliant geo-narratives | Compiled |

------------------------------------------------------------------------

## 💻 System Requirements {#system-requirements}

### Minimum (Testing with mock data)

-   **OS:** Windows 10/11, macOS 10.15+, or Linux
-   **RAM:** 8 GB
-   **Storage:** 10 GB free space
-   **R Version:** ≥ 4.2.0

### Recommended (Full 100k database)

-   **OS:** Linux (Ubuntu 20.04+ or CentOS 7+)
-   **RAM:** 128 GB+
-   **Storage:** 50 GB free space
-   **CPU:** 8+ cores
-   **R Version:** ≥ 4.2.0

### External Dependencies

-   **Spatial data files** (not included, must download separately):
    -   HydroBASINS Level 6/8/10 shapefiles
        ([download](https://www.hydrosheds.org/products/hydrobasins))
    -   FEOW shapefile ([download](https://www.feow.org/))
    -   Optional: WDPA shapefiles
        ([download](https://www.protectedplanet.net/))

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

The framework will auto-install missing packages, but you can
pre-install:

``` r
# Open R or RStudio
required_packages <- c(
  "sf", "dplyr", "tidyr", "jsonlite", "rnaturalearth", 
  "worrms", "wdpar", "geodata", "ecoregions", "digest",
  "future", "future.apply", "progress", "glue"
)

install.packages(required_packages)
```

### 3. Download Spatial Data

``` bash
# Create directory structure
mkdir -p spatial_data/hydrobasins
mkdir -p spatial_data/feow

# Download HydroBASINS (example for global level 10)
cd spatial_data/hydrobasins
wget https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_lev10_v1c.zip
unzip hybas_lev10_v1c.zip

# Download FEOW (follow instructions at feow.org)
cd ../feow
# Place feow_hydrosheds.shp and related files here
```

### 4. Prepare Input Data

``` bash
# Place your Excel database in project root
cp /path/to/your/database.tsv ./RollData.tsv
```

------------------------------------------------------------------------

## 🚀 Quick Start {#quick-start}

### Basic Usage (All Modules)

``` r
# 1. Open R/RStudio and set working directory
setwd("path/to/checkover")

# 2. Run the pipeline
source("checkcover_main.R")

# The script will:
# - Load configuration from config.R
# - Initialize logging
# - Run all modules in sequence
# - Save outputs to checkover_output/
```

### Output Location

All results are saved to:

```         
checkover_output/
├── runs/
│   └── run_<version>/
│       ├── clean_occurrences_with_metrics.csv
│       ├── maps/
│       ├── reports/
│       ├── narratives/
│       ├── narratives_canonical/
│       ├── narratives_formal/
│       └── species_packages/
└── logs/
    └── checkover_<timestamp>.log
```

------------------------------------------------------------------------

## ⚙️ Configuration {#configuration}

Edit `config.R` to customize the analysis:

### Basic Settings

``` r
CONFIG <- list(
  # Input/Output
  input_file = "RollData.tsv",
  root_output_dir = "checkover_output",
  version = "my_analysis_v1",  # Change to force new run
  
  # Taxonomy
  taxonomy = list(
    resolve = TRUE  # Use WoRMS API (slow but accurate)
  ),
  
  # Vernacular Names
  vernaculars = list(
    source = "file",  # "itis" or "file"
    path = "vernacular_names_wide.docx"
  )
)
```

### Memory Management (CRITICAL for large datasets)

``` r
CONFIG$memory <- list(
  max_worker_memory = 1500,  # MB per worker
  aggressive_gc = TRUE,       # Force garbage collection
  batch_size = 50,           # Species per batch
  use_disk_cache = TRUE      # Cache spatial layers
)
```

### Parallelization

``` r
CONFIG$parallel <- list(
  workers = "auto",           # or set to specific number: 4
  force_sequential = TRUE    # Set FALSE to enable parallel
)
```

### Spatial Data Paths

``` r
CONFIG$spatial <- list(
  hydro_dir = "spatial_data/hydrobasins",
  hydro_files = list(
    "6" = "hybas_lev10.shp",   # Replace with your filenames
    "8" = "hybas_lev10.shp",
    "10" = "hybas_lev10.shp"
  ),
  feow_source = "local",       # "auto", "feowR", or "local"
  feow_path = "spatial_data/feow/feow_hydrosheds.shp"
)
```

------------------------------------------------------------------------

## 📚 Module Overview {#module-overview}

### Module 1: Data Ingestion

**File:** `R/01_ingest.R`\
**Function:** `ingest_clean()`

``` r
# Run standalone
source("R/00_logging.R")
source("R/00_helpers.R")
source("R/01_ingest.R")

init_logger()
result <- ingest_clean(
  file_path = "RollData.tsv",
  output_dir = "test_output",
  resolve_taxonomy = TRUE
)
```

**Outputs:** - `clean_occurrences.tsv` - Standardized occurrence data -
`taxonomy_mapping_full.tsv` - WoRMS taxonomic hierarchy -
`type_localities.tsv` - Type locality records

### Module 2F: HydroBASINS (Memory-Optimized)

**File:** `R/02f_hydrobasins.R`\
**Function:** `enrich_with_hydrobasins_merged()`

``` r
# Example: Process in smaller batches for low-memory systems
result <- enrich_with_hydrobasins_merged(
  result,
  hydro_dir = "spatial_data/hydrobasins",
  global_files = CONFIG$spatial$hydro_files,
  cache_dir = "checkover_output/cache",
  output_dir = "checkover_output",
  batch_size = 20  # Reduce if running out of memory
)
```

### Module 8: Map Generation (Scenario-Aware)

**File:** `R/08_maps.R`\
**Function:** `generate_all_maps_seq()`

``` r
# Generate maps for all scenarios
maps <- generate_all_maps_seq(
  scenario_table = scenario_table,
  result_indigenous = result_indigenous,
  result_non_indigenous = result_non_indigenous,
  output_dir = "checkover_output",
  cache_dir = "checkover_output/cache",
  formats = c("geojson", "kml")
)
```

**Features:**
- Scenario 1: Indigenous populations only (orange basins)
- Scenario 2: Non-indigenous populations only (purple basins)
- Scenario 3: Both population types (mixed coloring per basin)
- Type locality markers (red 2km×2km squares)
- EOO convex hulls (yellow)
- AOO grid cells (yellow)

### Module 10: Canonical Narrative Generation

**File:** `R/10_canonical_narratives.R`\
**Function:** `generate_canonical_narratives()`

``` r
# Generate narratives
canonical_narratives <- generate_canonical_narratives(
  scenario_table = scenario_table,
  result_indigenous = result_indigenous,
  result_non_indigenous = result_non_indigenous,
  indigenous_reports = indigenous_reports,
  non_indigenous_reports = non_indigenous_reports,
  scenario3_merged = scenario3_merged,
  vernacular_lookup = VERNACULAR_LOOKUP,
  output_dir = run_env$run_dir,
  feow_lookup_path = CONFIG$dictionaries$feow,
  hydrobasin_names = HYDROBASIN_NAMES
)
```

**Outputs per species:**
- `{species}_canonical.md` - Full canonical narrative (all 5 sections)
- `{species}_narrative.txt` - Formal narrative text (Section 5 only)
- `{species}_narrative.json` - Structured formal narrative with metadata

------------------------------------------------------------------------

## 💡 Usage Examples {#usage-examples}

### Example 1: Quick Test with Mock Data

``` r
# 1. Edit config.R
CONFIG$version <- "test_run_v1"
CONFIG$input_file <- "RollData.tsv"  # 1k records
CONFIG$memory$batch_size <- 10

# 2. Run
source("checkcover_main.R")

# 3. Check results
list.files("checkover_output/runs/run_test_run_v1/reports/")
```

### Example 2: Production Run on Server

``` bash
# 1. SSH into server
ssh user@yourserver.com

# 2. Navigate to project
cd /path/to/checkover

# 3. Edit config for production
nano config.R
# Set: version = "production_full_v1"
#      input_file = "database.tsv"
#      batch_size = 100

# 4. Run in background
nohup Rscript checkcover_main.R > analysis.log 2>&1 &

# 5. Monitor progress
tail -f analysis.log

# 6. Check memory usage
htop
```

### Example 3: Resume After Interruption

``` r
# If the script was interrupted, simply re-run:
source("checkcover_main.R")

# The framework will:
# - Detect existing run folder
# - Enter RESUME mode
# - Skip completed modules
# - Continue from where it stopped
```

### Example 4: Run Individual Modules

``` r
# Load dependencies
source("R/00_logging.R")
source("R/00_helpers.R")
init_logger()

# Run specific module
source("R/01c_metrics.R")
result <- compute_distribution_context(result, output_dir = "test_output")

source("R/01d_fragmentation.R")
result <- analyze_fragmentation(result, output_dir = "test_output")
```

## 📂 Output Structure {#output-structure}

```         
checkover_output/
├── _registry.json                         # Run history
├── cache/                                 # Cached spatial layers (reusable)
│   ├── hydro_lev10_merged.rds
│   ├── ne_continents_50m.rds
│   ├── teow_worldecoregions_min.rds
│   └── wdpa_USA.rds
├── logs/
│   └── checkover_20250101_120000.log     # Timestamped log
└── runs/
    └── run_<version>/
        ├── _SUCCESS                       # Completion marker
        ├── clean_occurrences_with_metrics.csv
        ├── fragmentation_analysis.tsv
        ├── taxonomy_mapping_full.tsv
        ├── vernacular_names_lookup.tsv
        ├── maps/
        │   ├── EOO/
        │   │   ├── Species_name_EOO.geojson
        │   │   └── Species_name_EOO.kml
        │   ├── AOO/
        │   │   ├── Species_name_AOO.geojson
        │   │   └── Species_name_AOO.kml
        │   └── basins/
        │       ├── Species_name_basins.geojson  # Includes type locality
        │       └── Species_name_basins.kml
        ├── reports/
        │   ├── indigenous/
        │   │   └── Species_name.json
        │   ├── non_indigenous/
        │   │   └── Species_name.json
        │   ├── merged_scenario3/
        │   │   └── Species_name_merged.json
        │   ├── dataset_summary_statistics.json
        │   └── species_detailed_reports.tsv
        ├── narratives/
        │   ├── Species_name_narrative.txt
        │   └── Species_name_eco_narrative.json
        ├── narratives_canonical/              
        │   └── Species_name_canonical.md
        ├── narratives_formal/                 # Section 5 extracts
        │   ├── Species_name_narrative.txt
        │   └── Species_name_narrative.json
        ├── citations/
        │   ├── Species_name_bibliography.json
        │   ├── Species_name_bibliography.bib
        │   └── Species_name_CITATION.cff
        └── species_packages/              # Publication-ready packages
            ├── packaging_summary.json
            └── Species_name/
                ├── package_metadata.json
                ├── file_manifest.tsv
                ├── data/
                │   └── Species_name_occurrences.tsv  # Privacy-filtered
                ├── maps/
                ├── narratives/
                └── citations/
```

------------------------------------------------------------------------

## 📄 Canonical Narrative Template {#canonical-narrative-template}

The canonical narratives (Module 10) with five sections:

### Section 1: Taxonomic Identity
- Scientific name (italicized)
- **Higher taxonomy:** Order > Infraorder > Superfamily > Family
- Common names 
- Type locality (if available): geographic unit, protected area, bibliographic reference

### Section 2: Indigenous Range Overview
- **Distribution category:** endemic / regional / cosmopolitan
- **EOO/AOO metrics** with km² units
- **Countries** (sorted by record count)
- **Subnational administrative units** (GADM Level 1)
- **Hydrographic basins** with names 
- **Protected area coverage** (count and percentage)
- **Biogeographic context:** TEOW and FEOW ecoregions
- **Temporal context:** year range, post-2000 percentage
- **Auto-generated contextual statements** (endemic restrictions, protection gaps, data recency)
- **Range dynamics** (if extinction records present): historical vs current EOO/AOO
- **Fragmentation assessment** (endemic/regional species only)

### Section 3: Non-Indigenous Range Overview
- Status and category (local / widespread)
- EOO/AOO metrics (informative, not for IUCN)
- Countries, basins, temporal coverage
- First record year

### Section 4: Data Quality, Traceability, and Provenance
- **Data summary:** total records, indigenous/non-indigenous breakdown
- **Data quality score:** 0–100 composite indicator (spatial accuracy 40%, temporal precision 25%, source reliability 25%, recency 10%)
- **High-accuracy records** percentage
- **Bibliographic coverage:** total references, DOI-linked references (not records)
- **Raw data provenance:** 
- **Processing framework provenance:** cheCkOVER version, data snapshot date, processing date
- **Interpretation note:** disclaimer about non-IUCN status

### Section 5: Formal Narrative Summary (300–500 words)
Human-readable prose in 5 paragraphs:
1. Taxonomic identity with higher taxonomy and distribution category
2. Indigenous range with countries, basins (named), ecoregions, temporal coverage
3. Conservation context with protection percentage, fragmentation, extinction records
4. Non-indigenous populations (if applicable)
5. Data provenance with quality metrics and disclaimer


### Data Sources to Cite

-   **Natural Earth:** Made with Natural Earth.
    <https://www.naturalearthdata.com>
-   **GADM:** GADM database v4.1. <https://gadm.org>
-   **TEOW:** Olson, D.M., et al. (2001). Terrestrial Ecoregions of the
    World. BioScience 51(11):933-938.
-   **FEOW:** Abell, R., et al. (2008). Freshwater Ecoregions of the
    World. BioScience 58(5):403-414.
-   **WDPA:** UNEP-WCMC and IUCN (2025), Protected Planet.
    <https://www.protectedplanet.net>
-   **HydroBASINS:** Lehner, B. & Grill, G. (2013). Global river
    hydrography and network routing. Hydrological Processes
    27(15):2171-2186.
-   **WoRMS:** WoRMS Editorial Board (2025). World Register of Marine
    Species. <https://www.marinespecies.org>

------------------------------------------------------------------------

## 📄 License {#license}

This project is licensed under **Creative Commons Attribution 4.0
International (CC BY 4.0)**.

You are free to: - **Share** — copy and redistribute the material -
**Adapt** — remix, transform, and build upon the material

Under the following terms: - **Attribution** — You must give appropriate
credit

See [LICENSE](LICENSE) file for details.

------------------------------------------------------------------------

## 🔄 Version History
### Planned Features (v1.1.0)

-   [ ] Web interface for interactive exploration
-   [ ] Integration with GBIF API
-   [ ] Automated IUCN Red List assessment templates
-   [ ] Docker containerization
-   [ ] Cloud deployment (AWS/Google Cloud)

------------------------------------------------------------------------

**Last Updated:** 2026-01-31\
**Documentation Version:** 1.0.1

