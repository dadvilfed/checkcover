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
-   [Troubleshooting](#troubleshooting)
-   [Performance Tuning](#performance-tuning)
-   [Output Structure](#output-structure)
-   [Citation](#citation)
-   [Contributing](#contributing)
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
-   **Conservation narratives** (automated ecological summaries)
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

### Enrichment Modules

| Module | Description | Data Source |
|----|----|----|
| **1. Ingest & Clean** | Load, validate, and standardize occurrence data | User Excel file |
| **1B. Vernacular Names** | Add common names in multiple languages | ITIS API or manual dictionary |
| **1C. Distribution Metrics** | Calculate EOO/AOO (historical vs current) | Computed |
| **1D. Fragmentation** | Detect spatial fragmentation patterns | Computed |
| **2A. Continents** | Tag records by continent | Natural Earth |
| **2B. GADM** | Add country and admin-1 boundaries | GADM v4.1 |
| **2C. TEOW** | Link to terrestrial ecoregions | WWF TEOW |
| **2D. FEOW** | Link to freshwater ecoregions | WWF FEOW |
| **2E. WDPA** | Identify protected area overlap | WDPA |
| **2F. HydroBASINS** | Assign hydrographic basins | HydroSHEDS |
| **3. Maps** | Generate EOO/AOO/basin maps (GeoJSON, KML) | Computed |
| **4. Reports** | Build species-level JSON reports | Compiled |
| **5. Narratives** | Generate ecological summaries | Compiled |
| **6. Citations** | Extract and format bibliographies | User data |
| **7. Export** | Package data for publication | Compiled |

------------------------------------------------------------------------

## 💻 System Requirements {#system-requirements}

### Minimum (Testing with mock data)

-   **OS:** Windows 10/11, macOS 10.15+, or Linux
-   **RAM:** 8 GB
-   **Storage:** 10 GB free space
-   **R Version:** ≥ 4.0.0

### Recommended (Full 100k database)

-   **OS:** Linux (Ubuntu 20.04+ or CentOS 7+)
-   **RAM:** 32 GB+
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
cp /path/to/your/database.xlsx ./database-WoC_mock2.xlsx
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
  input_file = "database-WoC_mock2.xlsx",
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
  force_sequential = FALSE    # Set TRUE to disable parallel
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
  file_path = "database-WoC_mock2.xlsx",
  output_dir = "test_output",
  resolve_taxonomy = TRUE
)
```

**Outputs:** - `clean_occurrences.csv` - Standardized occurrence data -
`taxonomy_mapping_full.csv` - WoRMS taxonomic hierarchy -
`type_localities.csv` - Type locality records

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

### Module 3: Map Generation

**File:** `R/03_maps.R`\
**Function:** `generate_maps()`

``` r
# Generate only GeoJSON (faster)
maps <- generate_maps(
  result,
  output_dir = "checkover_output",
  formats = c("geojson"),  # Exclude "kml" for speed
  parallel = FALSE         # Set TRUE on server
)
```

------------------------------------------------------------------------

## 💡 Usage Examples {#usage-examples}

### Example 1: Quick Test with Mock Data

``` r
# 1. Edit config.R
CONFIG$version <- "test_run_v1"
CONFIG$input_file <- "database-WoC_mock2.xlsx"  # 1k records
CONFIG$memory$batch_size <- 20

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
#      input_file = "database-WoC_FULL.xlsx"
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

------------------------------------------------------------------------

## 🐛 Troubleshooting {#troubleshooting}

### Common Issues

#### 1. Memory Errors

**Symptom:** `Error: cannot allocate vector of size...`

**Solution:**

``` r
# Edit config.R
CONFIG$memory$batch_size <- 10  # Reduce from 50
CONFIG$parallel$force_sequential <- TRUE  # Disable parallel
```

#### 2. GADM Download Fails

**Symptom:** `Failed to fetch GADM level 1 for 'XXX'`

**Solution:**

``` r
# Pre-download GADM files
library(geodata)
gadm("USA", level = 1, path = "checkover_output/cache")
gadm("CAN", level = 1, path = "checkover_output/cache")
# Repeat for all countries in your dataset
```

#### 3. HydroBASINS Cache Corruption

**Symptom:** `CORRUPTED CACHE DETECTED! L10 has only 1234 features`

**Solution:**

``` r
# Delete corrupted cache
unlink("checkover_output/cache/hydro_lev10_merged.rds")

# Re-run (will regenerate)
source("checkcover_main.R")
```

#### 4. WoRMS API Timeouts

**Symptom:** `Error resolving 'Species name': Timeout`

**Solution:**

``` r
# Disable taxonomy resolution for testing
CONFIG$taxonomy$resolve <- FALSE

# Or increase timeout (in module file)
options(timeout = 300)  # 5 minutes
```

#### 5. Parallel Processing Hangs

**Symptom:** Script freezes during parallel operations

**Solution:**

``` r
# Switch to sequential mode
CONFIG$parallel$force_sequential <- TRUE
CONFIG$reporting$parallel_maps <- FALSE
```

------------------------------------------------------------------------

## ⚡ Performance Tuning {#performance-tuning}

### For Low-Memory Systems (8GB RAM)

``` r
CONFIG$memory <- list(
  max_worker_memory = 500,   # Reduce from 1500
  batch_size = 10,           # Reduce from 50
  aggressive_gc = TRUE
)

CONFIG$parallel <- list(
  workers = 1,               # Single-threaded
  force_sequential = TRUE
)
```

### For High-Memory Servers (64GB+ RAM)

``` r
CONFIG$memory <- list(
  max_worker_memory = 4000,
  batch_size = 200,          # Process more species at once
  aggressive_gc = FALSE      # Less overhead
)

CONFIG$parallel <- list(
  workers = 16,              # Use more cores
  force_sequential = FALSE
)

CONFIG$reporting$parallel_maps <- TRUE  # Parallel map generation
```

### Speed Optimization Tips

1.  **Skip taxonomy resolution** (saves \~30 min for 700 species):

``` r
   CONFIG$taxonomy$resolve <- FALSE
```

2.  **Use cached spatial layers** (saves \~1 hour):

``` r
   # Keep checkover_output/cache/ between runs
```

3.  **Generate only essential maps**:

``` r
   CONFIG$reporting$formats <- c("geojson")  # Skip KML
```

4.  **Disable WDPA for testing** (saves \~2 hours):

``` r
   # Comment out in checkcover_main.R:
   # result <- enrich_with_wdpa(...)
```

------------------------------------------------------------------------

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
        ├── fragmentation_analysis.csv
        ├── taxonomy_mapping_full.csv
        ├── vernacular_names_lookup.csv
        ├── maps/
        │   ├── EOO/
        │   │   ├── Species_name_EOO.geojson
        │   │   └── Species_name_EOO.kml
        │   ├── AOO/
        │   └── Species_name_basins.geojson
        ├── reports/
        │   ├── dataset_summary_statistics.json
        │   ├── species_detailed_reports.csv
        │   └── species/
        │       └── Species_name.json
        ├── narratives/
        │   ├── Species_name_narrative.txt
        │   └── Species_name_eco_narrative.json
        ├── citations/
        │   ├── Species_name_bibliography.json
        │   ├── Species_name_bibliography.bib
        │   └── Species_name_CITATION.cff
        └── species_packages/              # Publication-ready packages
            ├── packaging_summary.json
            └── Species_name/
                ├── package_metadata.json
                ├── file_manifest.csv
                ├── data/
                │   └── Species_name_occurrences.csv  # Privacy-filtered
                ├── maps/
                ├── narratives/
                └── citations/
```

------------------------------------------------------------------------

## 📊 Expected Runtime

| Dataset Size        | System             | Configuration           | Runtime   |
|---------------------|--------------------|-------------------------|-----------|
| 1k records (mock)   | 8GB RAM, 4 cores   | Sequential, no taxonomy | \~10 min  |
| 1k records (mock)   | 8GB RAM, 4 cores   | Full pipeline           | \~30 min  |
| 100k records (full) | 32GB RAM, 8 cores  | Batch=50, sequential    | \~6 hours |
| 100k records (full) | 64GB RAM, 16 cores | Batch=100, parallel     | \~3 hours |

**Bottlenecks:** - Taxonomy resolution: \~5-10 sec per species (WoRMS
API) - WDPA enrichment: \~2-5 min per country (download + processing) -
HydroBASINS: \~10-30 min (first run, then cached)

------------------------------------------------------------------------

## 📖 Citation {#citation}

If you use cheCkOVER in your research, please cite:

``` bibtex
@software{checkover2025,
  title = {cheCkOVER: Unlocking biodiversity occurrence for artificial intelligence},
  author = {Your Name},
  year = {2025},
  url = {https://github.com/yourusername/checkover},
  version = {1.0.0}
}
```

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

## 🤝 Contributing {#contributing}

Contributions are welcome! To contribute:

1.  **Fork** the repository
2.  **Create** a feature branch: `git checkout -b feature/your-feature`
3.  **Commit** changes: `git commit -m 'Add amazing feature'`
4.  **Push** to branch: `git push origin feature/your-feature`
5.  **Open** a Pull Request

### Development Guidelines

-   Follow existing code style (use tidyverse conventions)
-   Add logging to new functions (`log_info()`, `log_debug()`)
-   Include progress bars for long operations
-   Write defensive code (check for NULL, handle errors)
-   Update documentation and README

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

## 🙏 Acknowledgments

-   **World of Crayfish** database contributors
-   Natural Earth, GADM, WWF, IUCN for spatial data
-   R Core Team and package maintainers
-   [Your institution/funding sources]

------------------------------------------------------------------------

## 📧 Contact

**Author:** [Your Name]\
**Email:**
[your.email\@institution.edu](mailto:your.email@institution.edu){.email}\
**GitHub:** [\@yourusername](https://github.com/yourusername)\
**Issues:** <https://github.com/yourusername/checkover/issues>

------------------------------------------------------------------------

## 🔄 Version History

### v1.0.0 (2025-01-XX)

-   ✅ Initial release
-   ✅ Full modular pipeline (Modules 1-7)
-   ✅ Memory-optimized HydroBASINS processing
-   ✅ Resume capability
-   ✅ Progress bars and comprehensive logging
-   ✅ Species package export

### Planned Features (v1.1.0)

-   [ ] Web interface for interactive exploration
-   [ ] Integration with GBIF API
-   [ ] Automated IUCN Red List assessment templates
-   [ ] Docker containerization
-   [ ] Cloud deployment (AWS/Google Cloud)

------------------------------------------------------------------------

**Last Updated:** 2025-01-XX\
**Documentation Version:** 1.0.0
