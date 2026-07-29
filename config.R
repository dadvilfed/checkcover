#### CONFIGURATION FILE ####
# Edit this file to change analysis parameters

# 1. Input/Output Paths
CONFIG <- list(
  # Input file (Excel database)
  input_file = "WoC_1_0.tsv",
  
  # Output directory
  root_output_dir = "checkover_output",
  
  # Version/Run ID (change to force new run)
  version = "production",
  
  # Output package version (semantic; drives processed/checkover/<version>/ folder)
  framework_version = "1.0",
  
  # 2. Taxonomy Settings
  taxonomy = list(
    resolve = TRUE  # Use WoRMS API to resolve taxonomy
  ),
  
  # 3. Vernacular Names
  vernaculars = list(
    source = "file",  # Options: "itis" or "file"
    path   = "vernacular_names_wide.tsv"
  ),
  
  # 4. Dictionary Paths (for renaming IDs to human-readable names)
  dictionaries = list(
    feow = "ecoregions_list.tsv",
    hydrobasins = "Table_S3.tsv"
  ),
  
  # 5. Spatial Data Settings
  spatial = list(
    ne_scale     = "medium",
    gadm_version = "4.1",
    wdpa_km      = 2,
    hydro_dir    = "spatial_data/hydrobasins",
    hydro_bbox   = 50,
    hydro_files  = list(
      "6"  = "hybas_lev06.shp",
      "8"  = "hybas_lev08.shp",
      "10" = "hybas_lev10.shp"
    ),
    feow_source  = "local",  # Options: "auto", "feowR", "local"
    feow_path    = "spatial_data/feow/feow_hydrosheds.shp"
  ),
  
  # 6. Reporting & Export
  reporting = list(
    formats = c("geojson", "kml"),
    parallel_maps = FALSE  # Set TRUE on Linux server, FALSE on PC
  ),
  
  # 7. Memory Management (CRITICAL FOR LARGE DATASETS)
  memory = list(
    # Maximum memory per worker (in MB)
    max_worker_memory = 1500,  # Adjust based on your RAM
    
    # Enable garbage collection after each species
    aggressive_gc = TRUE,
    
    # Batch size for processing species
    batch_size = 5,  # Process 50 species at a time
    
    # Enable disk caching for large objects
    use_disk_cache = TRUE
  ),
  
  # 8. Parallelization
  parallel = list(
    # Auto-detect or manually set workers
    workers = "auto",  # Or set to specific number: 4
    
    # Platform-specific settings
    force_sequential = TRUE  # Set TRUE to disable all parallel processing
  ),
  
  # 9. Temporal Change Detection (temporal_delta modules 11-13)
  temporal = list(
    # Master switch: set TRUE to enable versioned temporal tracking
    enabled = TRUE,
    
    # Map formats for temporal maps (separate from Module 8 maps)
    map_formats = c("geojson", "kml"),
    
    # Major version bump: set TRUE to force v1.X → v2.0 instead of v1.X+1
    major_bump = FALSE
  )
)

# Required R packages
REQUIRED_PACKAGES <- c(
  "sf", "sp", "raster", "terra", "dplyr", "tidyr", "jsonlite",
  "httr", "xml2", "rvest", "rnaturalearth", "rnaturalearthdata",
  "lwgeom", "units", "stringr", "lubridate", "ggplot2", "ggspatial",
  "mapview", "leaflet", "htmlwidgets", "glue", "readxl", "openxlsx",
  "yaml", "DT", "knitr", "rmarkdown", "worrms", "ritis", "wdpar",
  "geodata", "ecoregions", "digest", "future", "future.apply",
  "readtext", "docxtractr", "progress"
)
# # LINUX SERVER (production, full dataset):
# #   parallel$force_sequential = FALSE
# #   parallel$workers = "auto"  # or 8-16 depending on cores
# #   parallel$parallel_branches = TRUE
# #   parallel$parallel_maps = TRUE
# #   memory$batch_size = 100
# #   memory$max_worker_memory = 2000
# #   version = "production_v1"
