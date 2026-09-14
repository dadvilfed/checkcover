#### CONFIGURATION FILE ####
# Edit this file to change analysis parameters

# 1. Input/Output Paths
CONFIG <- list(
  # Input file (Excel database)
  input_file = "WoC_1_1.tsv",
  
  # Output directory
  root_output_dir = "checkover_output",
  
  # Version/Run ID (change to force new run)
  version = "production",
  
  # Output package version (semantic; drives processed/checkover/<version>/ folder)
  framework_version = "1.1",
  
  # 2. Taxonomy Settings
  taxonomy = list(
    resolve = TRUE  # Use WoRMS API to resolve taxonomy
  ),
  
  # 3. Vernacular Names
  # NB the "(Table_S2)" prefix is part of the filename. The repository carries
  # BOTH an unprefixed and a prefixed copy of this table (and of the ecoregion
  # list) with identical contents, because they are also published as manuscript
  # supplements under their supplement numbers. The Dockerfile copies only the
  # prefixed pair, so pointing config at the unprefixed name made Module 1B stop
  # with "Vernacular file path is invalid" inside the container. Naming the
  # prefixed file here works in every context.
  #
  # Keeping two copies of a 46 KB lookup in sync by hand is a trap — the
  # Cambarus emegi addition (2026-08) had to be applied to both. Worth
  # collapsing to one filename, but that is a repository decision, not a
  # config-file one.
  vernaculars = list(
    source = "file",  # Options: "itis" or "file"
    path   = "(Table_S2)vernacular_names_wide.tsv"
  ),

  # 4. Dictionary Paths (for renaming IDs to human-readable names)
  dictionaries = list(
    feow = "(Table_S4)ecoregions_list.tsv",
    hydrobasins = "Table_S3.tsv"
  ),
  
  # 5. Spatial Data Settings
  spatial = list(
    ne_scale     = "medium",
    gadm_version = "4.1",
    wdpa_km      = 2,
    # Passed to wdpar::wdpa_clean(). The package defaults this to TRUE, which
    # dissolves every overlap between protected areas in a country and is by
    # far the largest cost in the workflow — hours per country, and the wdpar
    # authors recommend disabling it for larger datasets.
    #
    # FALSE also suits what cheCkOVER asks of WDPA: it reports how many DISTINCT
    # protected areas a species occurs in, and a record inside a national park
    # nested within a biosphere reserve genuinely sits in both. Protection
    # percentage is unaffected either way. Set TRUE only if you specifically
    # need mutually exclusive, non-overlapping protected-area geometry.
    wdpa_erase_overlaps = FALSE,
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
  ),

  # 10. Spatial clustering (Module 1D)
  #
  # threshold_km is the absolute separation above which occurrences are treated
  # as belonging to different clusters: two points share a cluster when a chain
  # of points links them with no gap wider than this.
  #
  # It MUST be an absolute distance. Until 2026-09 the cut height was the mean
  # pairwise distance of the points themselves, which made a single cluster
  # unreachable by construction (complete linkage puts the root merge at the
  # MAXIMUM pairwise distance, and the mean is always below it). Every species
  # with enough coordinates therefore scored >1 cluster — 494 of 494 in v1.2 —
  # and the count tracked sample size rather than spatial structure. Reported by
  # Reviewer 1, Ecological Informatics, 2026-09.
  #
  # *** PROVISIONAL VALUE — awaiting an ecologically justified threshold from
  # *** Lucian. This is a configuration choice, not a property of the workflow;
  # *** it is written into every output so a package always states the value it
  # *** was computed under. Do not cite clustering results until it is settled.
  #
  # linkage: "single" asks "are there gaps wider than the threshold?", which is
  # the connectivity question this metric is for. "complete" constrains cluster
  # diameter instead and splits long river systems purely because they are long.
  clustering = list(
    threshold_km = 10,
    linkage      = "single"
  ),

  # 11. Reprocessing override (Phase 1.5)
  #
  # Sparse versioning fingerprints the INPUT DATA. A code-only change — a new
  # output property, a geometry fix, a renamed JSON key — is therefore invisible
  # to it: every species comes out "unchanged" and keeps inheriting the previous
  # version's artifacts, so the fix never reaches the output. This is the escape
  # hatch for that case.
  #
  #   FALSE                    normal sparse versioning (the default)
  #   TRUE                     force every species through the pipeline
  #   c("Cherax destructor")   force only these (display name or package id)
  #
  # Forced species are recorded as "reprocessed" with a change_summary saying
  # the reprocess was forced, so a manifest full of reprocessed species is never
  # mistaken for that many real data changes. The temporal delta (Phase 5C) is
  # unaffected: it runs its own data comparison and still skips species whose
  # occurrences are identical, so no spurious per-species versions are created.
  #
  # Use for code-only changes, then SET IT BACK TO FALSE. Data changes need no
  # override — sparse versioning already handles those.
  force_reprocess = FALSE
)

# Required R packages, available from CRAN.
REQUIRED_PACKAGES <- c(
  "sf", "sp", "raster", "terra", "dplyr", "tidyr", "jsonlite",
  "httr", "xml2", "rvest", "rnaturalearth", "rnaturalearthdata",
  "lwgeom", "units", "stringr", "lubridate", "ggplot2", "ggspatial",
  "mapview", "leaflet", "htmlwidgets", "glue", "readxl", "openxlsx",
  "yaml", "DT", "knitr", "rmarkdown", "worrms", "ritis", "wdpar",
  "geodata", "digest", "future", "future.apply",
  "readtext", "docxtractr", "progress"
)

# Packages that are NOT on CRAN. install.packages() cannot fetch these, so they
# are listed separately: the loader reports them with the command that works
# instead of failing with "package is not available" (Reviewer 1, Ecological
# Informatics, 2026-09 — `ecoregions` was in the CRAN list above, so a new user
# hit an install failure, and Supplementary S1 listed it as a CRAN package).
#
#   ecoregions - supplies the TEOW terrestrial ecoregion polygons used by
#                Module 2C. There is no local-file alternative for TEOW, so it
#                is required for a full run.
#   feowR      - OPTIONAL alternative source for FEOW freshwater ecoregions.
#                Module 2D already works without it: CONFIG$spatial$feow_source
#                accepts "local" (a shapefile you supply, the default) or
#                "feowR" / "auto" to use this package instead.
GITHUB_PACKAGES <- c(
  ecoregions = "jeffreyhanson/ecoregions",
  feowR      = "mhpob/feowR"
)

# Which of the above a full run genuinely needs.
GITHUB_PACKAGES_REQUIRED <- c("ecoregions")
# # LINUX SERVER (production, full dataset):
# #   parallel$force_sequential = FALSE
# #   parallel$workers = "auto"  # or 8-16 depending on cores
# #   parallel$parallel_branches = TRUE
# #   parallel$parallel_maps = TRUE
# #   memory$batch_size = 100
# #   memory$max_worker_memory = 2000
# #   version = "production_v1"