#### CONFIGURATION FILE ####
# Edit this file to change analysis parameters

# 1. Input/Output Paths
CONFIG <- list(
  # Input file (Excel database)
  input_file = "WoC_1_0.tsv",
  
  # Output directory. Holds revision folders (1.0/, 1.1/, ...) and NOTHING else:
  # it is what a platform mirrors and installs, so it must never contain
  # anything with coordinates.
  root_output_dir = "checkover_output",

  # Working state: runs/ (per-run work, including the cleaned input table),
  # cache/ (reference layers), logs/, temporal/ (per-species occurrence
  # snapshots) and _registry.json. runs/ and temporal/ carry coordinates, which
  # is why this is a separate directory and must never sit inside
  # root_output_dir. One state dir belongs to one output root: temporal/ is the
  # history of that root's revisions. cache/ holds reference layers only and may
  # be copied between state dirs.
  state_dir = "checkover_state",

  # Version/Run ID (change to force new run)
  version = "production",
  
  # Output package version (semantic; drives processed/checkover/<version>/ folder)
  framework_version = "1.0",
  
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
    # NO EFFECT. Parallel map generation is unfinished: R/08_maps_parallel.R
    # exists but is never sourced or called, and nothing reads this setting.
    # Every run is sequential. Kept so existing config files still load, and as
    # a marker that the work is open. See R/08_maps_parallel.R for why the
    # attempt was abandoned (per-worker copies of the HydroBASINS layers made it
    # memory-bound rather than faster), and README "Parallelisation".
    parallel_maps = FALSE  # inert
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
  # method: "basin" (default) or "euclidean".
  #
  # Euclidean distance is the wrong metric for freshwater crayfish: two
  # populations 5 km apart in separate catchments are functionally more
  # disconnected than two 50 km apart along the same river, and no purely
  # spatial threshold can express that (Lucian, 2026-09).
  #
  # "basin" connects records sharing a HydroBASINS unit regardless of distance,
  # and separates records in different units unless threshold_km links them. The
  # basin level is the one Module 2F already assigns per distributional category
  # (L10 endemic, L08 regional, L06 cosmopolitan), so resolution follows range
  # extent. It requires the hydrobasin column, which is why Module 3B now runs
  # after spatial enrichment rather than before it.
  #
  # "euclidean" is the distance-only fallback, retained and configurable.
  clustering = list(
    method       = "basin",
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
  force_reprocess = FALSE,

  # 12. Species scope (Phase 1.5)
  #
  #   NULL                      every taxon whose data changed is reprocessed
  #   c("Astacus astacus", ...) only these taxa may be reprocessed
  #
  # With a scope, any OTHER taxon whose data changed is carried over from its
  # source revision and recorded in the manifest as "deferred"; a taxon seen for
  # the first time outside the scope is "deferred_new" and gets no package. The
  # input is still the full cohort, because fingerprints and inheritance need
  # the whole table. Taxa in force_reprocess are always in scope.
  #
  # This is how a World of Crayfish run reprocesses exactly what an admin
  # approved, and the deferred list is the next request queue. Leave NULL for a
  # full run such as the clean 1.0.
  species_scope = NULL,

  # 13. Code version recorded as provenance.code_version (optional)
  #
  # Leave NULL for a manual run: the version then comes from the container
  # image or from `git describe`. A service run file passes the tag it deployed.
  code_tag = NULL
)

# A run file (JSON) named by the environment variable CHECKOVER_RUN overrides
# the settings above, so a service can run cheCkOVER without editing this file.
# See README "Running as a service" and R/00_run_file.R.

# Required R packages, available from CRAN.
#
# Every package here is actually used. Sixteen were removed in 2026-09 after
# Supplement S1 was regenerated from source and showed they were never
# referenced anywhere in the codebase -- not via `pkg::`, not via library(), and
# not by a bare call to any of their exports: sp, terra, raster, ggplot2,
# ggspatial, mapview, leaflet, htmlwidgets, DT, knitr, rmarkdown, yaml, httr,
# xml2, rvest, docxtractr. Several are heavy (terra, raster, rmarkdown,
# mapview), and load_packages() installs and ATTACHES everything in this vector,
# so each one was install time and memory a new user paid for nothing.
#
# Regenerate the evidence at any time with:
#   Rscript regenerate_supplement_S1.R
# The packages sheet flags anything listed here but not referenced in the code.
#
# Two entries are uncalled on purpose and must stay:
#   rnaturalearthdata - data backend that rnaturalearth loads
#   lwgeom            - geometry backend registered with sf
# NB rlang and stringi are called directly (rlang::sym in 11_temporal_delta.R,
# stringi::stri_enc_isutf8 in 01_ingest.R) but were never declared -- they
# resolved by accident as sub-dependencies of dplyr and stringr. Declared
# explicitly now, because "it happens to be installed" is not a dependency.
REQUIRED_PACKAGES <- c(
  "sf", "lwgeom", "units",
  "dplyr", "tidyr", "stringr", "stringi", "lubridate", "glue", "readr",
  "tibble", "rlang",
  "jsonlite", "digest",
  "rnaturalearth", "rnaturalearthdata", "geodata", "wdpar",
  "worrms", "ritis",
  "readxl", "openxlsx", "readtext",
  "future", "future.apply", "progress"
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
