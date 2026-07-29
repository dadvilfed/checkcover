# cheCkOVER — reproducible runtime
#
# rocker/geospatial pins R together with a working GDAL/GEOS/PROJ stack, which
# is the part of this pipeline that is painful to reproduce by hand. The tag
# pins the R version used for the published run; bump it deliberately, never
# implicitly, because spatial results depend on the GDAL/PROJ generation.
#
#   docker build -t checkover:1.0 .
#
#   docker run --rm \
#     -v "$PWD/data:/work/data:ro" \
#     -v "$PWD/spatial_data:/work/spatial_data:ro" \
#     -v "$PWD/checkover_output:/work/checkover_output" \
#     checkover:1.0
#
# Occurrence data and reference layers are deliberately NOT baked into the
# image — they are mounted at run time. See README "Data availability".

FROM rocker/geospatial:4.5.2

LABEL org.opencontainers.image.title="cheCkOVER" \
      org.opencontainers.image.description="Reproducible framework for versioned biodiversity occurrence packages" \
      org.opencontainers.image.source="https://github.com/dadvilfed/checkcover" \
      org.opencontainers.image.licenses="GPL-3.0-or-later"

# Only the packages rocker/geospatial does not already carry. sf, terra, units
# and lwgeom ship with the base image against its own GDAL build — reinstalling
# them from source here would risk linking against a different stack.
RUN install2.r --error --skipinstalled --ncpus -1 \
      jsonlite digest glue progress future future.apply \
      worrms ritis wdpar geodata rnaturalearth rnaturalearthdata \
 && rm -rf /tmp/downloaded_packages

WORKDIR /work

# Pipeline sources only. Data directories are mount points.
COPY R/                          R/
COPY tests/                      tests/
COPY checkcover_main.R config.R  ./

# Offline lookup tables. NB the two prefixed filenames: these tables are also
# published as manuscript supplements and carry their supplement number in the
# name. config.R must point at whatever names are used here.
COPY WoC_canonical_country_continent.tsv WoC_canonical_geography.md ./
COPY "(Table_S2)vernacular_names_wide.tsv" "(Table_S4)ecoregions_list.tsv" ./

# Fail fast on a broken image: the suite runs without occurrence data or
# reference layers, so it is a genuine smoke test of the build.
RUN Rscript tests/run_all.R

CMD ["Rscript", "checkcover_main.R"]
