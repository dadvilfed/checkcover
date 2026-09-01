#!/usr/bin/env Rscript
# Issue 3 (Lucian 2026-07): endemics assigned at HydroBASINS level 10 must show
# the fine RIVER names (Table_S3 5th column), not collapse to the coarse basin.
# Exercises the real loader + river-aware resolver against Table_S3.tsv.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!file.exists("Table_S3.tsv")) {
  cat("[test_basin_resolution] SKIP (Table_S3.tsv not present)\n"); quit(status = 0)
}

suppressWarnings(suppressMessages({
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/10_canonical_narratives.R")
}))

# Load exactly as the pipeline now does (keep the 5th river_name column).
hb <- read.delim("Table_S3.tsv", sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
base_names <- c("Basin_level","HYBAS_ID","Basin_name","Subbasin_name","river_name")
names(hb)[seq_len(min(ncol(hb), length(base_names)))] <- base_names[seq_len(min(ncol(hb), length(base_names)))]
if (!"river_name" %in% names(hb)) hb$river_name <- NA_character_
hb$HYBAS_ID <- as.character(hb$HYBAS_ID)

# Several distinct level-10 Tisza sub-basins (different rivers).
tisza <- hb[hb$Basin_level == "L10" & !is.na(hb$Basin_name) & hb$Basin_name == "Danube" &
            !is.na(hb$Subbasin_name) & hb$Subbasin_name == "Tisza" & !is.na(hb$river_name), ]
codes <- paste0("L10:", head(unique(tisza$HYBAS_ID), 6))
resolved <- .resolve_basin_col(codes, hb)

river_ok <- ("river_name" %in% names(hb)) &&
        length(unique(resolved)) >= 2 &&                 # rivers do NOT collapse to 1
        any(grepl(" - ", resolved)) &&                    # finest "Subbasin - river" form
        !all(resolved == "Danube - Tisza")                # not the coarse collapse

# A record occupying >1 basin is stored pipe-joined. Such a cell matches no
# lookup key, so it must be split before resolution or the whole composite
# resolves as a single unmatched value.
#
# The observable signal changed in 2026-08: the resolvers were unified into
# resolve_basin_names(), which returns "unnamed" for an unmatched code instead
# of echoing the raw "L10:..." string back. That is deliberate — the geojson and
# the narrative must print the same string for a basin, and Lucian's rule is
# that unnamed IS a name. The assertion below tests the same property through
# the new signal.
ids   <- head(unique(tisza$HYBAS_ID), 4)
cells <- c(paste0("L10:", ids[1]),
           paste0("L10:", ids[2], " | L10:", ids[3]),   # multi-basin record
           paste0("L10:", ids[4]))
old_way <- .resolve_basin_col(cells, hb)     # unsplit: composite matches nothing
new_way <- .resolve_basin_cells(cells, hb)   # split: every unit resolves
# Every code in `cells` has a river_name, so a real "unnamed" cannot appear here
# — any "unnamed" is a resolution failure, which is exactly what we want to see
# on the unsplit path and never on the split one.
split_ok <- any(old_way == "unnamed") && !any(new_way == "unnamed") &&
            !any(grepl("^L10:", new_way))

pass <- river_ok && split_ok
cat("[test_basin_resolution] sample:", paste(head(unique(resolved), 4), collapse = " | "), "\n")
cat(sprintf("[test_basin_resolution] %d distinct rivers from %d codes; pipe-split ok=%s -> %s\n",
            length(unique(resolved)), length(codes), split_ok, if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
