#!/usr/bin/env Rscript
################################################################################
# audit_reference_layers.R — Preflight check on cheCkOVER spatial layers
#
# Walks every reference spatial layer configured for the pipeline and reports
# features with:
#   - invalid geometries (st_is_valid == FALSE)
#   - antimeridian-crossing geometries (bbox width > 180 deg)
#   - latitude-spanning anomalies (bbox height > 170 deg)
#   - CRS issues (missing CRS, or weird datum)
#
# Output: a markdown report dropped at the project root with a timestamped
# filename. Always exits 0 (advisory). Run this before every release and
# whenever a reference layer is updated (new WDPA quarterly drop, etc.).
#
# Usage:
#   Rscript audit_reference_layers.R
#
# Design ref: sanitize_design.md, May 2026.
################################################################################

suppressPackageStartupMessages({
  library(sf)
})

# Local null-coalesce — audit is standalone, doesn't source other modules
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

if (!file.exists("config.R")) {
  stop("audit_reference_layers.R must be run from the project root (config.R not found here).")
}
source("config.R")

# ── Output report path ────────────────────────────────────────────────────
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_path <- sprintf("audit_reference_layers_%s.md", ts)

# Buffer report lines as we go
lines <- c(
  "# cheCkOVER reference layer audit",
  "",
  sprintf("**Run timestamp:** %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("**Working directory:** %s", getwd()),
  "",
  "Reports per-feature pathologies that could distort downstream",
  "spatial joins (point-in-polygon assignments).",
  "",
  "---",
  ""
)
emit <- function(...) lines <<- c(lines, ...)

n_layers_checked <- 0L
n_layers_with_issues <- 0L

# ── Per-feature audit helper ─────────────────────────────────────────────
#
# Returns a one-line tibble per problematic feature:
#   index | id | issue | detail
audit_layer <- function(sf_obj, layer_label, id_cols = NULL) {
  if (is.null(sf_obj) || !inherits(sf_obj, c("sf", "sfc"))) {
    return(data.frame(index = integer(0), id = character(0),
                      issue = character(0), detail = character(0),
                      stringsAsFactors = FALSE))
  }
  n_feat <- if (inherits(sf_obj, "sf")) nrow(sf_obj) else length(sf_obj)
  if (n_feat == 0L) {
    return(data.frame(index = integer(0), id = character(0),
                      issue = character(0), detail = character(0),
                      stringsAsFactors = FALSE))
  }

  geom <- sf::st_geometry(sf_obj)

  # Validity check — vectorized
  valid <- suppressWarnings(sf::st_is_valid(geom))
  # st_is_valid returns vector; NA = couldn't check
  valid[is.na(valid)] <- FALSE

  # Bbox audit — feature by feature (sf has no vectorized per-feature bbox)
  widths  <- numeric(n_feat)
  heights <- numeric(n_feat)
  for (i in seq_len(n_feat)) {
    bb <- tryCatch(sf::st_bbox(geom[[i]]), error = function(e) NULL)
    if (is.null(bb) || any(!is.finite(bb))) {
      widths[i]  <- NA_real_
      heights[i] <- NA_real_
    } else {
      widths[i]  <- as.numeric(bb["xmax"]) - as.numeric(bb["xmin"])
      heights[i] <- as.numeric(bb["ymax"]) - as.numeric(bb["ymin"])
    }
  }

  ids <- if (inherits(sf_obj, "sf") && !is.null(id_cols)) {
    cols_present <- intersect(id_cols, names(sf_obj))
    if (length(cols_present) > 0L) {
      apply(as.data.frame(sf_obj)[, cols_present, drop = FALSE], 1L,
            function(r) paste(r, collapse = " | "))
    } else as.character(seq_len(n_feat))
  } else as.character(seq_len(n_feat))

  rows <- list()
  for (i in seq_len(n_feat)) {
    if (!valid[i]) {
      rows[[length(rows) + 1L]] <- data.frame(
        index = i, id = ids[i], issue = "invalid_geometry",
        detail = "st_is_valid == FALSE",
        stringsAsFactors = FALSE)
    }
    if (!is.na(widths[i]) && widths[i] > 180) {
      rows[[length(rows) + 1L]] <- data.frame(
        index = i, id = ids[i], issue = "antimeridian_cross",
        detail = sprintf("bbox width = %.2f deg (>180)", widths[i]),
        stringsAsFactors = FALSE)
    }
    if (!is.na(heights[i]) && heights[i] > 170) {
      rows[[length(rows) + 1L]] <- data.frame(
        index = i, id = ids[i], issue = "latitude_spanning",
        detail = sprintf("bbox height = %.2f deg (>170)", heights[i]),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L) return(data.frame(
    index = integer(0), id = character(0),
    issue = character(0), detail = character(0),
    stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

# Helper: try to read a layer and audit it. Catches errors so one bad layer
# doesn't abort the whole audit.
audit_one_layer <- function(label, read_expr, id_cols = NULL) {
  n_layers_checked <<- n_layers_checked + 1L
  emit(sprintf("## %s", label), "")
  layer <- tryCatch(eval(read_expr), error = function(e) {
    emit(sprintf("> :x: **Failed to load:** %s", conditionMessage(e)), "")
    NULL
  })
  if (is.null(layer)) return(invisible(NULL))

  n_feat <- if (inherits(layer, "sf")) nrow(layer) else length(layer)
  crs_str <- as.character(sf::st_crs(layer)$input)
  emit(sprintf("- Features: %d", n_feat))
  emit(sprintf("- CRS: %s", if (is.na(crs_str) || !nzchar(crs_str)) "(missing)" else crs_str))

  if (is.na(crs_str) || !nzchar(crs_str)) {
    emit("- :warning: **No CRS metadata** — layer will be treated as EPSG:4326 at sanitize time.")
  }

  issues <- audit_layer(layer, label, id_cols)
  if (nrow(issues) == 0L) {
    emit("- :white_check_mark: **No per-feature pathologies detected.**", "")
    return(invisible(NULL))
  }

  n_layers_with_issues <<- n_layers_with_issues + 1L
  by_issue <- table(issues$issue)
  emit(sprintf("- :warning: **%d problematic feature(s)** across %d issue type(s):",
               nrow(issues), length(by_issue)))
  for (k in names(by_issue)) {
    emit(sprintf("    - %s: %d", k, by_issue[k]))
  }
  emit("", "Per-feature detail (first 20):", "")
  emit("```")
  head_rows <- head(issues, 20L)
  for (r in seq_len(nrow(head_rows))) {
    emit(sprintf("  [%s] feature #%d (%s): %s",
                 head_rows$issue[r], head_rows$index[r],
                 head_rows$id[r], head_rows$detail[r]))
  }
  if (nrow(issues) > 20L) emit(sprintf("  ... +%d more", nrow(issues) - 20L))
  emit("```", "")
  invisible(NULL)
}

# ── Audit each configured layer ──────────────────────────────────────────

cat(sprintf("Auditing reference layers; report -> %s\n", report_path))

# Natural Earth continents — fetched on demand by rnaturalearth
if (requireNamespace("rnaturalearth", quietly = TRUE)) {
  audit_one_layer(
    "Natural Earth — countries (admin-0, used by Module 2A)",
    quote(suppressWarnings(rnaturalearth::ne_countries(scale = "medium",
                                                      returnclass = "sf"))),
    id_cols = c("name", "iso_a3", "continent")
  )
}

# HydroBASINS — all configured levels
hb_dir   <- CONFIG$spatial$hydro_dir %||% NULL
hb_files <- CONFIG$spatial$hydro_files %||% list()
if (!is.null(hb_dir) && length(hb_files) > 0L) {
  for (lvl in names(hb_files)) {
    fp <- file.path(hb_dir, hb_files[[lvl]])
    if (file.exists(fp)) {
      audit_one_layer(
        sprintf("HydroBASINS Level %s — %s", lvl, basename(fp)),
        bquote(suppressWarnings(sf::st_read(.(fp), quiet = TRUE,
                                            stringsAsFactors = FALSE))),
        id_cols = c("HYBAS_ID", "MAJ_NAME", "SUB_NAME")
      )
    } else {
      emit(sprintf("## HydroBASINS Level %s", lvl), "",
           sprintf("> :x: **File not found:** %s", fp), "")
    }
  }
}

# FEOW
feow_path <- CONFIG$spatial$feow_path %||% NULL
if (!is.null(feow_path) && file.exists(feow_path)) {
  audit_one_layer(
    sprintf("FEOW — %s", basename(feow_path)),
    bquote(suppressWarnings(sf::st_read(.(feow_path), quiet = TRUE,
                                        stringsAsFactors = FALSE))),
    id_cols = c("FEOW_ID", "ECO_ID", "ECOREGION", "ECO_NAME")
  )
} else if (!is.null(feow_path)) {
  emit("## FEOW", "",
       sprintf("> :x: **File not found:** %s", feow_path), "")
}

# TEOW — best effort, path may vary in config
teow_path <- CONFIG$spatial$teow_path %||%
             CONFIG$spatial$teow_shp_path %||% NULL
if (!is.null(teow_path) && file.exists(teow_path)) {
  audit_one_layer(
    sprintf("TEOW — %s", basename(teow_path)),
    bquote(suppressWarnings(sf::st_read(.(teow_path), quiet = TRUE,
                                        stringsAsFactors = FALSE))),
    id_cols = c("ECO_NAME", "BIOME", "REALM")
  )
}

# WDPA — audit any cached country shapefiles
wdpa_cache_dir <- CONFIG$spatial$wdpa_cache_dir %||% file.path("cache", "wdpa")
if (dir.exists(wdpa_cache_dir)) {
  rds_files <- list.files(wdpa_cache_dir, pattern = "\\.rds$",
                          full.names = TRUE)
  if (length(rds_files) > 0L) {
    emit("## WDPA — cached per-country layers", "")
    for (rf in rds_files) {
      iso <- sub("^([A-Z]{3}).*$", "\\1", basename(rf))
      audit_one_layer(
        sprintf("WDPA / %s (cached)", iso),
        bquote(suppressWarnings(readRDS(.(rf)))),
        id_cols = c("WDPA_PID", "WDPAID", "NAME", "DESIG")
      )
    }
  } else {
    emit("## WDPA",
         "> WDPA cache directory exists but contains no .rds files. ",
         "Run the pipeline once to populate, then re-audit.", "")
  }
} else {
  emit("## WDPA",
       sprintf("> WDPA cache not found at: %s (skipping audit).", wdpa_cache_dir),
       "> WDPA layers are fetched per-country at pipeline run time.", "")
}

# ── Write report and summary ─────────────────────────────────────────────

emit("",
     "---",
     "",
     "## Summary",
     "",
     sprintf("- Layers checked: %d", n_layers_checked),
     sprintf("- Layers with issues: %d", n_layers_with_issues),
     "",
     "Issues flagged here are advisory. They DO NOT block the pipeline —",
     "sanitize_spatial_layer() applied at load time absorbs most of them",
     "(invalid geometries fixed, antimeridian-crossers wrapped, latitude-",
     "spanning features dropped). Use this report to (a) confirm",
     "sanitization will be effective, and (b) flag source-data corruption",
     "worth reporting to the layer maintainer.",
     "")

writeLines(lines, report_path)

cat(sprintf("Audit complete: %d layers checked, %d with issues.\n",
            n_layers_checked, n_layers_with_issues))
cat(sprintf("Report: %s\n", report_path))

# Always exit 0 (advisory)
invisible(NULL)
