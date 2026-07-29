#### MODULE 00_SPATIAL_SANITIZE: UNIFIED LAYER SANITIZATION ####
#
# One function applied at every reference-layer load site to neutralize
# the antimeridian-crossing bug class that has bitten us three times:
#   - HydroBASINS global bbox -> degenerate crop (P. clarkii under-assignment)
#   - FEOW global bbox -> wrong nearest-feature (Austropotamobius -> Yukon/Alaska)
#   - WDPA dateline-crossing polygons -> stretched globally (Papahanaumokuakea
#     over-assigned to 1.9M+ records for P. clarkii alone)
#
# Same root cause every time: a feature whose geometry crosses or is computed
# across +-180 degrees longitude in a planar sf representation gets either
# corrupted (split incorrectly) or stretched (treated as ~360 degrees wide).
# All three patches were per-layer ad-hoc. This module replaces that with one
# uniform sanitization pipeline applied to every reference layer at load time.
#
# Steps (in this order):
#   1. st_make_valid()          - fix self-intersections, broken rings
#   2. st_transform(target_crs) - normalize CRS
#   3. st_wrap_dateline()       - split features crossing +-180 onto correct sides
#   4. st_make_valid() again    - wrap can introduce minor invalidities
#   5. per-feature bbox audit   - drop anything that survived broken
#   6. log: features in, dropped, valid out
#
# Idempotent: safe to call multiple times. Defensive on cached layers
# (re-applied after readRDS).
#
# Design ref: sanitize_design.md (May 2026, post-Papahanaumokuakea bug).
# ────────────────────────────────────────────────────────────────────────────


#' Sanitize an sf reference layer for safe spatial joining
#'
#' Applies a uniform pipeline of geometric fixes that protect against the
#' antimeridian-crossing bug class plus generic geometry validity issues.
#' Returns a cleaned sf object ready for any downstream spatial join.
#'
#' @param x                  An sf or sfc object. NULL or empty inputs are
#'                           returned unchanged.
#' @param target_crs         CRS to transform into. Default: EPSG:4326 (lat/lon).
#'                           Use the CRS of the points you'll be joining against.
#' @param drop_corrupt_bbox  If TRUE, drop features whose bbox spans more than
#'                           180 degrees longitude or 170 degrees latitude after
#'                           wrap (= almost certainly corrupt). Default TRUE.
#'                           Set FALSE to inspect what would be dropped.
#' @param layer_name         Short identifier for log messages (e.g. "WDPA_USA",
#'                           "HydroBASINS_L8", "FEOW"). Default "<unnamed>".
#' @param module             Logging module name. Default "SPATIAL_SANITIZE".
#'
#' @return An sf object with sanitized geometries. Same columns as input
#'         minus any features dropped by the bbox audit.
#' @export
sanitize_spatial_layer <- function(x,
                                   target_crs        = 4326,
                                   drop_corrupt_bbox = TRUE,
                                   layer_name        = "<unnamed>",
                                   module            = "SPATIAL_SANITIZE") {

  # Null / empty short-circuit
  if (is.null(x)) return(NULL)
  if (!inherits(x, c("sf", "sfc"))) {
    if (exists("log_warn", mode = "function")) {
      log_warn("sanitize_spatial_layer(%s): input is not an sf/sfc object (class=%s); returning unchanged",
               layer_name, paste(class(x), collapse = "/"), module = module)
    }
    return(x)
  }
  if (nrow(as.data.frame(x)) == 0L) {
    return(x)
  }

  n_in <- if (inherits(x, "sf")) nrow(x) else length(x)

  # ── Step 1: make valid (fix self-intersections, ring issues) ──────────────
  x <- tryCatch(
    suppressWarnings(sf::st_make_valid(x)),
    error = function(e) {
      if (exists("log_warn", mode = "function")) {
        log_warn("sanitize_spatial_layer(%s): st_make_valid step 1 failed: %s",
                 layer_name, conditionMessage(e), module = module)
      }
      x
    }
  )

  # ── Step 2: uniform CRS ──────────────────────────────────────────────────
  if (is.na(sf::st_crs(x))) {
    # Layer has no CRS metadata. Assume target_crs and proceed.
    sf::st_crs(x) <- target_crs
    if (exists("log_warn", mode = "function")) {
      log_warn("sanitize_spatial_layer(%s): layer has no CRS metadata; assumed EPSG:%s",
               layer_name, as.character(target_crs), module = module)
    }
  } else if (sf::st_crs(x) != sf::st_crs(target_crs)) {
    x <- tryCatch(
      suppressWarnings(sf::st_transform(x, target_crs)),
      error = function(e) {
        if (exists("log_warn", mode = "function")) {
          log_warn("sanitize_spatial_layer(%s): st_transform to EPSG:%s failed: %s",
                   layer_name, as.character(target_crs), conditionMessage(e),
                   module = module)
        }
        x
      }
    )
  }

  # ── Step 3: wrap dateline ────────────────────────────────────────────────
  # st_wrap_dateline splits any polygon crossing +-180 longitude into two
  # parts on the correct sides. WRAPDATELINE=YES is required; DATELINEOFFSET
  # of 10 degrees handles most real-world geometries cleanly.
  x <- tryCatch(
    suppressWarnings(sf::st_wrap_dateline(
      x,
      options = c("WRAPDATELINE=YES", "DATELINEOFFSET=10")
    )),
    error = function(e) {
      if (exists("log_warn", mode = "function")) {
        log_warn("sanitize_spatial_layer(%s): st_wrap_dateline failed: %s",
                 layer_name, conditionMessage(e), module = module)
      }
      x
    }
  )

  # ── Step 4: make valid again (wrap may introduce minor invalidities) ──────
  x <- tryCatch(
    suppressWarnings(sf::st_make_valid(x)),
    error = function(e) x
  )

  # ── Step 5: per-feature bbox audit ───────────────────────────────────────
  # Any feature still spanning >180 deg longitude or >170 deg latitude is
  # almost certainly corrupt (stretched across the globe or some other
  # geometry pathology). Drop with logging.
  n_dropped <- 0L
  if (drop_corrupt_bbox) {
    # Per-geometry bbox extraction is unfortunately not vectorized in sf, so
    # we walk features. Fast even on ~1M-feature layers (HydroBASINS L10).
    geom <- sf::st_geometry(x)
    n_feat <- length(geom)
    bad_mask <- logical(n_feat)
    for (i in seq_len(n_feat)) {
      bb <- tryCatch(sf::st_bbox(geom[[i]]), error = function(e) NULL)
      if (is.null(bb) || any(!is.finite(bb))) {
        bad_mask[i] <- TRUE
        next
      }
      width  <- as.numeric(bb["xmax"]) - as.numeric(bb["xmin"])
      height <- as.numeric(bb["ymax"]) - as.numeric(bb["ymin"])
      if (is.finite(width)  && width  > 180) bad_mask[i] <- TRUE
      if (is.finite(height) && height > 170) bad_mask[i] <- TRUE

      # ── Antimeridian rescue ────────────────────────────────────────────────
      # A feature that LEGITIMATELY straddles +-180 (USA via the Aleutians,
      # Russia via Chukotka, Fiji, Kiribati, New Zealand) ends up, after
      # st_wrap_dateline, with parts near both -180 and +180. Its overall bbox is
      # then ~360 deg wide even though every individual part is small and
      # correct, and the naive width test above discards the whole country. That
      # silently removed USA and RUS from the country-detection layer, so GADM
      # was never fetched for them and the entire North American fauna ended up
      # with country = NA (Lucian, 2026-07).
      #
      # Re-check any flagged feature using the MINIMAL COVERING ARC of its
      # longitudes rather than the bbox. A bbox cannot describe extent across
      # the antimeridian: Russia has vertices near both -180 and +180, so its
      # bbox is 360 deg wide even though the country only spans ~172 deg. The
      # covering arc is 360 minus the largest circular gap between consecutive
      # longitudes, which is the true angular extent and is dateline-agnostic.
      # A genuinely stretched geometry has vertices spread all the way round, so
      # its largest gap is small and its arc stays large — still flagged.
      # (st_wrap_dateline splits USA/NZL cleanly but leaves one 360-deg part on
      # RUS/FJI, so the earlier part-wise check rescued only some of them.)
      # Only runs for already-flagged features, so clean layers cost nothing.
      if (bad_mask[i]) {
        arc_ok <- tryCatch({
          cc  <- sf::st_coordinates(geom[[i]])
          lon <- ((as.numeric(cc[, 1]) + 180) %% 360) - 180
          lat <- as.numeric(cc[, 2])
          lon <- sort(unique(lon[is.finite(lon)]))
          lat <- lat[is.finite(lat)]
          if (length(lon) < 2L || length(lat) < 1L) {
            FALSE
          } else {
            gaps     <- diff(lon)
            wrap_gap <- (lon[1] + 360) - lon[length(lon)]
            arc      <- 360 - max(c(gaps, wrap_gap))
            hgt      <- max(lat) - min(lat)
            is.finite(arc) && is.finite(hgt) && arc <= 180 && hgt <= 170
          }
        }, error = function(e) FALSE)
        if (isTRUE(arc_ok)) bad_mask[i] <- FALSE
      }
    }
    n_dropped <- sum(bad_mask)
    if (n_dropped > 0L) {
      # Try to identify the dropped features by common name columns
      dropped_ids <- character(0)
      if (inherits(x, "sf")) {
        # Priority order: prefer columns that uniquely identify a feature
        # (WDPA_PID, FEOW_ID, ISO codes, names) over grouping columns like
        # "continent" or "region" which may appear in many features and
        # mislead the log.
        for (id_col in c("WDPA_PID", "WDPAID", "WDPA_ID",
                         "FEOW_ID", "ECO_ID", "ECO_NAME", "ECOREGION",
                         "HYBAS_ID", "MAJ_NAME", "SUB_NAME",
                         "iso_a3", "iso_a3_eh", "adm0_a3", "GID_0", "GID_1",
                         "name", "NAME", "name_long",
                         "continent", "continents")) {
          if (id_col %in% names(x)) {
            dropped_ids <- as.character(x[[id_col]][bad_mask])
            dropped_ids <- dropped_ids[!is.na(dropped_ids) & nzchar(dropped_ids)]
            break
          }
        }
      }
      if (exists("log_warn", mode = "function")) {
        if (length(dropped_ids) > 0L) {
          # Truncate the list when many drops (e.g. corrupt L10 hydrobasin shard)
          sample_ids <- head(unique(dropped_ids), 8L)
          tail_note  <- if (length(dropped_ids) > 8L) sprintf(", ... +%d more",
                                                              length(dropped_ids) - 8L) else ""
          log_warn(
            "sanitize_spatial_layer(%s): dropped %d/%d feature(s) with corrupt bbox (>180deg width or >170deg height): %s%s",
            layer_name, n_dropped, n_feat,
            paste(sample_ids, collapse = ", "), tail_note,
            module = module)
        } else {
          log_warn(
            "sanitize_spatial_layer(%s): dropped %d/%d feature(s) with corrupt bbox",
            layer_name, n_dropped, n_feat, module = module)
        }
      }
      x <- x[!bad_mask, , drop = FALSE]
    }
  }

  n_out <- if (inherits(x, "sf")) nrow(x) else length(x)
  if (exists("log_info", mode = "function") && (n_dropped > 0L || n_in > 1000L)) {
    log_info(
      "sanitize_spatial_layer(%s): %d in -> %d valid out (%d dropped)",
      layer_name, n_in, n_out, n_dropped, module = module)
  }

  x
}
