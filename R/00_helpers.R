#### HELPER FUNCTIONS ####
# Shared utilities used across modules

# Null coalescing operator: NULL or a zero-length value falls back to `y`.
# The ONE definition. 11_temporal_delta.R used to redefine it (with the
# zero-length rule) and, being sourced later, won for the whole pipeline while
# tests ran this file's NULL-only version; the rule the pipeline actually ran
# with is now the only one.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# ---------------------------------------------------------------------------
# Total-extinction / zero-active terminal state
# ---------------------------------------------------------------------------
# Mandatory disclaimer wherever a species is reported with zero remaining
# occurrences. This is a DATA-STATE signal, never a biological-extinction
# verdict (Lucian, 2026-06; ties to the Ecography Forum "local extinction as
# information"). Keep this wording in one place so output + narrative agree.
CHECKOVER_EXTINCTION_DISCLAIMER <- paste0(
  "This status reflects the STATE OF THE WORLD OF CRAYFISH DATA (zero remaining ",
  "occurrences on record), not a field-verified biological extinction. A species ",
  "flagged extinct on the basis of data warrants intensive, thorough field ",
  "verification before any real-world conclusion is drawn. Treat this as a signal ",
  "that the data indicate zero remaining occurrences, never as a determination of ",
  "biological extinction."
)

#' Count distinct usable geographic values (countries, admin units, ...).
#'
#' Excludes NA, blanks and the explicit `unresolved` sentinel, so a record whose
#' geography could not be resolved never inflates a count. Use this instead of
#' `n_distinct(x, na.rm = TRUE)` for any geographic field.
n_distinct_geo <- function(x) {
  v <- trimws(as.character(x))
  if (exists("is_geo_unresolved", mode = "function")) {
    v <- v[!is_geo_unresolved(v)]
  } else {
    v <- v[!is.na(v) & nzchar(v) & tolower(v) != "unresolved"]
  }
  length(unique(v))
}

#' Count the continents a species genuinely occupies.
#'
#' A plain `n_distinct(continents)` makes one stray record enough to call a
#' species cosmopolitan: *A. torrentium* qualified on 4 Asian records out of
#' 3,484 (0.1%), *O. pellucidus* (a Kentucky cave endemic) on a single European
#' record, and *E. spinifer/suttoni* purely because a BLANK label counted as a
#' second continent. Rule agreed with Lucian (2026-07):
#'
#'   * blank / NA labels are excluded outright — they are missing data, not a
#'     continent;
#'   * a continent counts only if it holds at least `min_records` records AND at
#'     least `min_share` of the species' labelled records.
#'
#' If no continent clears the bar the species is still somewhere, so the
#' dominant one is kept and the count is 1 — never 0, which would fall through
#' the classifier's `n_continents == 1` branch into "regional".
#'
#' @param cont        Character vector of per-record continent labels.
#' @param min_records Minimum records for a continent to count (default 5).
#' @param min_share   Minimum share of labelled records (default 0.05 = 5%).
#' @return integer(1)
count_continents <- function(cont, min_records = 5L, min_share = 0.05) {
  v <- trimws(as.character(cont))
  # Drop blanks AND the explicit `unresolved` sentinel: a value the fallback
  # could not resolve must never contribute to a continent count, because that
  # would let a data gap change a species' biogeographic category — exactly what
  # happened to E. suttoni (Lucian, 2026-07).
  if (exists("is_geo_unresolved", mode = "function")) {
    v <- v[!is_geo_unresolved(v)]
  } else {
    v <- v[!is.na(v) & nzchar(v) & tolower(v) != "unresolved"]
  }
  if (length(v) == 0L) return(0L)
  tb <- table(v)
  keep <- (as.integer(tb) >= min_records) & ((as.integer(tb) / length(v)) >= min_share)
  n <- as.integer(sum(keep))
  if (n == 0L) 1L else n
}

#' Build the finest human-readable basin name from Table_S3 components.
#'
#' Table_S3 carries three name levels: Basin_name (e.g. Danube) >
#' Subbasin_name (e.g. Tisza) > river_name (e.g. Crișul Alb, the level-10
#' river). Endemics assigned at level 10 must show the RIVER, not collapse to
#' the coarse basin (Lucian, 2026-07). Uses the two finest non-empty, distinct
#' components for a specific-but-readable label; falls back to `fallback`
#' (the raw code) when nothing resolves.
#'
#' @param basin,subbasin,river Scalar name components (any may be NA/empty).
#' @param fallback Value to return when no component is available.
#' @return Character(1).
basin_display_name <- function(basin, subbasin, river, fallback = NA_character_) {
  comps <- c(basin, subbasin, river)
  comps <- comps[!is.na(comps) & nzchar(comps)]
  comps <- comps[!duplicated(comps)]
  if (length(comps) == 0L) return(fallback)
  if (length(comps) >= 2L) paste(utils::tail(comps, 2L), collapse = " - ") else comps[1]
}

#' Canonical HydroBASINS code -> display-name resolution.
#'
#' ONE resolver, used by the narratives, the per-species reports AND the map
#' exports. Before 2026-08 there were three: .resolve_basin_col() in the
#' narrative module, .resolve_basin_3c() in the report module, and a coarse
#' Basin_name-only lookup in the maps. They disagreed, and that produced two
#' of the four defects Lucian reported:
#'
#'   * the geojson said "Danube" for all 21 Austropotamobius bihariensis
#'     basins while the narrative said "Tisza - Crisul Alb" etc. (11 names);
#'   * .resolve_basin_3c() had no case-insensitive rescue for the lookup's
#'     column names, so when they arrived in another case it silently returned
#'     the RAW CODES. Distinct raw codes == distinct units, so n_named_basins
#'     collapsed to n_hydrobasins and the summary sentence quoted the unit
#'     count as if it were the name count (483 of 676 species).
#'
#' Everything now delegates here, so the string on a geojson feature is by
#' construction the string the narrative prints for that basin.
#'
#' Accepts codes as bare ids ("2100513510") or level-prefixed ("L10:2100513510").
#' HYBAS_ID is globally unique across L6/L8/L10 (1,148,084 distinct ids in
#' 1,148,084 rows), so the bare id alone is an unambiguous key.

.HB_LOOKUP_CACHE <- new.env(parent = emptyenv())

#' Normalise a Table_S3-style lookup to canonical column names.
#' Case-insensitive on input: the rescue that .resolve_basin_3c() lacked.
.hb_normalise_lookup <- function(hb_lookup) {
  if (is.null(hb_lookup) || !is.data.frame(hb_lookup) || nrow(hb_lookup) == 0L) return(NULL)
  canon <- c(basin_level = "Basin_level", hybas_id = "HYBAS_ID",
             basin_name = "Basin_name", subbasin_name = "Subbasin_name",
             river_name = "river_name")
  nm  <- names(hb_lookup)
  key <- tolower(trimws(nm))
  for (i in seq_along(nm)) if (!is.na(canon[key[i]])) nm[i] <- unname(canon[key[i]])
  names(hb_lookup) <- nm

  if (!all(c("HYBAS_ID", "Basin_name") %in% names(hb_lookup))) return(NULL)
  for (col in c("Subbasin_name", "river_name")) {
    if (!col %in% names(hb_lookup)) hb_lookup[[col]] <- NA_character_
  }
  hb_lookup$HYBAS_ID <- as.character(hb_lookup$HYBAS_ID)
  hb_lookup
}

#' Memoised lookup index. Stores the normalised frame plus its id vector; the
#' name cascade itself runs only on the codes actually requested, so loading
#' the 1.15M-row table costs one normalise, not 1.15M cascades.
.hb_index <- function(hb_lookup = NULL) {
  if (!is.null(.HB_LOOKUP_CACHE$hb)) return(.HB_LOOKUP_CACHE)
  if (is.null(hb_lookup) && exists("HYDROBASIN_NAMES", envir = globalenv())) {
    hb_lookup <- get("HYDROBASIN_NAMES", envir = globalenv())
  }
  hb <- .hb_normalise_lookup(hb_lookup)
  if (is.null(hb)) return(NULL)
  .HB_LOOKUP_CACHE$hb  <- hb
  .HB_LOOKUP_CACHE$ids <- hb$HYBAS_ID
  .HB_LOOKUP_CACHE
}

#' Reset the memo. Tests only — the lookup is fixed for the life of a run.
.hb_index_reset <- function() {
  rm(list = ls(.HB_LOOKUP_CACHE), envir = .HB_LOOKUP_CACHE)
  invisible(NULL)
}

#' Resolve HydroBASINS codes to display names.
#'
#' @param codes Character vector, bare or "Lxx:"-prefixed.
#' @param hb_lookup Table_S3 frame, or NULL for the HYDROBASIN_NAMES global.
#' @param fallback Value for a code absent from the lookup. "unnamed" keeps
#'   every output a usable string (Lucian's rule: unnamed IS a name). Pass
#'   NA to have unresolved codes returned verbatim instead.
#' @return list(names, n_named, n_unnamed, n_unmatched).
#' @param granularity "fine" (default) is the narrative's label: the two finest
#'   of Basin > Subbasin > river, e.g. "Tisza - Crisul Repede". "coarse" is the
#'   root Basin_name alone, e.g. "Danube". Map features carry both (Lucian,
#'   2026-09): the root groups basins into river systems, the fine name tells
#'   an assessor which water each polygon actually is.
resolve_basin_names <- function(codes, hb_lookup = NULL, fallback = "unnamed",
                                granularity = c("fine", "coarse")) {
  granularity <- match.arg(granularity)
  codes <- as.character(codes)
  ids   <- sub("^L[0-9]+:", "", codes)
  out   <- rep(NA_character_, length(codes))

  ix <- .hb_index(hb_lookup)
  if (is.null(ix)) {
    out[] <- if (is.na(fallback)) codes else fallback
    return(list(names = out, n_named = 0L, n_unnamed = length(out),
                n_unmatched = length(out)))
  }

  j     <- match(ids, ix$ids)
  found <- !is.na(j)

  if (any(found)) {
    hb <- ix$hb
    jj <- j[found]
    blank <- function(x) { x <- as.character(x); x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_; x }
    b <- blank(hb$Basin_name[jj]); s <- blank(hb$Subbasin_name[jj]); r <- blank(hb$river_name[jj])
    # Finest available label, river-aware: Basin > Subbasin > river. Two finest
    # distinct components, exactly as basin_display_name() has always done for
    # the narratives -- level-10 endemics must show the RIVER, not collapse to
    # the coarse basin (Lucian, 2026-07).
    out[found] <- if (granularity == "coarse") {
      b
    } else {
      vapply(seq_along(jj), function(k)
        basin_display_name(b[k], s[k], r[k], fallback = NA_character_),
        character(1))
    }
  }

  # A row whose components are all blank resolves to nothing -> "unnamed",
  # which is also the literal Basin_name for 59,703 rows of the source table.
  unresolved <- is.na(out)
  out[unresolved] <- if (is.na(fallback)) codes[unresolved] else fallback

  list(
    names       = out,
    n_named     = sum(!is.na(out) & out != "unnamed"),
    n_unnamed   = sum(out == "unnamed", na.rm = TRUE),
    n_unmatched = sum(!found)
  )
}

#' THE species-name normalisation, applied once at ingest.
#'
#' Every name in cheCkOVER passes through this before anything else sees it, and
#' species folders are then make_package_id(normalize_species_name(x)). World of
#' Crayfish reproduces the same two steps to match folders to taxa; the rule must
#' live in exactly one place on each side, and agree (Lucian, 2026-09 — a first
#' comparison mis-matched 23 Cambarellus taxa until the lowercasing below was
#' reproduced). The steps, precisely, for porting:
#'
#'   1. collapse every run of whitespace to a single space; trim both ends
#'   2. sentence case: the first character upper case, EVERY other character
#'      lower case (ICU sentence case, stringr::str_to_sentence). This
#'      lowercases a subgenus: "Cambarellus (Pandicambarus) rotatus" becomes
#'      "Cambarellus (pandicambarus) rotatus"
#'
#' and for the folder name, make_package_id():
#'
#'   3. delete "(" and ")"
#'   4. replace every run of whitespace with "_"; collapse runs of "_"; trim "_"
#'
#' e.g. "Cambarellus  (Pandicambarus) rotatus " -> species
#' "Cambarellus (pandicambarus) rotatus" -> folder
#' "Cambarellus_pandicambarus_rotatus".
normalize_species_name <- function(x) {
  x <- trimws(gsub("\\s+", " ", as.character(x)))
  stringr::str_to_sentence(x)
}

#' HydroBASINS topology fields carried onto every basin feature (Lucian,
#' 2026-09). MAIN_BAS is the outlet basin of the river system a polygon drains
#' to; NEXT_DOWN is the immediately downstream basin (0 at an outlet). Together
#' they let a consumer resolve an anonymous basin through the real drainage
#' hierarchy instead of inferring it from which records co-occur: about one in
#' five level-8 polygons has no name of its own.
HB_TOPOLOGY_FIELDS <- c("MAIN_BAS", "NEXT_DOWN")

#' The install command for a GitHub-only package, from GITHUB_PACKAGES in
#' config.R: the one place the repository (and its pinned commit) is named.
#' Messages used to carry their own copies of the repository name, and those
#' went stale (jeffreyhanson/ecoregions, mhpob/feowR: neither exists).
github_install_hint <- function(pkg) {
  spec <- get0("GITHUB_PACKAGES", envir = globalenv(), ifnotfound = character(0))[pkg]
  if (length(spec) != 1L || is.na(spec)) {
    return(sprintf("see GITHUB_PACKAGES in config.R for the repository of '%s'", pkg))
  }
  sprintf("remotes::install_github(\"%s\")", unname(spec))
}

# ── How to cite cheCkOVER (CHECKOVER_REFERENCE in config.R) ──────────────────

.reference_or_default <- function(ref) {
  if (!is.null(ref)) return(ref)
  if (exists("CHECKOVER_REFERENCE", envir = globalenv())) get("CHECKOVER_REFERENCE", envir = globalenv())
  else stop("CHECKOVER_REFERENCE is not defined: source config.R first.", call. = FALSE)
}

#' The reference as one line, rebuilt from its structured fields.
#'
#' Authors "Family Given" (or the group name) joined by ", ", then ": ", the
#' title, ". ", and the DOI as a URL. Must equal CHECKOVER_REFERENCE$text.
checkover_reference_from_parts <- function(ref = NULL) {
  ref <- .reference_or_default(ref)
  who <- vapply(ref$authors, function(a)
    if (!is.null(a$name)) a$name else paste(a$family, a$given), character(1))
  sprintf("%s: %s. https://doi.org/%s", paste(who, collapse = ", "), ref$title, ref$doi)
}

#' The reference as package_metadata.json carries it (`preferred_citation`).
checkover_reference_metadata <- function(ref = NULL) {
  ref <- .reference_or_default(ref)
  list(
    text    = ref$text,
    title   = ref$title,
    doi     = ref$doi,
    url     = paste0("https://doi.org/", ref$doi),
    authors = lapply(ref$authors, function(a)
      if (!is.null(a$name)) list(name = a$name)
      else list(family_names = a$family, given_names = a$given))
  )
}

#' The `preferred-citation:` block of a CITATION.cff (CFF 1.2.0), as lines.
checkover_reference_cff <- function(ref = NULL) {
  ref <- .reference_or_default(ref)
  q <- function(x) sprintf('"%s"', gsub('"', '\\\\"', x))
  who <- unlist(lapply(ref$authors, function(a) {
    if (!is.null(a$name)) sprintf("    - name: %s", q(a$name))
    else c(sprintf("    - family-names: %s", q(a$family)),
           sprintf("      given-names: %s", q(a$given)))
  }))
  c("preferred-citation:",
    "  type: article",
    if (!is.null(ref$status)) sprintf("  status: %s", ref$status),
    sprintf("  title: %s", q(ref$title)),
    "  authors:",
    who,
    sprintf("  doi: %s", q(ref$doi)),
    sprintf("  url: %s", q(paste0("https://doi.org/", ref$doi))))
}

#' Format HydroBASINS ids as exact decimal strings.
#'
#' The source shapefiles store ids as doubles, and R's default conversion of a
#' double to text can switch to scientific notation (3100000000 -> "3.1e+09"),
#' which silently destroys a join key. sprintf("%.0f") is exact for every
#' integer below 2^53, far above any HydroBASINS id. NEXT_DOWN = 0 (an outlet)
#' is kept as "0", the HydroBASINS convention.
hb_id_string <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x)
  out[ok] <- sprintf("%.0f", x[ok])
  out
}

#' Map-export wrapper. Kept as its own name because 08_maps.R reports the
#' named/unnamed split in its log line.
resolve_basin_map_names <- function(ids, hb_lookup = NULL) {
  resolve_basin_names(ids, hb_lookup, fallback = "unnamed")
}
#' Is a species in the zero-active (total-extinction) terminal state?
#'
#' TRUE when the active (post-suppression, non-extinct) record count is 0 while
#' the species is still known to the dataset (>=1 extinct/suppressed record).
#' Distinguishes a genuinely extirpated species from one simply absent in a
#' population branch (which has 0 records of ANY kind).
#'
#' @param active_count Number of active records.
#' @param known_count  Total records of any temporal_status for the species.
#' @return logical(1)
is_zero_active_terminal <- function(active_count, known_count) {
  isTRUE(active_count == 0L) && isTRUE(known_count > 0L)
}

# Safe type conversions
na_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
na_lgl <- function(x) if (is.null(x) || length(x) == 0) NA else as.logical(x)
na_num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)
one_chr <- function(x) if (length(x) >= 1) as.character(x[[1]]) else NA_character_
one_num <- function(x) if (length(x) >= 1) suppressWarnings(as.numeric(x[[1]])) else NA_real_

# Safe min/max
.safe_min <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

.safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

# String cleaning
nz_or_na <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

.str_clean <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u00A0\t\r\n]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Standardize sf geometry column name
.std_geom <- function(x) {
  x <- sf::st_as_sf(x)
  g <- attr(x, "sf_column")
  if (!identical(g, "geometry")) {
    names(x)[names(x) == g] <- "geometry"
    attr(x, "sf_column") <- "geometry"
  }
  x
}

# Build filesystem-safe package ID from a species display name.
# Per cheCkOVER package spec v1.0 (Lucian / WoC integration):
#   1. Remove parentheses (subgenus survives as a bare word)
#   2. Replace whitespace with underscore
#   3. Collapse consecutive underscores
# Casing is preserved (subgenus capitalisation is part of the name).
#
#   make_package_id("Astacus astacus")
#     -> "Astacus_astacus"
#   make_package_id("Cambarellus (Cambarellus) chapalanus")
#     -> "Cambarellus_Cambarellus_chapalanus"
#   make_package_id("Procambarus hagenianus vesticeps")
#     -> "Procambarus_hagenianus_vesticeps"
#
# NB make_package_id() itself preserves case, but it never sees the raw name:
# ingest passes every name through normalize_species_name() first, and that
# lowercases the subgenus. So the real folder is
# "Cambarellus_cambarellus_chapalanus", not the "Cambarellus_Cambarellus_..."
# above. 23 taxa carry a subgenus and are affected.
make_package_id <- function(sp) {
  out <- trimws(as.character(sp))
  out <- gsub("[()]", "", out, perl = TRUE)
  out <- gsub("\\s+", "_", out, perl = TRUE)
  out <- gsub("_+", "_", out, perl = TRUE)
  out <- gsub("^_+|_+$", "", out, perl = TRUE)
  out
}

# Expand bbox by kilometers (Equal Area projection)
.expand_bbox_km <- function(bbox_sfc, target_crs, km) {
  if (km <= 0) return(bbox_sfc)
  ea <- 6933
  bb_ea <- try(sf::st_transform(bbox_sfc, ea), silent = TRUE)
  if (inherits(bb_ea, "try-error")) return(bbox_sfc)
  bb_poly <- sf::st_as_sfc(sf::st_bbox(bb_ea))
  buf <- sf::st_buffer(bb_poly, dist = km * 1000)
  tryCatch(sf::st_transform(buf, target_crs), error = function(e) bbox_sfc)
}

# Safe digest (hashing)
.safe_digest <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(x, algo = "xxhash64"))
  }
  NA_character_
}

# Format latitude/longitude
fmt_latlon <- function(lat, lon, digits = 2) {
  if (!is.finite(lat) || !is.finite(lon)) return(NA_character_)
  ns <- if (lat >= 0) "N" else "S"
  ew <- if (lon >= 0) "E" else "W"
  paste0(abs(round(lat, digits)), "°", ns, ", ", abs(round(lon, digits)), "°", ew)
}

# EOO/AOO calculation helpers
.calc_eoo_val <- function(lon, lat) {
  coords <- unique(data.frame(lon, lat))
  coords <- coords[is.finite(coords$lon) & is.finite(coords$lat), ]
  if (nrow(coords) >= 3) {
    pts <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
    return(as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(pts)))) / 1e6)
  }
  return(NA_real_)
}

#' The EOO layer's hull, by the metric's rule: none below three DISTINCT points.
#'
#' Module 8 used to count records instead of localities. A taxon with three
#' records at two localities got a hull anyway, and s2 draws two points as a
#' thin triangle with an invented third corner. In 1.0 (2026-09-26) that hit 8
#' taxa, one corner up to 1.9 degrees from any record, while their metadata
#' and narratives said EOO undefined.
#'
#' @param pts An sf of points.
#' @return The convex hull (sfc), or NULL when the EOO is undefined.
eoo_hull <- function(pts) {
  if (is.null(pts) || nrow(pts) == 0L) return(NULL)
  xy <- sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE]
  xy <- unique(xy[is.finite(xy[, 1]) & is.finite(xy[, 2]), , drop = FALSE])
  if (nrow(xy) < 3L) return(NULL)
  hull <- sf::st_convex_hull(sf::st_union(pts))
  if (!all(sf::st_is_valid(hull))) hull <- sf::st_make_valid(hull)
  hull
}

#' Area of occupancy on a true equal-area 2 x 2 km lattice.
#'
#' THE canonical AOO implementation. Seven copies of the same lattice arithmetic
#' existed across the modules (00_helpers, 03a, 04a, 04_reports, 08_maps,
#' 08_maps_parallel, 11_temporal_delta); they now all delegate here.
#'
#' Until 2026-09 the lattice was 0.018 degrees of longitude/latitude with each
#' occupied cell credited a flat 4 km². Cell width in degrees contracts with
#' latitude, so the credited area was correct only near the equator and
#' increasingly wrong towards the poles — roughly 2.4x overcredited at 65°N,
#' where a 0.018° cell is about 0.85 km wide, not 2 km. The manuscript disclosed
#' the bias but justified it as the cost of avoiding per-species reprojection.
#' Reviewer 1 (Ecological Informatics, 2026-09) pointed out that the workflow
#' already reprojects to an equal-area CRS in the clustering module, so the
#' justification did not hold. It does not: the transform below costs
#' microseconds per species.
#'
#' EPSG:6933 (NSIDC EASE-Grid 2.0 Global) is cylindrical equal-area in metres,
#' the same CRS the clustering module uses, so a cell is 2 x 2 km everywhere.
#'
#' @param lon,lat Numeric vectors of coordinates in EPSG:4326.
#' @param cell_km Cell edge in kilometres. 2 is the IUCN Criterion B2 standard.
#' @return Occupied area in km², or NA_real_ when no usable coordinates.
CHECKOVER_AOO_CELL_KM <- 2
CHECKOVER_AOO_CRS     <- 6933

calc_aoo_km2 <- function(lon, lat, cell_km = CHECKOVER_AOO_CELL_KM) {
  keep <- is.finite(lon) & is.finite(lat)
  lon <- lon[keep]; lat <- lat[keep]
  if (length(lon) == 0L) return(NA_real_)

  cell_m <- cell_km * 1000

  xy <- tryCatch({
    pts <- sf::st_as_sf(data.frame(lon = lon, lat = lat),
                        coords = c("lon", "lat"), crs = 4326)
    sf::st_coordinates(sf::st_transform(pts, CHECKOVER_AOO_CRS))
  }, error = function(e) NULL)

  if (is.null(xy)) {
    # Never silently fall back to the degree lattice — that would reintroduce
    # the latitude bias under a function that promises equal area.
    if (exists("log_warn", mode = "function")) {
      log_warn("calc_aoo_km2(): equal-area transform failed; AOO not computed",
               module = "HELPERS")
    }
    return(NA_real_)
  }

  n_cells <- length(unique(paste(floor(xy[, 1] / cell_m),
                                 floor(xy[, 2] / cell_m))))
  n_cells * (cell_km ^ 2)
}

# Back-compatible name used by 01c_metrics.R. Delegates; no second algorithm.
.calc_aoo_val <- function(lon, lat) calc_aoo_km2(lon, lat)

# Distribution category from EOO
.level_for_cat <- function(cat) {
  switch(cat, 
         "micro-endemic" = 10L, 
         "endemic" = 10L, 
         "regional" = 8L, 
         "cosmopolitan" = 6L, 
         8L)
}

# CRITICAL FIX: Safe intersects with multiple fallbacks
.safe_intersects <- function(pts_sf, polys_sf) {
  out <- try(sf::st_intersects(pts_sf, polys_sf, sparse = TRUE), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  
  # Fallback 1: Validate geometries
  polys_ok <- try(suppressWarnings(lwgeom::st_make_valid(polys_sf)), silent = TRUE)
  if (inherits(polys_ok, "try-error")) polys_ok <- polys_sf
  polys_ok <- polys_ok[!sf::st_is_empty(polys_ok), , drop = FALSE]
  
  out <- try(sf::st_intersects(pts_sf, polys_ok, sparse = TRUE), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  
  # Fallback 2: Disable s2 and use Equal Area
  s2_old <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(s2_old), add = TRUE)
  sf::sf_use_s2(FALSE)
  
  ea <- 6933
  pts_ea <- try(sf::st_transform(pts_sf, ea), silent = TRUE)
  pol_ea <- try(sf::st_transform(polys_ok, ea), silent = TRUE)
  
  if (inherits(pts_ea, "try-error") || inherits(pol_ea, "try-error")) {
    return(sf::st_intersects(sf::st_geometry(pts_sf), sf::st_geometry(polys_ok), sparse = TRUE))
  }
  
  sf::st_intersects(pts_ea, pol_ea, sparse = TRUE)
}

# CRITICAL FIX: Safe distance calculation (always uses Equal Area)
.safe_within_distance <- function(pts_sf, pts_wdpa_sf, dist_m) {
  ea <- 6933
  tryCatch({
    pts_ea <- sf::st_transform(pts_sf, ea)
    wdpa_ea <- sf::st_transform(pts_wdpa_sf, ea)
    sf::st_is_within_distance(pts_ea, wdpa_ea, dist = dist_m)
  }, error = function(e) {
    rep(list(integer(0)), nrow(pts_sf))
  })
}

# CRITICAL FIX: File locking for cache writes
.save_with_lock <- function(obj, path, max_wait = 30) {
  lock_file <- paste0(path, ".lock")
  start_time <- Sys.time()
  
  # Try to create lock (atomic operation)
  while (!file.create(lock_file, showWarnings = FALSE)) {
    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > max_wait) {
      warning("Cache lock timeout. Proceeding anyway.")
      break
    }
    # Random wait to avoid thundering herd
    Sys.sleep(runif(1, 0.1, 0.5))
    # If file exists and lock is gone, we're good
    if (file.exists(path) && !file.exists(lock_file)) {
      return(invisible(NULL))
    }
  }
  
  tryCatch({
    saveRDS(obj, path)
    log_info("Saved cache: %s", basename(path), module = "CACHE")
  }, error = function(e) {
    log_error("Failed to save cache: %s", conditionMessage(e), module = "CACHE")
  }, finally = {
    unlink(lock_file)
  })
  
  invisible(NULL)
}

# Progress bar creator
create_progress_bar <- function(total, format = "[:bar] :percent ETA: :eta") {
  if (requireNamespace("progress", quietly = TRUE)) {
    progress::progress_bar$new(format = format, total = total, clear = FALSE)
  } else {
    # Fallback: simple counter
    list(
      tick = function() cat("."),
      terminate = function() cat("\n")
    )
  }
}

# Memory-safe batch processor
process_in_batches <- function(items, batch_size, process_fn, ...) {
  n_items <- length(items)
  n_batches <- ceiling(n_items / batch_size)
  
  log_info("Processing %d items in %d batches", n_items, n_batches, module = "BATCH")
  
  results <- vector("list", n_items)
  
  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_items)
    batch_items <- items[start_idx:end_idx]
    
    log_info("Batch %d/%d: Processing items %d-%d", i, n_batches, start_idx, end_idx, module = "BATCH")
    
    batch_results <- lapply(batch_items, process_fn, ...)
    results[start_idx:end_idx] <- batch_results
    
    # Aggressive garbage collection
    gc(verbose = FALSE)
    log_memory(sprintf("after_batch_%d", i), module = "BATCH")
  }
  
  results
}
# TSV write helper (replaces write.csv throughout)
write_tsv <- function(x, file, row.names = FALSE, quote = FALSE, na = "", ...) {
  write.table(x, file = file, sep = "\t", row.names = row.names, 
              quote = quote, na = na, ...)
  invisible(file)
}

# TSV read helper
read_tsv <- function(file, header = TRUE, stringsAsFactors = FALSE, 
                     quote = "", na.strings = c("", "NA"), ...) {
  read.delim(file, sep = "\t", header = header, 
             stringsAsFactors = stringsAsFactors, 
             quote = quote, na.strings = na.strings, ...)
}

#### SCENARIO DETECTION HELPERS ####

#' Detect population status for records
#' @param data Data frame with population_status and status columns
#' @return Character vector: "indigenous" or "non-indigenous"
detect_population_type <- function(data) {
  # Primary: Use population_status column if available
  if ("population_status" %in% names(data)) {
    pop_status <- tolower(trimws(as.character(data$population_status)))
    
    # Map to standard values
    result <- case_when(
      pop_status %in% c("indigenous", "native") ~ "indigenous",
      pop_status %in% c("non-indigenous", "non indigenous", "alien", "introduced") ~ "non-indigenous",
      TRUE ~ NA_character_
    )
    
    # Fallback: Use status (occurrence_origin) if population_status is NA
    if (any(is.na(result)) && "status" %in% names(data)) {
      origin_status <- tolower(trimws(as.character(data$status)))
      result[is.na(result)] <- case_when(
        origin_status[is.na(result)] %in% c("native") ~ "indigenous",
        origin_status[is.na(result)] %in% c("alien", "introduced") ~ "non-indigenous",
        TRUE ~ NA_character_
      )
    }
    
    return(result)
  }
  
  # Fallback: Use status column only
  if ("status" %in% names(data)) {
    origin_status <- tolower(trimws(as.character(data$status)))
    return(case_when(
      origin_status %in% c("native") ~ "indigenous",
      origin_status %in% c("alien", "introduced") ~ "non-indigenous",
      TRUE ~ NA_character_
    ))
  }
  
  # No valid columns
  return(rep(NA_character_, nrow(data)))
}

#' Create species scenario lookup table
#' @param data Data frame with species and population_type columns
#' @return Data frame with species, scenario, counts
create_scenario_table <- function(data) {
  if (!"population_type" %in% names(data)) {
    stop("Data must have 'population_type' column. Run detect_population_type() first.")
  }
  
  scenario_summary <- data %>%
    group_by(species, population_type) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = population_type,
      values_from = n,
      values_fill = 0
    )
  
  # Ensure both columns exist
  if (!"indigenous" %in% names(scenario_summary)) {
    scenario_summary$indigenous <- 0
  }
  if (!"non-indigenous" %in% names(scenario_summary)) {
    scenario_summary$`non-indigenous` <- 0
  }
  
  scenario_summary <- scenario_summary %>%
    mutate(
      scenario = case_when(
        indigenous > 0 & `non-indigenous` == 0 ~ 1L,
        indigenous == 0 & `non-indigenous` > 0 ~ 2L,
        indigenous > 0 & `non-indigenous` > 0 ~ 3L,
        TRUE ~ NA_integer_
      ),
      total_records = indigenous + `non-indigenous`
    ) %>%
    select(species, scenario, indigenous, `non-indigenous`, total_records) %>%
    arrange(scenario, species)
  
  return(scenario_summary)
}

#' Get species list by scenario
#' @param scenario_table Output from create_scenario_table()
#' @param scenario_num Scenario number (1, 2, or 3)
#' @return Character vector of species names
get_species_by_scenario <- function(scenario_table, scenario_num) {
  scenario_table %>%
    filter(scenario == scenario_num) %>%
    pull(species)
}

# ──────────────────────────────────────────────────────────────────────────────
# EXTINCTION MASKING ORCHESTRATOR (post-Phase-1.5, pre-Phase-2)
# ──────────────────────────────────────────────────────────────────────────────

#' Apply per-species extinction masking to both indigenous and non-indigenous
#' branches.
#'
#' Walks each active species, computes its extinction events, and applies the
#' 500m geodesic mask to both branches' clean_data. Adds two columns:
#'   temporal_status:           "active" | "suppressed" | "extinct"
#'   suppressed_by_extinction:  NA or extinction record_id
#'
#' Modules downstream (3A/3C/4A/4C reports + metrics) filter on
#' temporal_status == "active" so extinct + suppressed records don't pollute
#' metrics. Existing modules without temporal_status awareness will still see
#' all records (the column simply doesn't exist), so this is forward-compatible.
#'
#' Requires apply_spatial_temporal_mask() + parse_extinction_causes() from
#' Module 11 (R/11_temporal_delta.R) to be sourced.
#'
#' @param result_indigenous       List with $clean_data and $clean_sf.
#' @param result_non_indigenous   List with $clean_data and $clean_sf.
#' @param active_species          Character vector of species names to process.
#' @return Named list with the same two objects, with `temporal_status` +
#'         `suppressed_by_extinction` columns added.
apply_extinction_masking_to_branches <- function(result_indigenous,
                                                 result_non_indigenous,
                                                 active_species) {
  
  module <- "EXTINCTION_MASKING"
  if (exists("log_info", mode = "function")) {
    log_info("=== Applying extinction masking (500m geodesic) ===", module = module)
  }
  
  # Process one branch (indigenous or non_indigenous). For each species:
  #   1. Slice the branch to that species' records
  #   2. Parse extinctions (records with is_extinct == TRUE)
  #   3. Apply 500m mask (per-species, scoped)
  #   4. Stitch tagged slice back into the branch's clean_data
  process_branch <- function(result_branch, branch_label) {
    if (is.null(result_branch) || is.null(result_branch$clean_data) ||
        nrow(result_branch$clean_data) == 0L) {
      return(result_branch)
    }
    cd <- result_branch$clean_data
    
    # Initialize columns (default everything active)
    cd$temporal_status          <- "active"
    cd$suppressed_by_extinction <- NA_character_
    
    n_extinct_total    <- 0L
    n_suppressed_total <- 0L
    
    for (sp in active_species) {
      idx <- which(cd$species == sp)
      if (length(idx) == 0L) next
      sp_slice <- cd[idx, , drop = FALSE]
      # Need is_extinct present
      if (!"is_extinct" %in% names(sp_slice)) next
      if (!any(!is.na(sp_slice$is_extinct) & sp_slice$is_extinct == TRUE)) next
      
      ext <- tryCatch(parse_extinction_causes(sp_slice),
                      error = function(e) NULL,
                      warning = function(w) parse_extinction_causes(sp_slice))
      if (is.null(ext) || nrow(ext) == 0L) next
      
      masked <- tryCatch(
        suppressWarnings(apply_spatial_temporal_mask(sp_slice, ext, log_unlinked = FALSE)),
        error = function(e) {
          if (exists("log_warn", mode = "function")) {
            log_warn("Masking failed for %s: %s", sp, conditionMessage(e), module = module)
          }
          sp_slice  # fall through with unmasked slice
        }
      )
      
      # Stitch back
      cd$temporal_status[idx]          <- masked$temporal_status
      cd$suppressed_by_extinction[idx] <- masked$suppressed_by_extinction
      
      n_extinct_total    <- n_extinct_total    + sum(masked$temporal_status == "extinct",    na.rm = TRUE)
      n_suppressed_total <- n_suppressed_total + sum(masked$temporal_status == "suppressed", na.rm = TRUE)
    }
    
    if (exists("log_info", mode = "function")) {
      log_info("Branch '%s': %d extinct + %d suppressed (of %d records, %d will count as 'active')",
               branch_label, n_extinct_total, n_suppressed_total, nrow(cd),
               nrow(cd) - n_extinct_total - n_suppressed_total, module = module)
    }
    
    # Also tag clean_sf if present
    result_branch$clean_data <- cd
    if (!is.null(result_branch$clean_sf) && nrow(result_branch$clean_sf) == nrow(cd)) {
      result_branch$clean_sf$temporal_status          <- cd$temporal_status
      result_branch$clean_sf$suppressed_by_extinction <- cd$suppressed_by_extinction
    }
    result_branch
  }
  
  result_indigenous     <- process_branch(result_indigenous,     "indigenous")
  result_non_indigenous <- process_branch(result_non_indigenous, "non_indigenous")
  
  list(
    result_indigenous     = result_indigenous,
    result_non_indigenous = result_non_indigenous
  )
}

# ---------------------------------------------------------------------------
# Dependency self-heal: canonical geographic vocabulary
# ---------------------------------------------------------------------------
# R/00_geo_canon.R supplies canon_continent(), canon_country(), geo_usable(),
# is_geo_unresolved() and the GEO_* constants. Module 1 (ingest), 2A, 2B, 2C and
# the report/narrative writers all call into it, so a run where it has not been
# sourced dies mid-pipeline with "could not find function canon_continent"
# rather than at load time.
#
# checkcover_main.R sources it explicitly, but the modules are also documented as
# individually runnable (see README) and are sourced directly by the test suite,
# so load order must not be able to break them. 00_helpers.R is sourced before
# everything else in every entry point, which makes this the one place that
# guarantees availability. Mirrors the defensive `%||%` definitions used
# elsewhere in the codebase.
if (!exists("canon_continent", mode = "function")) {
  for (.geo_canon_path in c("R/00_geo_canon.R", "00_geo_canon.R",
                            file.path("..", "R", "00_geo_canon.R"))) {
    if (file.exists(.geo_canon_path)) {
      source(.geo_canon_path)
      break
    }
  }
  rm(.geo_canon_path)
}
