#### MODULE 00-GEO-CANON: CANONICAL GEOGRAPHIC VOCABULARY ####
# Authoritative vocabulary for every geographic string cheCkOVER writes or
# reports — geo-narratives, JSON, TSV, logs (Lucian + WoC canon, 2026-07).
# Source of truth: WoC_canonical_geography.md + WoC_canonical_country_continent.tsv,
# both derived from the full WoC export. A new value must be added to the canon
# deliberately; it is never introduced ad hoc by the pipeline.
#
# Two hard rules:
#   * Continent has exactly SIX permitted values. "Australia" is NEVER one of
#     them (it is a country, in Oceania). Antarctica is not in use.
#   * Six countries legitimately span more than one continent, so continent must
#     NOT be derived from country for them — it has to come from coordinates.

# The six permitted continents, in WoC record-count order.
CHECKOVER_CONTINENTS <- c(
  "North America", "Europe", "Asia", "Oceania", "South America", "Africa"
)

# Explicit failure state. A field the fallback could not resolve carries this
# sentinel — never a guessed, computed-but-doubtful, or placeholder value. It is
# excluded from every count and from category assignment, exactly like NA, so a
# data gap can never change a species' biogeographic classification.
GEO_UNRESOLVED <- "unresolved"

# Maximum distance a nearest-feature "snap" may move a point, in km. A snap is
# meant to rescue a record sitting just offshore (coastal, small island, minor
# coordinate imprecision) — not to relocate it to another ocean. Five Euastacus
# suttoni records with a dropped longitude digit (15.5 instead of ~150.5) were
# silently snapped ~8,000 km onto Africa, which alone flipped the species into
# the cosmopolitan class. Beyond this threshold the record is marked unresolved
# and reported.
GEO_MAX_SNAP_KM <- 100

# ---------------------------------------------------------------------------
# Alias tables (input normalisation -> canonical form)
# ---------------------------------------------------------------------------
# Continent aliases. Keys are lowercased for case-insensitive matching.
.GEO_CONTINENT_ALIASES <- c(
  "australia"       = "Oceania",     # country, not a continent
  "australasia"     = "Oceania",
  "australia/oceania" = "Oceania",
  "oceania"         = "Oceania",
  "n. america"      = "North America",
  "n america"       = "North America",
  "northamerica"    = "North America",
  "north america"   = "North America",
  "s. america"      = "South America",
  "s america"       = "South America",
  "southamerica"    = "South America",
  "south america"   = "South America",
  "europe"          = "Europe",
  "asia"            = "Asia",
  "africa"          = "Africa"
)

# Country aliases from WoC_canonical_geography.md ("Use this" / "NOT this").
.GEO_COUNTRY_ALIASES <- c(
  "usa"                      = "United States",
  "u.s.a."                   = "United States",
  "us"                       = "United States",
  "united states of america" = "United States",
  "uk"                       = "United Kingdom",
  "great britain"            = "United Kingdom",
  "england"                  = "United Kingdom",
  "czechia"                  = "Czech Republic",
  "russian federation"       = "Russia",
  "republic of korea"        = "South Korea",
  "korea"                    = "South Korea",
  "türkiye"             = "Turkey",
  "turkiye"                  = "Turkey",
  "macedonia"                = "North Macedonia",
  "fyrom"                    = "North Macedonia",
  "swaziland"                = "Eswatini"
)

# ---------------------------------------------------------------------------
# Canon table (country -> continent, transcontinental flag)
# ---------------------------------------------------------------------------
.GEO_CANON <- NULL   # cached after first load

#' Load the canonical country->continent table.
#'
#' @param path TSV with columns country | canonical_continent | records |
#'   transcontinental_needs_coord_check.
#' @return data.frame, invisibly cached for later calls.
load_geo_canon <- function(path = "WoC_canonical_country_continent.tsv",
                           module = "GEO_CANON") {
  if (!is.null(.GEO_CANON)) return(.GEO_CANON)
  if (!file.exists(path)) {
    if (exists("log_warn", mode = "function"))
      log_warn("Canonical country/continent table not found: %s. Country->continent derivation disabled.",
               path, module = module)
    return(NULL)
  }
  tb <- tryCatch(
    read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
               quote = "", na.strings = c("", "NA"), fileEncoding = "UTF-8"),
    error = function(e) NULL)
  if (is.null(tb) || !all(c("country", "canonical_continent") %in% names(tb))) {
    if (exists("log_warn", mode = "function"))
      log_warn("Canonical table unreadable or missing columns: %s", path, module = module)
    return(NULL)
  }
  if (!"transcontinental_needs_coord_check" %in% names(tb))
    tb$transcontinental_needs_coord_check <- "False"
  tb$transcontinental <- tolower(trimws(as.character(
    tb$transcontinental_needs_coord_check))) %in% c("true", "yes", "1")
  tb$country_key <- tolower(trimws(tb$country))
  .GEO_CANON <<- tb
  if (exists("log_info", mode = "function"))
    log_info("Canonical geography loaded: %d countries (%d transcontinental).",
             nrow(tb), sum(tb$transcontinental), module = module)
  tb
}

# ---------------------------------------------------------------------------
# Predicates
# ---------------------------------------------------------------------------
#' Is this geographic value unusable for counting/classification?
#' TRUE for NA, empty string, and the explicit `unresolved` sentinel.
is_geo_unresolved <- function(x) {
  v <- trimws(as.character(x))
  is.na(v) | !nzchar(v) | tolower(v) == GEO_UNRESOLVED
}

#' Keep only values usable for counting/classification.
geo_usable <- function(x) {
  x <- as.character(x)
  x[!is_geo_unresolved(x)]
}

# ---------------------------------------------------------------------------
# Normalisation
# ---------------------------------------------------------------------------
#' Normalise a continent value to one of the six canonical strings.
#'
#' Unknown / blank / Antarctica -> GEO_UNRESOLVED (never a guess).
#' Vectorised.
canon_continent <- function(x) {
  v <- trimws(as.character(x))
  out <- rep(GEO_UNRESOLVED, length(v))
  ok <- !is.na(v) & nzchar(v)
  key <- tolower(v[ok])
  # exact canonical match first, then alias table
  hit <- match(key, tolower(CHECKOVER_CONTINENTS))
  mapped <- ifelse(!is.na(hit), CHECKOVER_CONTINENTS[hit],
                   unname(.GEO_CONTINENT_ALIASES[key]))
  mapped[is.na(mapped)] <- GEO_UNRESOLVED
  out[ok] <- mapped
  out[!(out %in% CHECKOVER_CONTINENTS)] <- GEO_UNRESOLVED
  out
}

#' Normalise a country value to its canonical WoC form.
#'
#' Applies the alias table, then verifies membership of the canon. A country not
#' in the canon is returned as-is but reported by geo_canon_violations(), so new
#' values surface instead of silently entering the outputs. Vectorised.
canon_country <- function(x) {
  v <- trimws(as.character(x))
  out <- v
  ok <- !is.na(v) & nzchar(v)
  key <- tolower(v[ok])
  alias <- unname(.GEO_COUNTRY_ALIASES[key])
  out[ok] <- ifelse(!is.na(alias), alias, v[ok])
  out[is.na(v) | !nzchar(v)] <- GEO_UNRESOLVED
  out
}

#' Continent implied by a country — ONLY where that is deterministic.
#'
#' Returns NA for the six transcontinental countries (France, United States,
#' Russia, Turkey, Spain, Papua New Guinea): their split across continents is
#' correct, not an error, so continent must be resolved from coordinates. Also
#' returns NA for countries absent from the canon. Vectorised.
continent_for_country <- function(country, canon = load_geo_canon()) {
  v <- canon_country(country)
  out <- rep(NA_character_, length(v))
  if (is.null(canon)) return(out)
  idx <- match(tolower(trimws(v)), canon$country_key)
  hit <- !is.na(idx)
  out[hit] <- canon$canonical_continent[idx[hit]]
  # never derive for transcontinental countries
  out[hit][canon$transcontinental[idx[hit]]] <- NA_character_
  out[!(out %in% CHECKOVER_CONTINENTS)] <- NA_character_
  out
}

#' Is this country transcontinental (continent must come from coordinates)?
is_transcontinental <- function(country, canon = load_geo_canon()) {
  if (is.null(canon)) return(rep(FALSE, length(country)))
  idx <- match(tolower(trimws(canon_country(country))), canon$country_key)
  out <- rep(FALSE, length(country))
  out[!is.na(idx)] <- canon$transcontinental[idx[!is.na(idx)]]
  out
}

# ---------------------------------------------------------------------------
# Canon compliance check (for the integrity report)
# ---------------------------------------------------------------------------
#' Values that are not in the canon, so they can be reported rather than shipped.
#'
#' @return list with `continents` and `countries` character vectors (offending
#'   distinct values, excluding the legitimate `unresolved` sentinel).
geo_canon_violations <- function(continents = character(0),
                                 countries  = character(0),
                                 canon = load_geo_canon()) {
  bad_cont <- unique(geo_usable(continents))
  bad_cont <- bad_cont[!(bad_cont %in% CHECKOVER_CONTINENTS)]
  bad_ctry <- character(0)
  if (!is.null(canon)) {
    cc <- unique(geo_usable(countries))
    bad_ctry <- cc[!(tolower(trimws(cc)) %in% canon$country_key)]
  }
  list(continents = sort(bad_cont), countries = sort(bad_ctry))
}

# NULL coalescing (module may be sourced standalone)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}
