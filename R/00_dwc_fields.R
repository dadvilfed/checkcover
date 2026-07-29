#### MODULE 00-DwC: DARWIN CORE FIELD ALIGNMENT (EXPOSED LAYER ONLY) ####
# Aligned with Lucian + Emmy Delekta (Auburn), 2026-06. See WoC-template.xlsx
# sheet "WoC_data_collector" for the authoritative exposed field names.
#
# GOVERNING PRINCIPLE
#   Darwin Core is applied ONLY at the EXPOSED layer — the input table header,
#   output properties, JSON keys, and the DwC-Archive export. The internal R
#   "osatura" (column names AND values used across modules) is NOT changed and
#   NOT translated. This module is the single place that translates between the
#   exposed names and the internal names.
#
#   * dwc:/dcterms: prefixes appear ONLY in the meta.xml of a DwC Archive — never
#     in live headers or in the working data frames (they break formulas/parsers,
#     and a ":" is illegal in an internal column name).
#   * establishmentMeans (WoC-native) and occurrenceOrigin (WoC-native) stay
#     SEPARATE and keep their WoC values. The DwC-vocabulary values are DERIVED
#     only at export (see the crosswalk helpers).
#   * We never export absences, so occurrenceStatus = "absent" is never produced.

# ---------------------------------------------------------------------------
# 1. INPUT ALIASES — exposed header name -> internal (legacy) column name
# ---------------------------------------------------------------------------
# Lets ingest accept the new DwC-aligned template header while the downstream
# mutate()/select() logic keeps using the original internal names unchanged.
# Legacy WoC headers keep working too (a legacy name maps to itself implicitly).
DWC_INPUT_ALIASES <- c(
  # exposed (DwC-aligned) name        = internal/legacy WoC header
  occurrenceID            = "WoCID",
  scientificName          = "Crayfish_scientific_name",
  decimalLatitude         = "Lat",
  decimalLongitude        = "Long",
  year                    = "Year_of_record",
  establishmentMeans      = "Population_status",
  occurrenceOrigin        = "Occurrence_origin",
  claimExtinction         = "Claim_extinction",
  accuracy                = "Accuracy",
  bibliographicCitation   = "DOI",
  associatedReferences    = "URL",
  sourceCitation          = "Citation",
  occurrenceRemarks       = "Comments",
  confidentialityLevel    = "Confidentiality_level",
  contributor             = "Contributor",
  associatedSequences     = "NCBI_accession_code",
  extirpationBuffer       = "Extirpation_buffer",
  # voucher (new)
  catalogNumber           = "catalogNumber",
  institutionCode         = "institutionCode",
  # WoC-native geography. These are curated in the WoC database (canonical
  # country names with correct diacritics, coastal points snapped to the nearest
  # shore) and TAKE PRIORITY over cheCkOVER's own Natural Earth / GADM lookup,
  # which now runs only to fill gaps (Lucian, 2026-07). Several spellings of the
  # subnational field are accepted (judet / state / province / county).
  country                 = "WoC_country",
  countryName             = "WoC_country",
  continent               = "WoC_continent",
  continents              = "WoC_continent",
  stateProvince           = "WoC_admin1",
  state                   = "WoC_admin1",
  province                = "WoC_admin1",
  county                  = "WoC_admin1",
  judet                   = "WoC_admin1",
  `județ`            = "WoC_admin1",
  admin_1                 = "WoC_admin1",
  admin1                  = "WoC_admin1",
  # pathogen / associated taxa
  associatedTaxa          = "Pathogen_scientific_name",
  associatedTaxaSequences = "Pathogen_NCBI_accession_code",
  pathogenScientificName  = "Pathogen_scientific_name",
  pathogenAssociatedSequences = "Pathogen_NCBI_accession_code",
  pathogenGenotypeGroup   = "Genotype_group",
  pathogenHaplotype       = "Haplotype",
  pathogenYear            = "Pathogen_year_of_record"
)

#' Normalize an input data frame's headers to internal (legacy) names.
#'
#' Case-insensitive; strips any dwc:/dcterms: prefix; leaves unrecognized and
#' already-internal columns untouched. Idempotent.
#'
#' @param df Raw input data frame (new DwC-aligned OR legacy WoC header).
#' @return df with columns renamed to the internal names the pipeline expects.
dwc_normalize_input_headers <- function(df) {
  if (is.null(df) || ncol(df) == 0L) return(df)
  nm <- names(df)
  # strip any accidental namespace prefix on exposed names
  bare <- sub("^(dwc|dcterms):", "", nm)
  key  <- tolower(bare)
  alias_key <- tolower(names(DWC_INPUT_ALIASES))
  hit <- match(key, alias_key)
  changed <- !is.na(hit)
  nm[changed] <- unname(DWC_INPUT_ALIASES[hit[changed]])
  names(df) <- nm
  df
}

# ---------------------------------------------------------------------------
# 2. OUTPUT MAP — internal column name -> exposed DwC-aligned name
# ---------------------------------------------------------------------------
# Applied to exposed occurrence outputs (per-species occurrence table, any
# geojson attribute table). Internal columns with no exposed counterpart are
# left as-is by dwc_rename_output_columns().
DWC_OUTPUT_MAP <- c(
  record_id             = "occurrenceID",
  species               = "scientificName",
  latitude              = "decimalLatitude",
  longitude             = "decimalLongitude",
  year                  = "year",
  population_status     = "establishmentMeans",   # WoC-native values kept
  occurrence_origin     = "occurrenceOrigin",      # WoC-native values kept
  accuracy              = "accuracy",
  doi                   = "bibliographicCitation",
  url                   = "associatedReferences",
  citation              = "sourceCitation",
  comments              = "occurrenceRemarks",
  confidentiality_level = "confidentialityLevel",
  contributor           = "contributor",
  catalog_number        = "catalogNumber",
  institution_code      = "institutionCode",
  ncbi_accession_code   = "associatedSequences",
  extirpation_buffer    = "extirpationBuffer",
  pathogen_scientific_name     = "pathogenScientificName",
  pathogen_ncbi_accession_code = "pathogenAssociatedSequences",
  pathogen_genotype_group      = "pathogenGenotypeGroup",
  pathogen_haplotype           = "pathogenHaplotype",
  pathogen_year                = "pathogenYear"
)

#' Rename a data frame's columns from internal names to exposed DwC names.
#' Only recognized columns are renamed; the rest pass through unchanged.
dwc_rename_output_columns <- function(df) {
  if (is.null(df) || ncol(df) == 0L) return(df)
  nm  <- names(df)
  hit <- match(nm, names(DWC_OUTPUT_MAP))
  changed <- !is.na(hit)
  nm[changed] <- unname(DWC_OUTPUT_MAP[hit[changed]])
  names(df) <- nm
  df
}

# ---------------------------------------------------------------------------
# 3. VOUCHER: basisOfRecord is DERIVED, never an input field
# ---------------------------------------------------------------------------
#' PreservedSpecimen when a catalogNumber is present, else HumanObservation.
#' Vectorized. Enables "voucher-only" filtering downstream.
dwc_derive_basis_of_record <- function(catalog_number) {
  has_voucher <- !is.na(catalog_number) & nzchar(trimws(as.character(catalog_number)))
  ifelse(has_voucher, "PreservedSpecimen", "HumanObservation")
}

# ---------------------------------------------------------------------------
# 4. EXPORT-ONLY CROSSWALK (DwC Archive / API) — never applied in the DB
# ---------------------------------------------------------------------------
# establishmentMeans (DwC controlled vocab) derived from the WoC-native value.
#   indigenous -> native ; non-indigenous -> introduced
dwc_establishment_means <- function(woc_establishment_means) {
  v <- tolower(trimws(as.character(woc_establishment_means)))
  out <- rep(NA_character_, length(v))
  out[v == "indigenous"]     <- "native"
  out[v == "non-indigenous"] <- "introduced"
  out
}

# degreeOfEstablishment (DwC controlled vocab) derived from WoC occurrenceOrigin.
#   invasive -> invasive ; introduced/established -> established ;
#   cryptogenic -> NA (unknown origin has no degree) ; native/type locality -> NA
dwc_degree_of_establishment <- function(woc_occurrence_origin) {
  v <- tolower(trimws(as.character(woc_occurrence_origin)))
  out <- rep(NA_character_, length(v))
  out[v == "invasive"]                 <- "invasive"
  out[v %in% c("introduced", "established")] <- "established"
  out
}

#' Serialize the WoC `contributor` as a DwC dynamicProperties key:value.
#' NOT recordedBy — recordedBy would be field collectors, which we do not have.
dwc_dynamic_properties <- function(contributor) {
  v <- trimws(as.character(contributor))
  ifelse(is.na(v) | !nzchar(v), NA_character_,
         sprintf('{"contributor":"%s"}', gsub('"', '\\\\"', v)))
}

# NULL-coalescing safety (module may be sourced standalone)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}
