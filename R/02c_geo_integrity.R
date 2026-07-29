#### MODULE 2C: GEOGRAPHIC INTEGRITY REPORT ####
# Per-run accounting of where every geographic value came from: the WoC database
# (native, authoritative), cheCkOVER's spatial fallback (computed), or nowhere
# (`unresolved`). Runs immediately after Modules 2A/2B so gaps surface in the
# log at once instead of being discovered later in a manuscript figure
# (Lucian, 2026-07).
#
# Also reports:
#   * nearest-land snaps rejected by the distance / species-consistency guards,
#     with the offending species and distance;
#   * any value that is not in the canonical WoC vocabulary.
#
# Writes <run_dir>/geographic_integrity.{json,tsv} and logs a summary.

#' Build and persist the geographic integrity report.
#'
#' @param result     Pipeline result (list with $clean_data) post 2A/2B.
#' @param output_dir Run directory for the artifacts.
#' @return `result` unchanged (invisibly side-effecting), with the report
#'   attached as `result$geo_integrity`.
report_geographic_integrity <- function(result, output_dir = "checkover_output") {
  module <- "MODULE2C_GEO_INTEGRITY"

  with_log_section(module, {
    log_info("=== MODULE 2C: GEOGRAPHIC INTEGRITY REPORT ===", module = module)

    cd <- result$clean_data
    n  <- nrow(cd)
    if (n == 0L) {
      log_warn("No records to report on.", module = module)
      return(result)
    }

    .src <- function(field) {
      s <- cd[[paste0(field, "_source")]]
      if (is.null(s)) rep(NA_character_, n) else as.character(s)
    }
    .val <- function(field) {
      v <- cd[[field]]
      if (is.null(v)) rep(NA_character_, n) else as.character(v)
    }

    FIELDS <- list(
      continent = list(value = "continents", source = "continent"),
      country   = list(value = "country",    source = "country"),
      admin_1   = list(value = "admin_1",    source = "admin1")
    )

    # ── Cohort-level tallies ────────────────────────────────────────────────
    tally <- lapply(names(FIELDS), function(f) {
      src <- .src(FIELDS[[f]]$source)
      val <- .val(FIELDS[[f]]$value)
      unres <- is_geo_unresolved(val)
      list(field      = f,
           native     = sum(src == "WoC",      na.rm = TRUE),
           computed   = sum(src == "computed", na.rm = TRUE),
           unresolved = sum(unres),
           total      = n)
    })
    names(tally) <- names(FIELDS)

    log_info("--- Geographic provenance (%d records) ---", n, module = module)
    for (t in tally) {
      log_info("  %-10s native(WoC) %6d | fallback %5d | unresolved %5d  (%.2f%% resolved)",
               t$field, t$native, t$computed, t$unresolved,
               100 * (t$native + t$computed) / max(t$total, 1), module = module)
    }

    # ── Per-species breakdown, unresolved species first ─────────────────────
    sp <- as.character(cd$species)
    per_species <- do.call(rbind, lapply(split(seq_len(n), sp), function(ix) {
      row <- data.frame(species = sp[ix][1], records = length(ix),
                        stringsAsFactors = FALSE)
      for (f in names(FIELDS)) {
        src <- .src(FIELDS[[f]]$source)[ix]
        val <- .val(FIELDS[[f]]$value)[ix]
        row[[paste0(f, "_native")]]     <- sum(src == "WoC",      na.rm = TRUE)
        row[[paste0(f, "_fallback")]]   <- sum(src == "computed", na.rm = TRUE)
        row[[paste0(f, "_unresolved")]] <- sum(is_geo_unresolved(val))
      }
      row
    }))
    per_species$unresolved_total <-
      per_species$continent_unresolved + per_species$country_unresolved +
      per_species$admin_1_unresolved
    per_species <- per_species[order(-per_species$unresolved_total,
                                     per_species$species), , drop = FALSE]

    affected <- per_species[per_species$unresolved_total > 0, , drop = FALSE]
    if (nrow(affected) > 0) {
      log_warn("%d species have at least one unresolved geographic field:",
               nrow(affected), module = module)
      for (i in seq_len(min(nrow(affected), 25L))) {
        a <- affected[i, ]
        log_warn("    %-34s records=%-5d unresolved: continent=%d country=%d admin_1=%d",
                 a$species, a$records, a$continent_unresolved,
                 a$country_unresolved, a$admin_1_unresolved, module = module)
      }
      if (nrow(affected) > 25L)
        log_warn("    ... and %d more (see geographic_integrity.tsv)",
                 nrow(affected) - 25L, module = module)
    } else {
      log_info("No unresolved geographic fields in this run.", module = module)
    }

    # ── Rejected nearest-land snaps ─────────────────────────────────────────
    rejects <- NULL
    if ("continent_reject" %in% names(cd)) {
      rj <- which(!is.na(cd$continent_reject))
      if (length(rj) > 0) {
        rejects <- data.frame(
          species  = sp[rj],
          longitude = if ("longitude" %in% names(cd)) cd$longitude[rj] else NA,
          latitude  = if ("latitude"  %in% names(cd)) cd$latitude[rj]  else NA,
          snap_km  = if ("continent_snap_km" %in% names(cd)) round(cd$continent_snap_km[rj], 1) else NA,
          reason   = cd$continent_reject[rj],
          stringsAsFactors = FALSE)
        rejects <- rejects[order(-rejects$snap_km), , drop = FALSE]
        log_warn("%d record(s) had a nearest-land snap REJECTED (kept as '%s'):",
                 nrow(rejects), GEO_UNRESOLVED, module = module)
        for (i in seq_len(min(nrow(rejects), 20L))) {
          r <- rejects[i, ]
          log_warn("    %-34s (%.4f, %.4f) snap=%s km  reason=%s",
                   r$species, as.numeric(r$longitude), as.numeric(r$latitude),
                   ifelse(is.na(r$snap_km), "n/a", format(r$snap_km)), r$reason,
                   module = module)
        }
        log_warn("  These usually indicate a bad source coordinate (e.g. a dropped digit),",
                 module = module)
        log_warn("  not a pipeline error. They are excluded from all counts and classification.",
                 module = module)
      }
    }

    # ── Canonical vocabulary compliance ─────────────────────────────────────
    viol <- geo_canon_violations(.val("continents"), .val("country"))
    if (length(viol$continents) > 0)
      log_warn("NON-CANONICAL continent value(s) present: %s",
               paste(viol$continents, collapse = ", "), module = module)
    if (length(viol$countries) > 0)
      log_warn("Country value(s) absent from the WoC canon: %s",
               paste(head(viol$countries, 15), collapse = ", "), module = module)
    if (length(viol$continents) == 0 && length(viol$countries) == 0)
      log_info("All geographic values conform to the canonical WoC vocabulary.",
               module = module)

    # ── Persist ─────────────────────────────────────────────────────────────
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    tsv_path  <- file.path(output_dir, "geographic_integrity.tsv")
    json_path <- file.path(output_dir, "geographic_integrity.json")
    write_tsv(per_species, tsv_path)

    payload <- list(
      generated        = as.character(Sys.time()),
      total_records    = n,
      totals           = tally,
      species_affected = nrow(affected),
      per_species_unresolved = if (nrow(affected) > 0) affected else NULL,
      rejected_snaps   = rejects,
      snap_limit_km    = GEO_MAX_SNAP_KM,
      canon_violations = viol
    )
    jsonlite::write_json(payload, json_path, pretty = TRUE, auto_unbox = TRUE, na = "null")

    log_info("Saved integrity report: %s (+ .tsv)", json_path, module = module)

    result$geo_integrity <- payload
    result$files_created <- unique(c(result$files_created, tsv_path, json_path))
    result
  })
}
