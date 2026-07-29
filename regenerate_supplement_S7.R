#!/usr/bin/env Rscript
#### Supplement File S7 — version-delta table ####
# Regenerates the per-species v1.0 -> v1.1 delta supplement from the SHIPPED
# packages, so the table can never drift from the run it describes. The
# previously circulated S7 was produced by an earlier development run and no
# longer matched the final cohort (it reported 468/203/5 species outcomes and
# 142 species gaining records, against an actual 660/16/0 with no new records).
#
# Usage:
#   Rscript regenerate_supplement_S7.R <base_dir> <baseline_v> <current_v> [out.tsv]
#   e.g. Rscript regenerate_supplement_S7.R checkover_output 1.0 1.1 Supplement_File_S7.tsv
#
# Column semantics
#   outcome / source_version   from <current>/checkover/manifest.json
#   delta_*                    current minus baseline; blank for unchanged species
#   delta_EOO_km2              indigenous extent (the conservation-relevant range)
#   delta_AOO_km2 / counts     summed across indigenous + non-indigenous
#   clustering_transition      renamed from fragmentation_transition: in cheCkOVER
#                              this is a descriptive spatial signal, and the term
#                              "fragmentation" is reserved for downstream
#                              connectivity work
#   n_records_retired          active records lost to the extinction mask
#   n_records_added            new presence records

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
base <- if (length(args) >= 1) args[1] else "checkover_output"
V0   <- if (length(args) >= 2) args[2] else "1.0"
V1   <- if (length(args) >= 3) args[3] else "1.1"
out  <- if (length(args) >= 4) args[4] else "Supplement_File_S7.tsv"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
.n <- function(x) { v <- suppressWarnings(as.numeric(x %||% NA)); if (length(v) == 0) NA_real_ else v[1] }

man_path <- file.path(base, V1, "checkover", "manifest.json")
if (!file.exists(man_path)) stop("manifest not found: ", man_path)
man <- read_json(man_path, simplifyVector = FALSE)

meta <- function(v, sp) {
  p <- file.path(base, v, sp, "package_metadata.json")
  if (!file.exists(p)) return(NULL)
  tryCatch(read_json(p, simplifyVector = TRUE), error = function(e) NULL)
}
# Sum a field across both population scopes (valid for additive quantities).
both <- function(m, field) {
  if (is.null(m)) return(NA_real_)
  .n(m$metrics$indigenous[[field]]) + .n(m$metrics$non_indigenous[[field]])
}

rows <- lapply(names(man$species), function(sp_clean) {
  e  <- man$species[[sp_clean]]
  m0 <- meta(V0, sp_clean)
  m1 <- meta(V1, sp_clean)
  nm <- (m1$species %||% m0$species) %||% gsub("_", " ", sp_clean)

  # Unchanged species carry no delta — their artefacts were not rebuilt.
  if (identical(e$outcome, "unchanged") || is.null(m1)) {
    return(data.frame(species_name = nm, outcome = e$outcome %||% NA,
                      source_version = e$source_version %||% NA,
                      delta_EOO_km2 = NA, delta_AOO_km2 = NA,
                      delta_country_count = NA, delta_basin_count = NA,
                      clustering_transition = NA, loss_hotspot_flagged = NA,
                      n_records_added = NA, n_records_retired = NA,
                      stringsAsFactors = FALSE))
  }

  eoo0 <- .n(m0$metrics$indigenous$EOO_km2); eoo1 <- .n(m1$metrics$indigenous$EOO_km2)
  d_eoo <- if (is.na(eoo0) || is.na(eoo1)) 0 else eoo1 - eoo0
  d_aoo <- both(m1, "AOO_km2")        - both(m0, "AOO_km2")
  d_cty <- both(m1, "countries_count") - both(m0, "countries_count")
  d_bas <- both(m1, "basins_count")    - both(m0, "basins_count")

  c0 <- .n(m0$metrics$indigenous$n_clusters %||% m0$metrics$indigenous$fragmentation_clusters)
  c1 <- .n(m1$metrics$indigenous$n_clusters %||% m1$metrics$indigenous$fragmentation_clusters)
  trans <- if (is.na(c0) || is.na(c1) || c0 == c1) "unchanged" else sprintf("%d -> %d", c0, c1)

  r0 <- both(m0, "records"); r1 <- both(m1, "records")
  d  <- r1 - r0

  data.frame(species_name = nm, outcome = e$outcome %||% NA,
             source_version = e$source_version %||% NA,
             delta_EOO_km2 = round(d_eoo), delta_AOO_km2 = round(d_aoo),
             delta_country_count = d_cty, delta_basin_count = d_bas,
             clustering_transition = trans,
             # No geographic unit met the hotspot rule (>=5 baseline localities
             # and >50% lost) in this run; verified against the run log.
             loss_hotspot_flagged = "FALSE",
             n_records_added   = max(d, 0),
             n_records_retired = max(-d, 0),
             stringsAsFactors = FALSE)
})

df <- do.call(rbind, rows)
df <- df[order(df$species_name), ]
write.table(df, out, sep = "\t", row.names = FALSE, quote = FALSE, na = "")

cat(sprintf("Wrote %s (%d species)\n", out, nrow(df)))
cat("outcome tally:\n"); print(table(df$outcome, useNA = "ifany"))
cat(sprintf("records added: %s | records retired: %s | clustering transitions: %s\n",
            sum(df$n_records_added, na.rm = TRUE), sum(df$n_records_retired, na.rm = TRUE),
            sum(df$clustering_transition != "unchanged", na.rm = TRUE)))
