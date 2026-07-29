#### TEST SUITE for 01e_change_detection.R ####
#
# Self-contained. Uses tempdir() for filesystem fixtures. Tests all four
# outcomes paths (new on first-run, new on incremental, unchanged, reprocessed)
# plus side effects (fingerprint files, manifest reading).
#
# Run from project root:
#   Rscript test_change_detection.R
#
# Or from R:
#   source("test_change_detection.R")
# ──────────────────────────────────────────────────────────────────────────────

source("R/00_helpers.R")          # needs make_package_id()
source("R/00_run_context.R")
source("R/01e_change_detection.R")

# Test harness (same style as test_run_context.R)
PASS <- 0L
FAIL <- 0L
check <- function(label, condition, detail = NULL) {
  if (isTRUE(condition)) {
    PASS <<- PASS + 1L
    cat(sprintf("  [PASS] %s\n", label))
  } else {
    FAIL <<- FAIL + 1L
    cat(sprintf("  [FAIL] %s%s\n", label,
                if (!is.null(detail)) sprintf(" -- %s", detail) else ""))
  }
}

cat("\n========== 01e_change_detection.R test suite ==========\n\n")


# ──────────────────────────────────────────────────────────────────────────────
# Helpers for building synthetic clean_occurrences data
# ──────────────────────────────────────────────────────────────────────────────
mk_records <- function(species, n, start_id = 1, is_extinct_idx = integer(0)) {
  ids <- sprintf("r%s_%03d", gsub(" ", "", species), seq(start_id, start_id + n - 1L))
  is_ext <- rep(FALSE, n); is_ext[is_extinct_idx] <- TRUE
  data.frame(
    record_id              = ids,
    species                = rep(species, n),
    longitude              = rep(25.5, n),
    latitude               = rep(45.5, n),
    year                   = seq(2000, 2000 + n - 1),
    is_extinct             = is_ext,
    is_type_locality       = rep(FALSE, n),
    population_status      = rep("indigenous", n),
    status.x               = rep("native", n),
    accuracy               = rep(100, n),
    doi                    = rep("10.1234/x", n),
    url                    = rep("https://example.org", n),
    citation               = rep("Author 2020", n),
    contributor            = rep("smith", n),
    confidentiality_level  = rep("public", n),
    is_sensitive           = rep(FALSE, n),
    stringsAsFactors       = FALSE
  )
}

write_clean_tsv <- function(ctx, df) {
  if (!dir.exists(ctx$current_scaffolding_dir)) {
    dir.create(ctx$current_scaffolding_dir, recursive = TRUE, showWarnings = FALSE)
  }
  write.table(df,
              file.path(ctx$current_scaffolding_dir, "clean_occurrences.tsv"),
              sep = "\t", quote = FALSE, row.names = FALSE, na = "NA")
}

# Write a manifest into <root>/<v>/checkover/manifest.json with the species
# entries provided as a named list. Used to set up prior-version state for
# tests that need to walk a manifest chain.
write_manifest <- function(td, version, prior_version = NULL,
                           species_entries = list()) {
  d <- file.path(td, version, "checkover")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  m <- list(
    framework         = "cheCkOVER",
    framework_version = version,
    prior_version     = prior_version,
    species           = species_entries
  )
  jsonlite::write_json(m, file.path(d, "manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null")
}


# ──────────────────────────────────────────────────────────────────────────────
# 1. First run: every species is "new"
# ──────────────────────────────────────────────────────────────────────────────
cat("[1] First run short-circuit (no prior versions)\n")

td <- file.path(tempdir(), "test_cd_first_run")
unlink(td, recursive = TRUE); dir.create(td, recursive = TRUE)

cfg <- list(framework_version = "1.0", root_output_dir = td)
ctx <- RunContext_init(cfg, run_id = "first_run_test")

# Populate ingest output: two species, both fresh
df_v1_0 <- rbind(
  mk_records("Astacus astacus",                       5),
  mk_records("Cambarellus (Cambarellus) chapalanus",  3)
)
write_clean_tsv(ctx, df_v1_0)

ctx$all_species <- c("Astacus astacus", "Cambarellus (Cambarellus) chapalanus")
ctx <- detect_species_changes(ctx)  # reads from disk

check("no prior versions detected", length(ctx$prior_versions) == 0L)
check("active_species contains all (both new)",
      setequal(ctx$active_species,
               c("Astacus astacus", "Cambarellus (Cambarellus) chapalanus")))
check("species_outcomes populated for both",
      length(ctx$species_outcomes) == 2L)
check("Astacus outcome == 'new'",
      ctx$species_outcomes[["Astacus astacus"]]$outcome == "new")
check("Astacus source_version == current (1.0)",
      ctx$species_outcomes[["Astacus astacus"]]$source_version == "1.0")
check("Astacus prior_source_version is NULL",
      is.null(ctx$species_outcomes[["Astacus astacus"]]$prior_source_version))
check("Astacus fingerprint has sha256: prefix",
      grepl("^sha256:[0-9a-f]{64}$",
            ctx$species_outcomes[["Astacus astacus"]]$fingerprint))

# Subgenus species: ensures package_id mapping is consistent
ca_clean <- ctx$species_outcomes[["Cambarellus (Cambarellus) chapalanus"]]$species_clean
check("subgenus species_clean = Cambarellus_Cambarellus_chapalanus",
      ca_clean == "Cambarellus_Cambarellus_chapalanus")

# Per-species fingerprint files on disk
check("fingerprints dir created",
      dir.exists(file.path(td, "1.0", "checkover", "fingerprints")))
check("Astacus fingerprint file exists",
      file.exists(file.path(td, "1.0", "checkover", "fingerprints",
                            "Astacus_astacus.json")))
fp_astacus <- jsonlite::read_json(
  file.path(td, "1.0", "checkover", "fingerprints", "Astacus_astacus.json"),
  simplifyVector = TRUE)
check("Astacus fingerprint file's content matches ctx",
      identical(fp_astacus$fingerprint,
                ctx$species_outcomes[["Astacus astacus"]]$fingerprint))
check("Astacus fingerprint file has outcome=new",
      fp_astacus$outcome == "new")


# ──────────────────────────────────────────────────────────────────────────────
# 2. Incremental run: unchanged path
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[2] Incremental run — unchanged species\n")

# Re-use td, but now we want to simulate v1.1 where Astacus is identical.
# Set up the v1.0 manifest first (only species entries — that's what walk uses).
fp_astacus_v1_0 <- ctx$species_outcomes[["Astacus astacus"]]$fingerprint
fp_cambar_v1_0  <- ctx$species_outcomes[["Cambarellus (Cambarellus) chapalanus"]]$fingerprint

write_manifest(td, "1.0", prior_version = NULL, species_entries = list(
  Astacus_astacus = list(
    outcome              = "new",
    source_version       = "1.0",
    prior_source_version = NULL,
    fingerprint          = fp_astacus_v1_0,
    fingerprint_at_source = fp_astacus_v1_0
  ),
  Cambarellus_Cambarellus_chapalanus = list(
    outcome              = "new",
    source_version       = "1.0",
    prior_source_version = NULL,
    fingerprint          = fp_cambar_v1_0,
    fingerprint_at_source = fp_cambar_v1_0
  )
))

# Now spin up v1.1 with identical data
cfg2 <- list(framework_version = "1.1", root_output_dir = td)
ctx2 <- RunContext_init(cfg2, run_id = "v1_1_test")
check("v1.1 sees v1.0 as prior", identical(ctx2$prior_versions, "1.0"))

write_clean_tsv(ctx2, df_v1_0)  # IDENTICAL data
ctx2$all_species <- c("Astacus astacus", "Cambarellus (Cambarellus) chapalanus")
ctx2 <- detect_species_changes(ctx2)

check("Astacus outcome == 'unchanged'",
      ctx2$species_outcomes[["Astacus astacus"]]$outcome == "unchanged")
check("Astacus source_version still points to v1.0",
      ctx2$species_outcomes[["Astacus astacus"]]$source_version == "1.0")
check("Astacus prior_source_version is also '1.0' (unchanged)",
      identical(ctx2$species_outcomes[["Astacus astacus"]]$prior_source_version, "1.0"))
check("active_species is EMPTY (both unchanged)",
      length(ctx2$active_species) == 0L)


# ──────────────────────────────────────────────────────────────────────────────
# 3. Incremental run: reprocessed (data changed)
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[3] Incremental run — reprocessed species\n")

# Re-use td. Now v1.1 has Astacus with one extinction flag flipped.
df_v1_1 <- df_v1_0
df_v1_1$is_extinct[df_v1_1$species == "Astacus astacus"][1] <- TRUE
write_clean_tsv(ctx2, df_v1_1)

ctx2 <- RunContext_init(cfg2, run_id = "v1_1_test_b")  # fresh ctx for clean state
ctx2$all_species <- c("Astacus astacus", "Cambarellus (Cambarellus) chapalanus")
ctx2 <- detect_species_changes(ctx2)

check("Astacus outcome == 'reprocessed'",
      ctx2$species_outcomes[["Astacus astacus"]]$outcome == "reprocessed")
check("Astacus source_version == current (1.1)",
      ctx2$species_outcomes[["Astacus astacus"]]$source_version == "1.1")
check("Astacus prior_source_version still points to v1.0",
      ctx2$species_outcomes[["Astacus astacus"]]$prior_source_version == "1.0")
check("Cambarellus stays 'unchanged' (data wasn't touched)",
      ctx2$species_outcomes[["Cambarellus (Cambarellus) chapalanus"]]$outcome == "unchanged")
check("active_species = {Astacus} only",
      identical(ctx2$active_species, "Astacus astacus"))


# ──────────────────────────────────────────────────────────────────────────────
# 4. Multi-version walk-back: unchanged-then-reprocessed chain
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[4] Multi-version walk-back (1.0 -> 1.1 unchanged -> 1.4 reprocessed)\n")

# Reset
td4 <- file.path(tempdir(), "test_cd_walkback")
unlink(td4, recursive = TRUE); dir.create(td4, recursive = TRUE)

# Hand-craft synthetic fingerprints (we know these strings are different)
fp_at_1_0 <- "sha256:0000000000000000000000000000000000000000000000000000000000000aaa"
fp_at_1_4 <- "sha256:0000000000000000000000000000000000000000000000000000000000000bbb"

# v1.0: Astacus is new
write_manifest(td4, "1.0", prior_version = NULL, species_entries = list(
  Astacus_astacus = list(
    outcome               = "new",
    source_version        = "1.0",
    fingerprint           = fp_at_1_0,
    fingerprint_at_source = fp_at_1_0
  )
))

# v1.1: Astacus unchanged, source = 1.0
write_manifest(td4, "1.1", prior_version = "1.0", species_entries = list(
  Astacus_astacus = list(
    outcome               = "unchanged",
    source_version        = "1.0",
    prior_source_version  = "1.0",
    fingerprint           = fp_at_1_0,
    fingerprint_at_source = fp_at_1_0
  )
))

# v1.2: Astacus unchanged
write_manifest(td4, "1.2", prior_version = "1.1", species_entries = list(
  Astacus_astacus = list(
    outcome               = "unchanged",
    source_version        = "1.0",
    prior_source_version  = "1.0",
    fingerprint           = fp_at_1_0,
    fingerprint_at_source = fp_at_1_0
  )
))

# v1.3: NO entry for Astacus (species was not in cohort at v1.3)
write_manifest(td4, "1.3", prior_version = "1.2", species_entries = list(
  Some_other_species = list(
    outcome               = "new",
    source_version        = "1.3",
    fingerprint           = "sha256:dead",
    fingerprint_at_source = "sha256:dead"
  )
))

# v1.4: NOW we're running it. Astacus is back with changed data.
cfg4 <- list(framework_version = "1.4", root_output_dir = td4)
ctx4 <- RunContext_init(cfg4, run_id = "walkback_test")

check("prior_versions in correct numeric-desc order",
      identical(ctx4$prior_versions, c("1.3", "1.2", "1.1", "1.0")))

# Provide v1.4 data for Astacus that hashes to something different
df_v1_4 <- mk_records("Astacus astacus", 6, is_extinct_idx = 1)  # different from v1.0 (5 records, no extinctions)
write_clean_tsv(ctx4, df_v1_4)
ctx4$all_species <- "Astacus astacus"
ctx4 <- detect_species_changes(ctx4)

out <- ctx4$species_outcomes[["Astacus astacus"]]
check("walk skips v1.3 (no entry) and v1.2/v1.1 (unchanged pointing back)",
      # All four prior versions had to be consulted, but the *source* found is 1.0
      out$prior_source_version == "1.0")
check("outcome == 'reprocessed' (fp differs from prior source fp)",
      out$outcome == "reprocessed")
check("source_version == current (1.4)",
      out$source_version == "1.4")


# ──────────────────────────────────────────────────────────────────────────────
# 5. Walk-back where species was missing entirely from priors -> 'new'
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[5] Species absent from all priors -> 'new'\n")

# v1.4 has a brand-new species that none of v1.0-v1.3 mentioned
df_v1_4_with_new <- rbind(df_v1_4, mk_records("Brand new species", 4))
write_clean_tsv(ctx4, df_v1_4_with_new)
ctx4$all_species <- c("Astacus astacus", "Brand new species")
ctx4 <- detect_species_changes(ctx4)

bn_out <- ctx4$species_outcomes[["Brand new species"]]
check("brand-new species outcome == 'new'",
      bn_out$outcome == "new")
check("brand-new species source_version == current",
      bn_out$source_version == "1.4")
check("brand-new species prior_source_version is NULL",
      is.null(bn_out$prior_source_version))


# ──────────────────────────────────────────────────────────────────────────────
# 6. Error path: missing clean_occurrences.tsv
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[6] Error handling\n")

td6 <- file.path(tempdir(), "test_cd_missing")
unlink(td6, recursive = TRUE); dir.create(td6, recursive = TRUE)
cfg6 <- list(framework_version = "1.0", root_output_dir = td6)
ctx6 <- RunContext_init(cfg6, run_id = "missing_test")
ctx6$all_species <- "Astacus astacus"

err <- tryCatch(detect_species_changes(ctx6),
                error = function(e) conditionMessage(e))
check("missing clean_occurrences.tsv produces informative error",
      is.character(err) && grepl("clean_occurrences.tsv not found", err))

# pre-ingest call should fail validate_RunContext
ctx_preingest <- RunContext_init(cfg6, run_id = "pre_ingest_test")
err2 <- tryCatch(detect_species_changes(ctx_preingest),
                 error = function(e) conditionMessage(e))
check("rejects RunContext that hasn't been through ingest",
      is.character(err2) && grepl("all_species", err2))


# ──────────────────────────────────────────────────────────────────────────────
# 7. In-memory clean_data argument (test convenience)
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[7] In-memory clean_data argument\n")

td7 <- file.path(tempdir(), "test_cd_inmem")
unlink(td7, recursive = TRUE); dir.create(td7, recursive = TRUE)
cfg7 <- list(framework_version = "1.0", root_output_dir = td7)
ctx7 <- RunContext_init(cfg7, run_id = "inmem_test")
ctx7$all_species <- "Astacus astacus"

df_inmem <- mk_records("Astacus astacus", 3)
ctx7 <- detect_species_changes(ctx7, clean_data = df_inmem)  # bypass disk read

check("in-memory path works",
      identical(ctx7$active_species, "Astacus astacus"))
check("in-memory path still writes fingerprint to disk",
      file.exists(file.path(td7, "1.0", "checkover", "fingerprints",
                            "Astacus_astacus.json")))


# ──────────────────────────────────────────────────────────────────────────────
# Final summary
# ──────────────────────────────────────────────────────────────────────────────
cat("\n========== RESULTS ==========\n")
cat(sprintf("%d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0L) stop("Test suite has failures")
invisible(NULL)
