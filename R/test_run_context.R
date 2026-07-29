#### TEST SUITE for 00_run_context.R ####
#
# Self-contained. Uses tempdir() for filesystem fixtures; no pipeline
# dependencies. Run from project root:
#
#   Rscript test_run_context.R
#
# Or from R:
#   source("test_run_context.R")
#
# Expected: "RESULTS: N passed, 0 failed" at the bottom.
# ──────────────────────────────────────────────────────────────────────────────

source("R/00_run_context.R")

# Tiny test harness (mirrors test_temporal_delta.R style)
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

cat("\n========== 00_run_context.R test suite ==========\n\n")


# ──────────────────────────────────────────────────────────────────────────────
# 1. list_prior_versions()
# ──────────────────────────────────────────────────────────────────────────────
cat("[1] list_prior_versions\n")

td <- file.path(tempdir(), "test_rc_lpv")
unlink(td, recursive = TRUE); dir.create(td, recursive = TRUE)

# Empty dir
check("empty root -> character(0)",
      identical(list_prior_versions(td), character(0)))

# Non-existent dir
check("nonexistent root -> character(0)",
      identical(list_prior_versions(file.path(td, "nope")), character(0)))

# Mixed: some version dirs, some non-version
dir.create(file.path(td, "1.0"))
dir.create(file.path(td, "1.1"))
dir.create(file.path(td, "1.10"))
dir.create(file.path(td, "1.2"))
dir.create(file.path(td, "checkover"))    # not a version
dir.create(file.path(td, "logs"))         # not a version
dir.create(file.path(td, "v1.5"))         # not a version (has "v" prefix)

versions <- list_prior_versions(td)
check("excludes non-version dirs",
      all(versions %in% c("1.0", "1.1", "1.2", "1.10")),
      detail = paste(versions, collapse = ", "))
check("numeric-descending sort: 1.10 > 1.2",
      identical(versions, c("1.10", "1.2", "1.1", "1.0")),
      detail = paste(versions, collapse = ", "))

# Exclude current
versions_excl <- list_prior_versions(td, current_version = "1.1")
check("current_version is excluded",
      identical(versions_excl, c("1.10", "1.2", "1.0")))


# ──────────────────────────────────────────────────────────────────────────────
# 2. RunContext_init() + validate_RunContext()
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[2] RunContext_init + validate_RunContext\n")

td <- file.path(tempdir(), "test_rc_init")
unlink(td, recursive = TRUE); dir.create(td, recursive = TRUE)
dir.create(file.path(td, "1.0"))   # prior version present

cfg <- list(framework_version = "1.1", root_output_dir = td)
ctx <- RunContext_init(cfg, run_id = "test_run_1")

check("ctx has class 'RunContext'", inherits(ctx, "RunContext"))
check("framework_version stored", identical(ctx$framework_version, "1.1"))
check("run_id stored", identical(ctx$run_id, "test_run_1"))
check("current_version_dir = <root>/1.1",
      identical(ctx$current_version_dir, file.path(td, "1.1")))
check("current_scaffolding_dir = <root>/1.1/checkover",
      identical(ctx$current_scaffolding_dir, file.path(td, "1.1", "checkover")))
check("prior_versions discovers 1.0",
      identical(ctx$prior_versions, "1.0"))
check("all_species starts NULL", is.null(ctx$all_species))
check("active_species starts NULL", is.null(ctx$active_species))

# Invalid framework_version
err1 <- tryCatch(
  RunContext_init(list(framework_version = "v1.0", root_output_dir = td), "x"),
  error = function(e) conditionMessage(e)
)
check("rejects 'v1.0' (has prefix)",
      is.character(err1) && grepl("does not match", err1))

err2 <- tryCatch(
  RunContext_init(list(framework_version = "draft", root_output_dir = td), "x"),
  error = function(e) conditionMessage(e)
)
check("rejects 'draft' (non-numeric)",
      is.character(err2) && grepl("does not match", err2))

# Phase validation
check("validate at 'init' passes",
      isTRUE(validate_RunContext(ctx, "init")))

err3 <- tryCatch(validate_RunContext(ctx, "post_ingest"),
                 error = function(e) conditionMessage(e))
check("validate at 'post_ingest' fails when all_species NULL",
      is.character(err3) && grepl("all_species", err3))

ctx$all_species <- c("Astacus astacus", "Procambarus clarkii")
check("validate at 'post_ingest' passes once all_species set",
      isTRUE(validate_RunContext(ctx, "post_ingest")))

err4 <- tryCatch(validate_RunContext(ctx, "post_change_detection"),
                 error = function(e) conditionMessage(e))
check("validate at 'post_change_detection' fails without active_species",
      is.character(err4) && grepl("active_species", err4))

ctx$active_species   <- "Procambarus clarkii"
ctx$species_outcomes <- list(
  "Astacus astacus"     = list(outcome = "unchanged",   source_version = "1.0"),
  "Procambarus clarkii" = list(outcome = "reprocessed", source_version = "1.1")
)
check("validate at 'post_change_detection' passes once fully populated",
      isTRUE(validate_RunContext(ctx, "post_change_detection")))

# Non-RunContext rejection
err5 <- tryCatch(validate_RunContext(list()),
                 error = function(e) conditionMessage(e))
check("rejects non-RunContext object",
      is.character(err5) && grepl("not a RunContext", err5))


# ──────────────────────────────────────────────────────────────────────────────
# 3. Path construction helpers
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[3] species_dir_in + current_species_dir\n")

check("species_dir_in builds <root>/<v>/<sp>",
      identical(species_dir_in(ctx, "1.0", "Astacus_astacus"),
                file.path(td, "1.0", "Astacus_astacus")))

check("species_dir_in with artifact_subpath",
      identical(species_dir_in(ctx, "1.0", "Astacus_astacus", "maps/x.geojson"),
                file.path(td, "1.0", "Astacus_astacus", "maps/x.geojson")))

check("current_species_dir uses ctx$framework_version",
      identical(current_species_dir(ctx, "Procambarus_clarkii"),
                file.path(td, "1.1", "Procambarus_clarkii")))

# Subgenus species: helper should accept a name that already came through
# make_package_id() — we just pass it through.
check("subgenus species name preserved",
      identical(current_species_dir(ctx, "Cambarellus_Cambarellus_chapalanus"),
                file.path(td, "1.1", "Cambarellus_Cambarellus_chapalanus")))


# ──────────────────────────────────────────────────────────────────────────────
# 4. compute_species_fingerprint()
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[4] compute_species_fingerprint\n")

mk_data <- function(record_ids, is_extinct = FALSE, year = 2020, lon = 25.5, lat = 45.5) {
  data.frame(
    record_id         = as.character(record_ids),
    longitude         = rep(lon, length(record_ids)),
    latitude          = rep(lat, length(record_ids)),
    year              = rep(year, length(record_ids)),
    is_extinct        = if (length(is_extinct) == 1L) rep(is_extinct, length(record_ids)) else is_extinct,
    is_type_locality  = rep(FALSE, length(record_ids)),
    population_status = rep("indigenous", length(record_ids)),
    status.x          = rep("native", length(record_ids)),
    accuracy          = rep(100, length(record_ids)),
    doi               = rep("10.1234/x", length(record_ids)),
    url               = rep("https://example.org", length(record_ids)),
    citation          = rep("Author 2020", length(record_ids)),
    contributor       = rep("smith", length(record_ids)),
    confidentiality_level = rep("public", length(record_ids)),
    is_sensitive      = rep(FALSE, length(record_ids)),
    stringsAsFactors  = FALSE
  )
}

d1 <- mk_data(c("r1", "r2", "r3"))
fp1 <- compute_species_fingerprint(d1)

check("fingerprint has sha256: prefix and 64-hex body",
      grepl("^sha256:[0-9a-f]{64}$", fp1))

# Deterministic: same input → same hash
fp1_again <- compute_species_fingerprint(d1)
check("same data → same fingerprint",
      identical(fp1, fp1_again))

# Row-order independence
d1_shuffled <- d1[c(3, 1, 2), ]
fp_shuffled <- compute_species_fingerprint(d1_shuffled)
check("row order doesn't affect fingerprint",
      identical(fp1, fp_shuffled))

# Tiny change → different hash
d2 <- d1
d2$is_extinct[1] <- TRUE
fp2 <- compute_species_fingerprint(d2)
check("flipping one is_extinct flag changes fingerprint",
      !identical(fp1, fp2))

# Add a record → different hash
d3 <- mk_data(c("r1", "r2", "r3", "r4"))
fp3 <- compute_species_fingerprint(d3)
check("adding a record changes fingerprint",
      !identical(fp1, fp3))

# Remove a record → different hash
d4 <- mk_data(c("r1", "r2"))
fp4 <- compute_species_fingerprint(d4)
check("removing a record changes fingerprint",
      !identical(fp1, fp4))

# Empty / NULL: known constant
fp_empty_null <- compute_species_fingerprint(NULL)
fp_empty_df   <- compute_species_fingerprint(d1[0, ])
check("NULL and zero-row df produce the same empty-set fingerprint",
      identical(fp_empty_null, fp_empty_df))
check("empty-set fingerprint matches sha256('')",
      identical(fp_empty_null,
                paste0("sha256:", digest::digest("", algo = "sha256", serialize = FALSE))))

# NA handling: NA is canonical "NA", different from empty string
d_na <- d1
d_na$citation[1] <- NA
fp_na <- compute_species_fingerprint(d_na)
d_empty_str <- d1
d_empty_str$citation[1] <- ""
fp_empty_str <- compute_species_fingerprint(d_empty_str)
check("NA value vs empty string produce different fingerprints",
      !identical(fp_na, fp_empty_str))

# Coordinate precision: tiny float differences below 6 decimals collapse to same hash
d_eps <- d1
d_eps$longitude[1] <- 25.5 + 1e-10
fp_eps <- compute_species_fingerprint(d_eps)
check("sub-6-decimal coordinate noise is absorbed",
      identical(fp1, fp_eps))

# Whereas a real change at 6th decimal does NOT collapse
d_diff <- d1
d_diff$longitude[1] <- 25.500001
fp_diff <- compute_species_fingerprint(d_diff)
check("6th-decimal coordinate change IS detected",
      !identical(fp1, fp_diff))

# Missing columns: function works with subset of expected columns
d_subset <- d1[, c("record_id", "longitude", "latitude", "year")]
fp_subset <- compute_species_fingerprint(d_subset)
check("works with subset of columns (graceful)",
      grepl("^sha256:[0-9a-f]{64}$", fp_subset))
check("subset has different fingerprint than full data",
      !identical(fp1, fp_subset))

# No matching columns: errors
err_nocols <- tryCatch(
  compute_species_fingerprint(data.frame(x = 1, y = 2)),
  error = function(e) conditionMessage(e)
)
check("errors when no fingerprint columns are present",
      is.character(err_nocols) && grepl("None of the fingerprint columns", err_nocols))


# ──────────────────────────────────────────────────────────────────────────────
# 5. resolve_species_path() + read_version_manifest()
# ──────────────────────────────────────────────────────────────────────────────
cat("\n[5] resolve_species_path + read_version_manifest\n")

td <- file.path(tempdir(), "test_rc_resolve")
unlink(td, recursive = TRUE); dir.create(td, recursive = TRUE)

# Set up: v1.0 has the data, v1.1 has a sparse manifest pointing back to 1.0
dir.create(file.path(td, "1.0", "Astacus_astacus", "maps"), recursive = TRUE)
dir.create(file.path(td, "1.1", "checkover"), recursive = TRUE)
dir.create(file.path(td, "1.1", "Cherax_destructor"), recursive = TRUE)

manifest_v1_1 <- list(
  framework         = "cheCkOVER",
  framework_version = "1.1",
  species = list(
    Astacus_astacus = list(
      outcome        = "unchanged",
      source_version = "1.0",
      fingerprint    = "sha256:abc"
    ),
    Cherax_destructor = list(
      outcome        = "reprocessed",
      source_version = "1.1",
      fingerprint    = "sha256:def"
    )
  )
)
jsonlite::write_json(manifest_v1_1,
                     file.path(td, "1.1", "checkover", "manifest.json"),
                     pretty = TRUE, auto_unbox = TRUE)

cfg2 <- list(framework_version = "1.1", root_output_dir = td)
ctx2 <- RunContext_init(cfg2, run_id = "test_run_2")

# Unchanged species resolves to v1.0 (where artifacts live)
p_unchanged <- resolve_species_path(ctx2, "Astacus_astacus")
check("unchanged species resolves to source_version (1.0), not current (1.1)",
      identical(p_unchanged, file.path(td, "1.0", "Astacus_astacus")))

# Reprocessed species resolves to v1.1 (current)
p_reproc <- resolve_species_path(ctx2, "Cherax_destructor")
check("reprocessed species resolves to current version",
      identical(p_reproc, file.path(td, "1.1", "Cherax_destructor")))

# With artifact_subpath
p_with_subpath <- resolve_species_path(ctx2, "Astacus_astacus",
                                       artifact_subpath = "maps/foo.geojson")
check("artifact_subpath is appended",
      identical(p_with_subpath,
                file.path(td, "1.0", "Astacus_astacus", "maps/foo.geojson")))

# Species not in manifest: error
err_resolve <- tryCatch(
  resolve_species_path(ctx2, "Nonexistent_species"),
  error = function(e) conditionMessage(e)
)
check("missing species errors with informative message",
      is.character(err_resolve) && grepl("not found in manifest", err_resolve))

# Missing manifest: graceful fallback to current_version path
td3 <- file.path(tempdir(), "test_rc_no_manifest")
unlink(td3, recursive = TRUE); dir.create(file.path(td3, "1.1"), recursive = TRUE)
ctx3 <- RunContext_init(list(framework_version = "1.1", root_output_dir = td3),
                        run_id = "test_run_3")
p_fallback <- resolve_species_path(ctx3, "Astacus_astacus")
check("missing manifest falls back to current version path",
      identical(p_fallback, file.path(td3, "1.1", "Astacus_astacus")))

# read_version_manifest: present + absent
m_v1_1 <- read_version_manifest(ctx2, "1.1")
check("read_version_manifest returns parsed list when present",
      is.list(m_v1_1) && identical(m_v1_1$framework_version, "1.1"))

m_missing <- read_version_manifest(ctx2, "9.9")
check("read_version_manifest returns NULL when absent",
      is.null(m_missing))


# ──────────────────────────────────────────────────────────────────────────────
# Final summary
# ──────────────────────────────────────────────────────────────────────────────
cat("\n========== RESULTS ==========\n")
cat(sprintf("%d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0L) stop("Test suite has failures")
invisible(NULL)
