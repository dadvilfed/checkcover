#### TEST SUITE: 11_temporal_delta.R ####
# Exercises every public function with synthetic data; no external deps beyond
# the same packages module 11 uses. Run with: Rscript test_temporal_delta.R
#
# This is intentionally a flat assertion script (not testthat) so it works as a
# quick smoke check on the server without extra package installs.

source("C:/Users/david/Desktop/claude/R/11_temporal_delta.R")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))

# ──────────────────────────────────────────────────────────────────────────────
# Helpers
# ──────────────────────────────────────────────────────────────────────────────

PASS <- 0L; FAIL <- 0L
check <- function(label, ok, detail = NULL) {
  if (isTRUE(ok)) {
    cat(sprintf("  PASS  %s\n", label))
    PASS <<- PASS + 1L
  } else {
    cat(sprintf("  FAIL  %s\n", label))
    if (!is.null(detail)) cat(sprintf("        %s\n", detail))
    FAIL <<- FAIL + 1L
  }
}

section <- function(s) cat(sprintf("\n=== %s ===\n", s))

# Use a temp directory as the test root so we don't pollute anything real
TEST_ROOT <- file.path(tempdir(), paste0("temporal_test_", Sys.getpid()))
if (dir.exists(TEST_ROOT)) unlink(TEST_ROOT, recursive = TRUE)
dir.create(TEST_ROOT, recursive = TRUE)
cat("Test root:", TEST_ROOT, "\n")

SP <- "Astacus_astacus"

# ──────────────────────────────────────────────────────────────────────────────
# Synthetic data
# ──────────────────────────────────────────────────────────────────────────────

# A baseline of 12 occurrences across a small region of Romania (Banat-ish).
# All native, all active, no extinctions yet.
make_baseline_occ <- function() {
  tibble::tibble(
    record_id = sprintf("R%03d", 1:12),
    species   = SP,
    longitude = c(21.230, 21.231, 21.229, 21.232,    # cluster A near 21.230
                  22.000, 22.001, 22.002, 21.999,    # cluster B at 22.000
                  21.500, 21.501,                    # cluster C at 21.500
                  20.900, 23.100),                   # outliers
    latitude  = c(45.750, 45.751, 45.749, 45.752,
                  45.800, 45.801, 45.799, 45.798,
                  45.900, 45.899,
                  45.700, 45.700),
    year      = c(1995, 2005, 2008, 2018,            # cluster A: spans 1995-2018
                  1990, 2000, 2010, 2020,            # cluster B
                  2002, 2015,
                  1985, 2015),
    status    = "native",
    population_status = "indigenous",
    is_extinct = FALSE,
    comments  = NA_character_,
    country   = c(rep("Romania", 10), "Serbia", "Romania"),
    hydrobasin = c(rep("Mureș-10", 4), rep("Timiș-8", 4),
                   rep("Bega-8", 2), "Danube-6", "Olt-6"),
    freshwater_ecoregion = rep("Lower Danube", 12),
    protected_area = c(rep("PN Retezat", 4), rep(NA_character_, 8))
  )
}

# v1.1 input: ALL baseline records + 3 new presences + 2 extinctions
# Extinction 1: at cluster A (21.230, 45.750), year 2010, cause invasive
#   → should suppress R001 (1995), R002 (2005), R003 (2008) — all < 2010
#   → should NOT suppress R004 (2018) — recovery
# Extinction 2: at cluster B (22.000, 45.800), year 2015, cause habitat loss
#   → should suppress R005 (1990), R006 (2000), R007 (2010) — all < 2015
#   → should NOT suppress R008 (2020)
make_v11_input <- function() {
  base <- make_baseline_occ()
  new_records <- tibble::tibble(
    record_id = c("R013", "R014", "R015"),
    species   = SP,
    longitude = c(21.300, 22.500, 21.700),
    latitude  = c(45.770, 45.820, 45.880),
    year      = c(2022, 2024, 2023),
    status    = "native",
    population_status = "indigenous",
    is_extinct = FALSE,
    comments  = NA_character_,
    country   = c("Romania", "Romania", "Romania"),
    hydrobasin = c("Mureș-10", "Timiș-8", "Bega-8"),
    freshwater_ecoregion = "Lower Danube",
    protected_area = NA_character_
  )
  ext_records <- tibble::tibble(
    record_id = c("E001", "E002"),
    species   = SP,
    longitude = c(21.230, 22.000),
    latitude  = c(45.750, 45.800),
    year      = c(2010, 2015),
    status    = "native",
    population_status = "indigenous",
    is_extinct = TRUE,
    comments  = c("Invasive Pontastacus leptodactylus displacement",
                  "Habitat loss due to channelization"),
    country   = "Romania",
    hydrobasin = c("Mureș-10", "Timiș-8"),
    freshwater_ecoregion = "Lower Danube",
    protected_area = c("PN Retezat", NA_character_)
  )
  bind_rows(base, new_records, ext_records)
}


# ──────────────────────────────────────────────────────────────────────────────
section("Path utilities")
# ──────────────────────────────────────────────────────────────────────────────

check("clean_species_name strips spaces",
      clean_species_name("Astacus astacus") == "Astacus_astacus")

check("clean_species_name strips dots",
      clean_species_name("A. astacus var. typicus") == "A_astacus_var_typicus")

check("clean_species_name strips trailing/leading underscores",
      clean_species_name("__weird name__") == "weird_name")

td <- temporal_root_dir(TEST_ROOT)
check("temporal_root_dir builds correctly",
      td == file.path(TEST_ROOT, "temporal"))

sd <- species_temporal_dir(SP, TEST_ROOT)
check("species_temporal_dir creates directory",
      dir.exists(sd))


# ──────────────────────────────────────────────────────────────────────────────
section("detect_prior_artifacts: empty case")
# ──────────────────────────────────────────────────────────────────────────────

art0 <- detect_prior_artifacts(SP, TEST_ROOT)
check("empty dir → artifacts_exist FALSE", isFALSE(art0$artifacts_exist))
check("empty dir → latest_version NULL", is.null(art0$latest_version))


# ──────────────────────────────────────────────────────────────────────────────
section("parse_extinction_causes")
# ──────────────────────────────────────────────────────────────────────────────

baseline <- make_baseline_occ()
ext_empty <- parse_extinction_causes(baseline)
check("baseline has no extinctions", nrow(ext_empty) == 0L)

v11_input <- make_v11_input()
ext <- parse_extinction_causes(v11_input)
check("parses 2 extinctions from v1.1 input", nrow(ext) == 2L)
check("extinction 1 cause → invasive",
      ext$cause_category[ext$record_id == "E001"] == "invasive")
check("extinction 2 cause → habitat_loss",
      ext$cause_category[ext$record_id == "E002"] == "habitat_loss")
check("species column preserved", all(ext$species == SP))
check("buffer radius set to 500", all(ext$buffer_radius_m == 500))

# Case insensitivity
mixed_case <- tibble::tibble(
  record_id = "X1", species = SP, longitude = 21, latitude = 45, year = 2010,
  is_extinct = TRUE, comments = "INVASIVE Pacifastacus leniusculus"
)
ext_ci <- parse_extinction_causes(mixed_case)
check("uppercase 'INVASIVE' detected", ext_ci$cause_category[1] == "invasive")

# Empty / NA comments
empty_cmt <- tibble::tibble(
  record_id = c("X1","X2","X3"), species = SP,
  longitude = 21, latitude = 45, year = 2010, is_extinct = TRUE,
  comments = c(NA_character_, "", "no recognized keyword")
)
ext_nc <- parse_extinction_causes(empty_cmt)
check("NA/empty/unknown all → 'unknown'",
      all(ext_nc$cause_category == "unknown"))


# ──────────────────────────────────────────────────────────────────────────────
section("apply_spatial_temporal_mask: 500m geodesic radius")
# ──────────────────────────────────────────────────────────────────────────────

# Suppress warnings about unlinked extinctions in this test (we have linked ones)
masked <- suppressWarnings(apply_spatial_temporal_mask(v11_input, ext))

# Pull statuses by record_id for assertions
get_status <- function(df, id) df$temporal_status[df$record_id == id]
get_supp   <- function(df, id) df$suppressed_by_extinction[df$record_id == id]

check("R001 (1995, in cluster A) → suppressed by E001",
      get_status(masked, "R001") == "suppressed" &
      get_supp(masked, "R001") == "E001")
check("R002 (2005, in cluster A) → suppressed",
      get_status(masked, "R002") == "suppressed")
check("R003 (2008, in cluster A) → suppressed",
      get_status(masked, "R003") == "suppressed")
check("R004 (2018, in cluster A, AFTER ext) → ACTIVE (recovery)",
      get_status(masked, "R004") == "active")
check("R005 (1990, in cluster B) → suppressed by E002",
      get_status(masked, "R005") == "suppressed" &
      get_supp(masked, "R005") == "E002")
check("R007 (2010, in cluster B, BEFORE 2015 ext) → suppressed",
      get_status(masked, "R007") == "suppressed")
check("R008 (2020, in cluster B, AFTER ext) → ACTIVE",
      get_status(masked, "R008") == "active")
check("R009 (cluster C, far from any ext) → ACTIVE",
      get_status(masked, "R009") == "active")
check("R011 (Serbia outlier) → ACTIVE",
      get_status(masked, "R011") == "active")

n_supp <- sum(masked$temporal_status == "suppressed")
check("total suppressed = 6 (3 in cluster A + 3 in cluster B)",
      n_supp == 6L,
      detail = sprintf("got %d", n_supp))

# Empty extinctions case
masked0 <- apply_spatial_temporal_mask(baseline, ext_empty)
check("empty extinctions → all active",
      all(masked0$temporal_status == "active"))

# Unlinked extinction warning
unlinked <- tibble::tibble(
  record_id = "E_FAR", species = SP,
  longitude = 50.000, latitude = 50.000, year = 2020,
  is_extinct = TRUE, comments = "habitat_loss"
)
input_unlinked <- bind_rows(baseline, unlinked)
ext_unlinked <- parse_extinction_causes(input_unlinked)
warned <- FALSE
withCallingHandlers(
  apply_spatial_temporal_mask(input_unlinked, ext_unlinked),
  warning = function(w) {
    if (grepl("Unlinked extinction", conditionMessage(w))) warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
check("unlinked extinction emits warning", warned)


# ──────────────────────────────────────────────────────────────────────────────
section("Save baseline v1.0 snapshot + detect/load")
# ──────────────────────────────────────────────────────────────────────────────

baseline_metrics <- list(
  EOO_km2 = .calc_eoo_default(baseline$longitude, baseline$latitude),
  AOO_km2 = .calc_aoo_default(baseline$longitude, baseline$latitude),
  countries = unique(baseline$country),
  basins    = unique(baseline$hydrobasin)
)
baseline_with_status <- baseline %>% mutate(
  temporal_status = "active", suppressed_by_extinction = NA_character_)

saved <- save_versioned_snapshot(
  species_clean   = SP,
  occurrences     = baseline_with_status,
  metrics         = baseline_metrics,
  version         = "v1.0",
  delta_data      = NULL,
  root_output_dir = TEST_ROOT
)
check("v1.0 RDS created", file.exists(saved$rds))
check("v1.0 JSON created", file.exists(saved$json))

art1 <- detect_prior_artifacts(SP, TEST_ROOT)
check("after save → artifacts detected", isTRUE(art1$artifacts_exist))
check("latest_version = v1.0", art1$latest_version == "v1.0")

prev <- load_previous_version(SP, root_output_dir = TEST_ROOT)
check("load_previous_version returns 12 records",
      nrow(prev$occurrences_previous) == 12L)
check("metrics_previous has EOO_km2",
      !is.null(prev$metrics_previous$EOO_km2) && prev$metrics_previous$EOO_km2 > 0)


# ──────────────────────────────────────────────────────────────────────────────
section("generate_version_number")
# ──────────────────────────────────────────────────────────────────────────────

check("after v1.0 → next is v1.1",
      generate_version_number(SP, root_output_dir = TEST_ROOT) == "v1.1")
check("major bump → v2.0",
      generate_version_number(SP, major_bump = TRUE, root_output_dir = TEST_ROOT) == "v2.0")


# ──────────────────────────────────────────────────────────────────────────────
section("Numeric version sort: v1.10 > v1.2")
# ──────────────────────────────────────────────────────────────────────────────

# Drop in fake JSON files for v1.2 and v1.10 to test the sort key
sd2 <- file.path(temporal_root_dir(TEST_ROOT), SP)
file.create(file.path(sd2, sprintf("%s_v1.2.json", SP)))
file.create(file.path(sd2, sprintf("%s_v1.10.json", SP)))
file.create(file.path(sd2, sprintf("%s_occurrences_v1.2.rds", SP)))
file.create(file.path(sd2, sprintf("%s_occurrences_v1.10.rds", SP)))

art_sorted <- detect_prior_artifacts(SP, TEST_ROOT)
check("latest = v1.10 not v1.2 (numeric sort)",
      art_sorted$latest_version == "v1.10",
      detail = sprintf("got %s", art_sorted$latest_version))

# Cleanup the fake files so the rest of the test uses the real v1.0
file.remove(file.path(sd2, sprintf("%s_v1.2.json", SP)))
file.remove(file.path(sd2, sprintf("%s_v1.10.json", SP)))
file.remove(file.path(sd2, sprintf("%s_occurrences_v1.2.rds", SP)))
file.remove(file.path(sd2, sprintf("%s_occurrences_v1.10.rds", SP)))


# ──────────────────────────────────────────────────────────────────────────────
section("Full temporal_delta orchestrator → v1.1")
# ──────────────────────────────────────────────────────────────────────────────

# Suppress the unlinked-extinction warnings inside the orchestrator (none here, but defensive)
res11 <- suppressWarnings(temporal_delta(
  occurrences_current = v11_input,
  species_clean       = SP,
  scope               = "indigenous",
  root_output_dir     = TEST_ROOT
))

check("temporal_delta returns is_baseline=FALSE",
      isFALSE(res11$is_baseline))
check("version = v1.1", res11$version == "v1.1")
check("previous_version = v1.0", res11$previous_version == "v1.0")
check("delta_data not NULL", !is.null(res11$delta_data))
check("delta_data$range_delta has signal",
      !is.null(res11$delta_data$range_delta$range_signal))
check("delta_data$changes$n_new = 3 (R013, R014, R015)",
      res11$delta_data$changes$n_new == 3L,
      detail = sprintf("got %d", res11$delta_data$changes$n_new))
check("delta_data$changes$n_extinct = 2",
      res11$delta_data$changes$n_extinct == 2L)
check("masking_summary$extinction_zones = 2",
      res11$delta_data$masking_summary$extinction_zones == 2L)
check("masking_summary$occurrences_suppressed = 6",
      res11$delta_data$masking_summary$occurrences_suppressed == 6L,
      detail = sprintf("got %d", res11$delta_data$masking_summary$occurrences_suppressed))


# ──────────────────────────────────────────────────────────────────────────────
section("analyze_geographic_changes / summarize_extinctions content")
# ──────────────────────────────────────────────────────────────────────────────

geo <- res11$delta_data$geo_changes
check("geo_changes returned a list with countries_extirpated",
      !is.null(geo$countries_extirpated))

# All cluster A localities suppressed but R004 (2018) is still active in Mureș-10,
# new R013 also in Mureș-10, so no basin should be fully extirpated. Verify.
check("no basins extirpated (R004 + R013 keep Mureș-10 alive)",
      length(geo$basins_extirpated) == 0L,
      detail = sprintf("got: %s", paste(geo$basins_extirpated, collapse=",")))

ext_sum <- res11$delta_data$extinction_summary
check("extinction_summary$total_extinctions = 2",
      ext_sum$total_extinctions == 2L)
check("by_cause has invasive and habitat_loss rows",
      all(c("invasive", "habitat_loss") %in% ext_sum$by_cause$cause_category))


# ──────────────────────────────────────────────────────────────────────────────
section("Manifest + archive + chained v1.2")
# ──────────────────────────────────────────────────────────────────────────────

# Save v1.1 snapshot
saved11 <- save_versioned_snapshot(
  species_clean = SP, occurrences = res11$occurrences,
  metrics = res11$metrics_current, version = res11$version,
  delta_data = res11$delta_data, root_output_dir = TEST_ROOT
)
check("v1.1 snapshot saved", file.exists(saved11$rds) && file.exists(saved11$json))

# Update manifest for v1.0 (baseline) then v1.1
update_version_manifest(
  species_clean = SP, version = "v1.0", delta_summary = NULL,
  metadata = list(record_count_total = 12, record_count_active = 12, record_count_suppressed = 0),
  root_output_dir = TEST_ROOT
)
manifest1 <- update_version_manifest(
  species_clean = SP, version = res11$version, delta_summary = res11$delta_data,
  metadata = list(
    record_count_total = nrow(res11$occurrences),
    record_count_active = res11$delta_data$masking_summary$occurrences_active,
    record_count_suppressed = res11$delta_data$masking_summary$occurrences_suppressed
  ),
  root_output_dir = TEST_ROOT
)
check("manifest has 2 versions", length(manifest1$versions) == 2L)
check("v1.1 entry has comparison_base = v1.0",
      manifest1$versions[[2]]$comparison_base == "v1.0")

# Archive v1.0
suppressMessages(archive_previous_version(SP, "v1.0", root_output_dir = TEST_ROOT))
arch_dir <- file.path(temporal_root_dir(TEST_ROOT), SP, "archive")
check("archive directory created", dir.exists(arch_dir))
check("archive contains v1.0 files",
      length(list.files(arch_dir, pattern = "v1.0_archived")) >= 2L)

# After archiving v1.0, the live dir should only have v1.1 + manifest
sd3 <- file.path(temporal_root_dir(TEST_ROOT), SP)
live_files <- list.files(sd3, pattern = "v1\\.")
check("live dir contains only v1.1 files",
      all(grepl("v1\\.1", live_files)),
      detail = sprintf("got: %s", paste(live_files, collapse=",")))

# Chain into v1.2: same input plus 1 more extinction and 2 more new presences
v12_input <- bind_rows(
  v11_input,
  tibble::tibble(
    record_id = c("R016", "R017"),
    species   = SP,
    longitude = c(21.600, 22.700),
    latitude  = c(45.850, 45.870),
    year      = c(2025, 2025),
    status    = "native", population_status = "indigenous",
    is_extinct = FALSE, comments = NA_character_,
    country = "Romania",
    hydrobasin = c("Bega-8", "Timiș-8"),
    freshwater_ecoregion = "Lower Danube",
    protected_area = NA_character_
  ),
  tibble::tibble(
    record_id = "E003",
    species   = SP,
    longitude = 21.500, latitude = 45.900,
    year      = 2024,
    status = "native", population_status = "indigenous",
    is_extinct = TRUE,
    comments = "Disease outbreak — Aphanomyces astaci",
    country = "Romania",
    hydrobasin = "Bega-8",
    freshwater_ecoregion = "Lower Danube",
    protected_area = NA_character_
  )
)

res12 <- suppressWarnings(temporal_delta(
  occurrences_current = v12_input,
  species_clean       = SP,
  scope               = "indigenous",
  root_output_dir     = TEST_ROOT
))

check("v1.2 generated (not v1.1, not v2.0)",
      res12$version == "v1.2",
      detail = sprintf("got %s", res12$version))
check("v1.2 compares to v1.1 (NOT v1.0)",
      res12$previous_version == "v1.1",
      detail = sprintf("got %s", res12$previous_version))
check("v1.2 baseline_version still v1.0",
      res12$delta_data$baseline_version == "v1.0")
check("v1.2 detects 1 new extinction (E003)",
      res12$delta_data$changes$n_extinct == 1L,
      detail = sprintf("got %d", res12$delta_data$changes$n_extinct))


# ──────────────────────────────────────────────────────────────────────────────
section("Baseline path: no priors → v1.0")
# ──────────────────────────────────────────────────────────────────────────────

SP2 <- "Procambarus_clarkii"
res_base <- temporal_delta(
  occurrences_current = make_baseline_occ() %>% mutate(species = SP2),
  species_clean       = SP2,
  scope               = "non_indigenous",
  root_output_dir     = TEST_ROOT
)
check("new species → version v1.0", res_base$version == "v1.0")
check("new species → is_baseline TRUE", isTRUE(res_base$is_baseline))
check("new species → delta_data NULL", is.null(res_base$delta_data))
check("new species → all temporal_status = active",
      all(res_base$occurrences$temporal_status == "active"))


# ──────────────────────────────────────────────────────────────────────────────
section("calculate_range_delta signal classification")
# ──────────────────────────────────────────────────────────────────────────────

mk_active <- function(occ) occ %>% mutate(temporal_status = "active")

rd_stable <- calculate_range_delta(
  current = mk_active(baseline),
  previous_metrics = list(EOO_km2 = baseline_metrics$EOO_km2,
                          AOO_km2 = baseline_metrics$AOO_km2)
)
check("identical data → stable",
      rd_stable$range_signal == "stable",
      detail = sprintf("got %s, ΔEOO=%s%%, ΔAOO=%s%%",
                       rd_stable$range_signal,
                       rd_stable$EOO_change_percent,
                       rd_stable$AOO_change_percent))

rd_contract <- calculate_range_delta(
  current = mk_active(baseline),
  previous_metrics = list(EOO_km2 = baseline_metrics$EOO_km2 * 2,  # was twice as big
                          AOO_km2 = baseline_metrics$AOO_km2 * 2)
)
check("range halved → contraction",
      rd_contract$range_signal == "contraction")

rd_expand <- calculate_range_delta(
  current = mk_active(baseline),
  previous_metrics = list(EOO_km2 = baseline_metrics$EOO_km2 / 2,
                          AOO_km2 = baseline_metrics$AOO_km2 / 2)
)
check("range doubled → expansion",
      rd_expand$range_signal == "expansion")


# ──────────────────────────────────────────────────────────────────────────────
# Summary
# ──────────────────────────────────────────────────────────────────────────────

cat(sprintf("\n========\n  RESULTS: %d passed, %d failed (of %d total)\n========\n",
            PASS, FAIL, PASS + FAIL))

if (FAIL > 0L) quit(status = 1L) else quit(status = 0L)
