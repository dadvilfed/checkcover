#!/usr/bin/env Rscript
# The version delta reports what was measured against the taxon's previous
# package, and what changed in its input; never a trend (Lucian, SEB review,
# 2026-10).
#
#   * Section 4.2/4.3: records, AOO, EOO and documented local extinctions,
#     previous -> current, and the input changes (extinction claims added,
#     records added, withdrawn, revised). No "Current trend". The caveat only
#     when the cause cannot be told from those counts.
#   * Section 5: "The documented native range is smaller than in the previous
#     package ..." with the values, instead of "Native range exhibits decline".
#   * trend_vs_previous reads "decrease" when the AOO drops to 0 (Cambarus
#     veitchorum, 4 -> 0 km², read null in 1.1).
#   * Section 1: "Type locality represented among the analyzed records: yes",
#     not "Locality: TRUE".

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

for (p in c("dplyr", "jsonlite", "digest")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("[test_version_delta] SKIP (%s not installed)\n", p)); quit(status = 0)
  }
}
suppressWarnings(suppressMessages({
  library(dplyr)
  source("R/00_logging.R"); source("R/00_helpers.R"); source("R/00_run_context.R")
  source("R/10_canonical_narratives.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
has <- function(txt, s) grepl(s, txt, fixed = TRUE)

cat("[test_version_delta]\n")

# ---- trend_vs_previous ----
ok(identical(trend_vs_previous_block(0L, 0L, 4L), "decrease"),
   "AOO 4 -> 0 with no active record: decrease (was null)")
ok(is.na(trend_vs_previous_block(0L, 0L, 0L)), "absent before and now: null")
ok(is.na(trend_vs_previous_block(0L, 0L, NULL)), "absent now, first package: null")
ok(identical(trend_vs_previous_block(10L, 196L, 208L), "decrease"), "208 -> 196 (-5.8%): decrease")
ok(identical(trend_vs_previous_block(10L, 200L, 208L), "stable"), "within 5%: stable")

# ---- what changed in a taxon's input ----
recs <- function(id, lon, claim = FALSE, pop = "indigenous") data.frame(
  species = "Testus astacus", record_id = id, longitude = lon, latitude = 46, year = 2020,
  is_extinct = claim, population_status = pop, status = ifelse(pop == "indigenous", "native", "alien"),
  stringsAsFactors = FALSE)
prev <- rbind(recs("R1", 22.1), recs("R2", 22.2), recs("R3", 22.3), recs("R4", 22.4, TRUE),
              recs("R9", 30.0, pop = "non-indigenous"))
curr <- rbind(recs("R1", 22.1, TRUE),              # claim added on a kept record
              recs("R2", 22.25),                   # coordinate revised
              recs("R4", 22.4, FALSE),             # claim withdrawn
              recs("R5", 22.5, TRUE),              # added, with a claim
              recs("R9", 30.0, pop = "indigenous"))# revised: moved population
ff <- function(d) { x <- fingerprint_frame(d); x$package_id <- make_package_id(d$species); x }
ch <- compare_taxon_inputs(ff(prev), ff(curr))
ok(!is.null(ch), "two snapshots with unique record ids can be compared")
ok(ch$indigenous$claims_added == 2, "claims added: on a kept record and on an added one")
ok(ch$indigenous$claims_withdrawn == 1, "claims withdrawn counted")
ok(ch$indigenous$records_added == 1, "records added (R5)")
ok(ch$indigenous$records_withdrawn == 1, "records withdrawn (R3)")
ok(ch$indigenous$records_revised == 2, "records revised: a moved coordinate, a population change")
ok(ch$non_indigenous$records_revised == 1 && ch$non_indigenous$records_withdrawn == 0,
   "a record that changed population counts as revised in its former population too")
rem <- function(d, r) { d$extinction_remarks <- r; d }
p2 <- rem(rbind(recs("R1", 22.1), recs("R2", 22.2, TRUE)), c(NA, "dried out"))
c2 <- rem(rbind(recs("R1", 22.1, TRUE), recs("R2", 22.2, TRUE)), c("dam built 2019", "dried out in 2018"))
ch2 <- compare_taxon_inputs(ff(p2), ff(c2))
ok(ch2$indigenous$claims_added == 1 && ch2$indigenous$records_revised == 1,
   "a claim's own remark is part of the claim; a remark edited on a standing claim is a revision")
ok(is.null(compare_taxon_inputs(NULL, ff(curr))), "no previous snapshot: not comparable")
dup <- ff(curr); dup$record_id[2] <- dup$record_id[1]
ok(is.null(compare_taxon_inputs(ff(prev), dup)), "duplicated record ids: not comparable")
same <- compare_taxon_inputs(ff(prev), ff(prev))
ok(sum(unlist(same)) == 0, "identical inputs: every count 0")

# ---- a previous package and stored inputs on disk ----
tmp <- file.path(tempdir(), paste0("vdelta_", as.integer(runif(1) * 1e8)))
root <- file.path(tmp, "output"); state <- file.path(tmp, "state")
dir.create(file.path(root, "1.0", "checkover"), recursive = TRUE)
dir.create(file.path(root, "1.0", "Testus_astacus"), recursive = TRUE)
jsonlite::write_json(list(framework_version = "1.0",
                          species = list(Testus_astacus = list(outcome = "new", source_version = "1.0"))),
                     file.path(root, "1.0", "checkover", "manifest.json"), auto_unbox = TRUE)
prev_meta <- function(code = "runtime-1.0") list(
  snapshot = list(version = "1.0", date = "2026-09-25"),
  provenance = list(code_version = code),
  metrics = list(indigenous = list(AOO_km2 = 208L, EOO_km2 = 3815L, records = 116L, extinctions_count = 0L),
                 non_indigenous = list(AOO_km2 = 0L, EOO_km2 = NA, records = 0L, extinctions_count = 0L)))
jsonlite::write_json(prev_meta(), file.path(root, "1.0", "Testus_astacus", "package_metadata.json"),
                     auto_unbox = TRUE, na = "null")
ctx <- list(prior_versions = list("1.0"), root_output_dir = root, state_dir = state,
            framework_version = "1.1", code_version = "runtime-1.1")
write_input_snapshot(prev, state, "1.0")
write_input_snapshot(curr, state, "1.1")
ok(file.exists(input_snapshot_path(state, "1.1")), "the run's input snapshot is stored in the state dir")

ind_report <- list(
  metrics = list(n_records = 106L, aoo_km2 = 196, eoo_km2 = 3636.4, eoo_status = "computed",
                 iucn_category = "endemic"),
  counts = list(n_countries = 1L, n_hydrobasins = 21L, n_named_basins = 11L, n_extinctions = 3L),
  temporal = list(year_min = 2010L, year_max = 2026L, pct_post_2000 = 100),
  conservation = list(n_protected_records = 61L, protection_percentage = 57.5),
  taxonomy = list(higher_taxonomy = "Decapoda > Astacoidea > Astacidae"),
  type_locality_present = TRUE)

delta <- .build_package_delta(ctx, "Testus_astacus", ind_report, NULL)
ok(identical(delta$previous_version, "1.0"), "the previous package is the taxon's own last one (1.0)")
s4 <- .build_section4("Testus astacus", NULL, NULL, ind_report, NULL, "1.1", "v1.1", "2026-10-23",
                      delta = delta, citations = NULL)
ok(has(s4, "- **Previous package of this taxon:** v1.0 (2026-09-25)"), "4.2 names the previous package")
ok(has(s4, "- **Records:** 116 → 106"), "4.2 records previous -> current")
ok(has(s4, "- **AOO:** 208 → 196 km²"), "4.2 AOO previous -> current")
ok(has(s4, "- **EOO:** 3,815 → 3,636 km²"), "4.2 EOO previous -> current (was not stated)")
ok(has(s4, "- **Documented local extinctions:** 0 → 3"), "4.2 documented local extinctions")
ok(has(s4, "- **Input changes since v1.0:** extinction claims added: 2; extinction claims withdrawn: 1; records added: 1; records withdrawn: 1; records revised: 2"),
   "4.2 lists the input changes")
ok(!has(s4, "Current trend") && !has(s4, "Trend reflects"), "no trend line, no fixed caveat")
ok(!has(s4, "cannot be told"), "no caveat when the input changes explain the difference")
ok(!has(s4, "4.3 Version delta"), "no 4.3 for a taxon without non-indigenous records")

# The cause cannot be told: the previous input is not stored.
unlink(input_snapshot_path(state, "1.0"))
s4b <- .build_section4("Testus astacus", NULL, NULL, ind_report, NULL, "1.1", "v1.1", "2026-10-23",
                       delta = .build_package_delta(ctx, "Testus_astacus", ind_report, NULL))
ok(has(s4b, "not available (the input of v1.0 is not stored)") && has(s4b, "cannot be told from the data"),
   "previous input missing: changes not available, and the caveat")

# Input unchanged, code changed: the code is named as the cause.
write_input_snapshot(curr, state, "1.0")
s4c <- .build_section4("Testus astacus", NULL, NULL, ind_report, NULL, "1.1", "v1.1", "2026-10-23",
                       delta = .build_package_delta(ctx, "Testus_astacus", ind_report, NULL))
ok(has(s4c, "come from the processing code (runtime-1.0 → runtime-1.1)"),
   "input unchanged, code changed: the difference is attributed to the code")
d_code <- .build_package_delta(ctx, "Testus_astacus", ind_report, NULL)
s5c <- .range_change_sentence(d_code, d_code$indigenous, "native")
ok(has(s5c, "The input records of the documented native range are unchanged since the previous package of this taxon, v1.0") &&
   has(s5c, "come from the processing code (runtime-1.0 → runtime-1.1)") && !has(s5c, "smaller"),
   "section 5: a change of method is not called a smaller range")

# Nothing changed: no caveat at all.
same_report <- ind_report
same_report$metrics$n_records <- 116L; same_report$metrics$aoo_km2 <- 208; same_report$metrics$eoo_km2 <- 3815
same_report$counts$n_extinctions <- 0L
s4d <- .build_section4("Testus astacus", NULL, NULL, same_report, NULL, "1.1", "v1.1", "2026-10-23",
                       delta = .build_package_delta(ctx, "Testus_astacus", same_report, NULL))
ok(!has(s4d, "> *"), "no difference: no caveat")

# ---- section 5 ----
write_input_snapshot(prev, state, "1.0")
s5 <- .build_section5("Testus astacus", 1, NULL, NULL, NULL, ind_report, NULL,
                      delta = .build_package_delta(ctx, "Testus_astacus", ind_report, NULL),
                      output_version = "v1.1")$text
ok(has(s5, "The documented native range is smaller than in the previous package of this taxon, v1.0 (AOO 208 → 196 km², EOO 3,815 → 3,636 km²)."),
   "section 5 states the measured values")
ok(!grepl("decline|stable|expansion|contraction", s5), "section 5 has no trend wording")
ok(!has(s5, "not yet tracked"), "the obsolete 'not yet tracked' sentence is gone")
ok(has(s5, "updating the previous package of this taxon (v1.0)"), "version prefix names the previous package")
s5_first <- .build_section5("Testus astacus", 1, NULL, NULL, NULL, ind_report, NULL, delta = NULL,
                            output_version = "v1.3")$text
ok(!has(s5_first, "previous package"), "a taxon's first package (even in 1.3) has no delta sentence")
grow <- .build_package_delta(ctx, "Testus_astacus",
                             modifyList(ind_report, list(metrics = list(aoo_km2 = 220, eoo_km2 = 3700))), NULL)
ok(has(.range_change_sentence(grow, grow$indigenous, "native"), "differs from the previous package"),
   "AOO up and EOO down: 'differs', not a direction")

# ---- section 1 ----
s1 <- .build_section1("Testus astacus", NULL, ind_report, NULL)
ok(has(s1, "- **Type locality represented among the analyzed records:** yes"), "type locality: yes")
ok(!has(s1, "Locality:** TRUE"), "no bare TRUE flag")
s1n <- .build_section1("Testus astacus", NULL, modifyList(ind_report, list(type_locality_present = FALSE)), NULL)
ok(has(s1n, "analyzed records:** no"), "type locality: no")

# ---- EOO text ----
ok(identical(.eoo_text(list(metrics = list(eoo_km2 = NA, eoo_status = "exceeds_hemisphere"))),
             "undefined (the localities do not fit in one hemisphere, so no convex hull exists)"),
   "an EOO beyond a hemisphere is printed as undefined, with the reason")

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_version_delta] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
