#!/usr/bin/env Rscript
# Service mode (UVT server setup, sections 03, 05-07 and 11; 2026-09).
#
#   * a run file named by CHECKOVER_RUN overrides config.R, and a bad one is
#     refused with its full problem list in <run home>/preflight.json
#   * exit codes: 0 succeeded, 1 failed, 2 refused before processing, with
#     <run home>/status.json saying which and why
#   * the output root holds revision folders only; working state (runs/,
#     cache/, logs/, temporal/, _registry.json) lives in state_dir, and a state
#     dir belongs to exactly one output root
#
# sf-free, like the rest of the suite.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("[test_run_file] SKIP (jsonlite not installed)\n"); quit(status = 0)
}
suppressWarnings(suppressMessages({
  source("R/00_run_file.R"); source("R/00_preflight.R")
}))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
refused <- function(expr) {
  .CHECKOVER_EXIT$refused <- FALSE
  e <- tryCatch({ capture.output(expr); NULL }, error = function(e) conditionMessage(e))
  list(error = e, flagged = isTRUE(.CHECKOVER_EXIT$refused))
}
read_json <- function(p) jsonlite::read_json(p, simplifyVector = TRUE)

cat("[test_run_file]\n")

tmp  <- normalizePath(file.path(tempdir(), paste0("runfile_", as.integer(runif(1) * 1e8))),
                      winslash = "/", mustWork = FALSE)
dir.create(tmp, recursive = TRUE)
base <- list(input_file = "WoC_1_0.tsv", root_output_dir = "checkover_output",
             state_dir = "checkover_state", framework_version = "1.0",
             spatial = list(hydro_dir = "spatial_data/hydrobasins", hydro_bbox = 50),
             temporal = list(enabled = TRUE),
             force_reprocess = FALSE, species_scope = NULL, code_tag = NULL)
home <- function(id) { d <- file.path(tmp, "runs", id); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
put  <- function(id, json) { f <- file.path(home(id), "run.json"); writeLines(json, f); f }

# ---- no run file: config.R is the whole configuration ----
cfg0 <- apply_run_file(base, path = "")
ok(is.null(cfg0$service) && identical(cfg0[names(base)], base[names(base)]),
   "no CHECKOVER_RUN: config unchanged, not a service run")

# ---- a valid run file ----
f1 <- put("run_a1", '{
  "run_id": "run_a1", "framework_version": "1.3",
  "input_file": "/data/runs/run_a1/input.tsv", "root_output_dir": "/data/output",
  "state_dir": "/data/state", "species_scope": ["Astacus_astacus", "Faxonius validus"],
  "force_reprocess": [], "code_tag": "runtime-1.3",
  "spatial": {"hydro_dir": "/data/spatial/hydrobasins"} }')
cfg1 <- apply_run_file(base, path = f1)
ok(identical(cfg1$framework_version, "1.3") && identical(cfg1$root_output_dir, "/data/output") &&
   identical(cfg1$state_dir, "/data/state") && identical(cfg1$code_tag, "runtime-1.3"),
   "run file values override config.R")
ok(identical(cfg1$species_scope, c("Astacus_astacus", "Faxonius validus")),
   "species_scope arrives as a character vector")
ok(identical(cfg1$force_reprocess, FALSE), "an empty force_reprocess list means FALSE")
ok(identical(cfg1$spatial$hydro_dir, "/data/spatial/hydrobasins") && identical(cfg1$spatial$hydro_bbox, 50),
   "an object merges into its section (other keys kept)")
ok(identical(cfg1$service$run_id, "run_a1") &&
   identical(cfg1$service$work_dir, file.path(dirname(normalizePath(f1, winslash = "/")), "work")) &&
   identical(basename(cfg1$service$log_file), "run.log"),
   "service paths: work/ and run.log in the run home")

f2 <- put("run_b2", '{"framework_version": "1.4", "input_file": "x.tsv",
  "root_output_dir": "o", "state_dir": "s", "species_scope": null}')
cfg2 <- apply_run_file(modifyList(base, list(species_scope = "Astacus astacus")), path = f2)
ok(identical(cfg2$service$run_id, "run_b2"), "run_id defaults to the run home's folder name")
ok("species_scope" %in% names(cfg2) && is.null(cfg2$species_scope),
   "an explicit null clears a setting instead of deleting it")

# ---- refusals: every problem at once, written to preflight.json ----
f3 <- put("run_bad", '{"framework_version": 1.10, "input_file": "x.tsv",
  "root_outptu_dir": "o", "species_scope": [], "service": {}}')
r3 <- refused(apply_run_file(base, path = f3))
pf <- read_json(file.path(dirname(f3), "preflight.json"))
st <- read_json(file.path(dirname(f3), "status.json"))
items <- pf$problems$item
ok(!is.null(r3$error) && r3$flagged, "a bad run file stops the run and flags a refusal (exit 2)")
ok(identical(pf$status, "refused") && identical(st$status, "refused") && identical(st$exit_code, 2L),
   "preflight.json and status.json both say refused, exit code 2")
ok(all(c("root_outptu_dir", "service", "framework_version", "species_scope",
         "root_output_dir", "state_dir") %in% items),
   "all problems listed: typo'd and reserved keys, number version, empty scope, missing keys")
ok(any(grepl("1.10 would be read as 1.1", pf$problems$detail, fixed = TRUE)),
   "a numeric framework_version is refused with the reason")

f4 <- put("run_json", '{"framework_version": "1.3", ')
r4 <- refused(apply_run_file(base, path = f4))
ok(r4$flagged && grepl("not valid JSON",
   read_json(file.path(dirname(f4), "preflight.json"))$problems$detail),
   "invalid JSON is refused with the parser's message")

f5 <- put("run_ver", '{"framework_version": "1.3.1", "input_file": "x.tsv",
  "root_output_dir": "o", "state_dir": "s"}')
ok(refused(apply_run_file(base, path = f5))$flagged, "framework_version must be MAJOR.MINOR")

ok(refused(apply_run_file(base, path = file.path(home("run_none"), "run.json")))$flagged &&
   file.exists(file.path(home("run_none"), "preflight.json")),
   "a missing run file is refused, and the refusal is still written to the run home")

# ---- the preflight writes its full list for a service run ----
svc_cfg <- modifyList(base, list(input_file = file.path(tmp, "absent.tsv"),
                                 root_output_dir = file.path(tmp, "pf_out"),
                                 state_dir = file.path(tmp, "pf_state")))
svc_cfg$service <- service_paths(file.path(home("run_pf"), "run.json"))
rp <- refused(preflight_check(svc_cfg, strict = TRUE))
pfp <- read_json(file.path(home("run_pf"), "preflight.json"))
ok(rp$flagged && identical(pfp$status, "refused") && pfp$n_fatal >= 2L &&
   "input_file" %in% pfp$problems$item,
   "a preflight refusal writes every problem to <run home>/preflight.json")

# ---- the output root holds revisions only; a state dir has one output root ----
lay <- function(out, state, service = FALSE, temporal = TRUE) {
  cfg <- list(root_output_dir = out, state_dir = state, temporal = list(enabled = temporal))
  if (service) cfg$service <- list(run_id = "x")
  check_state_layout(cfg)
}
mk <- function(...) { p <- file.path(tmp, ...); dir.create(p, recursive = TRUE, showWarnings = FALSE); invisible(p) }
rev_ <- function(out, v) mk(basename(out), v, "checkover")

o1 <- mk("o1"); s1 <- mk("s1")
ok(nrow(lay(o1, s1)) == 0L, "fresh output root and fresh state dir: nothing to report")

l2 <- lay(o1, file.path(o1, "state"))
ok(any(l2$severity == "FATAL" & l2$item == "state_dir"), "state_dir inside the output root is FATAL")

o3 <- mk("o3"); rev_(o3, "1.0"); mk("o3", "runs"); mk("o3", "cache")
writeLines("{}", file.path(o3, "_registry.json"))
l3m <- lay(o3, mk("s3")); l3s <- lay(o3, mk("s3"), service = TRUE)
ok(any(l3m$severity == "WARNING" & l3m$item == "root_output_dir contents" &
       grepl("runs", l3m$detail) & grepl("coordinates", l3m$detail)),
   "manual run: working state inside the output root is a WARNING naming the coordinates")
ok(any(l3s$severity == "FATAL" & l3s$item == "root_output_dir contents"),
   "service run: anything but revision folders in the output root is FATAL")

o4 <- mk("o4"); s4 <- mk("s4"); mk("s4", "temporal", "Astacus_astacus")
l4 <- lay(o4, s4)
ok(any(l4$severity == "FATAL" & grepl("another series", l4$detail)),
   "temporal history without revisions is FATAL: the state dir belongs to another series")

o5 <- mk("o5"); rev_(o5, "1.0"); mk("o5", "temporal", "Astacus_astacus")
l5 <- lay(o5, mk("s5"))
ok(any(l5$severity == "FATAL" & grepl("still in the old place", l5$detail)),
   "revisions with the history still in <root>/temporal: FATAL, move it first")

o6 <- mk("o6"); rev_(o6, "1.0")
l6 <- lay(o6, mk("s6"))
ok(any(l6$severity == "WARNING" & grepl("restarts as a baseline", l6$detail)) &&
   !any(l6$severity == "FATAL"),
   "revisions but no history anywhere: a WARNING (temporal restarts as baselines)")

# ---- the temporal history is transactional across a run ----
st7 <- mk("s7"); t7 <- file.path(st7, "temporal")
mk("s7", "temporal", "Astacus_astacus"); writeLines("v1.0", file.path(t7, "Astacus_astacus", "a_v1.0.md"))
invisible(temporal_checkpoint_open(st7))
writeLines("v1.1", file.path(t7, "Astacus_astacus", "a_v1.1.md"))       # the run writes...
mk("s7", "temporal", "Faxonius_limosus")                                # ...and dies
rb <- suppressMessages(temporal_checkpoint_open(st7))                   # the retry starts
ok(isTRUE(rb) && file.exists(file.path(t7, "Astacus_astacus", "a_v1.0.md")) &&
   !file.exists(file.path(t7, "Astacus_astacus", "a_v1.1.md")) &&
   !dir.exists(file.path(t7, "Faxonius_limosus")),
   "a retry rolls back the temporal writes of a run that did not complete")
ok(dir.exists(file.path(st7, "temporal.checkpoint", "temporal")),
   "and takes a fresh checkpoint for itself")
writeLines("v1.1", file.path(t7, "Astacus_astacus", "a_v1.1.md"))
temporal_checkpoint_close(st7)
ok(!dir.exists(file.path(st7, "temporal.checkpoint")) &&
   file.exists(file.path(t7, "Astacus_astacus", "a_v1.1.md")) &&
   !isTRUE(suppressMessages(temporal_checkpoint_open(st7, take = FALSE))),
   "a completed run keeps its writes; the next run has nothing to roll back")

st8 <- mk("s8")
invisible(temporal_checkpoint_open(st8))                                 # first ever run
mk("s8", "temporal", "Astacus_astacus")                                  # writes, dies
o8 <- mk("o8")
l8 <- lay(o8, st8)
ok(!any(l8$severity == "FATAL"),
   "after a failed FIRST run, the preflight judges the checkpoint, so the retry is not refused")
suppressMessages(temporal_checkpoint_open(st8))
ok(!dir.exists(file.path(st8, "temporal")),
   "and the retry removes the history that the failed first run created")

st9 <- mk("s9"); mk("s9", "temporal", "Astacus_astacus"); mk("s9", "temporal.checkpoint.partial", "junk")
ok(!isTRUE(suppressMessages(temporal_checkpoint_open(st9))) &&
   dir.exists(file.path(st9, "temporal", "Astacus_astacus")) &&
   !dir.exists(file.path(st9, "temporal.checkpoint.partial")),
   "an interrupted checkpoint copy is discarded, never restored")

# ---- temporal/ resolves to the state dir ----
suppressWarnings(suppressMessages(source("R/00_helpers.R")))
tr <- local({
  src <- readLines("R/11_temporal_delta.R", warn = FALSE)
  i <- grep("^temporal_root_dir <- function", src)
  j <- i + which(src[(i + 1):length(src)] == "}")[1]
  eval(parse(text = src[i:j]))
  CONFIG <<- list(root_output_dir = "OUT", state_dir = "STATE")
  on.exit(rm(CONFIG, envir = globalenv()))
  temporal_root_dir()
})
ok(identical(tr, file.path("STATE", "temporal")), "temporal_root_dir() defaults to CONFIG$state_dir")

# ---- the main script is wired to the state dir, never the output root ----
mainsrc <- paste(readLines("checkcover_main.R", warn = FALSE), collapse = "\n")
ok(!grepl('file.path(CONFIG$root_output_dir, "logs")', mainsrc, fixed = TRUE) &&
   !grepl('file.path(CONFIG$root_output_dir, "temporal")', mainsrc, fixed = TRUE) &&
   !grepl('file.path(root_output_dir, "runs")', mainsrc, fixed = TRUE) &&
   !grepl('file.path(root_output_dir, "cache")', mainsrc, fixed = TRUE),
   "checkcover_main.R puts no logs, runs, cache or temporal under the output root")
ok(regexpr("apply_run_file(CONFIG)", mainsrc, fixed = TRUE) <
   regexpr("Framework-version guard", mainsrc, fixed = TRUE),
   "the run file is applied before the overwrite guard reads framework_version")

# ---- exit codes, end to end in a real non-interactive R ----
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
run_child <- function(id, body) {
  h <- home(id)
  script <- file.path(h, "child.R")
  writeLines(c(
    sprintf('setwd("%s")', gsub("\\\\", "/", .root)),
    'source("R/00_run_file.R")',
    'install_exit_handler()',
    sprintf('CONFIG <- list(framework_version = "1.0", service = service_paths("%s/run.json"))', h),
    'write_run_status(CONFIG, "running")',
    body), script)
  code <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(script)),
                                   stdout = FALSE, stderr = FALSE))
  list(code = code, status = read_json(file.path(h, "status.json")))
}
c0 <- run_child("exit_ok", 'write_run_status(CONFIG, "succeeded")')
c1 <- run_child("exit_fail", 'stop("boom in module 5")')
c2 <- run_child("exit_refuse", 'refuse_run(data.frame(severity = "FATAL", item = "x", detail = "y"), "nope", CONFIG)')
ok(identical(as.integer(c0$code), 0L) && identical(c0$status$status, "succeeded"),
   "a completed run exits 0 and status.json says succeeded")
ok(identical(as.integer(c1$code), 1L) && identical(c1$status$status, "failed") &&
   grepl("boom in module 5", c1$status$message),
   "an error during the run exits 1; status.json says failed, with the error message")
ok(identical(as.integer(c2$code), 2L) && identical(c2$status$status, "refused"),
   "a refusal exits 2; status.json says refused")

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_run_file] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
