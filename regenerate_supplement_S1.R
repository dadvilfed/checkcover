#!/usr/bin/env Rscript
#### Supplement File S1 — function and dependency inventory ####
#
# Regenerates S1 BY PARSING THE SOURCES, so it cannot drift from the code again.
#
# Reviewer 1 (Ecological Informatics, 2026-09) found the shipped S1 "wrong on
# many counts, listing functions that do not exist at all, exist under different
# names, or simply do not do what is claimed. The very first function listed,
# run_pipeline(), claims to be the main orchestration function, but does not
# exist. Instead the workflow runs from a script." The dependency sheet had
# comparable errors, including `ecoregions` listed as a CRAN package when it is
# only on GitHub.
#
# This script does not write prose. It reports what is actually defined, where,
# whether it is reachable from the entry point, and the roxygen title where the
# source carries one. Anything undocumented is marked as such rather than
# described from imagination.
#
# Usage:
#   Rscript regenerate_supplement_S1.R [outdir]
#
# Outputs (TSV is the source of truth; XLSX matches the supplement format):
#   Supplement_File_S1_functions.tsv
#   Supplement_File_S1_packages.tsv
#   Supplement_File_S1_reconciliation.tsv   what the old S1 got wrong
#   Supplement_File_S1.xlsx                 (when openxlsx is available)

suppressWarnings(suppressMessages({
  library(tools)
}))

args   <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[1] else "."
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

ROOT <- normalizePath(".")

# ── 1. Which files are actually part of a run? ────────────────────────────────
main_src <- readLines("checkcover_main.R", warn = FALSE)

# Sourced directly at the top of the entry point.
direct <- regmatches(main_src, regexpr('(?<=source\\(")[^"]+\\.R(?=")', main_src, perl = TRUE))
direct <- unique(direct[nzchar(direct)])

# Sourced via the module_files vector. Commented-out entries are excluded, which
# is the point: 01c_metrics.R is listed there but commented out.
mf_start <- grep("^module_files <- c\\(", main_src)
mf_end   <- if (length(mf_start)) which(grepl("^\\)", main_src) & seq_along(main_src) > mf_start)[1] else NA
module_files <- character(0)
if (length(mf_start) && !is.na(mf_end)) {
  blk <- main_src[mf_start:mf_end]
  blk <- blk[!grepl("^\\s*#", blk)]                      # drop commented lines
  module_files <- regmatches(blk, regexpr('(?<=")[^"]+\\.R(?=")', blk, perl = TRUE))
  module_files <- unique(module_files[nzchar(module_files)])
}

live_files <- unique(c("checkcover_main.R", "config.R", direct, module_files))

all_r <- sort(c(list.files("R", pattern = "\\.R$", full.names = TRUE), "checkcover_main.R"))
all_r <- gsub("\\\\", "/", all_r)

status_of <- function(f) {
  if (grepl("_DEPRECATED\\.R$", f))      return("deprecated")
  if (f %in% live_files)                 return("live")
  "present_but_not_sourced"
}

# ── 2. Inventory every function definition ───────────────────────────────────
roxygen_title <- function(lines, def_line) {
  # Walk back over the contiguous comment block above the definition and take
  # the first non-empty roxygen line as the title.
  i <- def_line - 1L
  blk <- character(0)
  while (i >= 1L && grepl("^\\s*#", lines[i])) { blk <- c(lines[i], blk); i <- i - 1L }
  if (!length(blk)) return(NA_character_)
  rx <- blk[grepl("^\\s*#'", blk)]
  cand <- if (length(rx)) rx else blk
  cand <- sub("^\\s*#'?\\s*", "", cand)
  cand <- cand[nzchar(cand) & !grepl("^@", cand) & !grepl("^[-=#]{3,}$", cand)]
  if (!length(cand)) return(NA_character_)
  t <- cand[1]
  if (nchar(t) > 300) t <- paste0(substr(t, 1, 297), "...")
  t
}

rows <- list()
for (f in all_r) {
  lines <- readLines(f, warn = FALSE)
  # Top-level assignments of a function, including dot-prefixed internals.
  hit <- grep("^\\s*`?([A-Za-z._][A-Za-z0-9._]*)`?\\s*(<-|=)\\s*function\\s*\\(", lines)
  for (h in hit) {
    nm <- sub("^\\s*`?([A-Za-z._][A-Za-z0-9._]*)`?\\s*(<-|=)\\s*function.*$", "\\1", lines[h])
    # Collect the signature, which may span lines.
    sig <- lines[h]; k <- h
    while (k < length(lines) &&
           lengths(regmatches(paste(lines[h:k], collapse = " "), gregexpr("\\(", paste(lines[h:k], collapse = " ")))) >
           lengths(regmatches(paste(lines[h:k], collapse = " "), gregexpr("\\)", paste(lines[h:k], collapse = " "))))) {
      k <- k + 1L; sig <- paste(sig, trimws(lines[k]))
    }
    sig <- sub("^.*?function\\s*", "function", sig)
    sig <- sub("\\s*\\{.*$", "", sig)
    sig <- gsub("\\s+", " ", trimws(sig))
    if (nchar(sig) > 200) sig <- paste0(substr(sig, 1, 197), "...")

    rows[[length(rows) + 1L]] <- data.frame(
      file        = f,
      file_status = status_of(f),
      function_name = nm,
      exported    = !startsWith(nm, "."),
      signature   = sig,
      line        = h,
      documented  = !is.na(roxygen_title(lines, h)),
      description = roxygen_title(lines, h) %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
fn <- do.call(rbind, rows)
fn <- fn[order(fn$file_status != "live", fn$file, fn$line), ]
rownames(fn) <- NULL

# ── 3. Dependencies, from config.R rather than by hand ───────────────────────
cfg_env <- new.env()
sys.source("config.R", envir = cfg_env)
cran   <- get("REQUIRED_PACKAGES", envir = cfg_env)
gh     <- if (exists("GITHUB_PACKAGES", envir = cfg_env)) get("GITHUB_PACKAGES", envir = cfg_env) else character(0)
gh_req <- if (exists("GITHUB_PACKAGES_REQUIRED", envir = cfg_env)) get("GITHUB_PACKAGES_REQUIRED", envir = cfg_env) else character(0)

# Which packages the code actually calls, so a listed-but-unused dependency is
# visible rather than implied to be in use.
src_all <- unlist(lapply(all_r, readLines, warn = FALSE))
used_ns <- unique(gsub("::.*$", "", regmatches(src_all,
             regexpr("[A-Za-z][A-Za-z0-9.]*::", src_all))))
lib_calls <- unique(gsub(".*(library|require|requireNamespace)\\(\\s*[\"']?([A-Za-z0-9._]+).*", "\\2",
             grep("(library|require|requireNamespace)\\(", src_all, value = TRUE)))
used <- unique(c(used_ns, lib_calls))

pk <- rbind(
  data.frame(package = cran, source = "CRAN",
             install = sprintf('install.packages("%s")', cran),
             required = TRUE, stringsAsFactors = FALSE),
  if (length(gh)) data.frame(
    package = names(gh), source = "GitHub",
    install = sprintf('remotes::install_github("%s")', unname(gh)),
    required = names(gh) %in% gh_req, stringsAsFactors = FALSE)
)
pk$referenced_in_code <- pk$package %in% used

# Some packages are legitimately required without ever being called by name:
# they are data backends or system-stack providers that another package loads.
# Distinguishing these matters, because everything else in the not-referenced
# list is a genuine install burden for no benefit.
TRANSITIVE <- c(
  rnaturalearthdata = "data backend for rnaturalearth; never called directly",
  lwgeom            = "geometry backend registered with sf"
)
pk$note <- ifelse(
  pk$referenced_in_code, "",
  ifelse(pk$package %in% names(TRANSITIVE),
         unname(TRANSITIVE[match(pk$package, names(TRANSITIVE))]),
         "NOT REFERENCED ANYWHERE IN THE CODE - candidate for removal"))
pk$reference_url <- ifelse(pk$source == "CRAN",
                           sprintf("https://cran.r-project.org/package=%s", pk$package),
                           sprintf("https://github.com/%s", unname(gh)[match(pk$package, names(gh))]))
pk <- pk[order(pk$source != "CRAN", pk$package), ]
rownames(pk) <- NULL

# ── 4. Reconcile against the shipped S1 ──────────────────────────────────────
recon <- NULL
old_path <- file.path("manuscript", "supplementary files", "Supplement_File_S1.xlsx")
if (file.exists(old_path) && requireNamespace("readxl", quietly = TRUE)) {
  old_fn <- readxl::read_excel(old_path, sheet = "core_modules", .name_repair = "minimal")
  claimed <- trimws(as.character(old_fn[["Module / function"]]))
  claimed <- claimed[!is.na(claimed) & nzchar(claimed)]
  bare <- sub("\\(\\)$", "", claimed)

  verdict <- vapply(seq_along(claimed), function(i) {
    nmi <- bare[i]
    if (nmi %in% fn$function_name) {
      st <- unique(fn$file_status[fn$function_name == nmi])
      if ("live" %in% st) "OK - defined and reachable"
      else sprintf("PRESENT BUT NOT REACHABLE (%s)", paste(st, collapse = "/"))
    } else if (any(grepl(nmi, all_r, fixed = TRUE))) {
      "NOT A FUNCTION - matches a file/script name, not a definition"
    } else {
      "DOES NOT EXIST"
    }
  }, character(1))

  recon <- data.frame(s1_entry = claimed, verdict = verdict, stringsAsFactors = FALSE)

  old_pk <- readxl::read_excel(old_path, sheet = "R_packages", .name_repair = "minimal")
  opk <- trimws(as.character(old_pk[["Package"]]))
  opk <- opk[!is.na(opk) & nzchar(opk)]
  pk_verdict <- vapply(opk, function(p) {
    if (!(p %in% pk$package)) return("NOT A DEPENDENCY - listed but not in config.R")
    i <- match(p, pk$package)
    if (pk$source[i] == "GitHub") "SOURCE WRONG IN OLD S1 - GitHub only, not CRAN"
    else if (!pk$referenced_in_code[i] && nzchar(pk$note[i]) &&
             !grepl("^NOT REFERENCED", pk$note[i])) "OK - indirect dependency"
    else if (!pk$referenced_in_code[i]) "LISTED BUT NEVER CALLED - candidate for removal"
    else "OK"
  }, character(1))
  recon <- rbind(recon, data.frame(s1_entry = paste0("package: ", opk),
                                   verdict = unname(pk_verdict), stringsAsFactors = FALSE))
}

# ── 5. Write ─────────────────────────────────────────────────────────────────
w <- function(d, f) {
  p <- file.path(outdir, f)
  utils::write.table(d, p, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
  cat(sprintf("  wrote %-46s %d rows\n", f, nrow(d)))
}
cat("\nSupplement S1 regenerated from source\n")
cat("-------------------------------------\n")
w(fn,    "Supplement_File_S1_functions.tsv")
w(pk,    "Supplement_File_S1_packages.tsv")
if (!is.null(recon)) w(recon, "Supplement_File_S1_reconciliation.tsv")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "functions");  openxlsx::writeData(wb, "functions", fn)
  openxlsx::addWorksheet(wb, "R_packages"); openxlsx::writeData(wb, "R_packages", pk)
  if (!is.null(recon)) {
    openxlsx::addWorksheet(wb, "reconciliation"); openxlsx::writeData(wb, "reconciliation", recon)
  }
  openxlsx::saveWorkbook(wb, file.path(outdir, "Supplement_File_S1.xlsx"), overwrite = TRUE)
  cat(sprintf("  wrote %-46s %d sheets\n", "Supplement_File_S1.xlsx", 2 + !is.null(recon)))
}

cat(sprintf("\n  functions defined      : %d across %d files\n",
            nrow(fn), length(unique(fn$file))))
cat(sprintf("    in live modules      : %d\n", sum(fn$file_status == "live")))
cat(sprintf("    deprecated           : %d\n", sum(fn$file_status == "deprecated")))
cat(sprintf("    present, not sourced : %d\n", sum(fn$file_status == "present_but_not_sourced")))
cat(sprintf("    documented           : %d of %d\n", sum(fn$documented), nrow(fn)))
cat(sprintf("  dependencies           : %d CRAN, %d GitHub\n",
            sum(pk$source == "CRAN"), sum(pk$source == "GitHub")))
cat(sprintf("    listed but uncalled  : %d\n", sum(!pk$referenced_in_code)))
if (!is.null(recon)) {
  bad <- sum(!grepl("^OK", recon$verdict))
  cat(sprintf("  old S1 entries wrong   : %d of %d\n", bad, nrow(recon)))
}
cat("\n")
