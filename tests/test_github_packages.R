#!/usr/bin/env Rscript
# GitHub-only packages are named in ONE place, GITHUB_PACKAGES in config.R.
#
# The first container build (2026-09-23) failed because the Dockerfile, the
# README and three error messages named jeffreyhanson/ecoregions, which does not
# exist; production had always used tomroh/ecoregions. feowR was named two ways
# (mhpob/feowR, which does not exist, and brunomioto/feowR). This test keeps
# every mention in step with config.R, and the image pinned to the commit
# production uses.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)
suppressWarnings(suppressMessages({ source("config.R"); source("R/00_helpers.R") }))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
cat("[test_github_packages]\n")

spec <- GITHUB_PACKAGES
repo <- sub("@.*$", "", spec)
ok(all(grepl("^[A-Za-z0-9-]+/[A-Za-z0-9._-]+(@[0-9a-f]{40})?$", spec)),
   "every GITHUB_PACKAGES entry is owner/repo, optionally @ a full commit hash")
ok(all(grepl("@[0-9a-f]{40}$", spec[GITHUB_PACKAGES_REQUIRED])),
   "required GitHub packages are pinned to a commit")
ok(identical(unname(repo["ecoregions"]), "tomroh/ecoregions") &&
   identical(unname(repo["feowR"]), "brunomioto/feowR"),
   "the repositories that exist: tomroh/ecoregions, brunomioto/feowR")
ok(identical(github_install_hint("ecoregions"), sprintf('remotes::install_github("%s")', spec[["ecoregions"]])),
   "error messages build the install command from GITHUB_PACKAGES")

# Every install_github("...") literal in the sources must be a configured spec
# (or its repository). The Dockerfile and README are not in the container image,
# so they are checked only where present.
files <- c(list.files("R", pattern = "\\.R$", full.names = TRUE),
           intersect(c("Dockerfile", "README.md", "RUNBOOK.md", "checkcover_main.R"), list.files(".")))
files <- files[!grepl("DEPRECATED", files)]
lits <- unlist(lapply(files, function(f) {
  tx <- readLines(f, warn = FALSE, encoding = "UTF-8")
  m <- regmatches(tx, gregexpr("install_github\\([\"']([^\"']+)[\"']", tx))
  sub("^install_github\\([\"']([^\"']+)[\"']$", "\\1", unlist(m))
}))
stray <- setdiff(unique(lits), c(spec, repo))
ok(length(stray) == 0L,
   sprintf("no install_github() names a repository outside GITHUB_PACKAGES%s",
           if (length(stray)) paste0(": ", paste(stray, collapse = ", ")) else ""))
# Comments may record the history; no instruction or message may name them.
code_lines <- unlist(lapply(files, function(f) {
  tx <- readLines(f, warn = FALSE, encoding = "UTF-8"); tx[!grepl("^\\s*#", tx)]
}))
ok(!any(grepl("jeffreyhanson/ecoregions|mhpob/feowR", code_lines)),
   "the non-existent repositories appear in no code, message or instruction")

if (file.exists("Dockerfile")) {
  df <- paste(readLines("Dockerfile", warn = FALSE), collapse = "\n")
  ok(all(vapply(GITHUB_PACKAGES_REQUIRED, function(p)
           grepl(sprintf('install_github("%s"', spec[[p]]), df, fixed = TRUE), logical(1))),
     "the Dockerfile installs each required package exactly as configured (same pin)")
}

cat(sprintf("\n[test_github_packages] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
