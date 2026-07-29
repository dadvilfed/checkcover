#!/usr/bin/env Rscript
# Regression: a zero-active species must skip ONLY itself during map generation.
#
# Module 8 uses return(NULL) to mean "skip this species". Inside a bare
# tryCatch({...}) that return() exits generate_all_maps() entirely — so the first
# fully-extirpated species (Cambarus veitchorum in v1.1) aborted the whole loop,
# every later species lost its maps, and Module 9 received all_maps = NULL, which
# is why NO v1.1 package had a maps/ folder (2026-07). The body must therefore be
# an immediately-invoked function so return(NULL) is scoped to one species.

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

species <- c("A", "B_zero_active", "C", "D")

# The pattern Module 8 now uses.
loop_with_iife <- function() {
  out <- list()
  for (sp in species) {
    m <- tryCatch((function() {
      if (sp == "B_zero_active") return(NULL)
      paste0(sp, "_maps")
    })(), error = function(e) NULL)
    if (!is.null(m)) out[[sp]] <- m
  }
  out
}

# The old (broken) pattern, kept to prove the difference is real.
loop_bare_trycatch <- function() {
  out <- list()
  for (sp in species) {
    m <- tryCatch({
      if (sp == "B_zero_active") return(NULL)
      paste0(sp, "_maps")
    }, error = function(e) NULL)
    if (!is.null(m)) out[[sp]] <- m
  }
  out
}

fixed  <- loop_with_iife()
broken <- loop_bare_trycatch()

semantics_ok <- setequal(names(fixed), c("A", "C", "D")) &&
                (is.null(broken) || length(broken) < 3)

# Static guard: 08_maps.R must keep the IIFE wrapper.
src <- paste(readLines("R/08_maps.R", warn = FALSE), collapse = "\n")
wrapper_ok <- grepl("tryCatch((function()", src, fixed = TRUE) &&
              grepl("})(), error = function(e)", src, fixed = TRUE)

pass <- semantics_ok && wrapper_ok
cat(sprintf("[test_maps_skip_semantics] fixed=%s broken=%s wrapper_present=%s -> %s\n",
            paste(names(fixed), collapse = ","),
            if (is.null(broken)) "NULL(aborted)" else paste(names(broken), collapse = ","),
            wrapper_ok, if (pass) "PASS" else "FAIL"))
quit(status = if (pass) 0 else 1)
