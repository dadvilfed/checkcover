#!/usr/bin/env Rscript
# Every function the pipeline uses is defined ONCE, and every call reaches a
# definition that accepts its arguments.
#
# The first full run of the clean 1.0 (2026-09-23) died in Module 3C:
#   unused argument (fallback = "unnamed")
# R/04_reports.R still defined an old resolve_basin_names(basin_codes,
# hb_lookup). Sourced after 00_helpers.R, it silently replaced the unified
# resolver, so the call in 03c reached a function without `fallback`. No test
# saw it: the report modules need sf, which the suite avoids, so no test ever
# loaded 04_reports.R. The same scan found eleven more names defined twice.
#
# This test needs nothing but base R. It PARSES (never runs) the files
# checkcover_main.R sources, in its order, and checks:
#   1. no function name is defined at the top level of two files, or twice in
#      one (the later definition would silently win);
#   2. every call to one of those functions with a named argument names a
#      formal of the definition it reaches (or the function takes `...`).

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}
cat("[test_single_definitions]\n")

# ---- the files checkcover_main.R sources, in order ----
main  <- readLines("checkcover_main.R", warn = FALSE)
direct <- gsub('source\\("|"\\)', "", unlist(regmatches(main, gregexpr('source\\("[^"]+"\\)', main))))
m0 <- grep("^module_files <- c\\(", main)
m1 <- m0 + which(grepl("^\\)", main[(m0 + 1):length(main)]))[1]
mods <- gsub('"', "", unlist(regmatches(main[m0:m1], gregexpr('"R/[^"]+\\.R"', main[m0:m1]))))
files <- unique(c(direct, mods, "checkcover_main.R"))
files <- files[file.exists(files)]
ok(length(mods) >= 25 && all(c("R/00_helpers.R", "R/04_reports.R", "R/12_temporal_outputs.R") %in% files),
   sprintf("found the %d files the pipeline sources", length(files)))

is_fun_def <- function(e) {
  is.call(e) && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
    is.name(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
}
exprs <- lapply(files, function(f) parse(f, keep.source = FALSE))
names(exprs) <- files

defs <- do.call(rbind, lapply(files, function(f) {
  d <- Filter(is_fun_def, as.list(exprs[[f]]))
  if (!length(d)) return(NULL)
  data.frame(fun = vapply(d, function(e) as.character(e[[2]]), ""), file = f,
             stringsAsFactors = FALSE)
}))
dups <- unique(defs$fun[duplicated(defs$fun)])
ok(length(dups) == 0L,
   if (!length(dups)) sprintf("each of the %d top-level functions is defined once", nrow(defs))
   else paste0("defined more than once (the later copy silently wins): ",
               paste(sprintf("%s [%s]", dups, vapply(dups, function(d)
                 paste(defs$file[defs$fun == d], collapse = ", "), "")), collapse = "; ")))

# ---- named arguments reach a definition that accepts them ----
formals_of <- list()
for (f in files) for (e in as.list(exprs[[f]])) if (is_fun_def(e)) {
  formals_of[[as.character(e[[2]])]] <- names(as.list(e[[3]][[2]]))
}
# Names also bound locally somewhere (inside a function) may be local closures:
# skip them rather than guess which binding a call reaches.
# Parts of a call or pairlist, without empty arguments (the `` in x[, 1]).
parts <- function(e) {
  el <- as.list(e)
  # An empty argument is the zero-length symbol; test it by index with
  # primitives, since binding it to a variable is itself an error.
  empty <- vapply(seq_along(el), function(i)
    is.symbol(el[[i]]) && !nzchar(as.character(el[[i]])), logical(1))
  el[!empty]
}
local_names <- character(0)
collect_locals <- function(e, depth) {
  if (is.call(e)) {
    if (depth > 0 && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) && is.name(e[[2]]))
      local_names <<- c(local_names, as.character(e[[2]]))
    d <- if (identical(e[[1]], as.name("function"))) depth + 1 else depth
    for (x in parts(e)[-1]) if (!is.null(x)) collect_locals(x, d)
  } else if (is.pairlist(e)) for (x in parts(e)) if (!is.null(x)) collect_locals(x, depth)
}
for (f in files) for (e in as.list(exprs[[f]])) collect_locals(e, 0)
checkable <- setdiff(names(formals_of), unique(local_names))

bad <- character(0)
walk <- function(e, where) {
  if (is.call(e)) {
    h <- e[[1]]
    if (is.name(h) && as.character(h) %in% checkable) {
      fm <- formals_of[[as.character(h)]]
      an <- names(as.list(e))[-1]
      an <- an[!is.na(an) & nzchar(an)]
      if (!"..." %in% fm) for (a in an) {
        if (!any(startsWith(fm, a))) bad <<- c(bad, sprintf("%s: %s(%s = ...) but %s() has no such argument",
                                                          where, as.character(h), a, as.character(h)))
      }
    }
    for (x in parts(e)) if (!is.null(x)) walk(x, where)
  } else if (is.pairlist(e)) for (x in parts(e)) if (!is.null(x)) walk(x, where)
}
for (f in files) for (e in as.list(exprs[[f]])) walk(e, f)
bad <- unique(bad)
ok(length(bad) == 0L,
   if (!length(bad)) sprintf("every named argument in a call to one of %d pipeline functions is accepted", length(checkable))
   else paste(c("calls with arguments the reached definition does not accept:", head(bad, 15)), collapse = "\n        "))

# ---- the call that killed the first 1.0 run ----
ok(identical(defs$file[defs$fun == "resolve_basin_names"], "R/00_helpers.R") &&
   "fallback" %in% formals_of[["resolve_basin_names"]],
   "resolve_basin_names() is defined once, in 00_helpers.R, and takes `fallback`")

cat(sprintf("\n[test_single_definitions] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
