#!/usr/bin/env Rscript
# Run every tests/test_*.R in its own R process (isolation avoids cross-test
# state and keeps a crashy optional dep from taking down the whole suite), then
# print a summary. Exits non-zero if any test fails.
#
#   Rscript tests/run_all.R

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
tests   <- sort(list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE))

cat(sprintf("Running %d test file(s) with %s\n\n", length(tests), rscript))
results <- data.frame(test = character(), status = integer(), stringsAsFactors = FALSE)

for (t in tests) {
  cat(sprintf("── %s ──\n", basename(t)))
  out <- suppressWarnings(system2(rscript, shQuote(t), stdout = TRUE, stderr = TRUE))
  st  <- attr(out, "status"); if (is.null(st)) st <- 0L
  # Show only the summary/FAIL lines to keep output readable.
  keep <- grep("PASS|FAIL|passed|failed|Error|cons\\*|integ\\*", out, value = TRUE)
  if (length(keep)) cat(paste0("   ", keep), sep = "\n")
  cat(sprintf("   -> exit %d\n\n", st))
  results <- rbind(results, data.frame(test = basename(t), status = st))
}

n_fail <- sum(results$status != 0)
cat("================ SUMMARY ================\n")
for (i in seq_len(nrow(results)))
  cat(sprintf("  %-32s %s\n", results$test[i], if (results$status[i] == 0) "PASS" else "FAIL"))
cat(sprintf("========================================\n%d/%d passed\n",
            nrow(results) - n_fail, nrow(results)))
quit(status = if (n_fail > 0) 1 else 0)
