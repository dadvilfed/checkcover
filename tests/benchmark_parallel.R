#!/usr/bin/env Rscript
#### PARALLELIZATION BENCHMARK SCRIPT ####
# Run this to compare sequential vs parallel performance
# Usage: Rscript benchmark_parallel.R

cat("\n")
cat("==============================================================\n")
cat("   cheCkOVER PARALLELIZATION BENCHMARK                        \n")
cat("==============================================================\n\n")

# Cross-platform RAM detection
get_system_ram <- function() {
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Windows: use wmic
      ram_info <- system("wmic ComputerSystem get TotalPhysicalMemory /value", intern = TRUE)
      ram_line <- grep("TotalPhysicalMemory", ram_info, value = TRUE)
      ram_bytes <- as.numeric(gsub("[^0-9]", "", ram_line))
      return(round(ram_bytes / 1024^3, 1))
    } else {
      # Linux/macOS
      if (file.exists("/proc/meminfo")) {
        # Linux
        meminfo <- readLines("/proc/meminfo", n = 1)
        ram_kb <- as.numeric(gsub("[^0-9]", "", meminfo))
        return(round(ram_kb / 1024^2, 1))
      } else {
        # macOS
        ram_bytes <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
        return(round(ram_bytes / 1024^3, 1))
      }
    }
  }, error = function(e) {
    return(NA)
  })
}

# Check system info
cat("System Information:\n")
cat("  OS:", .Platform$OS.type, "-", Sys.info()["sysname"], "\n")
cat("  Cores (physical):", parallel::detectCores(logical = FALSE), "\n")
cat("  Cores (logical):", parallel::detectCores(logical = TRUE), "\n")

ram_gb <- get_system_ram()
if (!is.na(ram_gb)) {
  cat("  RAM:", ram_gb, "GB\n")
} else {
  cat("  RAM: (unable to detect)\n")
}
cat("\n")

# Simple benchmark comparing parallel_lapply
cat("\n")
cat("==============================================================\n")
cat("   QUICK PARALLEL TEST                                        \n")
cat("==============================================================\n")

library(future)
library(future.apply)

# Test function (simulate species processing)
test_fun <- function(x) {
  # Simulate some work (CPU-bound task)
  Sys.sleep(0.1)
  sum(rnorm(10000))
}

n_items <- 50
test_data <- 1:n_items

cat("\nTesting with", n_items, "items...\n\n")

# Sequential
plan(sequential)
cat("Running sequential test...\n")
t1 <- system.time({
  res_seq <- lapply(test_data, test_fun)
})
cat(sprintf("Sequential: %.2f seconds\n", t1["elapsed"]))

# Parallel (multisession for Windows compatibility)
n_workers <- max(1, parallel::detectCores(logical = FALSE) - 1)

cat(sprintf("\nStarting parallel test with %d workers...\n", n_workers))
cat("(This may take a moment to initialize workers on Windows)\n\n")

plan(multisession, workers = n_workers)

t2 <- system.time({
  res_par <- future_lapply(test_data, test_fun, future.seed = TRUE)
})
cat(sprintf("Parallel (%d workers): %.2f seconds\n", n_workers, t2["elapsed"]))

# Calculate speedup
speedup <- t1["elapsed"] / t2["elapsed"]
efficiency <- speedup / n_workers * 100

cat("\n")
cat("==============================================================\n")
cat("   RESULTS                                                    \n")
cat("==============================================================\n")
cat("\n")
cat(sprintf("Sequential time:     %.2f seconds\n", t1["elapsed"]))
cat(sprintf("Parallel time:       %.2f seconds\n", t2["elapsed"]))
cat(sprintf("Speedup:             %.2fx\n", speedup))
cat(sprintf("Efficiency:          %.1f%%\n", efficiency))
cat("\n")

if (speedup > 1.5) {
  cat(">> Parallelization is effective on this system!\n")
  cat("   Recommendation: Enable parallel processing in config.R\n")
} else if (speedup > 1.1) {
  cat("~~ Moderate parallelization benefit.\n")
  cat("   Recommendation: Test with your actual dataset\n")
} else {
  cat("!! Limited parallelization benefit.\n")
  cat("   This is common on Windows due to multisession overhead.\n")
  cat("   Recommendation: Try on Linux server for better results\n")
}

cat("\n")

# Cleanup
plan(sequential)

cat("\n")
cat("==============================================================\n")
cat("   RECOMMENDED CONFIG SETTINGS                                \n")
cat("==============================================================\n")
cat("\n")

if (.Platform$OS.type == "windows") {
  cat("For Windows (your current system):\n")
  cat("  # Windows uses 'multisession' which has more overhead\n")
  cat("  # Best for longer-running tasks (>1 sec per item)\n")
  cat("\n")
  cat("  parallel$force_sequential = FALSE\n")
  cat("  parallel$workers =", min(4, n_workers), "\n")
  cat("  parallel$parallel_branches = TRUE   # Good speedup\n")
  cat("  parallel$parallel_maps = FALSE      # May not help much\n")
  cat("  memory$max_worker_memory = 1500\n")
  cat("\n")
  cat("Note: On Windows, parallel branch execution (running\n")
  cat("Indigenous and Non-indigenous simultaneously) gives the\n")
  cat("best speedup. Per-species parallelization has more overhead.\n")
} else {
  cat("For Linux/macOS (multicore backend - most efficient):\n")
  cat("  parallel$force_sequential = FALSE\n")
  cat("  parallel$workers = \"auto\"  # or", n_workers, "\n")
  cat("  parallel$parallel_branches = TRUE\n")
  cat("  parallel$parallel_maps = TRUE\n")
  cat("  memory$max_worker_memory = 2000\n")
}

cat("\n")
cat("==============================================================\n")
cat("   BRANCH-LEVEL PARALLELIZATION TEST                          \n")
cat("==============================================================\n")
cat("\n")
cat("Simulating Branch A + B parallel execution...\n\n")

# Simulate branch execution
simulate_branch <- function(name, duration) {
  cat(sprintf("  %s started\n", name))
  Sys.sleep(duration)
  cat(sprintf("  %s completed (%.1f sec)\n", name, duration))
  return(list(name = name, duration = duration))
}

branch_a_time <- 2.0  # Simulated indigenous processing time
branch_b_time <- 1.5  # Simulated non-indigenous processing time

# Sequential
cat("Sequential execution:\n")
t_seq <- system.time({
  plan(sequential)
  simulate_branch("Branch A (Indigenous)", branch_a_time)
  simulate_branch("Branch B (Non-indigenous)", branch_b_time)
})
cat(sprintf("  Total: %.2f seconds\n\n", t_seq["elapsed"]))

# Parallel
cat("Parallel execution:\n")
plan(multisession, workers = 2)
t_par <- system.time({
  f_a <- future({ Sys.sleep(branch_a_time); "A done" })
  f_b <- future({ Sys.sleep(branch_b_time); "B done" })
  value(f_a)
  value(f_b)
})
cat(sprintf("  Branch A + B ran simultaneously\n"))
cat(sprintf("  Total: %.2f seconds\n\n", t_par["elapsed"]))

branch_speedup <- t_seq["elapsed"] / t_par["elapsed"]
cat(sprintf("Branch-level speedup: %.2fx\n", branch_speedup))
cat("\n")

if (branch_speedup > 1.3) {
  cat(">> Branch-level parallelization works well!\n")
  cat("   This is the recommended approach for Windows.\n")
} else {
  cat("~~ Branch-level parallelization shows modest improvement.\n")
}

# Final cleanup
plan(sequential)

cat("\n")
cat("Done!\n\n")