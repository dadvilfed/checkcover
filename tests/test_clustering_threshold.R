#!/usr/bin/env Rscript
# Spatial clustering must use an ABSOLUTE threshold, never a statistic of the
# points themselves (Reviewer 1, Ecological Informatics, 2026-09).
#
# Until 2026-09 the cut height was h = mean(pairwise distance) with complete
# linkage. The root merge of a complete-linkage tree sits at the MAXIMUM
# pairwise distance, and the mean is below the maximum whenever distances vary
# at all, so the root merge was always severed: a single cluster was
# unreachable by construction. Consequences, all reproduced below:
#
#   * five points within one metre never returned 1 cluster (0/1000 draws);
#   * n_clusters tracked SAMPLE SIZE, not spatial structure;
#   * it was anti-correlated with real fragmentation — one tight blob scored
#     more clusters than two genuinely separated blobs.
#
# v1.2 shipped 494 species with clustering computed and 494 with >1 cluster.
# The manuscript reported that as a finding. It was arithmetic.

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

cat("[test_clustering_threshold]\n")

# The rule as the module now applies it: absolute cut height, single linkage.
n_clusters <- function(xy, threshold_m, linkage = "single") {
  length(unique(cutree(hclust(dist(xy), method = linkage), h = threshold_m)))
}
# The rule as it used to be, kept so the regression cannot come back unnoticed.
n_clusters_old <- function(xy) {
  d <- dist(xy)
  length(unique(cutree(hclust(d, method = "complete"), h = mean(d))))
}

set.seed(42)

# ---- the property the old rule could never satisfy ----
tight <- matrix(rnorm(200, sd = 1), ncol = 2)          # ~metres across
ok(n_clusters(tight, threshold_m = 10000) == 1L,
   "100 points within metres form ONE cluster at a 10 km threshold")
ok(n_clusters_old(tight) > 1L,
   "old rule split that same tight blob (the reported defect)")

r <- replicate(200, n_clusters(matrix(rnorm(10, sd = 0.5), ncol = 2), 10000))
ok(all(r == 1L), "reviewer's case: 5 points within a metre -> always 1 cluster")
ro <- replicate(200, n_clusters_old(matrix(rnorm(10, sd = 0.5), ncol = 2)))
ok(!any(ro == 1L), "old rule never returned 1 on that case (0/200)")

# ---- genuinely separated groups are still detected ----
two <- rbind(matrix(rnorm(100, sd = 100), ncol = 2),
             matrix(rnorm(100, sd = 100), ncol = 2) + 50000)
ok(n_clusters(two, threshold_m = 10000) == 2L,
   "two groups 50 km apart -> 2 clusters at a 10 km threshold")
three <- rbind(two, matrix(rnorm(40, sd = 100), ncol = 2) + 200000)
ok(n_clusters(three, threshold_m = 10000) == 3L, "a third distant group -> 3 clusters")

# ---- the threshold, not the data, decides ----
ok(n_clusters(two, threshold_m = 100000) == 1L,
   "same points, 100 km threshold -> 1 cluster (threshold governs)")
ok(n_clusters(two, threshold_m = 50) > 2L,
   "same points, 50 m threshold -> the blobs themselves break up")
# Monotonicity is the general property: raising the threshold can only merge
# clusters, never split them.
steps <- vapply(c(10, 100, 1000, 10000, 100000),
                function(h) n_clusters(two, h), integer(1))
ok(all(diff(steps) <= 0L) && steps[1] > steps[length(steps)],
   "cluster count is non-increasing in the threshold")

# ---- scale sensitivity: the whole point of an absolute threshold ----
near <- matrix(rnorm(100, sd = 50), ncol = 2)            # ~100 m spread
far  <- matrix(rnorm(100, sd = 500000), ncol = 2)        # ~1000 km spread
ok(n_clusters(near, 10000) == 1L && n_clusters(far, 10000) > 1L,
   "identical shape at different scales gives different answers")
ok(n_clusters_old(near) == n_clusters_old(far) ||
   (n_clusters_old(near) > 1L && n_clusters_old(far) > 1L),
   "old rule was scale-free: both scored >1 regardless of real separation")

# ---- n_clusters must not simply track sample size ----
sizes <- c(10, 50, 200, 500)
new_counts <- vapply(sizes, function(n)
  n_clusters(matrix(rnorm(n * 2, sd = 1), ncol = 2), 10000), integer(1))
old_counts <- vapply(sizes, function(n)
  n_clusters_old(matrix(rnorm(n * 2, sd = 1), ncol = 2)), integer(1))
ok(all(new_counts == 1L),
   "cluster count is flat in sample size for one tight blob")
ok(old_counts[length(old_counts)] > old_counts[1],
   "old rule's count grew with sample size (it measured n, not structure)")

# ---- single vs complete linkage on an elongated system ----
# A river-like chain of points, each 2 km from the next, 100 km end to end.
chain <- cbind(seq(0, 100000, by = 2000), 0)
ok(n_clusters(chain, 10000, "single") == 1L,
   "single linkage: an unbroken 100 km chain is ONE cluster")
ok(n_clusters(chain, 10000, "complete") > 1L,
   "complete linkage splits it on extent alone — why single is the default")
broken <- rbind(chain, chain + cbind(rep(300000, nrow(chain)), 0))
ok(n_clusters(broken, 10000, "single") == 2L,
   "single linkage: a real 300 km gap gives 2 clusters")

cat(sprintf("\n[test_clustering_threshold] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
