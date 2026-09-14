#!/usr/bin/env Rscript
# Basin-aware spatial clustering (Lucian, 2026-09).
#
# "Two populations 5 km apart in separate catchments are functionally more
#  disconnected than two populations 50 km apart along the same river. A purely
#  spatial threshold cannot express that."
#
# Rule implemented:
#   * records sharing a HydroBASINS unit are connected regardless of distance
#   * records in different units are separate unless within threshold_km
#
# Mechanically: same-basin pairs get a pairwise distance of 0, then a SINGLE
# linkage tree is cut at the threshold. Single linkage merges on any pair within
# the cut height, so the result is exactly the connected components of
# "same basin OR within threshold". Complete linkage would NOT give this.

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

# The rule as the module applies it, on a plain coordinate matrix in metres.
cluster <- function(xy, basins = NULL, threshold_m = 10000, linkage = "single") {
  d <- as.matrix(dist(xy))
  if (!is.null(basins)) {
    sets <- strsplit(ifelse(is.na(basins), "", basins), "\\s*\\|\\s*")
    sets <- lapply(sets, function(s) s[nzchar(s)])
    for (u in unique(unlist(sets))) {
      ix <- which(vapply(sets, function(s) u %in% s, logical(1)))
      if (length(ix) > 1L) d[ix, ix] <- 0
    }
  }
  length(unique(cutree(hclust(as.dist(d), method = linkage), h = threshold_m)))
}

cat("[test_basin_clustering]\n")

# ---- Lucian's scenario, both halves ----
# A: two groups 5 km apart in DIFFERENT basins -> must stay separate.
near_diff <- rbind(matrix(rnorm(20, sd = 50), ncol = 2),
                   matrix(rnorm(20, sd = 50), ncol = 2) + 5000)
b_near    <- c(rep("L10:1", 10), rep("L10:2", 10))
ok(cluster(near_diff, b_near, threshold_m = 1000) == 2,
   "5 km apart in different basins -> 2 clusters")

# B: two groups 50 km apart in the SAME basin -> must be one.
far_same <- rbind(matrix(rnorm(20, sd = 50), ncol = 2),
                  matrix(rnorm(20, sd = 50), ncol = 2) + 50000)
b_far    <- rep("L10:7", 20)
ok(cluster(far_same, b_far, threshold_m = 1000) == 1,
   "50 km apart in the same basin -> 1 cluster (distance is irrelevant)")

# The distance-only metric gets both of these backwards.
ok(cluster(near_diff, NULL, threshold_m = 1000) == 2 &&
   cluster(far_same,  NULL, threshold_m = 1000) == 2,
   "euclidean-only cannot distinguish the two cases (both 2)")

# ---- the distance rule still links different basins when close ----
ok(cluster(near_diff, b_near, threshold_m = 10000) == 1,
   "different basins within the threshold ARE linked (explicit distance rule)")

# ---- single cluster is reachable, which it never was before v1.3 ----
one_basin <- matrix(rnorm(100, sd = 1000), ncol = 2)
ok(cluster(one_basin, rep("L10:1", 50), threshold_m = 10000) == 1,
   "a species confined to one basin resolves to a single cluster")

old_rule <- function(xy) {
  d <- dist(xy); length(unique(cutree(hclust(d, "complete"), h = mean(d))))
}
ok(old_rule(one_basin) > 1,
   "the pre-v1.3 rule split that same species (single cluster was impossible)")

# ---- multi-basin records: overlapping sets chain together ----
# Record 2 sits in both basins, so it bridges them into one cluster.
bridge_xy <- matrix(c(0,0, 100000,0, 200000,0), ncol = 2, byrow = TRUE)
ok(cluster(bridge_xy, c("L10:A", "L10:A | L10:B", "L10:B"), threshold_m = 1000) == 1,
   "a record in two basins bridges them into one cluster")
ok(cluster(bridge_xy, c("L10:A", "L10:A", "L10:B"), threshold_m = 1000) == 2,
   "without the bridging record they stay separate")

# ---- records with no basin assignment fall back to distance only ----
ok(cluster(bridge_xy, c("L10:A", NA, "L10:B"), threshold_m = 1000) == 3,
   "an unassigned record is connected only by distance")
ok(cluster(bridge_xy, c("L10:A", NA, "L10:A"), threshold_m = 1000) == 2,
   "the two assigned records still join via their shared basin")

# ---- linkage matters: the trick requires single ----
# Where the two diverge is CHAINING. With a record bridging two basins, the
# pairwise distances are d(1,2)=0 and d(2,3)=0 but d(1,3) is large. Single
# linkage merges along that chain into one cluster, which is the intended
# reading of "connected". Complete linkage requires every pair in a cluster to
# be within the cut height, so it refuses the merge and splits a genuinely
# connected system. The same-basin-distance-zero construction therefore only
# yields connected components under single linkage.
ok(cluster(bridge_xy, c("L10:A", "L10:A | L10:B", "L10:B"),
           threshold_m = 1000, linkage = "single") == 1,
   "single linkage: a bridging record chains both basins into one cluster")
ok(cluster(bridge_xy, c("L10:A", "L10:A | L10:B", "L10:B"),
           threshold_m = 1000, linkage = "complete") > 1,
   "complete linkage refuses that merge (why single is required, not preferred)")
# And for a species wholly inside one basin the two agree, because every
# pairwise distance is zeroed.
ok(cluster(one_basin, rep("L10:1", 50), threshold_m = 10000, linkage = "complete") == 1,
   "wholly-one-basin species is a single cluster under either linkage")

# ---- threshold still governs the non-basin part ----
steps <- vapply(c(100, 1000, 10000, 100000),
                function(h) cluster(near_diff, b_near, threshold_m = h), integer(1))
ok(all(diff(steps) <= 0L), "cluster count is non-increasing in the threshold")
ok(steps[1] == 2 && steps[length(steps)] == 1,
   "tight threshold keeps basins apart; wide threshold merges them")

cat(sprintf("\n[test_basin_clustering] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
