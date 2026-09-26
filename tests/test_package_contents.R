#!/usr/bin/env Rscript
# The cosmetic defects found reviewing 1.0 (2026-09-26), fixed for the next tag:
#   - WDPA site names with C1 control characters in place of punctuation
#   - a double space where an optional narrative phrase was empty (95 packages)
#   - READMEs listing EOO layers the package does not have (89 packages)
#   - file_manifest.csv with the server's absolute paths, and no README.md

.root <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(getwd())
})
setwd(.root)
for (p in c("stringi")) if (!requireNamespace(p, quietly = TRUE)) {
  cat(sprintf("[test_package_contents] SKIP (%s not installed)\n", p)); quit(status = 0)
}
suppressWarnings(suppressMessages({ source("R/00_helpers.R"); source("R/09_package_export.R") }))

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  PASS  %s\n", what)) }
  else              { fail <<- fail + 1L; cat(sprintf("  FAIL  %s\n", what)) }
}

# ---- C1 control characters ----
raw_name <- paste0("Bodrogzug", intToUtf8(0x96), "Kopasz-hegy", intToUtf8(0x96), "Takt", intToUtf8(0xe1), "k", intToUtf8(0xf6), "z")
ok(identical(fix_c1_controls(raw_name),
             paste0("Bodrogzug–Kopasz-hegy–Taktáköz")), "U+0096 becomes an en dash; letters kept")
ok(identical(fix_c1_controls(paste0("Serra de A", intToUtf8(0x81), "gua")), "Serra de Agua"),
   "a byte Windows-1252 leaves undefined is dropped")
ok(identical(fix_c1_controls(paste0("Smith", intToUtf8(0x92), "s")), "Smith’s"), "U+0092 becomes a right quote")
clean <- c("Parcul Natural Apuseni", "Șara Hațegului", NA)
ok(identical(fix_c1_controls(clean), clean), "clean names and NA are left alone")
ok(!any(grepl("[\\x{80}-\\x{9f}]", fix_c1_controls(raw_name), perl = TRUE)), "no C1 character survives")
wdpa <- paste(readLines("R/02e_wdpa.R", warn = FALSE), collapse = "\n")
ok(grepl("fix_c1_controls(c(n_poly, n_pts))", wdpa, fixed = TRUE), "WDPA names pass through fix_c1_controls()")

# ---- one space between sentences ----
ok(identical(join_sentences("Spans 2 basins.", "", "Records span 1990–2020."),
             "Spans 2 basins. Records span 1990–2020."), "an empty phrase leaves no double space")
ok(identical(join_sentences("A. ", NA, " B."), "A. B."), "NA and edge spaces are dropped")
narr <- paste(readLines("R/10_canonical_narratives.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok(grepl('freshwater ecoregion(s)."),\n    metrics_phrase, temporal_phrase', narr, fixed = TRUE),
   "the native-range paragraph joins its phrases with join_sentences()")

# ---- README and file manifest describe what the package holds ----
tmp <- file.path(tempdir(), "pkg"); id <- "Cambarus_sheltae"; dir <- file.path(tmp, id)
for (d in c("maps", "narratives", "citations")) dir.create(file.path(dir, d), recursive = TRUE, showWarnings = FALSE)
mk <- function(...) { p <- file.path(dir, ...); writeLines("x", p); p }
files <- c(mk("maps", paste0(id, "_AOO.geojson")), mk("maps", paste0(id, "_AOO.kml")),
           mk("maps", paste0(id, "_basins.geojson")), mk("maps", paste0(id, "_basins.kml")),
           mk("narratives", paste0(id, "_canonical.md")), mk("narratives", paste0(id, "_narrative.json")),
           mk("narratives", paste0(id, "_narrative.txt")),
           mk("citations", paste0(id, "_bibliography.json")), mk("citations", paste0(id, "_CITATION.cff")),
           mk("package_metadata.json"), mk("README.md"))
tree <- .package_tree(dir, id)
ok(!any(grepl("EOO", tree)), "a package without an EOO layer lists none")
ok(any(grepl("\\*_canonical\\.md$", tree)), "the canonical narrative is listed")
ok(identical(tail(tree, 1), "└── README.md") && tree[1] == paste0(id, "/"), "the tree runs from the id to README.md")
ok(identical(.package_map_kinds(list.files(file.path(dir, "maps"))), "AOO, HydroBASINS"), "map kinds: AOO, HydroBASINS")
ok(identical(.package_formats(list.files(file.path(dir, "narratives"))), "Markdown, text, JSON"), "narrative formats named")
fm <- .package_file_manifest(files, id)
ok(identical(fm$filepath[1], paste0("maps/", id, "_AOO.geojson")) && !any(startsWith(fm$filepath, "/")),
   "file paths are relative to the package folder")
ok("README.md" %in% fm$filepath, "README.md is listed")
ok(identical(fm$md5, unname(tools::md5sum(files))), "md5 of every file")
exp <- paste(readLines("R/09_package_export.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok(regexpr("readme_file <- file.path", exp) < regexpr("manifest_file <- file.path", exp) &&
   grepl("package_files$readme <- readme_file", exp, fixed = TRUE),
   "the README is written first and enters the manifest")

unlink(tmp, recursive = TRUE)
cat(sprintf("\n[test_package_contents] %d passed, %d failed\n", pass, fail))
quit(status = if (fail > 0) 1 else 0)
