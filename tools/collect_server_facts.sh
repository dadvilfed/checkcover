#!/usr/bin/env bash
# ============================================================================
# cheCkOVER — collect UVT server facts for the World of Crayfish service setup
# ============================================================================
# Answers the machine questions in Lucian's "UVT Server Setup" document
# (sections 01, 06 and 07; questions Q1, Q6, Q7) in one report file.
#
# READ-ONLY. It installs nothing, needs no sudo, and writes exactly one file:
# the report, in the current directory. It prints no coordinates, no record
# data, no environment-variable values and no credentials — only versions,
# sizes, counts, hashes and yes/no facts. Safe to send as it is.
#
# Usage, from the cheCkOVER checkout on the server:
#   bash tools/collect_server_facts.sh            # checkout = this repo
#   bash tools/collect_server_facts.sh /path/to/checkout
#   bash tools/collect_server_facts.sh /path/to/checkout /path/to/output_root
#
# The output root defaults to <checkout>/checkover_output. Pass the second
# argument when the mirror of World of Crayfish revisions lives elsewhere.
# ============================================================================

set -u
ROOT="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
OUT_DIR="${2:-$ROOT/checkover_output}"
SPATIAL="$ROOT/spatial_data"
REPORT="$PWD/server_facts_$(hostname -s)_$(date +%Y%m%d_%H%M).txt"

exec > >(tee "$REPORT") 2>&1

sec()  { printf '\n==== %s ====\n' "$1"; }
fact() { printf '  %-34s %s\n' "$1" "$2"; }
have() { command -v "$1" >/dev/null 2>&1; }
try()  { "$@" 2>/dev/null | head -n 1; }

echo "cheCkOVER server facts"
fact "generated"  "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
fact "host"       "$(hostname -s)"
fact "user"       "$(id -un) (uid $(id -u))"
fact "checkout"   "$ROOT"
fact "output root" "$OUT_DIR"

# ---------------------------------------------------------------------------
sec "Q1a  OS, CPU, memory, disk"
fact "os"         "$(. /etc/os-release 2>/dev/null && echo "$PRETTY_NAME")"
fact "kernel"     "$(uname -r)"
fact "cpu cores"  "$(nproc 2>/dev/null)"
fact "memory"     "$(free -h 2>/dev/null | awk '/^Mem:/ {print $2" total, "$7" available"}')"
fact "swap"       "$(free -h 2>/dev/null | awk '/^Swap:/ {print $2}')"
for p in "$ROOT" "$OUT_DIR" "$SPATIAL" /tmp; do
  [ -e "$p" ] && fact "disk under $(basename "$p")" \
    "$(df -h "$p" 2>/dev/null | awk 'NR==2 {print $4" free of "$2" ("$5" used) on "$6}')"
done
fact "open-files limit (ulimit -n)" "$(ulimit -n)"

# ---------------------------------------------------------------------------
sec "Q1b  Containers (Docker / Podman, and whether this account can run them)"
if have docker; then
  fact "docker"            "$(try docker --version)"
  if docker info >/dev/null 2>&1; then
    fact "docker without sudo" "yes"
    fact "docker rootless"   "$(docker info --format '{{.SecurityOptions}}' 2>/dev/null | grep -q rootless && echo yes || echo no)"
  else
    fact "docker without sudo" "no (daemon not reachable as $(id -un))"
  fi
else
  fact "docker" "not installed"
fi
if have podman; then
  fact "podman"          "$(try podman --version)"
  fact "podman rootless" "$(podman info --format '{{.Host.Security.Rootless}}' 2>/dev/null || echo 'podman info failed')"
else
  fact "podman" "not installed"
fi
fact "in group docker"    "$(id -nG | tr ' ' '\n' | grep -qx docker && echo yes || echo no)"
fact "subuid range"       "$(grep -q "^$(id -un):" /etc/subuid 2>/dev/null && echo present || echo absent) (needed for rootless Podman)"

# ---------------------------------------------------------------------------
sec "Q1c  Outbound HTTPS to World of Crayfish"
if have curl; then
  res=$(curl -sS -o /dev/null -m 20 -w '%{http_code} %{time_total}' https://world.crayfish.ro/ 2>&1)
  fact "GET https://world.crayfish.ro/" "$(echo "$res" | awk 'NF==2 && $1 ~ /^[0-9]+$/ {printf "HTTP %s in %.2f s", $1, $2; next} {print}')"
else
  fact "curl" "not installed"
fi
for v in https_proxy HTTPS_PROXY http_proxy HTTP_PROXY no_proxy NO_PROXY; do
  # Names only: a proxy URL can embed a password.
  [ -n "${!v:-}" ] && fact "proxy variable set" "$v"
done
fact "inbound firewall (ufw)" "$(try ufw status 2>/dev/null || echo 'not readable without sudo')"

# ---------------------------------------------------------------------------
sec "Q1d  Long-lived service: systemd user unit or cron"
fact "systemd --user"     "$(systemctl --user is-system-running 2>/dev/null || echo unavailable)"
fact "linger enabled"     "$(loginctl show-user "$(id -un)" -p Linger --value 2>/dev/null || echo unknown) (yes = user services survive reboot without a login)"
fact "cron daemon"        "$(systemctl is-active cron 2>/dev/null || echo unknown)"
fact "crontab usable"     "$(crontab -l >/dev/null 2>&1 && echo yes || (have crontab && echo 'yes (empty)' || echo no))"

# ---------------------------------------------------------------------------
sec "R stack (for the host-vs-container comparison)"
if have Rscript; then
  (cd "$ROOT" && Rscript --vanilla - <<'REOF' 2>&1
cat(sprintf("  %-34s %s\n", "R", R.version.string))
if (requireNamespace("sf", quietly = TRUE)) {
  v <- sf::sf_extSoftVersion()
  for (k in c("GEOS", "GDAL", "proj.4", "PROJ")) if (!is.na(v[k])) cat(sprintf("  %-34s %s\n", k, v[k]))
}
pk <- character(0)
if (file.exists("config.R")) {
  e <- new.env(); try(sys.source("config.R", envir = e), silent = TRUE)
  pk <- c(get0("REQUIRED_PACKAGES", envir = e, ifnotfound = character(0)),
          names(get0("GITHUB_PACKAGES", envir = e, ifnotfound = character(0))))
}
missing <- character(0)
for (p in pk) {
  if (requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("  %-34s %s\n", paste0("pkg ", p), as.character(utils::packageVersion(p))))
  } else missing <- c(missing, p)
}
cat(sprintf("  %-34s %s\n", "packages missing", if (length(missing)) paste(missing, collapse = ", ") else "none"))
REOF
  )
else
  fact "Rscript" "not found on PATH"
fi

# ---------------------------------------------------------------------------
sec "Deployed code"
if [ -d "$ROOT/.git" ] && have git; then
  fact "commit"   "$(git -C "$ROOT" rev-parse --short HEAD 2>/dev/null)"
  fact "describe" "$(git -C "$ROOT" describe --tags --always --dirty 2>/dev/null)"
  fact "branch"   "$(git -C "$ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null)"
  fact "uncommitted changes" "$(git -C "$ROOT" status --porcelain 2>/dev/null | wc -l) file(s)"
else
  fact "git" "checkout is not a git repository (code version cannot be verified)"
fi

# ---------------------------------------------------------------------------
sec "Q7  Sizes"
if [ -d "$SPATIAL" ]; then
  fact "spatial_data total" "$(du -sh "$SPATIAL" 2>/dev/null | cut -f1)"
  for d in "$SPATIAL"/*/; do [ -d "$d" ] && fact "  $(basename "$d")" "$(du -sh "$d" 2>/dev/null | cut -f1)"; done
else
  fact "spatial_data" "not found at $SPATIAL"
fi
if [ -d "$OUT_DIR" ]; then
  fact "checkover_output total" "$(du -sh "$OUT_DIR" 2>/dev/null | cut -f1)"
  for d in "$OUT_DIR"/*/; do [ -d "$d" ] && fact "  $(basename "$d")/" "$(du -sh "$d" 2>/dev/null | cut -f1)"; done
  echo "  largest single packages (species folders, any revision):"
  find "$OUT_DIR" -mindepth 2 -maxdepth 2 -type d -path "$OUT_DIR/[0-9]*.[0-9]*/*" ! -name checkover \
    -exec du -sk {} + 2>/dev/null | sort -rn | head -n 5 | \
    awk -v root="$OUT_DIR/" '{ sub(root, "", $2); printf "    %8.1f MB  %s\n", $1/1024, $2 }'
  echo "  largest single files:"
  find "$OUT_DIR" -type f -printf '%s %P\n' 2>/dev/null | sort -rn | head -n 5 | \
    awk '{ printf "    %8.1f MB  %s\n", $1/1048576, $2 }'
else
  fact "checkover_output" "not found at $OUT_DIR"
fi

# ---------------------------------------------------------------------------
sec "Q6  The mirror: what is in checkover_output/, and is it unchanged"
if [ -d "$OUT_DIR" ] && have Rscript; then
  (cd "$ROOT" && OUT_DIR="$OUT_DIR" Rscript --vanilla - <<'REOF' 2>&1
out <- Sys.getenv("OUT_DIR")
top <- list.files(out, all.files = FALSE)
revs <- top[grepl("^[0-9]+\\.[0-9]+$", top) & dir.exists(file.path(out, top))]
cat(sprintf("  %-34s %s\n", "revision folders", if (length(revs)) paste(revs, collapse = ", ") else "none"))
cat(sprintf("  %-34s %s\n", "everything else at top level",
            if (length(setdiff(top, revs))) paste(setdiff(top, revs), collapse = ", ") else "nothing"))

aud <- "tests/audit_packages.R"
if (file.exists(aud)) suppressMessages(source(aud))   # CLI is guarded; only defines functions

for (r in revs) {
  rd <- file.path(out, r)
  spp <- list.dirs(rd, recursive = FALSE, full.names = FALSE)
  spp <- spp[file.exists(file.path(rd, spp, "file_manifest.csv"))]
  ok <- 0L; bad <- 0L; gone <- 0L
  for (s in spp) {
    m <- tryCatch(utils::read.csv(file.path(rd, s, "file_manifest.csv"), stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !all(c("filepath", "md5") %in% names(m))) next
    # filepath is recorded relative to the checkout it was produced in; locate
    # the file by its path INSIDE the species folder, so a moved tree still checks.
    rel <- sub(paste0("^.*?/", r, "/", s, "/"), "", m$filepath, perl = TRUE)
    f <- file.path(rd, s, rel)
    ex <- file.exists(f)
    gone <- gone + sum(!ex)
    h <- unname(tools::md5sum(f[ex]))
    ok  <- ok  + sum(h == m$md5[ex])
    bad <- bad + sum(h != m$md5[ex])
  }
  cat(sprintf("\n  revision %s\n", r))
  cat(sprintf("    %-32s %d\n", "species packages", length(spp)))
  cat(sprintf("    %-32s %d match, %d differ, %d missing\n", "files vs file_manifest md5", ok, bad, gone))
  leak <- file.path(rd, "checkover", "clean_occurrences.tsv")
  cat(sprintf("    %-32s %s\n", "checkover/clean_occurrences.tsv",
              if (file.exists(leak)) sprintf("PRESENT (%.0f MB) - coordinates; must go", file.info(leak)$size / 1e6)
              else "absent"))
  cat(sprintf("    %-32s %s\n", "_audit_report.json",
              if (file.exists(file.path(rd, "checkover", "_audit_report.json"))) "checkover/_audit_report.json"
              else if (file.exists(file.path(rd, "_audit_report.json"))) "at top level (old location)"
              else "absent"))
  if (exists("audit_coordinate_free", mode = "function")) {
    a <- audit_coordinate_free(rd)
    cat(sprintf("    %-32s %d of %d files flagged\n", "coordinate check (read-only)", a$n_flagged, a$n_checked))
    for (f in utils::head(names(a$flags), 5)) cat(sprintf("      - %s\n", f))
  }
}

tmp <- file.path(out, "temporal")
if (dir.exists(tmp)) {
  n <- length(list.files(tmp, pattern = "\\.rds$", recursive = TRUE))
  cat(sprintf("\n  %-34s %d occurrence snapshot(s) (.rds, carry coordinates)\n", "temporal/", n))
}
REOF
  )
else
  fact "mirror check" "skipped (no checkover_output/ or no Rscript)"
fi

sec "done"
echo "  Report written to: $REPORT"
