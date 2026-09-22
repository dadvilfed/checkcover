#!/usr/bin/env bash
# ============================================================================
# cheCkOVER — one trial service run, end to end
# ============================================================================
# A stand-in for the runner, for testing on the server: it lays out a fresh
# trial (output, state and run folders), writes run.json exactly as the service
# contract says (SERVICE_CONTRACT.md), runs cheCkOVER in the container or on the
# host, audits the revision, prints what it cost, and copies the sample files
# World of Crayfish asked for, unedited.
#
# It is NOT the production runner: it never uploads anything and never touches
# /data/output, /data/state or /data/runs. Every trial lives under TRIAL_ROOT.
#
# Usage:
#   tools/service_trial.sh container|host <trial_name> <input.tsv> [scope]
#
#   scope   empty for a full run; otherwise package ids, comma-separated
#           ("Astacus_astacus,Faxonius_limosus"), or @file with one id per line
#
# Environment (defaults in brackets):
#   TRIAL_ROOT  where trials are created               [/data/trial]
#   SPATIAL     reference layers                        [/data/spatial]
#   CACHE_SEED  a cache/ to copy into the fresh state dir, to skip rebuilding
#               the reference-layer cache (optional)
#   IMAGE       container image                         [checkover:candidate]
#   VERSION     framework_version of the trial revision [1.0]
#   MEMORY      container memory cap                    [48g]
#   CHECKOUT    host mode: the cheCkOVER checkout       [this script's repo]
#
# Examples:
#   tools/service_trial.sh container demo_c demo_data/WoC_demo_Pontastacus.tsv
#   tools/service_trial.sh host      demo_h demo_data/WoC_demo_Pontastacus.tsv
#   tools/service_trial.sh container fulc  /data/inputs/woc_full.tsv Austropotamobius_fulcisianus
# ============================================================================

set -u
MODE="${1:-}"; NAME="${2:-}"; INPUT="${3:-}"; SCOPE="${4:-}"
if [ -z "$MODE" ] || [ -z "$NAME" ] || [ -z "$INPUT" ]; then
  sed -n '2,35p' "$0"; exit 64
fi
case "$MODE" in container|host) ;; *) echo "mode must be 'container' or 'host'"; exit 64 ;; esac

TRIAL_ROOT="${TRIAL_ROOT:-/data/trial}"
SPATIAL="${SPATIAL:-/data/spatial}"
IMAGE="${IMAGE:-checkover:candidate}"
VERSION="${VERSION:-1.0}"
MEMORY="${MEMORY:-48g}"
CHECKOUT="${CHECKOUT:-$(cd "$(dirname "$0")/.." && pwd)}"

T="$TRIAL_ROOT/$NAME"
if [ -e "$T" ]; then echo "Trial '$T' already exists. Pick a new name, or delete it first."; exit 65; fi
if [ ! -f "$INPUT" ]; then echo "Input '$INPUT' not found."; exit 66; fi
if [ ! -d "$SPATIAL/hydrobasins" ]; then echo "Reference layers not found under '$SPATIAL'."; exit 66; fi

# Rootless podman needs the user's runtime dir, which `sudo -iu` does not set.
if [ "$MODE" = container ] && [ -z "${XDG_RUNTIME_DIR:-}" ] && [ -d "/run/user/$(id -u)" ]; then
  export XDG_RUNTIME_DIR="/run/user/$(id -u)"
fi

OUT="$T/output"; STATE="$T/state"; HOME_RUN="$T/runs/$NAME"
mkdir -p "$OUT" "$STATE" "$HOME_RUN" || exit 73
cp "$INPUT" "$HOME_RUN/input.tsv" || exit 74
if [ -n "${CACHE_SEED:-}" ]; then
  echo "Seeding the cache from $CACHE_SEED ..."
  cp -a "$CACHE_SEED" "$STATE/cache" || exit 74
fi

# Paths as cheCkOVER will see them.
if [ "$MODE" = container ]; then
  P_OUT=/data/output; P_STATE=/data/state; P_HOME="/data/runs/$NAME"
else
  P_OUT="$OUT"; P_STATE="$STATE"; P_HOME="$HOME_RUN"
  if [ ! -e "$CHECKOUT/spatial_data" ]; then
    echo "Host mode reads the reference layers from $CHECKOUT/spatial_data. Link them once:"
    echo "  ln -s $SPATIAL $CHECKOUT/spatial_data"
    rm -rf "$T"; exit 66
  fi
fi

# species_scope as a JSON list of package ids (the only form a run file accepts).
SCOPE_JSON=""
if [ -n "$SCOPE" ]; then
  if [ "${SCOPE#@}" != "$SCOPE" ]; then ids=$(grep -v '^[[:space:]]*$' "${SCOPE#@}" | tr -d '\r' | paste -sd, -)
  else ids="$SCOPE"; fi
  SCOPE_JSON=$(printf '%s' "$ids" | awk -F, '{for(i=1;i<=NF;i++){gsub(/^ +| +$/,"",$i); printf "%s\"%s\"", (i>1?", ":""), $i}}')
  SCOPE_JSON=",
  \"species_scope\": [$SCOPE_JSON]"
fi

cat > "$HOME_RUN/run.json" <<EOF
{ "run_id": "$NAME",
  "framework_version": "$VERSION",
  "input_file": "$P_HOME/input.tsv",
  "root_output_dir": "$P_OUT",
  "state_dir": "$P_STATE"$SCOPE_JSON }
EOF

echo "Trial $NAME ($MODE) in $T"
cat "$HOME_RUN/run.json"; echo

# ---- run ----
START=$(date +%s)
if [ "$MODE" = container ]; then
  MEMFLAG="--memory=$MEMORY"
  CTRL="/sys/fs/cgroup/user.slice/user-$(id -u).slice/user@$(id -u).service/cgroup.controllers"
  if [ ! -r "$CTRL" ] || ! grep -qw memory "$CTRL"; then
    echo "(the memory controller is not delegated to this user: running without --memory)"; MEMFLAG=""
  fi
  podman run --rm --name "checkover_$NAME" $MEMFLAG \
    -e CHECKOVER_RUN="$P_HOME/run.json" \
    -v "$SPATIAL:/work/spatial_data:ro" \
    -v "$OUT:/data/output" \
    -v "$STATE:/data/state" \
    -v "$HOME_RUN:$P_HOME" \
    "$IMAGE" > "$HOME_RUN/console.log" 2>&1
  CODE=$?
else
  ( cd "$CHECKOUT" && CHECKOVER_RUN="$HOME_RUN/run.json" Rscript checkcover_main.R ) \
    > "$HOME_RUN/console.log" 2>&1
  CODE=$?
fi
WALL=$(( $(date +%s) - START ))

# ---- audit ----
AUDIT="not run"
if [ "$CODE" -eq 0 ]; then
  if [ "$MODE" = container ]; then
    podman run --rm -v "$OUT:/data/output" "$IMAGE" \
      Rscript tests/audit_packages.R "/data/output/$VERSION" > "$HOME_RUN/audit.log" 2>&1
  else
    ( cd "$CHECKOUT" && Rscript tests/audit_packages.R "$OUT/$VERSION" ) > "$HOME_RUN/audit.log" 2>&1
  fi
  A=$?; AUDIT=$([ "$A" -eq 0 ] && echo "PASS (exit 0)" || echo "FAIL (exit $A)")
fi

# ---- samples, unedited ----
S="$T/samples"; mkdir -p "$S"
for f in "$HOME_RUN/preflight.json" "$HOME_RUN/status.json" \
         "$OUT/$VERSION/checkover/manifest.json" "$OUT/$VERSION/checkover/records_used.tsv" \
         "$OUT/$VERSION/checkover/_audit_report.json"; do
  [ -f "$f" ] && cp "$f" "$S/"
done
PKG=$(find "$OUT/$VERSION" -mindepth 2 -maxdepth 2 -name package_metadata.json 2>/dev/null | sort | head -1)
[ -n "$PKG" ] && cp "$PKG" "$S/package_metadata_$(basename "$(dirname "$PKG")").json"

# ---- summary ----
echo
echo "==== trial $NAME ===="
echo "  mode        $MODE$([ "$MODE" = container ] && echo " ($IMAGE)")"
echo "  exit code   $CODE   (0 succeeded, 1 failed, 2 refused)"
echo "  wall time   ${WALL} s"
echo "  audit       $AUDIT"
if [ -f "$HOME_RUN/status.json" ] && command -v python3 >/dev/null 2>&1; then
  python3 - "$HOME_RUN/status.json" <<'PY'
import json, sys
s = json.load(open(sys.argv[1], encoding="utf-8"))
print(f"  status      {s.get('status')}  {s.get('message') or ''}")
print(f"  elapsed     {s.get('elapsed_seconds')} s   peak RAM {s.get('peak_rss_mb')} MB")
for p in s.get("phases", []):
    print(f"    {p['phase']:<26}{p['seconds']:>10} s")
PY
fi
[ "$CODE" -eq 2 ] && echo "  refused: see $HOME_RUN/preflight.json"
[ "$CODE" -eq 1 ] && echo "  failed: see the end of $HOME_RUN/run.log"
echo "  revision    $OUT/$VERSION"
echo "  samples     $S"
echo "  logs        $HOME_RUN/run.log, console.log$([ "$CODE" -eq 0 ] && echo ", audit.log")"
exit "$CODE"
