#!/usr/bin/env bash
# The input of a baseline revision: the full export with every extinction claim
# cleared, and nothing else changed.
#
# The clean 1.0 (2026-09) is computed on the whole cohort with the claims
# removed, so every claimed site is still an ordinary presence in 1.0. The next
# revision reads the export as it is, and the 1.0 -> 1.1 temporal delta records
# what the claims take away. A 1.0 built with the claims would hold the
# extinctions from the start, and no delta would ever show them.
#
# Clearing the column in a spreadsheet re-encodes the file and can rewrite
# numbers and text. This changes that one column only, then checks that every
# other column is byte-identical to the export.
#
#   tools/clear_extinction_claims.sh <export.tsv> <baseline.tsv>
#
# Prints counts only, never records: the export holds exact coordinates, so
# write the baseline into the run folder (/data/runs/<run_id>/input.tsv).
set -euo pipefail
export LC_ALL=C

usage="usage: $0 <export.tsv> <baseline.tsv>"
in="${1:?$usage}"
out="${2:?$usage}"
[ -f "$in" ]  || { echo "not found: $in" >&2; exit 2; }
[ -e "$out" ] && { echo "refusing to overwrite $out" >&2; exit 2; }

# Column positions from the header, Darwin Core or legacy WoC names.
colnum() {
  head -n 1 "$in" | tr -d '\r' | tr '\t' '\n' | grep -n -i -x -E "$1" | head -n 1 | cut -d: -f1
}
col=$(colnum 'claimExtinction|Claim_extinction')
spc=$(colnum 'scientificName|Crayfish_scientific_name')
[ -n "$col" ] || { echo "no claimExtinction column in $in" >&2; exit 2; }

# BINMODE=3 keeps CRLF line endings as they are where awk would translate them
# (Git Bash); on Linux it changes nothing. A CR is data to these scripts, so
# strip it before judging a value, and put it back when clearing the last column.
awk -F'\t' -v BINMODE=3 -v c="$col" -v s="${spc:-0}" '
  NR > 1 { x = $c; sub(/\r$/, "", x) }
  NR > 1 && x != "" {
    n++; v[x]++
    if (s) { t = $s; gsub(/[ \t]+/, " ", t); sub(/^ /, "", t); sub(/ $/, "", t); tx[t] = 1 }
  }
  END {
    printf "records:            %d\n", NR - 1
    printf "claims to clear:    %d\n", n
    for (k in v) printf "  %-16s  %d\n", k, v[k]
    if (s) { m = 0; for (k in tx) m++; printf "taxa with a claim:  %d\n", m }
  }' "$in"

awk -F'\t' -v OFS='\t' -v BINMODE=3 -v c="$col" '
  NR == 1 { print; next }
  { $c = (c == NF && $c ~ /\r$/) ? "\r" : ""; print }' "$in" > "$out"

# Every column but the cleared one must be exactly as exported.
if [ "$(wc -l < "$in")" -ne "$(wc -l < "$out")" ] ||
   ! cmp -s <(cut --complement -f "$col" "$in") <(cut --complement -f "$col" "$out"); then
  rm -f "$out"
  echo "FAILED: the output differs from the export outside column $col; nothing written." >&2
  exit 1
fi
left=$(awk -F'\t' -v BINMODE=3 -v c="$col" '
  NR > 1 { x = $c; sub(/\r$/, "", x); if (x != "") n++ } END { print n + 0 }' "$out")
[ "$left" -eq 0 ] || { rm -f "$out"; echo "FAILED: $left claims left; nothing written." >&2; exit 1; }
echo "written: $out (column $col cleared; every other column byte-identical to the export)"
