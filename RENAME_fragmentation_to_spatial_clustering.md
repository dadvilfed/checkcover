# Rename: "fragmentation" → "spatial clustering" (cheCkOVER, 2026-07)

**Rationale.** "Fragmentation" is reserved exclusively for **checKOLOGY**, where it is
computed as a real ecological metric. In cheCkOVER the same numbers are only a
*descriptive spatial signal*, so keeping the word here would put two different
"fragmentations" in circulation. "Spatial clustering" carries no ecological
connotation and is safe alongside the existing disclaimer.

**Scope.** Labels, output text and JSON keys only. **Logic, thresholds and cluster
values are unchanged** — the same numbers come out, under new names.

---

## JSON key mapping (old → new)

| Where | Old key | New key |
|---|---|---|
| `package_metadata.json` → `metrics.indigenous` / `metrics.non_indigenous` | `fragmentation_clusters` | **`n_clusters`** |
| per-species report JSON (`reports/indigenous/<sp>.json`) | `fragmentation` (block) | **`spatial_clustering`** (block) |
| per-species report JSON → non-indigenous `notes` | `fragmentation_analysis` | **`clustering_analysis`** |
| species detailed report (Module 4) | `fragmentation_signal` | **`clustering_signal`** |

Unchanged inside the block: `computed`, `status`, `n_clusters`, `cluster_sizes`,
`mean_threshold_km`, `scope`.

Readers accept **both** spellings for one release (`new %||% old`), so packages
produced before the rename still parse.

## Output text mapping (old → new)

| Location | Old | New |
|---|---|---|
| Canonical §2.3 header | `Fragmentation Assessment (Indigenous Range Only)` | `Spatial Clustering (Indigenous Range Only)` |
| Canonical §2.3 field | `- **Fragmentation signal:** detected` / `not detected` | `- **Spatial clustering signal:** detected` / `not detected` |
| Canonical §2.3 interpretation | `Native occurrences are distributed across multiple spatially disjunct clusters.` | `Native occurrences resolve into multiple distinct spatial clusters.` |
| Canonical §2.3 caveat | `This is a conservative descriptive signal and does not represent a formal connectivity or Red List assessment.` | `This is a conservative descriptive signal of spatial structure and does not represent a formal connectivity, fragmentation, or Red List assessment.` |
| Canonical §5, Para 3 | `Distributional analysis reveals **fragmentation**: native occurrences form N spatially disjunct clusters.` | `Spatial analysis identifies N distinct occurrence clusters within the native range.` |
| Legacy narrative (Module 5) | `Distributional fragmentation signal: …` | `Spatial clustering signal: …` |
| Legacy narrative (Module 6) | `**Distributional Fragmentation:** …` | `**Spatial clustering:** …` |
| Module 6, non-indigenous | `Fragmentation analysis is not applicable…` | `Spatial clustering analysis is not applicable…` |

Note the §2.3 caveat deliberately still contains the word "fragmentation" — it is the
disclaimer stating this is *not* a fragmentation assessment.

## Deliberately NOT renamed

Internal-only names, per "leave the function name if it touches too many internal calls":

- `analyze_fragmentation()` — function name
- `result_indigenous$fragmentation` — in-memory result field
- `fragmentation_analysis_indigenous.tsv` — intermediate run artifact (not shipped in packages)
- `R/01d_fragmentation.R` — module filename

These never reach a package or a narrative, so they carry no risk of being confused
with checKOLOGY's metric.

## Consumers updated

- `audit_packages.R` and `extract_manuscript_summaries.R` read `n_clusters` with a
  fallback to `fragmentation_clusters`.
- WoC ingestion should switch to `n_clusters` / `spatial_clustering`; the old keys
  disappear from packages produced by the next full run.
