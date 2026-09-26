# cheCkOVER service contract

The interface between cheCkOVER and a platform that runs it as a service
(World of Crayfish, WoC). Both sides build from this file. The procedures for
operating the service are in [RUNBOOK.md](RUNBOOK.md).

Contents: [1 Run file](#1-the-run-file) ·
[2 Input table](#2-the-input-table) ·
[3 What counts as a change](#3-what-counts-as-a-change) ·
[4 Numbering](#4-revision-numbering) ·
[5 Exit codes and run files](#5-exit-codes-and-the-files-of-the-run-home) ·
[6 Revision folder](#6-the-revision-folder) ·
[7 The runner's obligations](#7-the-runners-obligations) ·
[8 Citation](#8-how-to-cite-checkover)

---

## 1. The run file

The runner writes `/data/runs/<run_id>/run.json` and starts cheCkOVER with
`CHECKOVER_RUN` pointing at it. The folder holding the run file is the **run
home**.

```json
{ "run_id":            "run_20261015_a1b2",
  "framework_version": "1.10",
  "input_file":        "/data/runs/run_20261015_a1b2/input.tsv",
  "root_output_dir":   "/data/output",
  "state_dir":         "/data/state",
  "species_scope":     ["Austropotamobius_fulcisianus", "Faxonius_validus"],
  "force_reprocess":   false,
  "code_tag":          "runtime-1.0" }
```

| Key | Required | Type | Meaning |
|---|---|---|---|
| `framework_version` | yes | **string** `MAJOR.MINOR` | the revision this run builds. A JSON number is refused: `1.10` would reach R as `1.1` |
| `input_file` | yes | string | the frozen input table (section 2) |
| `root_output_dir` | yes | string | the mirror: revision folders only |
| `state_dir` | yes | string | working state: cache, temporal history, logs, registry |
| `run_id` | no | string of `[A-Za-z0-9._-]` | defaults to the run home's folder name |
| `species_scope` | no | list of **package ids**, or `null` | taxa that may be reprocessed; `null` or absent: every changed taxon |
| `force_reprocess` | no | `true`, `false`, or a list of **package ids** | rebuild these even if unchanged |
| `code_tag` | no | string | recorded as `provenance.code_version`; the image's build tag is used when absent |

Any other setting of `config.R` may also be given. An object merges into its
section, e.g. `"spatial": {"hydro_dir": "/data/spatial/hydrobasins"}`. An
**unknown key is refused**, never ignored.

### Taxon lists hold package ids

`species_scope` and `force_reprocess` name taxa by **package id**: the folder
name and the manifest key, e.g. `Astacus_astacus` or
`Cambarellus_pandicambarus_rotatus`. A manifest's `deferred` and `deferred_new`
keys therefore become the next run's `species_scope` unchanged.

- An entry with a space or a parenthesis (a display name) is **refused**, not
  translated.
- An entry that matches no taxon in the input is **refused**, including
  `"all"`. This is checked right after the input is read, before anything is
  written to the output root.
- Both are exit code 2, with **one `preflight.json` problem per refused
  entry**.
- An empty list `[]` for `species_scope` is refused. It would approve nothing.
  Use `null` for a run without a scope.

**"Run all eligible"** is `"species_scope": null`: every taxon whose data
changed is reprocessed, and nothing is deferred. There is no `"all"` value.
"Run selected" lists the approved package ids.

**A taxon in scope whose data did not change is inherited**, not rebuilt. It
comes out `unchanged`, unless it is also in `force_reprocess`. The scope
restricts what *may* be reprocessed; it never forces anything.

The package id is computed from a name in two steps, `normalize_species_name()`
then `make_package_id()` in `R/00_helpers.R`:

1. Collapse every run of whitespace to one space, and trim both ends.
2. Sentence case: the first character upper case, every other character lower
   case. This lowercases the subgenus.
3. Delete `(` and `)`.
4. Replace each run of whitespace with `_`.
5. Collapse repeated `_`, and trim `_` at both ends.

For example, `Cambarellus (Pandicambarus) rotatus` → `Cambarellus (pandicambarus) rotatus` → `Cambarellus_pandicambarus_rotatus`.

---

## 2. The input table

**Always the full table.** Every run receives every eligible record of every
taxon, even when `species_scope` names one taxon. Fingerprints and inheritance
are computed over the whole cohort, and the manifest lists every taxon in the
input. A taxon missing from the input would be missing from the revision.

**Format**

| | |
|---|---|
| Separator | tab (`\t`) |
| Header | first line; names are case-sensitive |
| Quoting | none: a `"` is an ordinary character. No field may contain a tab or a line break |
| Line endings | LF or CRLF |
| Encoding | UTF-8, without BOM. A service run **refuses** anything else (exit 2); a run by hand warns and decodes Windows-1252. See below |
| Missing value | an empty field, `NA` or `N/A`. The literal text `NA` in any field is read as missing |
| Decimal separator | a point. Coordinates also accept a comma (`46,63824`). No thousands separators |
| File name | must end in `.tsv` |
| Extra columns | ignored |
| A column named twice | the first is used, with a warning |

**Never through a spreadsheet.** The fingerprints compare text exactly, so the
table must reach the run folder as WoC wrote it:

- The runner fetches WoC's TSV as it is.
- Until then the export arrives by email as `.xlsx`. It is converted with
  `Rscript tools/xlsx_to_tsv.R <export.xlsx> <input.tsv>`, which writes every
  cell as the text the xlsx stores.

Saving the xlsx as text from a spreadsheet changes records without any error.
The 2026-09-23 export, saved that way, came out:

- in Windows-1252 instead of UTF-8, with letters that encoding lacks turned
  into `?` (`S?laj`, `Mehedin?i`);
- with 2,536 fields wrapped in quotes, their inner quotes doubled;
- with decimal commas, and one coordinate pair rounded to fewer decimals.

On that file, 92 of 680 taxa (8,214 records) differed from the script's
conversion of the same xlsx. A revision built from it would print the broken
text. At the next clean delivery, those taxa would then read `changed` without
any change in WoC. The encoding is the part the preflight can detect, so a
service run refuses a file that is not UTF-8, or that starts with a byte-order
mark.

**Columns.** Either the Darwin Core header or the legacy WoC header is
accepted for each column.

| Darwin Core header | Legacy WoC header | Required | Read as | Fingerprint column(s) |
|---|---|---|---|---|
| `occurrenceID` | `WoCID` | recommended | text | `record_id` |
| `scientificName` | `Crayfish_scientific_name` | **yes** | text; the taxon (section 1 rule) | groups the records |
| `decimalLatitude` | `Lat` | **yes** | number, −90…90 | `latitude` |
| `decimalLongitude` | `Long` | **yes** | number, −180…180 | `longitude` |
| `year` | `Year_of_record` | **yes** | number, 1500…current year | `year` |
| `establishmentMeans` | `Population_status` | **yes** | text (below) | `population_status` |
| `occurrenceOrigin` | `Occurrence_origin` | no | text (below) | `occurrence_origin`, `status.x`, `is_type_locality` |
| `claimExtinction` | `Claim_extinction` | no | text (below) | `is_extinct` |
| `accuracy` | `Accuracy` | no | text (below) | `accuracy` |
| `bibliographicCitation` | `DOI` | no | text, the DOI | `doi`, `doi_all` |
| `associatedReferences` | `URL` | no | text | `url`, `url_all` |
| `sourceCitation` | `Citation` | no | text | `citation`, `citation_all` |
| `contributor` | `Contributor` | no | text | `contributor` |
| `confidentialityLevel` | `Confidentiality_level` | no | number; > 0 is sensitive | `confidentiality_level`, `is_sensitive` |
| `occurrenceRemarks` | `Comments` | no | text | `extinction_remarks` (records with an extinction claim only) |
| `country` / `countryName` | `WoC_country` | no | text; WoC's curated value is authoritative | `country` |
| `continent` / `continents` | `WoC_continent` | no | text (must be one of the six continents) | `continents` |
| `stateProvince` / `state` / `province` / `county` / `judet` / `județ` / `admin_1` | `WoC_admin1` | no | text | `admin_1` |
| `catalogNumber` | — | no | text; voucher, sets basisOfRecord | — |
| `institutionCode` | — | no | text; voucher | — |
| `associatedSequences` | `NCBI_accession_code` | no | not used | — |
| `extirpationBuffer` | `Extirpation_buffer` | no | not used | — |

**Values that are interpreted.** Text is trimmed and compared case-insensitively:

| Column | Value | Becomes |
|---|---|---|
| `establishmentMeans` | `indigenous`, `native` | indigenous stream |
| | `non-indigenous`, `non indigenous`, `alien`, `introduced` | non-indigenous stream |
| | anything else | falls back to `occurrenceOrigin` (`native` → indigenous; `alien`, `introduced` → non-indigenous), else the record is excluded and reported |
| `occurrenceOrigin` | `native`, `indigenous`, `endemic` | native |
| | `type locality` | native, and a type locality |
| | `alien`, `invasive`, `introduced`, `non-native` | alien |
| | anything else, e.g. `cryptogenic` | unknown. The verbatim value is kept and printed |
| `claimExtinction` | `extinct`, `extirpated`, `yes`, `true`, `1` | extinct |
| | anything else, including empty | not extinct |
| `accuracy` | `high` / `medium` / `low` | exact / approximate / locality |
| | anything else | unknown |

**Records that never reach a package**, charged to the first reason that
applies and reported in the run's `ingest_validation_report.tsv`:

1. no `scientificName`
2. no coordinates
3. longitude outside −180…180
4. latitude outside −90…90
5. no year
6. a duplicate: same taxon, coordinates and year as another record
7. year outside 1500 to the current year

A duplicate group keeps **the record with the smallest `occurrenceID`**, in
byte order. Every citation, DOI and URL of the group is carried onto it
(`citation_all`, `doi_all`, `url_all`). The survivor does not depend on the
order of rows in the file.

**One example row.** This is the first record of
`demo_data/WoC_demo_Pontastacus.tsv`, which is public: confidentialityLevel 0.
As the file stores it (tab-separated; empty fields between tabs):

```
occurrenceID	scientificName	decimalLatitude	decimalLongitude	accuracy	year	establishmentMeans	occurrenceOrigin	bibliographicCitation	associatedReferences	sourceCitation	associatedSequences	claimExtinction	occurrenceRemarks	confidentialityLevel	contributor
WA1418	Pontastacus leptodactylus	46.63824	21.55498	high	2023	indigenous	native	10.1016/j.gecco.2024.e02847		Ion, M.C., Ács, A.R., Laza, A.V., Lorincz, I., Livadariu, D., Lamoly, A.M., Goia, B., Togor, A., Iorgu, E.I., Ștefan, A., Popa, O.P., Pârvulescu, L. (2024). Conservation status of the idle crayfish Austropotamobius bihariensis Pârvulescu, 2019. Global Ecology and Conservation 50, e02847.			paper dataset: 001	0	Lucian Pârvulescu
```

The same row, column by column:

| Column | Value |
|---|---|
| `occurrenceID` | `WA1418` |
| `scientificName` | `Pontastacus leptodactylus` |
| `decimalLatitude` | `46.63824` |
| `decimalLongitude` | `21.55498` |
| `accuracy` | `high` |
| `year` | `2023` |
| `establishmentMeans` | `indigenous` |
| `occurrenceOrigin` | `native` |
| `bibliographicCitation` | `10.1016/j.gecco.2024.e02847` |
| `associatedReferences` | *(empty)* |
| `sourceCitation` | `Ion, M.C., Ács, A.R., … (2024). Conservation status of the idle crayfish … Global Ecology and Conservation 50, e02847.` |
| `associatedSequences` | *(empty)* |
| `claimExtinction` | *(empty)* |
| `occurrenceRemarks` | `paper dataset: 001` |
| `confidentialityLevel` | `0` |
| `contributor` | `Lucian Pârvulescu` |

---

## 3. What counts as a change

A taxon is **changed** when its fingerprint differs from the fingerprint of the
data its current package was built from (`fingerprint_at_source` in the
manifest). The fingerprint is a SHA-256 over 23 columns of every record of
the taxon, after the cleaning above:

`record_id`, `longitude`, `latitude`, `year`, `is_extinct`, `is_type_locality`,
`population_status`, `status.x`, `accuracy`, `doi`, `url`, `citation`,
`contributor`, `confidentiality_level`, `is_sensitive`, `occurrence_origin`,
`country`, `continents`, `admin_1`, `extinction_remarks`, `citation_all`,
`doi_all`, `url_all`.

The last eight were added for the clean 1.0. Each of them reaches the packages,
so without them an edit in WoC left a stale package marked `unchanged`. There
is no threshold: **any** difference counts.

**How values are compared**

- Missing, empty and `NA` are the same value.
- Text is trimmed, and a tab or line break inside a field becomes a space.
- Anything that reads as a number is compared by value to 6 decimals:
  `46.1` = `46.10` = `46.100000`. A difference only in the 7th decimal is not
  seen.
- `TRUE`/`true`/`T` are the same, as are `FALSE`/`false`/`F`.
- Row order does not matter.

**For a freshness estimate that never says "unchanged" wrongly.** For each
taxon (grouped by package id), compare the current table against the archived
input of the taxon's `source_version`. Use every input column in the table
above that has a fingerprint column. If, for every record of the taxon (same
`occurrenceID`s, or the same rows where there is no id), every one of those
fields is identical as trimmed text, then the fingerprint is identical too. A
comparison on raw text can say "changed" where the fingerprint does not (e.g.
`46.1` against `46.10`, or an edit to a record that cleaning drops). It cannot
say "unchanged" where the fingerprint differs. The one exception is time
itself: a record dated in a future year is dropped until that year arrives,
and then enters the fingerprint.

**What change detection does not see:** the code and the reference layers. A
new code tag, or a new WDPA release in the cache, can change results with
unchanged data. That is what `force_reprocess` is for (RUNBOOK section 6).

---

## 4. Revision numbering

- `framework_version` is a **string** `MAJOR.MINOR` everywhere: in the run
  file, the manifest and every `package_metadata.json`.
- Versions compare **by numeric component**, never as text. 1.10 follows 1.9,
  and 1.100 follows 1.99. There is no limit on MINOR.
- A revision's **predecessor** is the latest existing revision *before* it.
  The prior revisions are those numerically smaller than the current one,
  newest first. A revision never inherits from a revision after it.
- **The preflight refuses a number that is not after the latest existing
  revision** (exit 2). A skipped number, e.g. 1.9 → 1.11, is a warning.
- 2.0 follows 1.x only with a software upgrade (a new code tag).

`tests/test_version_order.R` builds 1.2 → 1.9 → 1.10 → 1.11 → 1.100 and checks
every predecessor.

---

## 5. Exit codes and the files of the run home

| Exit | Meaning | Output root | Next step (RUNBOOK) |
|---|---|---|---|
| `0` | succeeded | a complete `<root>/<rev>/` | audit, then upload |
| `1` | failed during the run | possibly a partial `<root>/<rev>/`, never to be uploaded | section 4: delete it and `work/`, retry |
| `2` | refused before processing | nothing written | fix what `preflight.json` lists, rerun |

A refusal after the input was read (unknown taxa in the run file) leaves the
ingest's files in `work/`. They are working files; delete them or let the
retry overwrite them.

### `preflight.json`

Written for every run, whether it passed or was refused.

```json
{ "status": "refused",
  "run_id": "run_20261015_a1b2",
  "framework_version": "1.10",
  "checked_at": "2026-10-15T09:12:04+0300",
  "n_fatal": 2,
  "n_warning": 0,
  "problems": [
    { "severity": "FATAL", "item": "species_scope",
      "detail": "entry \"Astacus astacus\" is not a package id. Taxa are listed by package id, e.g. \"Astacus_astacus\" (the manifest key and folder name)." },
    { "severity": "FATAL", "item": "species_scope",
      "detail": "entry \"Bstacus_b\" matches no taxon in the input file. Taxa are listed by package id; check the spelling against the manifest keys." } ] }
```

| Field | Type | |
|---|---|---|
| `status` | `"passed"` or `"refused"` | `refused` ⇔ exit code 2 |
| `n_fatal`, `n_warning` | integer | |
| `problems[]` | objects of `severity` (`FATAL`/`WARNING`), `item`, `detail` | every problem found, one per refused taxon entry |

### `status.json`

Rewritten at every phase of the run, and once at the end.

```json
{ "status": "succeeded",
  "run_id": "run_20261015_a1b2",
  "framework_version": "1.10",
  "exit_code": 0,
  "started_at": "2026-10-15T09:12:01+0300",
  "updated_at": "2026-10-15T09:41:37+0300",
  "elapsed_seconds": 1776.2,
  "peak_rss_mb": 21344.5,
  "phases": [ { "phase": "startup", "seconds": 41.0 },
              { "phase": "ingest", "seconds": 212.4 } ],
  "revision_dir": "/data/output/1.10",
  "code_version": "runtime-1.0" }
```

| Field | Present | |
|---|---|---|
| `status` | always | `running`, `succeeded`, `failed` or `refused` |
| `exit_code` | when finished | `0` / `1` / `2` |
| `message` | failed, refused | the error, or the reason for the refusal |
| `current_phase` | running | the phase in progress |
| `elapsed_seconds`, `phases[]` | always | wall time, and seconds per phase |
| `peak_rss_mb` | on Linux | peak memory of the run, MB |
| `revision_dir`, `code_version` | succeeded | |

The phases, in order: `startup`, `ingest`, `full_cohort_enrichment`,
`change_detection`, `indigenous_branch`, `non_indigenous_branch`,
`merge_and_narratives`, `temporal`, `maps`, `citations`, `export`. The first
four run on the whole input whatever the scope, so they are the fixed cost of
a run.

A `status.json` that still says `running` while no container is running
belongs to an interrupted run (RUNBOOK section 7).

---

## 6. The revision folder

```
<root>/<rev>/
├── <Package_id>/                 one per taxon built in this revision
│   ├── package_metadata.json
│   ├── maps/  narratives/  citations/
│   ├── file_manifest.csv         size and md5 of every other file (see below)
│   └── README.md
└── checkover/
    ├── manifest.json             every taxon of the input, and where its package lives
    ├── fingerprints/             one JSON per taxon
    ├── records_used.tsv          the records behind this revision's packages
    ├── packaging_summary.json
    ├── INDEX.md
    └── _audit_report.json        written by the audit, after the run
```

Only taxa built in this revision have a folder. An `unchanged` or `deferred`
taxon's package stays in its `source_version` folder.

`file_manifest.csv` has the columns `filename`, `filepath`, `size_bytes`,
`file_type` and `md5`, one row per file of the package except itself.

- **From the tag after `runtime-1.0`:** `filepath` is relative to the package
  folder (`maps/Astacus_astacus_AOO.geojson`), and `README.md` is listed.
- **Packages built by `runtime-1.0`:** `filepath` is the server's absolute path
  (`/data/output/1.0/Astacus_astacus/maps/...`), and `README.md` is not listed.
  Read the part after `/<Package_id>/`, which works for both forms.

### `manifest.json`

```json
{ "framework": "cheCkOVER",
  "framework_version": "1.10",
  "code_version": "runtime-1.0",
  "run_id": "run_20261015_a1b2",
  "generated_date": "2026-10-15T06:41:30Z",
  "prior_version": "1.9",
  "totals": { "total_species_in_cohort": 677, "unchanged": 660, "reprocessed": 2,
              "new": 0, "deferred": 15, "deferred_new": 0, "active_runtime_species": 2 },
  "species": {
    "Austropotamobius_fulcisianus": {
      "outcome": "reprocessed", "source_version": "1.10", "prior_source_version": "1.0",
      "fingerprint": "sha256:…", "fingerprint_at_source": "sha256:…",
      "n_records": 2139, "change_summary": "data changed vs v1.0 …" } } }
```

| Field | |
|---|---|
| `prior_version` | the predecessor revision (section 4); `null` for the first |
| `species` | keyed by **package id**; one entry per taxon of the input |
| `outcome` | `new` · `reprocessed` · `unchanged` · `deferred` · `deferred_new` |
| `source_version` | the revision whose folder holds this taxon's package; `null` for `deferred_new` (no package) |
| `prior_source_version` | where the package lived before this revision; `null` for a first appearance |
| `fingerprint` | the current data |
| `fingerprint_at_source` | the data the package was built from. It equals `fingerprint`, except for `deferred`, where the next run compares against it, and `deferred_new`, where it is `null` (there is no package) |
| `n_records` | records of the taxon after cleaning |
| `change_summary` | human-readable reason, e.g. `identical to v1.9 (812 records)`, `forced reprocess (…)`, `deferred: data changed vs v1.0 (…); not in this run's species_scope` |

### `records_used.tsv`

Tab-separated, UTF-8, header row. There are **exactly** three columns (the
writer refuses anything else), and one row per record that entered a package
of this revision, sorted by `species` then `record_id`.

| Column | |
|---|---|
| `record_id` | `occurrenceID` of the record (the survivor, for a duplicate group) |
| `species` | the normalised name the package id is derived from |
| `state` | `active`, `suppressed` (masked by a later extinction nearby), or `extinct` |

It holds no coordinates. Added and retired records follow by anti-join against
the `records_used.tsv` of the taxon's previous source revision.

### `_audit_report.json`

Written by `tests/audit_packages.R <root>/<rev>` into `<rev>/checkover/`.

```json
{ "passed": true,
  "result": "PASS",
  "framework_version": "1.10",
  "code_version": "runtime-1.0",
  "audited_at": "2026-10-15T09:43:02+0300",
  "checks": {
    "integrity":       { "passed": true, "n_checked": 2,  "n_flagged": 0 },
    "consistency":     { "passed": true, "n_checked": 2,  "n_flagged": 0 },
    "coordinate_free": { "passed": true, "n_checked": 31, "n_flagged": 0 } },
  "integrity": { … }, "consistency": { … }, "coordinate_free": { … } }
```

**The verdict is `passed`** (boolean). It is `true` only when all three checks
pass. `result` says the same as text, and the audit's exit code says it again:
`0` pass, `1` fail. The three always agree (`tests/test_coordinate_free.R`
checks that they do). `framework_version` and `code_version` come from the
manifest, so a consumer can check the report belongs to the folder it is
installing. The detail blocks name files and reasons, never coordinates.

| Check | Fails when |
|---|---|
| `integrity` | a package lacks an expected artifact, or one is empty |
| `consistency` | a number in a narrative differs from `package_metadata.json` |
| `coordinate_free` | any file outside `maps/` holds coordinate columns or keys, a decimal latitude/longitude pair, or an R binary; or a map holds point geometries |

### `package_metadata.json`: the fields a platform reads

| Field | |
|---|---|
| `species`, `package_id` | display name and folder name |
| `status`, `scenario`, `metrics` | the results |
| `provenance.framework_version` | the revision (string) |
| `provenance.code_version` | the code that computed it |
| `provenance.license` | `CC-BY-4.0` |
| `preferred_citation` | section 8 |

---

## 7. The runner's obligations

1. Never start a run while another is running. There is one state dir, and a
   revision inherits from the one before it.
2. Upload a revision **only** if all three hold:
   - cheCkOVER exited `0`;
   - the audit exited `0`;
   - `_audit_report.json` has `"passed": true`, with a `framework_version`
     equal to the run's.

   Otherwise nothing leaves the server: mark the run failed and delete the
   revision folder (RUNBOOK section 3, step 5).
3. Upload `/data/output/<rev>/` only. Never anything from `/data/state/` or
   `/data/runs/`. The progress lines of `run.log` are safe to forward: the log
   prints record ids, never coordinates.
4. Recover from failures by RUNBOOK sections 4 and 7.

---

## 8. How to cite cheCkOVER

Every package carries this reference:

> Livadariu D, Bâcu VI, Nandra CI, Ștefănuț TT, Sabou A, WoC® Contributors,
> Crandall KA, Pârvulescu L: cheCkOVER: Assessment-support workflow for
> biogeographic metrics from species occurrence data.
> https://doi.org/10.64898/2025.12.29.696807

It appears in three places in each package:

- `citations/<Package_id>_CITATION.cff`, as `preferred-citation`;
- `package_metadata.json`, as `preferred_citation`, with `text` (exactly the
  line above), `title`, `doi`, `url` and `authors`;
- `README.md`.

It is defined once, `CHECKOVER_REFERENCE` in `config.R`, and
`tests/test_citation.R` pins it character for character. Ș and ț are U+0218 and
U+021B (comma below), and ® is U+00AE.
