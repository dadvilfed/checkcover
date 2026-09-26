# cheCkOVER runbook — running as a service

What to do, step by step, when cheCkOVER runs as a service on the UVT server for
World of Crayfish (WoC). The runner follows it literally; an administrator reads
it when a run shows "failed". The interface itself (run file, input table,
exit codes, file schemas) is specified in
[SERVICE_CONTRACT.md](SERVICE_CONTRACT.md). The *why* behind each rule is in the
README, sections [Where things live](README.md#where-things-live) and
[Running as a service](README.md#running-as-a-service).

---

## 1. Layout

| Host path | Mounted in the container at | Holds | Mirrored to WoC |
|---|---|---|---|
| `/data/spatial/` | `/work/spatial_data` (read-only) | reference layers | no |
| `/data/output/` | `/data/output` | revision folders `1.0/`, `1.1/`, … **only** | **yes: this is the mirror** |
| `/data/state/` | `/data/state` | `cache/`, `temporal/`, `logs/`, `_registry.json` | **never**: holds coordinates |
| `/data/runs/<run_id>/` | `/data/runs/<run_id>` | `run.json`, `input.tsv`, `run.log`, `preflight.json`, `status.json`, `work/` | **never**: `input.tsv` and `work/` hold coordinates |

`/data/state/` is the fourth volume, next to the three in the original plan. It
must persist across runs. `temporal/` is the version-to-version history that the
temporal delta compares against. `cache/` saves hours per run: WDPA by country,
merged HydroBASINS levels, TEOW, FEOW, Natural Earth.

**Invariants**

- Only the runner creates or deletes anything in `/data/output/`, and only as
  this runbook says.
- One run at a time. Two runs must never overlap: a revision inherits from the
  one before it, and all runs share `/data/state/`.
- Nothing leaves the server except a revision folder, its `_audit_report.json`,
  and the progress lines of `run.log`. The log prints record ids, never
  coordinates.

---

## 2. One-time setup

The runner lives in its own account, **`checkover`, without sudo**, because it
holds the access token to WoC. `ubuntu` stays for administration. All of step
1 is done as `ubuntu`; everything after it as `checkover`.

1. As `ubuntu`, once:
   ```bash
   sudo apt install podman                                          # rootless containers
   sudo adduser --disabled-password --gecos "cheCkOVER runner" checkover
   grep checkover /etc/subuid /etc/subgid                           # rootless Podman needs both lines
   sudo loginctl enable-linger checkover                            # its services survive reboots
   sudo mkdir -p /data/spatial /data/output /data/state /data/runs /data/trial
   sudo chown checkover:checkover /data/output /data/state /data/runs /data/trial
   sudo cp -r <reference layers>/. /data/spatial/                   # hydrobasins/, feow/
   sudo chmod -R a+rX /data/spatial                                 # readable, owned by root: read-only for the runner
   ```
   The service needs no inbound port, so nothing else should listen: close the
   print service. On the UVT server CUPS is a snap, closed with
   `sudo snap stop --disable cups` (2026-09-22); where it is a system package,
   use `sudo systemctl disable --now cups.socket cups.service cups-browsed.service`.
   Check that `sudo ss -ltnp | grep ':631'` prints nothing.
2. Work as `checkover`: `sudo -iu checkover`, then
   `export XDG_RUNTIME_DIR=/run/user/$(id -u)`. Rootless Podman needs this
   variable, and `sudo` does not set it.
3. Build the image as `checkover` (section 6). Rootless images belong to the
   account that built them.

---

## 3. A run, start to finish

For run `<run_id>`, revision `<rev>` and image tag `<tag>`:

1. **Check** that `/data/output/<rev>/` does not exist, and that the disk has at
   least 20 GB free (`df -h /data`).
2. **Write** `/data/runs/<run_id>/run.json` and `/data/runs/<run_id>/input.tsv`:
   ```json
   { "run_id": "<run_id>",
     "framework_version": "<rev>",
     "input_file": "/data/runs/<run_id>/input.tsv",
     "root_output_dir": "/data/output",
     "state_dir": "/data/state",
     "species_scope": ["<taxon>", "..."],
     "code_tag": "<tag>" }
   ```
   `framework_version` is a string. Leave out `species_scope`, or set it to
   `null`, for a full run. Taxa are listed by **package id**
   (`"Astacus_astacus"`), exactly as the manifest keys them. A display name
   is refused (SERVICE_CONTRACT section 1).
3. **Run**, and tail `run.log` for progress:
   ```bash
   podman run --rm --name checkover_<run_id> \
     -e CHECKOVER_RUN=/data/runs/<run_id>/run.json \
     -v /data/spatial:/work/spatial_data:ro \
     -v /data/output:/data/output \
     -v /data/state:/data/state \
     -v /data/runs/<run_id>:/data/runs/<run_id> \
     --memory=48g \
     checkover:<tag>
   ```
   cheCkOVER runs on a single core. The full cohort peaked at 22–34 GB of RAM,
   and a run of a few taxa needs far less, but it still loads the reference
   layers. `--memory=48g` stops a run from crowding other work on the machine.
   A rootless memory limit needs the memory controller delegated to the user;
   check with
   `cat /sys/fs/cgroup/user.slice/user-$(id -u).slice/user@$(id -u).service/cgroup.controllers`,
   which should list `memory`. If it does not, drop the flag.
4. **Act on the exit code.** `status.json` says the same thing in words.

   | Exit | `status.json` | Job state | Next |
   |---|---|---|---|
   | `0` | `succeeded` | completed, pending audit | step 5 |
   | `1` | `failed` | failed | section 4 |
   | `2` | `refused` | refused | show `preflight.json` on the workbench. Nothing was written to the output root. A refusal after the input was read (taxa the input does not contain) leaves working files in `work/`; delete them. Fix the cause and run again. |

5. **Audit** the revision; this writes `/data/output/<rev>/checkover/_audit_report.json`:
   ```bash
   podman run --rm -v /data/output:/data/output checkover:<tag> \
     Rscript tests/audit_packages.R /data/output/<rev>
   ```
   Upload only if the audit exited `0` **and** the report says
   `"passed": true` with the run's `framework_version`. Anything else: do not
   upload. Copy the report to `/data/runs/<run_id>/`, mark the run failed with
   it, then delete `/data/output/<rev>/`. The report names files and reasons,
   never coordinates, so it is safe to show.
6. **Upload** `/data/output/<rev>/` in parts.
7. **Afterwards.** WoC installs the revision, so it stays in the mirror. If WoC
   discards it, delete `/data/output/<rev>/`. Once uploaded, `work/` is only
   useful for debugging: keep it for a week, then delete it (section 5).

---

## 4. A run that dies mid-way

This covers exit `1`, a killed container, an out-of-memory kill and a full
disk. A rerun does **not** resume: it starts again from the beginning, and it
is safe.

1. Delete the partial revision folder. It was never uploaded, and a service run
   refuses (exit 2) while it exists:
   ```bash
   rm -rf /data/output/<rev>
   ```
2. Delete the work directory:
   ```bash
   rm -rf /data/runs/<run_id>/work
   ```
3. Leave `/data/state/` alone. The failed run may have written to
   `temporal/`, but it checkpointed `temporal/` first. The next run finds the
   checkpoint (`temporal.checkpoint/`), sees that the previous run did not
   complete, and restores the history before it does anything else. Never
   delete `temporal/` or `temporal.checkpoint/` by hand.
4. Rerun the same `run.json` and `input.tsv` (section 3, step 3). The cache is
   kept, so the retry does not rebuild the reference layers.

Retry once automatically. If the retry fails too, mark the run failed and show
the `message` from `status.json` and the tail of `run.log` on the workbench.

---

## 5. The disk fills

A run that hits a full disk fails with exit `1`. Free space, then follow
section 4.

As of 2026-09-22 the old layout held 3.0 GB of run work files, 7.8 GB of cache
and 176 MB of temporal history for three revisions, with 260 GB free.

| Safe to delete | Effect |
|---|---|
| `/data/runs/<run_id>/work/` of runs that are uploaded or abandoned | none |
| whole `/data/runs/<run_id>/` of old runs, keeping `status.json` and `run.log` if you want the history | none |
| unused images: `podman image ls`, then `podman rmi checkover:<old tag>` | none, if that tag is no longer deployed |
| `/data/state/cache/` | the next run rebuilds it, which costs hours (WDPA downloads) |

**Never delete:** `/data/output/<rev>/` of an installed revision,
`/data/state/temporal/`, or `/data/state/temporal.checkpoint/`.

---

## 6. Rebuilding the image

Rebuild only to deploy a **new** code tag. Tags are immutable: never rebuild an
existing tag in place. `ecoregions` is pinned to a commit, but the CRAN
packages come from whatever the base image's package snapshot serves, so a
rebuild months later can still differ. Keep the saved image (step 2).

1. In a clean clone (`git status --porcelain` prints nothing):
   ```bash
   git fetch --tags
   git checkout <tag>
   podman build --build-arg CODE_VERSION=<tag> -t checkover:<tag> .
   ```
   The build runs the test suite and fails if any test fails.
2. Keep the image of every tag that produced an installed revision:
   ```bash
   podman save -o checkover_<tag>.tar checkover:<tag>
   ```
3. Before the first real run with a new tag, run the demo
   (`demo_data/WoC_demo_Pontastacus.tsv`) as a trial (section 9) and compare it
   with the previous tag's demo output using `tools/compare_revisions.R`.
4. **A code change is invisible to change detection.** Fingerprints cover the
   data, not the code, so every taxon whose data did not change keeps its old
   package. If the new tag changes what cheCkOVER computes, the first run with
   it needs `"force_reprocess": true` to rebuild all taxa. The tag's release
   notes must say whether it does.

---

## 7. UVT reboots during a run

The container dies with the machine and there is no exit code. On start, the
runner looks for runs whose `status.json` still says `running`. Each of those
was interrupted:

1. Remove any leftover container: `podman rm -f checkover_<run_id>`
2. Follow section 4.

The temporal history is restored automatically on the retry, as after any
other failure.

---

## 8. Starting a fresh series

This is the clean 1.0 restart (2026-09). A new series needs a new output root
and a new state dir. The old revisions do not carry over, because the code that
computed them changed.

1. WoC withdraws every revision of the old series. Its reader picks the
   largest version folder, so an old `1.2` left anywhere would be served ahead
   of the new `1.0`.
2. Move the old tree aside. Do not mix it into the new one:
   ```bash
   mv /data/output /data/output_old_series
   mv /data/state  /data/state_old_series
   mkdir -p /data/output /data/state
   cp -a /data/state_old_series/cache /data/state/   # reference layers only; optional
   ```
3. **1.0 is a baseline, run by hand** (Lucian, 2026-09-23). Its input is the
   full cohort with every extinction claim cleared, and nothing else changed.
   So every claimed site is still an ordinary presence in 1.0, and 1.1 has a
   range to take it out of.
   ```bash
   mkdir -p /data/runs/<run_id>
   Rscript tools/xlsx_to_tsv.R <export>.xlsx /data/runs/<run_id>/export.tsv
   tools/clear_extinction_claims.sh /data/runs/<run_id>/export.tsv /data/runs/<run_id>/input.tsv
   ```
   The first script converts the emailed xlsx without a spreadsheet. The
   second clears the claims, and prints the records, the claims it cleared and
   the taxa they belong to. It then checks that every other column is
   byte-identical to the export.
   - Never open or save the export in a spreadsheet (SERVICE_CONTRACT section
     2). 1.0 is what every later revision compares with, so a byte that
     differs from later deliveries reads as a change forever after.
   - Run it exactly as section 3 does: same `run.json`, same volumes, same
     image. The runner finds it there as its predecessor.
   - No `species_scope`; with no prior revision, every taxon is `new`.
   - Audit it (section 3, step 5). WoC installs 1.0 by hand. Besides the
     revision folder it needs `input.tsv` itself, with its sha256: WoC
     measures what is new for each taxon against the table of that taxon's
     latest revision. For later revisions WoC keeps the table it sends, so this
     is the only time a run's input leaves the server. It carries exact
     coordinates: send it privately and on its own, never inside the revision
     folder.
4. **1.1 is the runner's first job** (Lucian, 2026-09): the full table as WoC
   sends it, claims included. It rebuilds the taxa with a claim (201 claims in
   19 taxa on the 2026-09-23 export). It also rebuilds every taxon edited in
   WoC since the baseline's export, and any taxon whose text the builder
   writes differently from the xlsx the baseline came from. Before 1.1 runs,
   compare one table from the builder with 1.0's input, so that the second
   kind is known in advance:
   ```bash
   Rscript tools/compare_inputs.R /data/runs/<1.0 run_id>/input.tsv <builder table>.tsv taxa.tsv
   ```
   It prints, per fingerprint column, how many records and taxa differ, and
   whether each difference is whitespace, Unicode form, case or a real edit.
   It prints counts and package ids only. On the 2026-09-23 export, the
   baseline against the full export differs in exactly the 201 claims of 19
   taxa.

The preflight refuses a state dir whose `temporal/` has history but whose output
root has no revisions, so the old history cannot leak into the new series by
accident.

---

## 9. Trial runs on the server

Before the runner exists, and after every new image, test with the stand-in
runner. It builds everything under `/data/trial/<name>/`, never touches the real
volumes, and uploads nothing:

```bash
tools/service_trial.sh container demo_c demo_data/WoC_demo_Pontastacus.tsv
tools/service_trial.sh host      demo_h demo_data/WoC_demo_Pontastacus.tsv
Rscript tools/compare_revisions.R /data/trial/demo_h/output/1.0 /data/trial/demo_c/output/1.0 diff.md
```

It prints the exit code, the audit verdict, the elapsed time, the peak memory
and the time per phase. It also copies `preflight.json`, `status.json`,
`manifest.json`, `records_used.tsv`, `_audit_report.json` and one
`package_metadata.json` into `samples/`, unedited. To time runs of N taxa of
mixed sizes:

```bash
Rscript tools/pick_taxa.R /path/to/full_input.tsv 10 ids10.txt
tools/service_trial.sh container q9_10 /path/to/full_input.tsv @ids10.txt
```
