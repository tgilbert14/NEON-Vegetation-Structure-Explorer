# Build–Test handoff

This is the release boundary for Vegetation Structure. A local render, a green
build, a Connect deployment log, and a semantically healthy public app are
different receipts. Release requires all of them on the same reviewed bytes.

## Pass 4 working receipt — 2026-07-19

**Outcome: IN PROGRESS / SCIENCE HOLD / NO DRIVER BYTE CHANGE.** Governance,
source-receipt, deterministic validation, staged official-release refresh, and
Living Poster receipt surfaces were established. The current 42-site legacy
family is not release-certified because it lacks complete upstream provenance and
discarded event/stem and sampling-opportunity identity. No current-source Driver
promotion is authorized.

### Candidate lineage and publication identity

- Working branch: `agent/vegetation-pass4`.
- Starting commit and `origin/main`: `a9e7fb4c54b85e0d0f47ed45aa687345abb8374c`.
- Watched publication branch: `main`.
- Public Pages: <https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/>.
- Connect content: `019ee110-8fd3-abae-aee3-02ea8e4274c8`.
- Public app: <https://019ee110-8fd3-abae-aee3-02ea8e4274c8.share.connect.posit.cloud/>.
- This entry is not a promotion receipt. Final PR, merge, Actions, Connect, Pages,
  semantic-health, export, and responsive receipts remain pending.

### Baseline bytes and inventory

- Legacy source family: 42 site bundles, introduced together by
  `6b758f993acb09b9a90425391213b26e2320d0ca` on 2026-06-19.
- Legacy source-family SHA-256:
  `b00197f2069c7f537a2e7736e33a3786853151cf55e7918eb910efcc2a7a670c`.
- Baseline `data/site_index.rds` SHA-256:
  `c3c8a698eaffc9d8a820880601a842f9eff371a3a96649e7a54bedbffbb45d10`.
- Baseline `data/search_index.rds` SHA-256:
  `c97d9a1e6dccd67b01a140017155a168e54b6a441dd8fedca1683d6d760ad9b8`.
- Baseline `manifest.json` SHA-256:
  `8296161efb608e1d0dcffd6acaa72e8e751b89820f942aa36c81b906e5aca191`.
- Baseline manifest: R 4.5.2, 91 packages, 56 runtime files.
- Current observation years in bundles: 2014–2024. Original build date, official
  release, query cutoff, raw digest, and fetch runtime are `NA`.

### Findings that block legacy promotion

1. The legacy canonical bundle discarded official event × individual ×
   temporary-stem identity and complete per-plot/per-year sampling opportunity.
2. WOOD contains qualifying live woody measurement rows, but its 14 unique
   measurement plot IDs match zero of 36 denominator plot IDs. Legacy
   `stand_site()` returning `NULL` is an unmatched-denominator state, not evidence
   of no woody vegetation.
3. Eight runtime files had MD5 drift relative to the committed manifest at the
   opening audit: `global.R`, `R/site_metadata.R`, `R/veg_helpers.R`, `server.R`,
   `ui.R`, `www/app.js`, `www/pincards.js`, and `www/veg.css`.
4. The old search-index builder stamped `Sys.Date()`, preventing byte-identical
   rebuilds.
5. The old refresh deleted production bundles before fetching, accepted a
   30-of-42 floor, and pushed directly to `main`. Scheduled runs could miss the
   first-Saturday gate after runner/time-zone delay.
6. The old social image was an 849-byte white placeholder. The Pass 4 social
   image is a nonblank exact 1200×630 composition.

### Governance and release work added

- Repository instructions, source receipt, draft science contract, Driver
  package, suite handoff, release checklist, and art provenance.
- Pinned ordinary validation for source/static/browser/helper/bundle/index/
  manifest/offline-boot contracts.
- Exact 42-site inventory and source-family guard.
- Official-release refresh targeting `RELEASE-2026` and DOI
  <https://doi.org/10.48443/pypa-qf12> with token-protected, empty raw staging.
- Two isolated candidate builds, exact-byte comparison, durable raw/bundle
  ledgers, read-only artifact publication, and no direct `main` push.
- A repository-owner-only `build-vegetation-candidate` PR label path that builds
  `vegetation-release-candidate-<head_sha>-<run_id>` from the exact
  same-repository PR head. Manual and scheduled runs upload diagnostic artifacts
  but cannot create a branch, open a PR, or publish data.
- A single monthly cron trigger with no second wall-clock calendar gate.
- A deterministic 84-row data-quality audit (42 sites × `tree_dbh` and
  `shrub_sapling_basal`) that inventories every support state, explicit
  absence, held reason, invalid required metric, preserved `dataQF`, non-ok tag
  status, changed measurement location, and exact source/contract receipt. The
  verifier regenerates it byte-for-byte and checks its dedicated SHA-256 ledger.
- A separate exact 84-row site × physical-channel network index. The national
  map keeps one deterministic default view, while species/threshold search and
  the in-app channel switch preserve every supported secondary channel.
- Analysis-ready tree exports now retain the registered mapping and measurement
  review fields, and both the data and QC dictionaries fail when any emitted
  column is undocumented.

### Cover and art receipt

- Living Poster source and both exact copies: 1672×941 RGB PNG, SHA-256
  `d972a85d5f790dbba2ec4f4f74fa4046b4d4c2b2a905b17341bf13d8eb9da860`.
- Social source `docs/social-card.html` SHA-256:
  `5815e16f29122fb7f82758761150974121c3977338b143ea4d1425f98f2db9dd`.
- `docs/og-image.png`: 1200×630 RGB PNG, SHA-256
  `4c572308daaa8c60e9c51658772b2b4adf996bf8d9c9bce0f405cf9326c87cae`.
- Full provider, prompt, disclosure, accessibility, and evidence boundary are in
  [IMAGE-PROVENANCE.md](IMAGE-PROVENANCE.md).

### Execution environment and checks so far

- Closeout snapshot recorded at `2026-07-19 11:49 MST`
  (`America/Phoenix`) from macOS workspace branch `agent/vegetation-pass4`.
- Read-only inventory/hash/history checks used `git`, `rg`, `find`, `md5sum`,
  `sha256sum`, `file`, and Python 3.
- Static worktree checks used `git diff --check` and Node 24 syntax/cover checks.
- Release/process subpass at `2026-07-19 13:21 MST`: Ruby/Psych parsed both
  workflow YAML files; pinned actionlint 1.7.7 reported no findings; the Living
  Poster and browser-contract Node checks passed; and `git diff --check` passed.
  The first linter invocation used an invalid `-color never` CLI form, made no
  repository change, and was rerun successfully with `-color=false`.
- Final local preflight at `2026-07-19 13:54 MST`: pinned actionlint 1.7.7,
  Ruby/Psych YAML parsing, Node cover/browser contracts, shell syntax,
  `git diff --check`, and tree-sitter parsing of all 23 R files passed after the
  artifact allowlist, species-resolution, dual-channel search, supported-zero,
  and export-completeness fixes. All eight pinned geographic source URLs,
  including `wk 0.9.5`, returned HTTP 200.
- No local R executable was available. Authoritative R parsing, package restore,
  helper/science fixtures, two-build determinism, manifest generation, offline
  boot, and exact committed-byte equality must run in pinned GitHub Actions.
- The new validation/refresh workflows are unrun at this working receipt; do not
  call them green until run URLs and artifact digests are appended.

### Failed/unsafe paths closed

- The previous direct-to-main refresh path is removed.
- The old delete-before-fetch and partial-success behavior is removed from the
  release design; candidate building occurs under runner staging.
- The first-Saturday wall-clock gate is removed.
- No manifest was regenerated locally and no source/data byte was promoted by
  this governance pass.
- Existing parallel app/UI changes were preserved and remain owned by the Pass 4
  implementation work.

### Residual risks and next exact action

- The event-keyed v2 bundler, opportunity/support model, and synthetic contract
  fixture are integrated in source but have not run in the pinned R validator or
  against the official release. `SCIENCE-CONTRACT.md` intentionally remains HOLD.
- The official-release candidate has not yet been fetched; API/schema changes may
  require reviewed adaptation. The NEON token must remain secret and never enter
  logs or artifacts.
- The committed search index and manifest will intentionally fail exact-byte
  gates until validator-produced candidates are reviewed and promoted.
- Public app/cover deployment still points to pre-Pass-4 bytes until merge and
  republish.

Next: run ordinary validation, apply `build-vegetation-candidate` to the exact PR head,
inspect/promote the validator artifact, re-run green head/merge checks, then
perform Connect/Pages semantic, export, desktop, and compact-width QA. Only after
that may this entry be replaced with a production receipt and the Driver central
learning records updated.

## Permanent release gates

The detailed checklist is [RELEASE-CHECKLIST.md](RELEASE-CHECKLIST.md). In brief:

1. exact official source/identity/opportunity receipts;
2. hard science fixtures and app/export parity;
3. deterministic search/manifest/candidate bytes;
4. green exact PR head and merge;
5. matching Connect and Pages release identity;
6. semantic app/site readiness, inspected exports, and desktop plus
   390/375/361/360/320 public QA;
7. app-local and central Driver/suite handoff with an explicit disposition.
