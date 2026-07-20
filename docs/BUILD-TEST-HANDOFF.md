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

1. The legacy canonical bundle discarded published source `uid`, event and
   temporary-stem locator fields, and complete per-plot/per-year sampling
   opportunity source rows.
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
- At the initial 13:54 working receipt the validation/refresh workflows were
  unrun. The exact later attempts and their non-green outcomes are appended
  below; none is a release receipt.

### Exact PR/candidate execution update — 2026-07-19 16:29 MST

- Draft PR: <https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/4>;
  reviewed remote head at the start of this subpass:
  `e9b8913230255ce35d7dcbc695676506aea34d9f`.
- Exact-head ordinary validation run `29706628148` passed the synthetic
  event-keyed science fixtures, then failed the expected committed-byte gate:
  the still-unpromoted legacy HARV bundle does not carry v2 support fields. This
  is not a green release receipt and did not mutate repository data.
- Exact labeled candidate run `29706634767` fetched all 42 official
  RELEASE-2026 sites successfully. Raw artifact `8448250381`,
  `vegetation-raw-e9b8913230255ce35d7dcbc695676506aea34d9f-29706634767`,
  records DOI `10.48443/pypa-qf12`, full-release selection,
  `neonUtilities 4.0.1`, and raw family SHA-256
  `02751f43d42898996dd0f54c916e545cf60e1cba96a9ca6df40e00a215a3623c`.
  The build stopped at CLBJ because its first discovered measurement key lacked
  a matching `vst_perplotperyear` row; no candidate artifact was emitted or
  promoted.
- Full-family comparison of that raw artifact against the preceding fetch found
  zero semantic table differences across all 42 sites × three required tables.
  The byte differences were API row-order noise. Source staging now materializes
  portable vectors, validates unique nonblank published `uid`, sorts in UID byte
  order, resets row names, and records the normalization before pinned RDS v3
  serialization.
- The full RELEASE-2026 join audit found 49 measurement-only `plotID × eventID`
  keys across 11 sites, preserving 4,365 apparent-individual rows. The working
  contract keeps every measurement row, marks records and contexts with
  `opportunity_source_missing = TRUE`, assigns both channels
  `held_opportunity_source_missing`, retains separately named
  measurement-sourced count/date-range fields, and invents no opportunity UID,
  date/year, effort, presence, design, coordinates, area, absence, or
  denominator. Published `opportunity_source` remains source-exact.
- Earlier failed/cancelled attempts retained for audit: candidate run
  `29704097876` exposed apparent-individual locator collisions; manual run
  `29706519086` was cancelled before fetch because the owner-label PR route is
  mandatory; labeled run `29706575984` exposed the species-rank-without-name
  fixture and was cancelled after diagnosis.
- Local post-change checks at this receipt: `git diff --check` passed and
  tree-sitter parsed all 23 R files with no syntax errors. No local R executable
  exists, so the expanded science/DQA/verifier fixtures remain unclaimed until a
  new exact labeled Actions candidate runs on the next pushed head.

### UI release-gate closeout — 2026-07-19 18:19 MST

**Outcome: STATIC PASS / LIVE BROWSER PENDING / SCIENCE HOLD UNCHANGED / NO
DRIVER BYTE CHANGE.** This subpass retained the approved Living Poster art, hook,
promise, and CTA while changing the in-app evidence, navigation, map, Plant
Career, responsive styling, and their static browser contract. It does not
authorize source promotion or publication.

- The in-app poster no longer carries the three-item metric/trust strip above the
  fold. Its 42-place scope now appears only after the `Pick a place` CTA in the
  gateway, and the browser contract rejects any return of that poster strip.
- The hero and structure summary now surface one site-wide coverage note for
  every recorded plant form, explicitly separate those counts from the active
  tree-DBH or shrub/sapling-basal view, and state that unmatched measurement rows
  are neither zero nor plant absence. The note links to an exact plot-event CSV
  ledger; the existing full-data ZIP remains the path to every preserved matching
  measurement row.
- Multi-stem shrub/sapling careers now agree internally: supported measurements
  are labeled `Measured · stems not aligned for change`, the card reports `0/N`
  comparable events, and the app explains why current structure remains useful
  while no change line or rate is shown.
- The 340 px Plant Career export source now shrinks safely inside a 320 px
  viewport, the threshold-search row can wrap without overflow, normal green text
  uses an AA-safe token, and dark-mode comparable/held evidence chips have explicit
  high-contrast surfaces.
- Low-risk gateway, overflow-menu, tour, download, evidence-info, pin open/close/
  resize, and chart toolbar targets are at least 44 px. The national site map keeps
  its visual marker sizes while adding invisible 44 px minimum hit circles, a
  plain-language solid/dashed/dotted measurement-view key, and a clear statement
  that held means unknown rather than zero.
- Help now begins with a three-step non-scientist path—pick a place, choose a
  story, check the evidence—with comparison methods behind a progressive
  disclosure. Stale instructions that referred to a removed sidebar now point to
  the plant picker above.
- Changed implementation surfaces in this subpass: `server.R`, `ui.R`,
  `R/map_picker.R`, `www/styles.css`, `www/veg.css`, and
  `scripts/check_browser_contracts.mjs`. This handoff entry is the only
  documentation surface intentionally added by this UI closeout.
- `git diff --check`, `node scripts/check_cover.mjs`,
  `node scripts/check_browser_contracts.mjs`, `node --check www/app.js`,
  `node --check www/pincards.js`, and `node --check
  scripts/check_browser_contracts.mjs` passed. Tree-sitter parsed all 26 current R
  files without syntax errors.
- No local R executable or attached browser surface was available in this
  workspace. Runtime R fixtures and live desktop plus 390/375/361/360/320 px QA,
  including PNG export inspection, keyboard order, dark mode, and map taps, remain
  explicit release gates on the exact candidate bytes.

### Runtime and derived-family trust hardening — 2026-07-19

**Outcome: IMPLEMENTED / PINNED R EXECUTION PENDING / SCIENCE HOLD UNCHANGED / NO
DRIVER BYTE CHANGE.** This subpass closes two pre-release review gaps without
changing source data, candidate bytes, UI, or publication state.

- The deployed `global.R` gate now independently reconstructs normalized tree and
  shrub presence, channel record counts, invalid required-metric counts, protocol
  identity-conflict counts, support-state precedence, exact support reasons,
  supported flags, and event keys from preserved measurement/opportunity rows.
  It no longer checks only the special opportunity-source-missing status.
- `scripts/derived_parity.R` uses the deployed `R/veg_helpers.R` consumer path—not
  the builder's embedded summary functions—to reconstruct each site's two
  physical-channel summaries, taxon rows, deterministic presentation channel,
  canonical site row, 84-row channel grid, and network search rows.
  `scripts/verify_derived_parity.R` applies that gate to the exact actual 42-site
  family after `scripts/verify_bundle.R` has independently certified support.
- Synthetic positive and corrupted site/channel/taxon/search-summary fixtures are
  part of `scripts/validation/test_bundle_contract.R`. A separate actual-family
  runtime fixture accepts an untouched bundle and rejects valid-vocabulary status,
  reason, and record-count mutations.
- Candidate and ordinary CI run both gates. The official refresh workflow no
  longer exposes bounded query inputs, and release/runtime verification requires
  `FULL_RELEASE` at both receipt bounds. The fetch script retains closed-month
  inputs only for non-promotable local diagnostics.
- Local static closeout passed `git diff --check`, Ruby workflow parsing, shell
  syntax for every workflow `run:` block, actionlint 1.7.10, tree-sitter parsing
  of all 26 R files, both Node cover/browser contracts and JavaScript syntax,
  exact workflow-wiring assertions, and the legacy 42-site source-family hash
  guard. These checks do not make the legacy data family release-eligible.
- No local R executable is available. The new consumer parity, runtime mutation
  fixture, 42-site numerical comparison, and exact candidate build remain
  unclaimed until the next pinned R 4.5.2 Actions run.

### Final science-audit corrections — 2026-07-19

**Outcome: IMPLEMENTED / PINNED R EXECUTION PENDING / SCIENCE HOLD UNCHANGED / NO
DRIVER BYTE CHANGE.** A final independent audit found one RELEASE-2026 presence
blocker and three traceability/parity gaps; the working implementation now closes
them without promoting any candidate bytes.

- Builder, deployed runtime, release verifier, and DQA independently normalize
  exact `Y`/`Yes` to present and `N`/`No` to absent. Synthetic fixtures cover
  sampled absence plus both conflict directions (`N` with records and `Y`
  without records).
- The canonical basal measurement-height field now accepts the actual published
  `basalStemDiameterMsrmntHeight` source column, with a raw-to-canonical fixture.
- Selected plot events are tested as atomic multi-stem units when source-row
  dates differ or are missing; no stem may be filtered out after event selection.
- Runtime, verifier, DQA, and consumer parity now independently reject stored
  `live`, year, taxonomy derivations, permanence, composite keys, mapping-match
  state, and taxonomy resolution that disagree with their preserved lower-level
  fields. Fixtures rebuild embedded summaries after corrupting rows to prove a
  coherent row-plus-summary mutation cannot pass.
- Mapping/tagging UIDs are required and preserved as `mapping_source_uid`.
  Multiple rows tied at the latest created timestamp for one physical plant fail
  before joining, rather than receiving an arbitrary UID/row-order/taxon winner.
- No local R executable was available. Static parsing and diff checks are local
  prerequisites only; all synthetic fixtures and the complete actual-family
  runtime/verifier/DQA/parity path remain mandatory on pinned R 4.5.2.

### Final release and accessibility corrections — 2026-07-19

**Outcome: IMPLEMENTED / EXACT CANDIDATE PENDING / SCIENCE HOLD UNCHANGED / NO
DRIVER BYTE CHANGE.** Independent release and UX audits found no static P0, but
closed the following issues before the candidate was allowed to run:

- Ordinary pull-request validation now checks out and proves the exact
  `pull_request.head.sha`; it no longer certifies GitHub's synthetic merge ref
  while describing the receipt as an exact reviewed-head check.
- Manual refresh dispatch is restricted to the repository owner on the default
  branch. The owner-labeled same-repository PR route remains the only way to
  build a source-refresh candidate for a feature head.
- Bright canopy controls, overview doors, popover/modal headers, DataTables
  inputs, hover/selected rows, and modal details use explicit readable dark-mode
  foregrounds and surfaces.
- Pinned cards clamp their complete rendered rectangle after creation, drag,
  scale, and viewport change. Resize also caps its scale to the chart bounds, so
  Close/Open/Resize controls cannot be stranded off-chart.
- Both Living Poster surfaces retain the canonical 1672×941 PNG fallback but now
  serve 317 KB full and 90 KB compact WebP variants through responsive `srcset`;
  their exact provenance, sizes, hashes, and byte budgets are contracted.
- The Pages skip target is programmatically focusable for reliable focus transfer.
- Promotion remains manual but is a hard release procedure: artifact identity
  must equal the labeled candidate head/run; the independent inspector and
  checksum ledger must pass; the promotion commit's parent must equal that head;
  and its changed paths must equal the ledger allowlist before exact promoted-head
  CI can authorize merge.
- Local preflight at `2026-07-19 19:31 MST` passed diff whitespace, cover and
  browser contracts, JavaScript syntax, source-family hash, YAML parsing,
  actionlint, shell syntax for repository scripts and every workflow `run:`
  block, CSS structure, all 26 R files through tree-sitter, and exact responsive
  WebP dimensions/copies. These remain static prerequisites, not an R/candidate
  or public-deployment receipt.

### Exact pinned-R correction receipt — 2026-07-19

- Exact-head ordinary run
  [29712763894](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29712763894)
  proved checkout SHA `6f0783020edb100f6b81b1726d0745f6f6b6fbf2`, restored the
  pinned runtime, passed deterministic numeric and all static contracts, then
  stopped in the synthetic helper gate before any byte promotion.
- The failure printed equal calendar dates while `identical()` also compared an
  implementation-only vector attribute. The fixture now requires `Date`
  inheritance and compares the exact underlying day values, retaining the
  scientific type/value contract without coupling it to lookup-vector names.
- Labeled candidate run
  [29712779229](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29712779229)
  completed the official 42-site fetch for the superseded head. It was cancelled
  before the redundant build after the exact-head fixture failure was diagnosed;
  it is diagnostic evidence only and cannot authorize promotion.
- Follow-up exact-head run
  [29713397503](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29713397503)
  proved the corrected scalar Date fixture, then exposed the same
  implementation-only names on row-aligned numeric summary vectors inside the
  independent DQA reconstruction. Runtime, verifier, and DQA now normalize away
  vector names only after validating exact integer/Date types and before comparing
  row-ordered values; DQA/verifier failures also identify the exact differing
  component (`record_count`, `date_min`, `date_max`, or `date_distinct_n`).
- Its paired candidate
  [29713408161](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29713408161)
  was cancelled during fetch as soon as that superseding correction was required.
  It emitted no candidate artifact and authorized no promotion.
- Diagnostic exact-head run
  [29713740519](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29713740519)
  confirmed that every stored/reconstructed date value and `NA` position agreed;
  only the one-dimensional `tapply()` array attribute differed. Runtime, verifier,
  and DQA now coerce the keyed lookup to a plain row-aligned numeric vector before
  exact comparison. This changes no calendar value, row order, or release byte;
  it removes only an internal array shape from independent validation input.
- Exact-head run
  [29713915212](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29713915212)
  proved that positive measurement count/date summary parity now passes on pinned
  R 4.5.2. Its following negative fixture deliberately changed the `E7` maximum
  date from `2024-08-15` to `2024-08-16`; DQA rejected that corruption with the
  new component-specific message, but the fixture still required its superseded
  generic wording. The fixture now matches the stable DQA message prefix. This is
  a test-expectation correction only: no science rule, source row, or release byte
  changed.
- Exact-head ordinary run
  [29714123262](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29714123262)
  passed the complete event-keyed fixture suite and then stopped, as intended,
  when the legacy committed HARV bundle could not satisfy the new live-DBH
  contract. Candidate run
  [29714226156](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29714226156)
  then fetched all 42 official sites and passed source-family, denominator, DQA,
  derived parity, event-key, helper, export-dictionary, runtime-mutation, and
  manifest gates on two isolated builds. It stopped while sourcing the complete
  app because the new Living Poster used an unqualified `figure()` tag helper.
  The UI now calls `tags$figure()`. No validated artifact was uploaded and no
  candidate byte from that superseded head was promoted.

### Failed/unsafe paths closed

- The previous direct-to-main refresh path is removed.
- The old delete-before-fetch and partial-success behavior is removed from the
  release design; candidate building occurs under runner staging.
- The first-Saturday wall-clock gate is removed.
- No manifest or source/data byte was regenerated or repaired locally. The exact
  validator artifact was promoted only after its head/run identity, 55-file
  inventory, 54-path ledger, and every payload checksum passed independently.
- Existing parallel app/UI changes were preserved and remain owned by the Pass 4
  implementation work.

### Residual risks and next exact action

- Exact-head candidate run
  [29715249829](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29715249829)
  passed both jobs and uploaded validated artifact `8450700945` (28,378,366
  bytes) for builder head `a8ccb56e95f643ba9343ca13d176782ebc050017`.
  Two isolated builds matched byte-for-byte after all 42-site source, science,
  DQA, parity, helper, export, runtime-mutation, manifest, and complete-app-source
  gates passed.
- Independent inspector SHA-256
  `819eca6d2f9a9b0663b8ad075796b0c558c5af07f740d3f5aa780826257416c5`
  passed the exact 55-file artifact / 54-file payload / 42-site inventory. It
  confirmed 68 runtime files, 91 packages, R 4.5.2, raw family
  `e8d78dd776fa4188c3f237548b7d2ab185eb5c03bc7b220991d03753ebca3e29`,
  bundle family
  `3e62514de12b0d7b11cbe8aa53dde76d9f05f65c0174418a3df64e1261a88ffb`,
  and the exact 49 plot-events / 4,365 rows / 11 sites source gap.
- The external inspector initially retained the retired workstation manifest
  locale `en_US` and omitted its validated source-gap object from the printable
  summary. Posit manifests accept a client-locale string; the immutable Linux
  validator reproducibly emits `C`. The inspector now requires `C`, prints the
  source-gap receipt, and fails closed. Repository manifest generation and
  verification also pin that locale explicitly.
- Promotion commit `800bd5ea64d5aa4f2eab194c1b16dcbee5a0638e` has direct
  parent equal to the labeled candidate head. Its changed paths are exactly the
  54 checksum-ledger payload paths, including seven new `data/source/` ledgers;
  all committed blobs independently match the candidate checksums.
- The NEON token remained secret and entered no release artifact. Public
  app/cover deployment still points to pre-Pass-4 bytes until merge and
  republish.

Next: push the promotion and locale-contract commits, require green exact-head
CI, merge, verify Pages and Connect at the merged revision, then append the
production receipt and update the Driver central learning records. Stop before
Ground Beetle Pass 5.

## Pass 4 production closeout receipt — 2026-07-19 MST / 2026-07-20 UTC

**Outcome: OFFICIAL-RELEASE BYTES PROMOTED / PRS #4–#8 PUBLISHED / #59 RESET,
RESPONSIVE, REPEATED-CLICK, AND SPLIT-LOG PASS / RUNTIME PRODUCTION VERIFIED /
DOCS-ONLY AND CENTRAL DRIVER PUBLICATION PENDING / DRIVER HOLD / CONTEXT ONLY /
NO DRIVER DATA BYTE CHANGE.** This entry
supersedes the working receipt's publication status and next action. It does not
rewrite the historical diagnostic and failure record above.

### Official source and candidate

- Exact candidate builder head:
  `a8ccb56e95f643ba9343ca13d176782ebc050017`.
- Exact owner-labelled candidate run:
  [29715249829](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29715249829),
  successful on pinned Ubuntu 22.04 / R 4.5.2 / dated Jammy packages / one-thread
  Haswell OpenBLAS.
- Validated candidate artifact: `8450700945`, 28,378,366 bytes. Retained raw
  artifact: `8450530222`, 29,782,052 bytes.
- Source: official `RELEASE-2026`, provisional data excluded, DOI
  `10.48443/pypa-qf12`, all 42 registered sites.
- Raw family SHA-256:
  `e8d78dd776fa4188c3f237548b7d2ab185eb5c03bc7b220991d03753ebca3e29`.
- Bundle family SHA-256:
  `3e62514de12b0d7b11cbe8aa53dde76d9f05f65c0174418a3df64e1261a88ffb`.
- Candidate inspector SHA-256:
  `819eca6d2f9a9b0663b8ad075796b0c558c5af07f740d3f5aa780826257416c5`.
  The tracked, CI-enforced inspector at
  `scripts/validation/inspect_vegetation_candidate.py` proved the 55-file
  artifact, 54 allowlisted payload paths, 42-site inventory, and exact
  49-event / 4,365-row / 11-site opportunity-source gap.
- Promotion commit:
  `800bd5ea64d5aa4f2eab194c1b16dcbee5a0638e`. Its direct parent is the
  candidate head; its changed paths equal the checksum-ledger payload exactly.
- PR #6 checkpoint derived-byte checksums after its manifest-only promotion:
  `data/site_index.rds`
  `bfead31cb5ed516c8604f11b781979b4c4e2ced20d5b242708fa4ca8f9ffc7f9`,
  `data/search_index.rds`
  `c4d145046d9486d7c7cf2c85339200ba1eaad3cf7e0de22bb2e378c7c944fc4b`,
  and `manifest.json`
  `c9356c29aaa1f6bf869442ceb44eca81c5128c86c9352a1256fbae8c374fac6b`.

The supported verification path remains:

```bash
python3 -B scripts/validation/inspect_vegetation_candidate.py "$VEG_AUDIT_ROOT"
shasum -a 256 data/site_index.rds data/search_index.rds manifest.json \
  scripts/validation/inspect_vegetation_candidate.py
```

`VEG_AUDIT_ROOT` must be a clean extraction of the exact candidate artifact;
the complete download command and external-log rule are in
[data-bundling-pattern.md](data-bundling-pattern.md).

### PR #4 — official family and core app release

- PR: <https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/4>.
- Promotion line: `800bd5e`; final reviewed head
  `5c7456b16abae2569d037bb3b731a9e5065b0906`; exact-head CI
  [29716974286](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29716974286).
- Merge: `987c102b84de98f18c11dd98de6c8113ab7f4c8c`.
- Post-merge CI
  [29717225014](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29717225014)
  and Pages
  [29717224521](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29717224521)
  passed; Connect deployment #55 published the core release.
- Core public export QA inspected the PDF, whole-site ZIP, and species CSV.

### PR #5 — initial site-state Plotly guard (#56 inspected clean)

- PR: <https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/5>.
- Run
  [29717387935](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29717387935)
  failed the expected stale-manifest equality gate and authorized no merge.
- Exact fix head: `5baa6a023a9763d03e15d2341985b8d492e36755`.
  Exact-head CI
  [29718292956](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29718292956)
  passed and emitted artifact `8451426404` (92,308 bytes).
- Merge: `91a7814c9e1275c5a890aed4a9c186485f614e60`.
  Post-merge CI
  [29718542229](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29718542229)
  passed with artifact `8451506471` (92,308 bytes); Pages run
  [29718541621](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29718541621)
  passed.
- Connect deployment #56 reported exact merge `91a7814`, R 4.5.2, and all 91
  packages. Fresh landing and repeated chart clicks produced no Plotly
  `event_data()` warning; only benign package-built-under-R-4.5.3 warnings
  remained. This is a bounded #56 receipt, not proof that the site-state guard
  covered every later first-chart render; #58 ultimately showed that it did not.

### PR #6 — production accessibility, exports, and keyboard controls

- PR: <https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/6>.
- Implementation head: `7c1ced5c68e2ab32bb698f2f1a913f22a46541f9`.
  It adds a real loading-dialog focus boundary and restore path, reduced-motion
  tour behavior, one active-channel plot-summary source shared by standalone
  CSV and ZIP, a Size Lab-local eligible-plant selector, and keyboard
  create/move/resize/close behavior for named pinned cards.
- First exact-head run
  [29719846128](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29719846128)
  passed every science, runtime, source, export, search, and browser gate, then
  failed only committed-manifest equality as expected after runtime source
  changes. Artifact `8452015013` (92,307 bytes) was diagnostic only. Its search
  index was byte-identical to the committed index at
  `c4d145046d9486d7c7cf2c85339200ba1eaad3cf7e0de22bb2e378c7c944fc4b`.
- Only the validator's five changed runtime checksums were promoted. Resulting
  manifest SHA-256:
  `c9356c29aaa1f6bf869442ceb44eca81c5128c86c9352a1256fbae8c374fac6b`;
  promotion head: `e5a12add8b1227453a904ff14741b92a5a435759`.
- Exact-head rerun
  [29720142868](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29720142868)
  passed every step. Artifact `8452100740`,
  `vegetation-structure-derived-e5a12add8b1227453a904ff14741b92a5a435759-29720142868`,
  is 92,307 bytes; archive SHA-256 is
  `6eb1b916e029c7c61d8e25b83a2b09c9cbfff3aa2962bcf5e50e2b0dfb4083cc`.
- Merge: `433bbd25acbe48224a75368c9edd6504e55271bd`.
  Post-merge CI
  [29720341082](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29720341082)
  passed. Artifact `8452189687`,
  `vegetation-structure-derived-433bbd25acbe48224a75368c9edd6504e55271bd-29720341082`,
  is 92,307 bytes with SHA-256
  `c4c84cf70f069fab6d086738e35b6c95c117244a0b9833fcfb5e78b717aa7d49`.
- Pages run
  [29720340743](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29720340743)
  passed. Artifact `8452121645` (`github-pages`) is 3,889,241 bytes with
  SHA-256
  `32bec54bb78b1e190d7369fe77e444c0041d638650c7ae61c663db6647be5675`;
  deployment ID is `5517445662`.
- Connect deployment #57 successfully published exact `433bbd25` under R 4.5.2
  with all 91 packages. Logs inspected before the final interaction sweep show
  only benign `plotly`/`shinyjs` built-under-R-4.5.3 warnings.

### PR #7 — searchable place reset follow-up (published; reset proof passed)

- PR:
  <https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/7>;
  branch `agent/vegetation-site-picker-reset`; implementation head
  `3835451f6945b25eca4ef31b4d0882b6406c07ae`.
- The #57 public sweep found a real reset-path defect after a successful first
  site visit. Returning to Places cleared the visible selection, but the
  server-backed Selectize data object was not re-registered. The picker still
  showed its placeholder while remote search had no site choices, so a second
  place could not be searched and opened.
- The fix defines one `site_picker_choices()` source over the validated site
  table and one `refresh_site_picker()` updater. Session initialization, site
  load, and `reset_to_places()` all call that updater, so selection state and
  remote choices cannot diverge.
- `scripts/check_browser_contracts.mjs` now requires exactly one direct
  `updateSelectizeInput()` implementation, the centralized choice source, and
  both initialization and reset registrations. The implementation head changes
  only `server.R` and that static contract; it changes no source family, bundle,
  index, estimator, support state, or Driver byte.
- Initial exact-head run
  [29722029052](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29722029052)
  passed every preceding source, science, parity, runtime, offline-source,
  search, and browser contract, then failed only committed derived-byte equality
  as expected. Artifact `8452805523`,
  `vegetation-structure-derived-3835451f6945b25eca4ef31b4d0882b6406c07ae-29722029052`,
  is 92,307 bytes with artifact digest
  `sha256:da06e1829f6d6c0a7c597f1240b2a7391ee183419388c4431278f432be4e8365`.
- The generated search index remained byte-identical at
  `c4d145046d9486d7c7cf2c85339200ba1eaad3cf7e0de22bb2e378c7c944fc4b`.
  The only generated manifest change was `server.R` MD5
  `fa51b4efead150f06706232045d443b2`; generated manifest SHA-256 is
  `4fca84d313623f045e0d425b6c9f0464629f12d82ff6a115deb79341fc44ed21`.
- Promotion commit/current head
  `8389c9c2d1a723b03f0e1ab88f64732fe454a134` changes only that exact manifest
  checksum. Exact-head run
  [29722349642](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29722349642)
  passed every `release_contracts` gate. Artifact `8452911612`,
  `vegetation-structure-derived-8389c9c2d1a723b03f0e1ab88f64732fe454a134-29722349642`,
  is 92,307 bytes with artifact digest
  `sha256:dde4ae1bac76051758abdd2f70a8d620c562949a907d6e2ed1b631992457af8d`.
- PR #7 merged at `2026-07-20T06:46:19Z` as
  `0709bd021c7c9f142b1f280aa83b2cf3afd49f30`.
- Pages run
  [29722613509](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29722613509)
  passed. Artifact `8452933484` (`github-pages`) is 3,889,240 bytes with digest
  `sha256:8de384a248795a09547d248e6353f83f2303f4c04291d5531f38ffe7a2ba92f7`;
  successful deployment ID `5517850060` serves the unchanged public URL.
- Main CI run
  [29722614074](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29722614074)
  passed. Artifact `8453019545`,
  `vegetation-structure-derived-0709bd021c7c9f142b1f280aa83b2cf3afd49f30-29722614074`,
  is 92,307 bytes with digest
  `sha256:337816a4e4171b9e629119186979c6bd962d30b5daa33aff8fb601af122300a0`.
- Connect deployment #58 reports exact merge `0709bd0`, R 4.5.2, and all 91
  packages.

### Science-edge and active-channel export QA established on #57

- JORN `tree_dbh` latest plot-summary export contains exactly 50 contexts: 25
  supported `sampled_absence` zeros and 25 `held_sampling_impractical`
  contexts. The UI reports 25 supported sampled plots, zero live plants, stems,
  and species, zero cross-sectional area and stem density, and unavailable QMD,
  height, and biggest-plant metrics. Plant selectors/actions are disabled; the
  supported zero does not offer an invalid plant record.
- WOOD remains on the place splash with an explicit held-not-zero warning. Both
  physical channels carry exactly 50 contexts: 14
  `held_opportunity_source_missing` plus 36 `held_opportunity_unknown`, and zero
  supported contexts. The shrub/sapling channel preserves 452 measurement rows,
  411 live, without converting any row to a denominator or a zero.
- BART carried the selected physical channel correctly. Its shrub/sapling view
  showed 995 live plants, 12 identified species, 2.7 m tallest, 4.1 cm biggest
  basal stem, 35 supported plots, 1,084 stems/ha, 0.2 m²/ha shrub/sapling
  stem-base cross-section, and QMD 1.6 cm. Its tree view showed 922 live trees,
  16 identified species, 29.1 m tallest, 79.2 cm biggest DBH, 24 supported
  plots, 725 stems/ha, 47.3 m²/ha tree-bole DBH cross-section, and QMD 28.5 cm.
- BART's active `shrub_sapling_basal` standalone plot-summary download,
  downloaded as
  `NEON-VegStructure_BART_shrub_sapling_basal_plot-summary_20260720 (1).csv`,
  is 20,791 bytes with SHA-256
  `fddca062b6e9a69ed72dd7f00b27725adc45d773755878fb39f3ec8614259a7e`.
  ZIP download `NEON-VegStructure_BART_data_20260720 (1).zip` is 1,320,874
  bytes; its `plot_summary_latest.csv` member has the same SHA-256 and is
  byte-identical to the standalone CSV.
- The BART ZIP contains exactly eight files: `trees_long.csv`,
  `plot_summary_latest.csv`, `plot_event_contexts_all.csv`,
  `plot_opportunity_source.csv`, `data_dictionary.csv`, `qc_report.csv`,
  `qc_dictionary.csv`, and `README.txt`.
- Manual export inspection also opened a valid one-page letter PDF, the full
  QC/flag CSV, a plant CSV, and the Plant, QC, Size Lab, and Growth PNG exports.
  These established checks do not replace a final deployment smoke after PR #8.

### #58 reset, responsive, and split-log receipt

- The regression path passed on exact #58: BART → Change site → JORN appeared
  as a searchable option → JORN loaded successfully. At loaded BART, Change and
  Plant navigation also passed at both 390 px and 320 px.
- Fresh 390/375/361/360/320 px sessions each showed zero horizontal overflow,
  a visible H1, CTA, Quick tour control, and searchable picker, with no visible
  error or disconnect. Loaded BART at 390 px and 320 px also retained zero
  horizontal overflow.
- The exact #58 browser log stream contained 71 entries and zero warning, error,
  suspect, or disconnect entries. This proves browser-side cleanliness only.
- The Connect worker's server logs separately recorded fresh-load `baBar`
  registration warnings at 23:48:50, 23:53:51, and 23:54:27 MST. Therefore #58
  passes the reset and responsive gates but fails the production clean-server-
  log gate. A browser console with zero warnings is not evidence that the Shiny
  worker emitted none.

### PR #8 — Plotly 4.12 registration lifecycle follow-up

Status: **PUBLISHED / PRODUCTION VERIFIED / CLEAN SPLIT-LOG PASS**.

- PR:
  <https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/8>;
  branch `agent/vegetation-plotly-registration-final`; implementation head
  `4ce0cb7b3a7125780a5c7ca60c28a3eae71a88f5`.
- `event_register("plotly_click")` remains on every rendered `baBar` widget.
  The server now observes raw Shiny input `plotly_click-baBar` and calls
  `plotly::event_data(..., priority = "event")` only inside that emitted-click
  observer. A raw click proves the widget has rendered and registered its event;
  loaded site state alone does not.
- The browser contract requires event registration in the render block, the raw
  click observer around `event_data()`, and removal of the eager intermediate
  reactive. Repeated identical clicks retain event priority.
- First implementation-head CI
  [29723373295](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29723373295)
  passed every preceding source, science, parity, runtime, offline-source,
  search, and browser contract, then failed closed only at committed
  derived-byte equality. Artifact `8453312072`,
  `vegetation-structure-derived-4ce0cb7b3a7125780a5c7ca60c28a3eae71a88f5-29723373295`,
  is 92,307 bytes with digest
  `sha256:986bd3f29a16cd945dedb97f2dc2e26ab750e215a4283c164b066417778d0f72`.
- The generated search index remained byte-identical at
  `c4d145046d9486d7c7cf2c85339200ba1eaad3cf7e0de22bb2e378c7c944fc4b`.
  The only generated manifest change was `server.R` MD5
  `855a7c350c2f79bc9546db2ce20fbdaf`; generated manifest SHA-256 is
  `b497f2e9f4228d772745b220da3f2ba6e9da00b8af4fec61af4272103d2e330c`.
- Promotion commit/current reviewed head
  `06904fe227119c2b87f80c9dc8334f19f7f79b05` changes only that exact manifest
  checksum. Exact-head run
  [29723718100](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29723718100)
  passed every `release_contracts` CI gate. Artifact `8453460662`,
  `vegetation-structure-derived-06904fe227119c2b87f80c9dc8334f19f7f79b05-29723718100`,
  is 92,307 bytes with digest
  `sha256:a37b64aa7bff81a4f963142ee9e19bb2737a5758697d29c222d92e4356229871`.
- PR #8 merged as
  `d566b30ec8eb52ae984325da402cadfec3f18bc9`.
- Main CI
  [29724062900](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29724062900)
  passed. Artifact `8453599842`,
  `vegetation-structure-derived-d566b30ec8eb52ae984325da402cadfec3f18bc9-29724062900`,
  is 92,307 bytes with digest
  `sha256:cf0fb363314e40004036652bd8968f8849196e51f9f626492c49e6bc08104f5f`.
  Its downloaded `manifest.json` and `data/search_index.rds` are byte-identical
  to the promoted files at SHA-256
  `b497f2e9f4228d772745b220da3f2ba6e9da00b8af4fec61af4272103d2e330c`
  and `c4d145046d9486d7c7cf2c85339200ba1eaad3cf7e0de22bb2e378c7c944fc4b`,
  respectively.
- Pages run
  [29724062095](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29724062095)
  passed. Artifact `8453482888` (`github-pages`) is 3,889,230 bytes with digest
  `sha256:24dda716e7d739d288cbacac2e958ffb587b86cc999ddb0b4e0072f0ac23cba1`;
  deployment `5518123037` succeeded.
- Connect deployment #59 published exact merge `d566b30`, R 4.5.2, and all 91
  packages in four seconds.

### Expected versus actual, failures, and rollback

- Expected: a source refresh would reveal unsupported observation/opportunity
  relationships rather than synthesize zeros. Actual: 49 measurement-only
  events / 4,365 rows / 11 sites are preserved and held, with no invented
  opportunity field or denominator.
- Expected: source-code changes would make the generated manifest differ on the
  first PR #5–#8 implementation run. Actual: each run failed closed; only exact
  validator-derived checksums were promoted, and each promoted exact-head run
  passed.
- Expected: public chart clicks would not create server-side registration
  warnings. Actual: #56's inspected landing/repeated-click window was clean, but
  #58 fresh-load worker logs exposed the remaining Plotly 4.12 lifecycle race.
  Guarding `event_data()` only on loaded site state was insufficient because the
  server could read during the first chart flush before `event_register()` had
  prepared the widget. PR #8 waits for the emitted raw click input instead; #59
  repeated the same first click twice without a worker warning.
- Expected: returning to Places would restore an empty selection backed by the
  complete validated search choices. Actual on #57: only the visible selection
  reset. PR #7 head `3835451` fixed both halves through one updater and a static
  regression contract; #58 proved the complete second-site path.
- No failed artifact became release authority. No PR #5–#8 source-family,
  bundle-family, index, or Driver data byte changed; PR #8 exact-head validation
  preserved those exact families.
- Runtime rollback target for PR #8 is merge `0709bd0` / Connect #58; earlier
  rollback targets remain `433bbd2` / #57 and `91a7814` / #56. The official
  RELEASE-2026 data family is identical across #55–#59 and the promoted PR #8
  head.

### Final #59 live matrix and remaining closeout

- BART shrub/sapling retained 995 live plants, 12 identified species, 2.7 m
  tallest, 4.1 cm biggest basal stem, 35 supported plots, 1,084 stems/ha,
  0.2 m²/ha shrub/sapling stem-base cross-section, and QMD 1.6 cm.
- The first `baBar` point remained *Fagus grandifolia* at 529. Clicking that
  same point twice opened the detail modal twice, proving repeated identical
  events remain observable.
- BART → reset restored the validated picker; typing JORN produced exactly one
  JORN choice, and JORN loaded. Its tree channel retained the exact supported-
  zero state: zero live plants, zero species, 25 supported plots, zero stems/ha,
  0.0 m²/ha tree-bole DBH cross-section, unavailable QMD, and disabled
  plant/highlight actions.
- WOOD remained on the place splash with tabs hidden and the exact held-not-zero
  notice; unsupported observations did not become an ecological zero.
- At exact viewport widths 390/375/361/360/320 px, the cover had zero horizontal
  overflow; H1, CTA, tour control, and picker remained horizontally in bounds;
  the splash stayed visible, tabs stayed hidden, and no visible failure appeared.
  Loaded BART at 320 and 390 px also had zero overflow, visible app main, hidden
  splash, and no failure. Change and Plant selected successfully at 320 px.
- The #59 browser-log slice from `07:18:54Z` through `07:23:16Z` contained 33
  entries, all level `log`, with zero warning, error, `baBar`, `event_data`,
  `undefined`, or disconnect entry.
- Refreshed #59 manager logs after all interactions remained limited to startup
  plus the two benign package-built-under-R-4.5.3 warnings for `plotly` and
  `shinyjs`. They contained zero `baBar`, `event_data`, source-not-registered,
  `undefined`, or Shiny error; clean interactions emitted no new server line.
- The earlier manual plant/QC/PDF/PNG receipts remain release authority because
  PR #8 changed only the Plotly event observer/static contract and the #59 main
  artifact preserved the exact promoted manifest/search bytes.

### PR #9 — docs-only publication receipt

- Docs-only PR
  [#9](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/9)
  changed only `README.md` and eight app-local documentation files. Exact head
  `68497de328b2723aa997e7016397bfd266e22337` passed PR CI
  [29724891796](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29724891796).
  Validator artifact `8453930434` is 92,307 bytes with digest
  `sha256:f92b5a9fc3d7eb1e9dbb70b894bed6882eff9c94d22a5907d3ec0207225684ce`.
- PR #9 merged as `3391e702e7be80a3f049c905782661f043be8db8`.
  Main CI
  [29725238531](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29725238531)
  passed. Artifact `8454053110`,
  `vegetation-structure-derived-3391e702e7be80a3f049c905782661f043be8db8-29725238531`,
  is 92,307 bytes with digest
  `sha256:71ec40bdfe63c2e2987a622c0759ad6c31bf3a749ef6c10a008a82afc1b9ef7f`.
  Downloaded `manifest.json` and `data/search_index.rds` are byte-identical to
  the PR #8 release at SHA-256
  `b497f2e9f4228d772745b220da3f2ba6e9da00b8af4fec61af4272103d2e330c`
  and `c4d145046d9486d7c7cf2c85339200ba1eaad3cf7e0de22bb2e378c7c944fc4b`.
- Pages run
  [29725237988](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29725237988)
  passed. Artifact `8453952616` is 3,902,344 bytes with digest
  `sha256:d871b82ae790998f03d8228981bcce3921be5724a97b52eabd27d72ee0948265`;
  deployment `5518345576` succeeded.
- Connect #60 published exact docs merge `3391e70`, R 4.5.2, and all 91
  packages in four seconds. Its logs retain only the two benign
  package-built-under-R-4.5.3 warnings and contain no `baBar`, `event_data`,
  source-not-registered, `undefined`, or Shiny error.
- Fresh public smoke proved the Pages Living Poster still had its artistic H1,
  Connect CTA, Driver suite link, illustration disclosure, and zero overflow.
  The Connect landing retained the artistic H1, CTA, searchable picker, splash,
  hidden tabs, zero overflow, and no visible failure.

The app-local runtime and documentation closeout is complete. Publish the
central Driver learning record as its own documentation-only identity.

Stop before the next companion app. The scientific disposition remains **HOLD /
CONTEXT ONLY / NO DRIVER DATA BYTE CHANGE** throughout.

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
