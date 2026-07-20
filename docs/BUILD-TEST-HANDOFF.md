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
- No manifest was regenerated locally and no source/data byte was promoted by
  this governance pass.
- Existing parallel app/UI changes were preserved and remain owned by the Pass 4
  implementation work.

### Residual risks and next exact action

- The event-keyed v2 fixtures ran on head `e9b8913`, but the new normalized-fetch
  and measurement-only-context changes are not yet pushed or validated in the
  pinned R runner. `SCIENCE-CONTRACT.md` intentionally remains HOLD.
- The official-release source was fetched, but no complete candidate family has
  passed; API/schema and source-linkage gaps required the reviewed adaptation
  above. The NEON token remains secret and must never enter logs or artifacts.
- The committed search index and manifest will intentionally fail exact-byte
  gates until validator-produced candidates are reviewed and promoted.
- Public app/cover deployment still points to pre-Pass-4 bytes until merge and
  republish.

Next: commit/push the normalized-fetch and measurement-only-context contract,
run ordinary validation, apply `build-vegetation-candidate` to that exact PR head,
inspect/promote the complete validator artifact, re-run green head/merge checks, then
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
