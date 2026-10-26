# Project status

## Status

- Release status is **HOLD** (see `AGENTS.md` and
  [SCIENCE-CONTRACT.md](SCIENCE-CONTRACT.md)). The public app is bundle-only and
  deploys when a reviewed PR merges to `main`.
- Every merge to `main` runs the post-deploy revision check
  (`.github/workflows/post-deploy.yml`, described in [DEPLOY.md](../DEPLOY.md)).
- The monthly refresh is read-only and uploads artifacts only.

## Science contract status

- Contract `NEON-VST-DP1.10098.001-v3` (PR #19): multi-bole secondary boles
  under Option A, the shared size screen, no personnel fields, the hk
  statistics rules, QC evidence and the NEON issue log, and cedar's
  2026-10-26 rulings (legacy-schema history state, reviewed partial events,
  disturbance labels, superseded and duplicate rows, parent-tag plant
  identity, the 5 cm measuring-point rule, the tree-first default channel,
  held-plot and cacti disclosures, status-only rows). Every rule and its
  numbers: [SCIENCE-CONTRACT.md](SCIENCE-CONTRACT.md) items 1 to 15.
- The family on `main` stays v2 until #19 merges with its promoted official
  v3 candidate.
- Deferred with a named unblocker: S3 (a woody "below 10% cover" state) and
  S5 (a `small_tree_dbh` channel); see the contract's "Deferred" paragraph.

## Deliberately deferred

- **`held_identity_conflict` hold.** Conflicting event x individual x
  temporary-stem rows stay held rather than resolved by row order or a preferred
  value. Justified in [EXPERT-REVIEW.md](EXPERT-REVIEW.md).
- **Cover keeps only the Atlas route.** The cover stays a Living Poster V1 with
  one app-native entry; other routes wait. See
  [SUITE-LEARNING-HANDOFF.md](SUITE-LEARNING-HANDOFF.md).
- **Artifact-only refresh.** The refresh never publishes a review branch or opens
  a PR; a person promotes a validated artifact. See [DEPLOY.md](../DEPLOY.md) and
  `.github/workflows/refresh-data.yml`.
- **RELEASE-2026 pin.** The source family is pinned to `RELEASE-2026`; the fetch
  warns when a newer release is published. Rollover checklist, every pin location:
  - `RELEASE-2026` tag: `.github/workflows/refresh-data.yml` (dispatch default
    and fetch env), `global.R`, `scripts/fetch_veg_data.R`,
    `scripts/bundle_veg_data.R`, `scripts/refresh_data.R`,
    `scripts/verify_bundle.R`, `scripts/write_source_receipt.R`,
    `scripts/write_data_quality_audit.R`,
    `scripts/validation/inspect_vegetation_candidate.py`,
    `scripts/validation/test_bundle_contract.R`,
    `scripts/validation/test_runtime_bundle_gate.R`.
  - 49 / 4,365 / 11 measurement-only linkage inventory: `global.R`,
    `scripts/verify_bundle.R`, `scripts/write_source_receipt.R`,
    `scripts/write_data_quality_audit.R`,
    `scripts/test_build_script_portability.R`,
    `scripts/validation/inspect_vegetation_candidate.py`.
  - Inspector hash: `.github/workflows/ci.yml` (`vegetation_inspector_hash`) and
    [data-bundling-pattern.md](data-bundling-pattern.md); both change whenever
    `scripts/validation/inspect_vegetation_candidate.py` changes.
  - Regenerated receipts: `data/source/*`,
    [VEGETATION-SOURCE-RECEIPT.md](VEGETATION-SOURCE-RECEIPT.md), and
    `manifest.json` come from the validated candidate artifact, never by hand.
- **bslib `nav_menu` axe finding.** axe reports `aria-required-children` on the
  bslib `nav_menu` markup; the fix belongs upstream in bslib, not in this repo.
- **Connect response headers.** CSP, HSTS, and `frame-ancestors` are set by
  Connect Cloud, not the repo; see [DEPLOY.md](../DEPLOY.md).
- **Branch protection.** The `main` ruleset (PR plus `release_contracts`, no
  force-push or deletion) is an owner setting; see [DEPLOY.md](../DEPLOY.md).

## Owner decisions

- **Option B for multi-bole trees.** v3 ships Option A (secondary boles of
  2.5 to 10 cm lift the hold but stay out of basal area, density and QMD).
  Option B would count them, change every forest basal area, and need the
  Atlas bundle regenerated.
- **A third `small_tree_dbh` channel (S5).** Spec ready; needs the picker, the
  search grid (126 rows) and the Atlas to accept a third channel.
- **Curation sign-off (S1, S2).** `scripts/curation/*.csv` rows were reviewed
  by the cedar and lead agents (`reviewed_by = agent:cedar+lead`). A person
  should confirm the 57 partial-event verdicts and the 47 disturbance labels,
  in particular the 9 `fire_unspecified` rows.
- **measuredBy / recordedBy in git history.** v3 stops publishing them; they
  remain in earlier commits of `data/sites/*.rds`. Rewriting history is the
  owner's call.
- **Atlas adapter.** The Atlas Veg adapter must accept
  `supported_legacy_schema`, the curated partial events, `plant_id` and the
  row exclusions before it reads a v3 family (note left for cass).
- **Branch protection and the CARTO key** stay owner settings.

## Post-merge steps

- Confirm the `Post-deploy revision check` run for the merge commit is green.
- Work through the post-merge items in [RELEASE-CHECKLIST.md](RELEASE-CHECKLIST.md)
  (CARTO tiles, axe at 375 and 1440 px).
- Add a dated `[Claude]` or `[Codex]` entry to
  [BUILD-TEST-HANDOFF.md](BUILD-TEST-HANDOFF.md) with the next action.
