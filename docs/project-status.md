# Project status

## Status

- Release status is **HOLD** (see `AGENTS.md` and
  [SCIENCE-CONTRACT.md](SCIENCE-CONTRACT.md)). The public app is bundle-only and
  deploys when a reviewed PR merges to `main`.
- Every merge to `main` runs the post-deploy revision check
  (`.github/workflows/post-deploy.yml`, described in [DEPLOY.md](../DEPLOY.md)).
- The monthly refresh is read-only and uploads artifacts only.

## Science contract status

Filled in by the lead.

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

- (to be added by the lead)

## Post-merge steps

- Confirm the `Post-deploy revision check` run for the merge commit is green.
- Work through the post-merge items in [RELEASE-CHECKLIST.md](RELEASE-CHECKLIST.md)
  (CARTO tiles, axe at 375 and 1440 px).
- Add a dated `[Claude]` or `[Codex]` entry to
  [BUILD-TEST-HANDOFF.md](BUILD-TEST-HANDOFF.md) with the next action.
