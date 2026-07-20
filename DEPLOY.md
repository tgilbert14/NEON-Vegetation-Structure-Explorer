# Deploy and release entry point

The app has two public surfaces:

- GitHub Pages serves the Living Poster from `docs/`.
- Posit Connect Cloud serves the bundle-only Shiny app from `main` using the
  validator-produced `manifest.json`.

Do not deploy legacy or locally hand-edited data, indexes, source receipts, or
manifest bytes. Use the exact candidate/promotion workflow in
[`docs/BUILD-TEST-HANDOFF.md`](docs/BUILD-TEST-HANDOFF.md) and complete
[`docs/RELEASE-CHECKLIST.md`](docs/RELEASE-CHECKLIST.md).

## Release path

1. Open the reviewed app/science PR.
2. Have the repository owner apply `build-vegetation-candidate` to that exact
   same-repository PR head. A manual dispatch has no PR review context and is not
   the first-release promotion route.
3. Let the labeled-PR run pull official `RELEASE-2026` with provisional data
   excluded, build all 42 sites twice, verify exact-byte determinism, and upload
   `vegetation-release-candidate-<head_sha>-<run_id>`. The workflow has read-only
   repository permissions and cannot create a branch, open a PR, or push.
4. Download that exact artifact, verify `CANDIDATE-SHA256SUMS.txt`, then promote
   every ledger-listed path—its complete `data/` family, `data-sample/`, source
   receipt, indexes, and validator-produced `manifest.json`—and no unlisted
   artifact-root file onto the same PR branch as one reviewed change.
5. Require CI, source-family, fixture, parity, offline-source, browser, cover,
   manifest, and image-provenance gates to pass.
6. Merge the reviewed PR. Verify the Pages deployment from `main`, then confirm
   or trigger Connect publication of the same revision.
7. Verify the public cover and public app at desktop and narrow-mobile widths,
   including a real place load, held/zero states, plant records, and downloads.

## Public URLs

- Living Poster:
  `https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/`
- Shiny app:
  `https://019ee110-8fd3-abae-aee3-02ea8e4274c8.share.connect.posit.cloud/`

The Living Poster's launch links are explicit public URLs in `docs/index.html`.
Keep every CTA, the README, and social metadata on the same public share URL;
never use an authenticated Connect content-management URL.

## Runtime and manifest

The public app is bundle-only (`VST_LIVE=0`) and makes no NEON data request at
runtime. Regenerate `manifest.json` only through the pinned validator workflow.
The manifest verifier requires complete HTTPS repository URLs, including archived
geospatial packages such as `wk`; this prevents the missing-protocol Connect
failure seen in the previous deployment.

## Routine refreshes

Scheduled and manual refreshes use the same official-release and double-build
gates, but only upload read-only diagnostic artifacts; they do not open or update
a PR. `NEON_TOKEN` is an Actions secret and must never be printed, copied into
artifacts, or placed in repository files. A refresh remains HOLD until its exact
artifact is associated with a reviewed PR, promoted as a complete family, and
merged.
