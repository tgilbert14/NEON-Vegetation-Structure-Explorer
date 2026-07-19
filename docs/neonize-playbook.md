# The NEONize playbook

This app-local playbook records the current quality and release pattern for a
NEON Explorer. The Driver repository's `docs/neonize-playbook.md` is the suite
catalog; this file adds Vegetation Structure's product-specific rules.

## Quality bar

| Dimension | Required result |
|---|---|
| Flow | A brief, inviting entry; one place-selection path; progressive questions; useful empty/HOLD states. |
| UI/UX | Mobile-first, accessible controls, vendored essential assets, coherent art and app chrome, and no startup font/network dependency. |
| Science | One registered unit, observation key, opportunity model, estimator, support vocabulary, and point-of-use caveat for every headline value. |
| Creativity | One product-native interaction and a Living Poster that is artistic, legible, and openly illustrative rather than simulated evidence. |
| QC | Inspectable records and neutral review flags that preserve rather than silently delete measurements. |
| Honesty | Zeros, missing values, held states, incompatible channels, uncertainty, and source vintage remain distinct everywhere. |

Product-native design comes first. Reuse suite interaction and accessibility
patterns, but never transplant an analysis merely because another app has it.

## Vegetation Structure contract

- Product: NEON `DP1.10098.001`.
- Plant identity: `plotID × individualID`.
- Sampling event: `plotID × eventID`.
- Stem row: `eventID × individualID × tempStemID`.
- Physical channels are disjoint:
  - `tree_dbh`: tree bole `stemDiameter` at breast height;
  - `shrub_sapling_basal`: shrub/sapling `basalStemDiameter` at the stem base.
- A plot-event opportunity and its finite positive channel-compatible sampled
  area must exist before any per-hectare value is eligible.
- `sampled_absence` is a supported zero. Every `held_*` state remains unknown
  and carries a reason.
- Standing structure is a slow sampled-plot state, not annual productivity,
  biomass, carbon, wall-to-wall inventory, or a causal driver response.

The exact formulas, thresholds, support states, and release gates live in
`SCIENCE-CONTRACT.md`; they must not be inferred from the interface.

## Build and release pattern

1. Read the source, science, build/test, Driver, and suite handoffs before
   changing analysis or release bytes.
2. Select an explicit official NEON release. Fetch every registered site into
   empty staging and preserve raw checksums and the release DOI.
3. Build two isolated candidates under pinned R 4.5.2, the dated Jammy package
   repository, and verified one-thread Haswell OpenBLAS settings.
4. Fail hard on any missing site, table, key, opportunity, denominator, source
   receipt, validator, or exact-byte comparison.
5. Keep the public app bundle-only (`VST_LIVE=0`). Missing or incompatible bytes
   produce a visible HOLD; there is no live-data fallback at runtime.
6. Build the first candidate from an exact same-repository PR head by having the
   repository owner apply `build-vegetation-candidate`.
7. Review the exact artifact
   `vegetation-release-candidate-<head_sha>-<run_id>`, verify its checksum
   ledger, and promote the complete family onto that same PR.
8. Merge only after exact-head CI, source/science review, exports, cover, and
   browser gates pass. Then verify the exact deployed Connect and Pages commits.

Scheduled and manual refreshes upload read-only diagnostic artifacts. They do
not create branches, open PRs, publish to `main`, or alter a source receipt.
`skip_download=true` is only for an already-promoted v2 family.

## App and cover process

1. Research the official product schema and protocol before deciding the unit
   or interface.
2. Lock the observation identity, sampling opportunity, zero/missing semantics,
   physical channel, and claim boundary before building charts.
3. Design the Living Poster as a short invitation: one field action, a compact
   hook, one promise, and one CTA. Put method detail below the first screen.
4. Keep generated art visibly editorial, disclose its provenance beside the
   image, and keep all factual text in accessible HTML.
5. Exercise every interactive surface, empty state, held state, export, and
   breakpoint in a running app. HTTP 200 alone is not semantic health.
6. Run an independent diff review for R correctness, science, JS, charts,
   accessibility, field-user failure modes, and release integrity.
7. Close with exact hashes, run URLs, deployment identities, residual risks, and
   an explicit Driver disposition in durable repository docs.

## Reusable lessons

- Preserve observation opportunity before deriving an occurrence or zero.
- Never pool repeated visits as independent spatial samples.
- Scope every area-scaled metric to the population sampled over that exact area.
- Keep physical measurement channels visible at every point of use.
- Build headline and index values from the same canonical functions.
- Reattach Plotly listeners after every render; anchor pinned annotations in
  data coordinates; keep exportable charts in SVG when raster capture requires
  it.
- Vend essential browser dependencies and keep server startup free of remote
  font or data downloads.
- A content hash proves bytes, not upstream vintage. Unknown provenance stays
  explicitly unknown until a complete source receipt exists.

The current working family remains on scientific HOLD until an official-release
candidate completes every empirical and public gate. This document does not
claim that those checks have run.
