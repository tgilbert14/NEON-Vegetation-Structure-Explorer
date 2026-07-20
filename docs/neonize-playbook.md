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
- Published source-row identity: unique `uid`; duplicate source `uid` is a hard
  failure.
- Preserve the selected mapping/tagging source UID. A tie at the latest created
  timestamp for `plotID × individualID` is unresolved and fails; never choose by
  UID, row order, or taxonomy.
- Sampling-opportunity locator: `plotID × eventID`.
- Documented apparent-individual locator:
  `eventID × individualID × tempStemID`; the operational locator adds `plotID`
  to distinguish cross-plot tag reuse.
- Distinct-`uid` locator collisions are preserved without row-order, date,
  metric, or status ranking; the affected physical channel remains held, with
  an explicit conflict count and `held_identity_conflict` status unless an
  earlier protocol/presence hold applies.
- Duplicate opportunity-source rows are preserved without an arbitrary winner;
  both channels for that `plotID × eventID` are `held_identity_conflict`.
- Measurement keys without a published opportunity source are preserved as
  measurement-only context, flagged on every record, and assigned
  `held_opportunity_source_missing`. Opportunity date, effort, presence, design,
  coordinates, area, absence, and denominator remain unknown.
- Physical channels are disjoint:
  - `tree_dbh`: tree bole `stemDiameter` at breast height;
  - `shrub_sapling_basal`: shrub/sapling `basalStemDiameter` at the stem base.
- A plot-event opportunity and its finite positive channel-compatible sampled
  area must exist before any per-hectare value is eligible.
- `sampled_absence` is a supported zero. Every `held_*` state remains unknown
  and carries a reason.
- Normalize exact RELEASE-2026 `Y`/`Yes` as present and `N`/`No` as absent;
  presence-record disagreement is held in either direction.
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
4. Fail hard on any missing site, table, published source `uid`, unaccounted
   observation/opportunity key, source receipt, validator, or exact-byte
   comparison. A registered measurement-only key may remain only when every
   measurement is preserved, both channels are held, and no denominator is
   invented. A duplicate source `uid` fails; distinct-`uid` locator collisions
   remain visible and held rather than being silently deduplicated.
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
- Treat API row order as transport noise: normalize by immutable published row
  identity before hashing, and describe the digest as normalized extraction
  bytes under the pinned serializer.
- Never pool repeated visits as independent spatial samples.
- Scope every area-scaled metric to the population sampled over that exact area.
- Keep physical measurement channels visible at every point of use.
- Build headline and index values from the same canonical functions.
- Reattach Plotly listeners after every render; anchor pinned annotations in
  data coordinates; keep exportable charts in SVG when raster capture requires
  it.
- In Plotly 4.12, loaded site/output state is not proof that a rendered widget
  has completed `event_register()`. Keep explicit registration on the widget,
  observe raw `plotly_click-<source>` on the Shiny input, and only then call
  `event_data(..., priority = "event")`. This preserves repeated clicks without
  reading ahead of registration during the first chart flush.
- Inspect browser and Shiny worker logs independently after fresh load and
  repeated clicks. A clean console and visually healthy chart do not prove that
  the server emitted no lifecycle warning.
- Treat a server-backed Selectize picker as selection plus remote choice state.
  Clearing `selected` does not prove that another search can run. Centralize one
  validated choice builder/updater, call it at initialization, selected-site
  load, and reset, and contract the complete first-site → reset → second-site
  path.
- Vend essential browser dependencies and keep server startup free of remote
  font or data downloads.
- A content hash proves bytes, not upstream vintage. Unknown provenance stays
  explicitly unknown until a complete source receipt exists.

The official-release family passed every empirical candidate gate in exact-head
run `29715249829` and was promoted through PR #4. PR #5 supplied the guarded
server-side Plotly path; PR #6 completed the focus boundary, active-channel
exports, local Size Lab selection, and keyboard pin controls. Merge `433bbd25`
has green main CI and Pages receipts and is Connect deployment #57 under R 4.5.2
with 91 packages. Its public sweep proved the JORN zero, WOOD held, and BART
active-channel export edges, then exposed a server-backed picker reset defect.
PR #7 implementation `3835451` fixes that path; `8389c9c` promotes only its
exact generated manifest checksum. Exact-head run `29722349642` passed every
`release_contracts` CI gate; merge `0709bd0` and Pages run `29722613509` are
green. Main CI `29722614074` passed and Connect #58 reports exact `0709bd0`; its reset
and compact-width checks passed. Fresh worker logs still exposed first-chart
`baBar` registration warnings, proving the earlier site-state guard incomplete.
PR #8 implementation `4ce0cb7` is the new clean-log boundary. First run
`29723373295` failed closed only at derived equality; promotion `06904fe` carries
the exact validator-derived manifest checksum, and exact-head run `29723718100`
passed. PR #8 merged as `d566b30`; main CI `29724062900`, Pages `29724062095`,
and Connect #59 published that exact runtime. The final identical repeated-click,
reset, responsive, science-state, browser-log, and worker-log receipts passed.
Failed and superseded attempts are diagnostic evidence, never release authority.
