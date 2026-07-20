# Suite learning handoff

App: Vegetation Structure · Pass 4 production closeout · 2026-07-19 MST / 2026-07-20 UTC

Status: **PRODUCTION VERIFIED at PR #8 merge `d566b30` / Connect #59; official-
release family, science, reset, responsive, repeated-click, and split-log proof
passed; Driver HOLD / CONTEXT ONLY / NO DRIVER DATA BYTE CHANGE**. Core
release PR #4, intermediate Plotly guard PR #5, and accessibility/export PR #6
are merged. Connect #57 reports exact merge `433bbd25` under R 4.5.2 with 91
packages. The public sweep proved the key science edges, then caught a
return-to-Places picker reset defect;
PR #7 implementation `3835451` fixes it and promotion head `8389c9c` carries
only the validator-derived manifest checksum. Exact-head run `29722349642`
passed every `release_contracts` CI gate; merge `0709bd0` and Pages run
`29722613509` are green. Main CI `29722614074` passed and Connect #58 reports
exact `0709bd0`, R 4.5.2, and 91 packages. Its reset/compact-width proof passed,
but fresh server
logs exposed first-chart `baBar` registration warnings. PR #8 implementation
`4ce0cb7` waits for a raw emitted click; promotion `06904fe` and exact-head run
`29723718100` are green. Merge `d566b30`, main CI `29724062900`, Pages
`29724062095`, and Connect #59 agree on the published runtime; repeated-click,
reset, responsive, science-state, browser-log, and worker-log proof passed.
Docs-only PR #9 published the app-local record as merge `3391e70`; exact PR and
main CI, Pages, Connect #60, and public-landing receipts preserved the released
manifest/search bytes. Only the separate central Driver documentation
publication remains in the Pass 4 closeout workflow.

## Product and cover lessons

1. A companion cover works best as a Living Poster: one app-native field action,
   a three-beat hook, one plain promise, and one invitation. Detailed method and
   caveats stay below the first screen.
2. Generated art must look openly editorial, not like documentary evidence. Put
   the disclosure beside the image and keep facts in live HTML.
3. Desktop hero art and the 1200×630 social card are different compositions and
   need separate receipts.
4. Non-scientist navigation should begin with questions: what stands here, how
   did this plant change, and how does this place compare?
5. Driver Cascade is the suite ambassador. Companion covers should state their
   own lens and point toward Driver without turning the page into a directory.

## Science and data lessons

1. Preserve the official observation key before making a “latest record.” For
   Vegetation Structure that includes event, individual, and temporary-stem
   identity—not only individual and date.
2. Preserve sampling opportunity before scaling a tally. A plot ID in a stem
   table does not prove the correct sampled-area denominator.
3. Never collapse all `NULL` states into ecological absence. Distinguish no
   qualifying records, one census, unmatched keys, invalid area, and genuine
   supported zero/empty states.
4. One census blocks change, not a supported current snapshot.
5. `tree_dbh` bole cross-section and `shrub_sapling_basal` stem-base
   cross-section need separate physical channel IDs even when both are expressed
   as m²/ha.
6. Standing structure is a slow state; composition and phenology belong to their
   respective companion apps.
7. A single default map row must not erase a supported secondary measurement
   channel. Network discovery needs an explicit site × channel index, and a
   search hit must carry that channel into the opened app view.
8. Preserve measurements when their opportunity source is absent, but never
   manufacture effort, absence, area, or a denominator. Give the event a
   dedicated status and separate published-opportunity counts from
   measurement-only context counts. Vegetation RELEASE-2026 makes this concrete:
   49 keys / 4,365 rows / 11 sites.

## Release lessons

1. Select an explicit immutable NEON release and DOI. Current/latest API output is
   not a substitute for a release receipt.
2. Fetch all registered sites into empty staging, hard-fail every missing site or
   table, and preserve raw per-file hashes.
3. Build twice from the same raw family and compare exact bytes.
4. A monthly schedule should have one actual monthly trigger; a delayed runner
   must not miss a calendar gate.
5. Refresh automation uploads a read-only candidate artifact and never creates a
   branch, opens a PR, or pushes data. Release begins from an existing reviewed
   PR whose exact head receives the owner-only candidate label.
6. `skip_download` accepts only an already-promoted v2 family, revalidates those
   bytes, and never mints a new source vintage.
7. The manifest is an exact generated closure. Exact URL-installed geo packages
   retain `RemoteType=url` and the full `url::<tarball>` reference while using
   `Source=CRAN` plus `https://cran.r-project.org` as Connect's deployment lane;
   only their non-semantic `Built` clocks are removed. Versions and `RemoteSha`
   are never fabricated. Ordinary packages remain on the dated Jammy snapshot,
   and `wk` can never inherit a relative `CRAN/...` path.
8. Source-family changes must force human review of science, empirical claims,
   cover facts, Driver disposition, and release receipts.
9. API row order is not evidence. Materialize each raw table, validate unique
   published row IDs, sort by that identity, reset row names, and hash the
   normalized extraction under a pinned serialization runtime.
10. Plotly 4.12 event registration occurs while the rendered widget is prepared.
    Loaded site/output state does not prove registration; `event_data()` can
    still run first during the chart flush. Keep `event_register("plotly_click")`
    on the widget, observe raw `plotly_click-<source>`, and only then read
    `event_data(..., priority = "event")`. Inspect browser and Shiny worker logs
    separately: a clean browser console does not prove a clean server lifecycle.
11. A server-backed Selectize picker owns both a visible selection and a remote
    choice data object. Resetting only `selected = ""` can leave a convincing
    placeholder backed by an empty search source. Centralize one validated
    choice builder, use it at initialization/load/reset, and test the complete
    Place → site → Change place → search another site loop.

## Dispositions from this pass

| Class | Finding | Disposition |
|---|---|---|
| app-local | Living Poster and question-led entry | `ADOPT`; #59 cover and compact-width proof passed |
| suite-platform | isolated official-release candidate PR workflow | `ADOPT`; run `29715249829`, PR #4 promotion, and exact merged bytes verified |
| suite-platform | site-state Plotly event guard | `REFINE`; PR #5 had a clean #56 window but was incomplete under #58's first-chart lifecycle |
| suite-platform | focus boundary, local selectors, and keyboard pin controls | `ADOPT`; PR #6 and combined public proof passed, with targeted #59 regression smoke |
| suite-platform | server-backed picker reset | `ADOPT`; #58 reset and five compact widths passed |
| suite-platform | Plotly 4.12 registration lifecycle | `ADOPT`; PR #8 implementation `4ce0cb7`, merge `d566b30`, Connect #59, repeated-click, and clean split-log proof passed |
| app-local QA | JORN supported zero and WOOD held state | `PASS`; exact export/UI evidence retained |
| app-local QA | active-channel standalone/ZIP parity | `PASS`; BART shrub plot summary byte-identical |
| scientific-contract | event/stem identity and opportunity ledger | `PASS` for the companion app; preserve in every consumer |
| scientific-contract | 49 measurement-only events / 4,365 rows at 11 sites | `HOLD` from scaling and derived summaries; preserve and expose |
| Driver-impacting | Vegetation channel/support evidence | `HOLD / CONTEXT ONLY / NO DRIVER DATA BYTE CHANGE` |
| complementary | Vegetation as slow standing-structure lens | `COMPLEMENT` Plant Diversity and Phenology |

## Pattern for the next app

- Read governance and source receipts first.
- Preserve observation opportunity and identity before feature work.
- When source opportunity is missing, preserve the observation and register the
  gap; do not turn a hard build stop into a hidden deletion or synthetic zero.
- Keep the poster face brief and artistic; place truth, provenance, and suite role
  below the fold.
- Vendor essential runtime dependencies, generate deterministic release bytes,
  and prove semantic public health across the suite breakpoints.
- Test a full reset-and-second-selection cycle for every server-backed picker;
  an initial successful selection does not prove that its remote choices survive.
- Audit browser and server logs as separate release surfaces after the first
  chart render and repeated events; visible health is not a server-log receipt.
- End with an app-local handoff plus central Driver evidence/disposition update.

## Driver action after this app closes

Record the unmatched-denominator and Plotly registration/split-log lessons in
the central evidence register and playbook now, but keep Driver data bytes
unchanged. The exact official-release data family is promoted and deployed;
the final PR #8 runtime is production-verified. A Vegetation Driver rung still
requires a separate adapter, exact Driver rebuild, old/new field parity, and
explicit disposition. Passing the companion app does not authorize that step.
