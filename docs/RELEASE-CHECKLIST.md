# Vegetation Structure release checklist

Release status: **PRODUCTION VERIFIED at PR #8 merge `d566b30` / Connect #59;
official-release bytes, runtime, science states, reset, responsive,
repeated-click, split-log, and app-local documentation-publication gates passed.
Only the central Driver documentation publication remains pending.** Check a
box only with an attached exact receipt.

## Source and science

- [x] Explicit official release and DOI selected (`RELEASE-2026`).
- [x] Source receipt is exactly `FULL_RELEASE` / `FULL_RELEASE`; bounded month
      queries remain local diagnostics and are not promotable.
- [x] Exactly 42 raw site responses and per-file/aggregate SHA-256 ledger.
- [x] Raw tables normalized by unique published `uid`, with row names reset and
      the pinned serialization rule recorded.
- [x] Selected mapping/tagging UID is preserved; blank/duplicate UIDs and
      ambiguous latest-created ties fail without an arbitrary winner.
- [x] Event × individual × temporary-stem identity preserved.
- [x] Every published per-plot/per-year opportunity row and sampled area preserved.
- [x] Exact measurement-only ledger passes: 49 plot-event keys / 4,365 rows /
      11 sites; all rows flagged and both channels held with no invented fields.
- [x] No silent measurement-to-denominator mismatch; source-backed and
      measurement-only key inventories are bidirectionally exact.
- [x] `tree_dbh` and `shrub_sapling_basal` estimands, physical labels, and
      thresholds registered.
- [x] Snapshot, growth, mortality, deterministic presentation-channel, support, and missingness fixtures pass.
- [x] Exact `Y`/`Yes`/`N`/`No` presence, both conflict directions, real basal
      measurement-height alias, and event-atomic differing/undated-stem fixtures pass.
- [x] Deployed `global.R` independently recomputes source-backed and source-gap
      presence, counts, support states, exact reasons, and supported flags; its
      positive and mutation fixtures pass on the actual candidate family.
- [x] App, indexes, CSV/ZIP/PDF, and fixtures match exactly.
- [x] Data Takeaways and Expert Review recomputed from the promoted family.

## Build and exact bytes

- [x] Pinned Ubuntu 22.04, R 4.5.2, dated Jammy snapshot, and one-thread Haswell runtime.
- [x] Every tracked R file parses; app sources offline from 42 bundles.
- [x] Browser/custom-handler/cover checks pass.
- [x] Candidate builds twice from the same raw family with identical bytes.
- [x] `data/site_index.rds` and `data/search_index.rds` deterministic and exact;
      search contains the canonical 42-site index plus the exact 84-row site ×
      physical-channel grid.
- [x] `scripts/verify_derived_parity.R` independently rebuilds every actual
      embedded site/taxon/channel summary and both network indexes through the
      runtime consumer path; positive and corrupted-summary fixtures pass/fail as
      expected.
- [x] Runtime, verifier, DQA, and parity reject coherent row-plus-summary
      corruption of live, year, taxonomy, permanence, and composite keys.
- [x] `manifest.json` regenerated twice with identical bytes and complete runtime
      checksums. The eight exact URL-installed geo packages retain their real
      versions and `url::<tarball>` origin, use the absolute CRAN deployment lane,
      and omit only their non-semantic `Built` clocks; ordinary packages record
      `CRAN` plus the exact dated Jammy snapshot. No `Version` or `RemoteSha` is
      fabricated or rewritten.
- [x] Owner applied `build-vegetation-candidate` to the exact same-repository PR
      head; artifact identity and digest are attached to that head.
- [x] Ordinary PR CI proves its checked-out `HEAD` equals that exact PR head,
      never the synthetic merge ref.
- [x] No source-family byte was hand-edited; PR #6 promoted only five exact
      validator-derived manifest checksums after byte/hash equality proof.
- [x] PR #7 picker-reset implementation `3835451` failed closed only at derived
      equality; promotion `8389c9c` carries its exact validator-derived
      `server.R` manifest checksum and no data/index byte.
- [x] PR #7 promoted head `8389c9c` has green exact-head run `29722349642` and
      matching derived artifact `8452911612`.
- [x] PR #8 implementation `4ce0cb7` failed closed only at derived equality in
      run `29723373295`; promotion `06904fe` carries the exact validator-derived
      `server.R` checksum, and promoted-head run `29723718100` passed with
      artifact `8453460662`.

## Cover and interaction

- [x] Living Poster hook/promise/CTA remain brief and legible.
- [x] Generated illustration disclosure, alt text, source, and checksum verified.
- [x] Separate 1200×630 social composition is nonblank and metadata-complete.
- [x] Exact #58 at 390/375/361/360/320 has zero horizontal overflow, visible
      H1/CTA/Quick tour/picker, and no visible error or disconnect; loaded BART
      also has zero overflow at 390 and 320 px.
- [x] Exact #59 cover at 390/375/361/360/320 px and loaded BART at 390/320 px
      have zero horizontal overflow, correct splash/main visibility, in-bounds
      controls, and no visible failure.
- [x] Keyboard/focus order, 44×44 targets, reduced motion, and loading-dialog
      focus passed in the full production sweep; #59's targeted regression
      retained correct cover/main transitions and app readiness.
- [x] Place, Change, Plant, More, search, compare, export, and tour paths passed
      in the full production sweep; #59 re-proved Change and Plant at 320 px.
- [x] BART dual-channel state and JORN supported-zero state are honest; JORN
      tree shows 25 supported sampled-absence plots, zero structure metrics, and
      disabled plant controls.
- [x] Exact #59 BART → reset → exactly one searchable JORN option → JORN load
      passes; loaded BART Change and Plant navigation pass at 320 px.
- [x] Species and threshold search open the selected channel in the established
      full interaction receipt; PR #8 changed only the Plotly click observer and
      the #59 targeted regression remained semantically ready.
- [x] Every unavailable state explains its actual reason; WOOD remains on the
      splash with an explicit held-not-zero warning and no supported context.

## Export and deployment

- [x] Whole-site ZIP, plot/plant CSV, QC export/card, pinned-chart images, and a
      valid one-page letter PDF inspected; PR #8 changed only the click observer
      and #59 preserved the exact promoted manifest/search bytes.
- [x] BART shrub/sapling standalone plot-summary CSV and ZIP member
      `plot_summary_latest.csv` are byte-identical at SHA-256
      `fddca062b6e9a69ed72dd7f00b27725adc45d773755878fb39f3ec8614259a7e`;
      the ZIP has the exact expected eight-file inventory.
- [x] Codebook covers every exported column and structural `NA` meaning.
- [x] PRs #4–#8 exact promoted commits merged after green head checks and review.
- [x] PR #7 merged as `0709bd0`; Pages run `29722613509` and deployment
      `5517850060` are green.
- [x] Promotion parent equals the labeled candidate head and its changed-path set
      equals the 54 checksum-ledger payload paths exactly.
- [x] Connect #57 reports exact merge `433bbd25`, R 4.5.2, and all 91 packages.
- [x] PR #7 post-merge main CI `29722614074` and Connect #58 report exact merge
      `0709bd0`, R 4.5.2, and all 91 packages.
- [x] Final PR #8 deployment reaches semantic app-ready and site-ready state.
- [x] Main CI `29724062900` artifact `8453599842` preserves the exact promoted
      manifest/search bytes; Pages run `29724062095`, artifact `8453482888`, and
      deployment `5518123037` are green at merge `d566b30`.
- [x] Connect #59 reports exact merge `d566b30`, R 4.5.2, all 91 packages, and a
      four-second deployment.
- [x] Exact #59 browser logs contain 33 level-`log` entries and zero warning,
      error, `baBar`, `event_data`, `undefined`, or disconnect entry.
- [x] Final #59 worker logs contain only startup and two benign package-version
      warnings, with no `baBar`, Plotly registration, source-not-registered,
      `undefined`, or Shiny error after repeated clicks.
- [x] PR #8 merge/main/Pages/Connect identities and artifacts agree exactly.
- [x] Rollback targets (`0709bd0` / Connect #58 for PR #8, `433bbd2` / #57 for
      PR #7, and `91a7814` / #56 for PR #6) and failed-attempt evidence recorded.

## Suite closeout

- [x] `BUILD-TEST-HANDOFF.md` finalized through the #59 runtime receipt with
      commands, runs, hashes, failures, and residual risk.
- [x] Source, science, art, app-local Driver package, suite-learning, Data
      Takeaways, and Expert Review agree.
- [x] Docs-only closeout PR published with unchanged-runtime main CI, Pages,
      Connect, and public landing identities recorded.
- [ ] Central Driver evidence register/backlog/revamp plan/playbook published as
      a separate documentation-only identity.
- [x] Driver disposition recorded as **HOLD / CONTEXT ONLY / NO DRIVER DATA BYTE
      CHANGE**; no Driver byte changed.
