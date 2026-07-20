# NEON Vegetation Structure Explorer

> **Tagged. Measured. Still changing.**

An unofficial R/Shiny explorer for NEON's **Vegetation structure** product
([DP1.10098.001](https://data.neonscience.org/data-products/DP1.10098.001)). It is a NEONize suite
companion built around one approachable idea: field crews tag woody plants, map them, and return to
measure what changed.

[Open the Living Poster](https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/) ·
[Open the app](https://019ee110-8fd3-abae-aee3-02ea8e4274c8.share.connect.posit.cloud/)

## Pass 4 status

The Living Poster and app experience are rebuilt around the
`NEON-VST-DP1.10098.001-v2` contract. Exact-head candidate run
[`29715249829`](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/29715249829)
passed the official 42-site RELEASE-2026 source, key, opportunity-state,
parity, runtime, manifest, app-source, and export gates; promotion commit
`800bd5e` contains only its 54 checksum-ledger payload paths. The core release
merged in
[PR #4](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/4),
an initial site-state guard for Plotly reads in
[PR #5](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/5),
and the production accessibility/export closeout in
[PR #6](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/6).
Main CI and Pages are green at merge `433bbd2`; Posit Connect deployment #57
reports that exact commit under R 4.5.2 with 91 packages. Final QA proved the
JORN supported-zero, WOOD held-not-zero, and BART active-channel export paths,
then exposed one return-to-Places regression: clearing the server-backed picker
also removed its remote search choices.
[PR #7](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/7)
re-registered the same validated 42-site choice family after reset. Its exact
validator-derived manifest checksum is promoted at `8389c9c`, and exact-head
run `29722349642` passed every `release_contracts` CI gate. PR #7 merged as
`0709bd0`, and main CI and Pages are green; Connect #58 reports that exact merge
under R 4.5.2 with 91 packages. The reset path and all five compact widths
passed, but fresh Connect server logs exposed first-chart `baBar` registration
warnings that the clean browser log did not show.
[PR #8](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/8)
waited for an emitted raw Plotly click before reading event data. First run
`29723373295` failed closed only at committed derived-byte equality; promotion
`06904fe` carries its exact validator-derived `server.R` manifest checksum, and
exact-head run `29723718100` passed every `release_contracts` CI gate. PR #8
merged as `d566b30`; main CI `29724062900` and Pages `29724062095` passed, and
Connect #59 published that exact merge under R 4.5.2 with all 91 packages. The
production sweep repeated the same `baBar` click twice, completed BART → reset →
JORN, preserved JORN zero and WOOD held states, and passed every required compact
width. The browser log was clean; fresh worker logs contained only the two benign
package-built-under-R-4.5.3 warnings. Runtime production proof is complete.
Docs-only [PR #9](https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/pull/9)
then published the app-local evidence as merge `3391e70`; its exact PR and main
CI, Pages, Connect #60, and public-landing receipts retained the released
manifest and search bytes. Only the separate central Driver handoff remains.
Vegetation remains
**HOLD / CONTEXT ONLY / NO DRIVER DATA BYTE CHANGE**.
`VEGETATION-SOURCE-RECEIPT.md` intentionally preserves its promotion-time
candidate wording; production status is append-only in `BUILD-TEST-HANDOFF.md`.

See:

- [`docs/DATA-TAKEAWAYS.md`](docs/DATA-TAKEAWAYS.md) — measured audit findings and disposition;
- [`docs/SCIENCE-CONTRACT.md`](docs/SCIENCE-CONTRACT.md) — release-blocking science contract;
- [`docs/VEGETATION-SOURCE-RECEIPT.md`](docs/VEGETATION-SOURCE-RECEIPT.md) — source/release receipt;
- [`docs/BUILD-TEST-HANDOFF.md`](docs/BUILD-TEST-HANDOFF.md) — exact build and verification path.

## Experience

| Surface | Question it answers |
|---|---|
| **Place** | Who and what was measured at one NEON place? |
| **Sampled Structure** | What size, height, channel-qualified cross-sectional area, and stem-density patterns occur in supported sampled plots? |
| **Change** | Which plot + plant records have comparable remeasurements, and how did their diameter or status change? |
| **Plant** | Where does one tagged plant sit in size × height space, and what does its preserved record show? |
| **Search / Compare** | Where was a species recorded, and how do compatible sampled contexts differ? |
| **Quality / Exports** | Which records need review, what was held, and can every number be traced to keys and support state? |

The interface includes a map-first place gateway, searchable site and plant pickers, accessible loading
and focus behavior, responsive 320 px layouts, local/offline UI dependencies, pin-and-download cards,
CSV/codebook/QC exports, and a sampled-plot PDF brief.

## Science contract in plain language

- A physical plant is `plotID + individualID`; `individualID` alone is not site-unique.
- Published `uid` is preserved as source-row identity and must be unique. NEON's documented
  apparent-individual locator, `eventID + individualID + tempStemID`, is not assumed unique: the
  operational locator also includes `plotID` so cross-plot tag reuse stays distinct.
- Distinct-`uid` rows that still collide on the operational locator are all preserved. No row wins by
  date, order, or metric; the affected physical channel cannot be supported. Its conflict count stays
  visible, and its status is `held_identity_conflict` unless an earlier protocol/presence hold applies.
- Duplicate `plotID + eventID` opportunity-source rows are likewise preserved and both physical
  channels are held as `held_identity_conflict`. Only a duplicate published source `uid` is a hard
  source-row-identity failure.
- RELEASE-2026 also contains 4,365 measurement rows across 49 plot-events at 11 sites without a
  matching published `vst_perplotperyear` row. Those measurements remain visible, but their event is
  `held_opportunity_source_missing`; no effort, absence, date, area, or denominator is invented.
- Sampled absence is a real zero. Sampling-impractical, dendrometer-only, invalid-area, and
  opportunity-source-missing contexts are held/NA.
- Sampled areas remain event-specific; every finite positive compatible area—including a 40 m² nested
  area—is retained.
- Large-tree DBH cross-sectional area and shrub/sapling stem-base
  cross-sectional area are separate physical channels. They are not one
  cross-biome ranking.
- Small-tree DBH rows remain in the preserved download but are withheld from summaries until their
  own nested-area DBH channel is registered and tested.
- Density counts stems. QMD is `sqrt(sum(d²) / n_stems)`.
- Growth requires comparable event order and measurement point. Unalignable multi-stem basal records
  are held rather than forced into a trajectory.
- Mortality reduces each event to any-live/all-dead, censors lost/unknown fates, and uses plot + plant
  identity.
- Size-class shape is descriptive; it does not prove recruitment, regeneration, or stand age.
- Biomass and whole-site inventory claims are intentionally absent.

## Data and release

The v2 build targets official **RELEASE-2026**, provisional data excluded, with source DOI
[10.48443/pypa-qf12](https://doi.org/10.48443/pypa-qf12). Per-site artifacts remain
`data/sites/<SITE>.rds = list(trees, plots, opportunity_source, meta, contract)`:

- `trees` preserves published source `uid`, the documented and plot-scoped operational locators,
  plot + plant identity, event/date, taxonomy, growth form, status, diameter/height,
  measurement-point, available QC fields, and an explicit opportunity-source-missing flag;
- `plots` carries one deterministic plot-event context row for every published opportunity key plus
  every measurement-only key. Source-missing contexts carry no invented opportunity date, effort,
  presence, design, or area; separate measurement-sourced date/count fields keep what is known;
- `opportunity_source` preserves every published plot-opportunity source row, including conflicting
  `plotID + eventID` records;
- `meta` declares site, release, contract ID, source, and build provenance.

Search, app, PDF, and export values must be produced by the same canonical builder before presentation
rounding. No separate search proxy is allowed.

## Run locally

The app is bundle-only at runtime and makes no data-network request:

```r
shiny::runApp(".", port = 8191)
```

Use the build/test handoff for the supported refresh and release commands. Do not hand-edit
`manifest.json`, promote a partial site family, or overwrite `data/` before the candidate artifact has
passed the full gate.

## Attribution

Built with data from the National Ecological Observatory Network (NEON), a U.S. National Science
Foundation program operated by Battelle. NEON data are licensed CC BY 4.0. This independent explorer
is not endorsed by NEON, Battelle, or the NSF.

Built by Desert Data Labs · desertdatalabs@gmail.com
