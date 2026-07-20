# Vegetation structure — current data takeaways and disposition

_Pass 4 audit · 2026-07-19 · NEON DP1.10098.001 · supersedes the June 2026 certification_

## Decision

**HOLD for Cascade metrics. CONTEXT ONLY for the verified RELEASE-2026 companion evidence.**

The RELEASE-2026 companion bundle preserves the keys and sampling-opportunity
states needed to distinguish a measured zero from missing, impractical,
dendrometer-only, identity-conflicted, or unmatched sampling. It supports a
channel-qualified sampled-plot standing-structure lens, not a certified
site-wide vegetation rung or annual productivity metric. Driver must not ingest
a new vegetation byte without a separate eligibility, support-parity, and
Driver rebuild receipt.

The earlier claim that Driver imported this app's `stand_site()` was also incorrect. Driver currently
uses an independently implemented, stricter vegetation path and already holds WOOD. That separation is
why this audit does not trigger a Driver data change.

## What is verified in the RELEASE-2026 candidate

- 42 site bundles, 6,200 event contexts per physical channel, 389,196 preserved
  measurement rows, and 316,914 live measurement rows.
- 49 measurement-only plot-events preserve 4,365 rows across 11 sites without
  inventing opportunity metadata, sampled area, effort, absence, or a denominator.
- Repeated diameter, height, status, taxonomy, plot, and area fields sufficient for record discovery
  and carefully labelled individual context.
- A strong visual/product opportunity: tagged plants are revisited, so the public story can truthfully
  lead with **“Tagged. Measured. Still changing.”**

Those row counts describe audited release records and support states; they are
not wall-to-wall site estimates.

## Release-blocking findings

1. **Source-row and locator identity were lost.** Published `uid` is the source-row identity. NEON's
   documented apparent-individual locator is `eventID × individualID × tempStemID`, but the legacy
   bundle drops `uid`, `eventID`, and `tempStemID`, so tied same-date events, multiple boles, and
   locator collisions cannot be adjudicated. We measured 15,596 tied latest legacy composite groups
   involving 60,367 rows; those legacy diagnostics are not a substitute for source `uid`.

2. **Plant identity was under-specified.** `individualID` alone is not unique inside a site: 460
   permanent-looking identifiers occur in more than one plot across 18 sites. App selections,
   remeasurement, QC, and mortality must use `plotID × individualID`.

3. **Sampling opportunity was collapsed away.** `vst_perplotperyear` is event-specific. The legacy
   bundler takes median areas across years and drops fields such as `samplingImpractical`,
   `dataCollected`, and presence/absence indicators. A sampled absence must become zero; an impractical
   or dendrometer-only event must remain held/unsupported. They are not interchangeable.

4. **WOOD is unsupported, not treeless.** The records contain 452 rows (411 meeting the legacy size
   screen), but the 14 record plot IDs have zero matches to the 36 denominator plot IDs. `NULL` therefore
   means an unmatched data contract, not “no woody stand.” The old treeless claim is withdrawn.

5. **Growth forms leaked across channels.** The old `woody_only()` checked diameter but not the selected
   growth-form set; 100 wrong-channel rows leaked across 17 sites. Large-tree DBH and nested
   shrub/sapling measurements use different physical measures and sampled areas.

6. **Valid small sampled areas were discarded.** The old `area_ha > 0.005` screen drops valid areas at
   or below 50 m². Every finite positive protocol-compatible area, including a 40 m² nested area, must
   be retained.

7. **Density and QMD labels did not match their calculations.** Density used distinct individuals while
   the interface called them stems. QMD summed all stem-row squared diameters but divided by distinct
   individuals. The v2 contract counts stem rows and uses `sqrt(sum(d²) / n_stems)`.

8. **The old cross-biome classifier compared unlike totals.** Tree DBH cross-section and shrub basal
   cover have different measurement heights and denominators. Their raw totals cannot decide whether a
   site is “forest” or “shrubland,” and they must never be ranked on one scale.

9. **Temporal inference needs event state.** Multi-stem shrubs can receive new `tempStemID` values across
   visits, and changed points of measurement break like-for-like diameter increments. Mortality must
   first reduce each event to any-live/all-dead for `plotID × individualID`, censor lost/unknown fates,
   and remain invariant to row order.

10. **A size distribution is not recruitment.** A reverse-J-looking snapshot cannot establish
    recruitment, regeneration, or age structure. Those causal labels and the fitted “expected” curve
    are removed.

11. **Search diverged from the app metric.** The legacy search basal-area proxy differed from the app
    summary at 23 of 41 supported sites, by as much as −51.6 m²/ha at NOGP. Search, report, export,
    and on-screen values must consume the same canonical builder without independent rounding.

## RELEASE-2026 identity preflight

These are source-identity findings, not a green candidate or release receipt:

- 42 sites contain 527,000 apparent rows.
- The documented `eventID × individualID × tempStemID` locator has 1,275 collision groups covering
  2,688 rows across 37 sites.
- Twenty-two groups are cross-plot tag reuse. The operational
  `plotID × eventID × individualID × tempStemID` locator leaves 1,253 true plot-scoped groups covering
  2,644 rows.
- Among those plot-scoped groups, 1,085 remain tied at the latest date and 845 conflict on metric or
  status fields. All distinct-`uid` rows are preserved, no arbitrary winner is ranked, and the affected
  physical channel remains held. The conflict count stays explicit; the status is
  `held_identity_conflict` unless an earlier protocol/presence hold applies.
- Ten `vst_perplotperyear` rows form five duplicate `plotID × eventID` groups across BLAN (two
  groups), DEJU, JERC, and JORN. All source rows are preserved and both physical channels are
  `held_identity_conflict`.
- RELEASE-2026 contains 4,365 apparent-individual rows across 49 measurement-only
  `plotID × eventID` keys at 11 sites: CLBJ, GRSM, HARV, HEAL, JERC, KONZ,
  ORNL, OSBS, SRER, WOOD, and WREF. No matching published
  `vst_perplotperyear` row exists for those keys. Measurements are preserved;
  `opportunity_source_missing = TRUE` is carried on the record and context;
  both channels are `held_opportunity_source_missing`; and no opportunity date,
  year, effort, absence, design, coordinates, area, or denominator is inferred.
- A duplicate published source `uid`, rather than a duplicate documented or operational locator, is
  the hard source-row-identity failure.

## V2 contract

The replacement bundle targets the official **RELEASE-2026** product release with provisional data
excluded. It preserves:

- published source-row identity: unique `uid`;
- documented apparent-individual locator: `eventID + individualID + tempStemID`;
- operational locator: `plotID + eventID + individualID + tempStemID`, with distinct-`uid` collisions
  preserved and held rather than deduplicated;
- physical plant key: `plotID + individualID`;
- every event-specific plot-opportunity source row, sampled area, design, presence, impractical,
  data-collected, and identity-conflict state;
- every source-missing measurement key as an explicit measurement-only context,
  plus record-level flags and measurement-sourced count/date-range fields that
  are never presented as opportunity metadata;
- measurement height/location, basal measurement height, status, growth form, and available QC fields;
- the published UID of the one unambiguous latest-created mapping/tagging row;
  a latest-timestamp tie fails instead of receiving an arbitrary `first()`, UID,
  row-order, or taxonomy winner;
- explicit support states: sampled-with-records, sampled-absence-zero, and held/unsupported.

All consumer surfaces must declare the same contract ID, release, source DOI, and artifact hashes.

## Required fixtures before certification

- same raw ID in two plots stays two plants;
- multi-stem and same-date distinct events remain; duplicate source `uid` fails, while distinct-`uid`
  operational-locator collisions remain preserved and hold the affected channel opportunity;
- duplicate `plotID × eventID` opportunity-source rows remain preserved and hold both channels;
- a measurement-only event preserves every measurement UID, receives exactly
  one context with zero source records and no invented opportunity fields, and
  holds both channels from all derived summaries;
- RELEASE-2026 `Y`/`N` sampled presence + records, explicit sampled absence,
  both presence-record conflict directions, sampling impractical, and dendrometer-only states
  resolve to measured, zero, held, and held respectively;
- large trees ignore shrub records; shrub/sapling channels use their compatible form and area;
- a valid 40 m² area survives; zero/NA area is held; areas remain event-specific;
- stem QMD fixture: boles 3, 4 and stem 12 produce 7.506 cm stem RMS, not 9.192 cm
  individual-equivalent diameter;
- changed measurement point and unalignable temporary stems cannot cross a growth interval;
- any-live multi-stem event remains live; only all-dead is death; lost is censored; row order changes nothing;
- search, app, report, and export return byte-equivalent canonical values before presentation rounding.

## Source and scope

- Product: [NEON Vegetation structure DP1.10098.001](https://data.neonscience.org/data-products/DP1.10098.001)
- Release: [RELEASE-2026 DOI 10.48443/pypa-qf12](https://doi.org/10.48443/pypa-qf12)
- Protocol semantics: [NEON vegetation structure user guide](https://data.neonscience.org/documents/10179/2838366/NEON_vegStructure_userGuide_vE/9b88927a-d5c0-658f-1fa9-d54f1e04c81e?download=true&version=1.0)

This review authorizes an app and data-contract rebuild. It does **not** certify a productivity,
biomass, recruitment, whole-site inventory, or Cascade causal metric.
