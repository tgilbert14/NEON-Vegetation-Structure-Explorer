# Vegetation structure — critical expert review

_Pass 4 re-review · 2026-07-19 · supersedes the June 2026 “Cedar” certification_

## Verdict

**The public product direction is strong; the legacy metric contract is not certifiable.**

The Living Poster framing is faithful to the product: NEON tags plants, maps them, and revisits their
diameter, height, and status. The app can be an excellent discovery and record-exploration tool.
However, the prior expert review certified `veg_ba_ha` after inspecting formulas without first proving
that the bundled tables retained NEON's event keys and sampling-opportunity semantics. They do not.
The certification and all WOOD “treeless” language are withdrawn.

## Protocol interpretation

The three-table spine is necessary but must remain at its native grain:

- `vst_apparentindividual`: published `uid` is the preserved, unique source-row
  identity. NEON documents `eventID × individualID × tempStemID` as the row
  locator, but `RELEASE-2026` contains collisions on those three fields. The
  operational locator therefore includes `plotID` to distinguish cross-plot tag
  reuse;
- `vst_mappingandtagging`: identity/taxonomy, joined from the one unambiguous
  latest-created `plotID × individualID` row with its published UID preserved;
  a tie at the latest created timestamp fails rather than being resolved by row
  order, UID, or taxon content;
- `vst_perplotperyear`: `plotID × eventID`, including exact event sampled areas and the fields that say
  whether sampling occurred, was impractical, was dendrometer-only, or recorded absence.

`individualID` alone is not a safe site-level plant key. A physical plant is scoped by
`plotID × individualID`; events, not dates alone, order the observations.

The official-release identity preflight found 527,000 apparent rows across all
42 sites. The documented three-field locator has 1,275 collision groups covering
2,688 rows across 37 sites. Twenty-two groups are cross-plot reuse; adding
`plotID` leaves 1,253 true plot-scoped groups covering 2,644 rows. Of those,
1,085 remain tied at the latest date and 845 conflict on metric or status fields.
No scientifically defensible ranking chooses one row. Every distinct-`uid` row
must remain preserved and the affected physical channel must remain held. Its
conflict count stays explicit; the canonical status is
`held_identity_conflict` unless an earlier protocol/presence hold applies. Only
duplicate published `uid` is a hard identity failure.

The opportunity table also contains 10 source rows forming five duplicate
`plotID × eventID` groups across BLAN (two groups), DEJU, JERC, and JORN. Those
rows must remain preserved, with both physical channels held as
`held_identity_conflict`; neither row order nor a preferred area/status value
may choose a winner.

The official source family also contains 4,365 apparent-individual rows across
49 `plotID × eventID` keys at 11 sites with no matching published
`vst_perplotperyear` row. This is a publication/linkage gap, not evidence that
field sampling did or did not occur. The records remain available for record-level
inspection, but absence, effort, sampled area, scaling, snapshot, taxonomy, and
longitudinal summaries are withheld under `held_opportunity_source_missing`.
The audit context retains only the measurement key and explicitly
measurement-sourced count/date range; it borrows no opportunity metadata.

## Measurement channels

Large-tree DBH cross-sectional area and shrub/sapling basal-cover area are both meaningful within their
protocol-compatible channels. They are not one biome-comparable quantity. The app may show both as
separate sampled-plot context, with their physical measure and denominator visible, but it must not:

- classify sites by comparing the two raw totals;
- call one universally “basal area” without the measurement height/channel;
- rank forest DBH area against shrub basal cover;
- interpret their ratio as biomass, productivity, or producer capacity.

Every finite positive event-specific sampled area is valid unless the protocol status says otherwise.
The former 50 m² minimum is removed.

## Structure estimates

For a supported plot-event and compatible channel:

- sampled presence with records is measured;
- explicit sampled absence is zero;
- impractical, dendrometer-only, invalid-area, or missing opportunity source is held/NA;
- density counts live measured stem rows per compatible sampled area;
- measured area is `sum(pi * (d / 200)^2) / area_ha`;
- QMD is stem-weighted `sqrt(sum(d^2) / n_stems)`;
- plot summaries may be described as means across supported sampled plots, not wall-to-wall site
  estimates.

Tower and distributed designs remain visible. A distributed-only subset can be useful context, but the
current app does not claim a fully weighted, certified whole-site estimator.

## Change and mortality

Growth is reportable only for like-for-like remeasurements. DBH intervals must not span a changed point
of measurement. Basal multi-stem records whose temporary stem labels cannot be aligned across events
are held rather than collapsed into an apparently precise whole-plant trajectory.

Mortality first reduces every event to one plant state: any live stem means live; death requires all
observed stems to be dead. Lost/removed/unknown fates are censored. Cohorts use `plotID × individualID`,
event order, and cluster-aware uncertainty. Row order must not change the result.

Diameter decreases are not automatically “usually real” or “wrong.” They can reflect biology, damage,
or measurement differences; retain them, disclose them, and apply explicit QC rules.

## Claims explicitly withheld

- recruitment or regeneration from a size-class shape;
- an “expected” reverse-J curve fit to the same snapshot;
- biomass or annual productivity;
- a whole-site woody inventory;
- cross-channel forest-vs-shrub rankings;
- WOOD as treeless;
- Driver/Cascade adoption before parity tests pass.

## FAIR and product requirements

A releasable build needs an exact release receipt, source DOI, contract ID, artifact hashes, bundle
inventory, third-party license notices, deterministic browser checks, responsive visual QA, and a
codebook that documents every emitted field and NA/support state. App, search, PDF, and CSV outputs must
be generated from one canonical metric builder.

## Driver disposition

**HOLD / CONTEXT ONLY / NO DRIVER BYTE CHANGE.**

After a full official RELEASE-2026 rebundle passes the science fixtures and cross-surface parity tests,
Driver may review the resulting channel-specific context. That later review is a separate decision;
passing this app's tests does not automatically promote vegetation into the causal cascade.

## Sources

- [NEON Vegetation structure DP1.10098.001](https://data.neonscience.org/data-products/DP1.10098.001)
- [RELEASE-2026 DOI 10.48443/pypa-qf12](https://doi.org/10.48443/pypa-qf12)
- [NEON vegetation structure user guide, Rev G](https://data.neonscience.org/api/v0/documents/NEON_vegStructure_userGuide_vG?inline=true)
