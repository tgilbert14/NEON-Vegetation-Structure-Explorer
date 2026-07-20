# Vegetation structure — critical expert review

_Pass 4 production re-review · 2026-07-19 MST / 2026-07-20 UTC · supersedes the June 2026 “Cedar” certification_

## Verdict

**The promoted RELEASE-2026 companion contract is certifiable within its
channel-qualified sampled-plot scope; Driver adoption is not.**

The Living Poster framing is faithful to the product: NEON tags plants, maps them, and revisits their
diameter, height, and status. The app can be an excellent discovery and record-exploration tool.
The prior expert review certified `veg_ba_ha` after inspecting formulas without
first proving that the legacy bundled tables retained NEON's event keys and
sampling-opportunity semantics. They did not. That legacy certification and all
WOOD “treeless” language remain withdrawn. The promoted v2 family instead
preserves source-row identity, event/stem locators, every published opportunity
row, explicit measurement-only contexts, and separate physical channels; exact
candidate, consumer-parity, promotion, and merge receipts now support the
limited companion-app claims below.

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

Large-tree DBH cross-sectional area and shrub/sapling stem-base cross-sectional
area are both meaningful within their protocol-compatible channels. They are
not one biome-comparable quantity. The app may show both as separate
sampled-plot context, with their physical measure and denominator visible, but
it must not:

- classify sites by comparing the two raw totals;
- call one universally “basal area” without the measurement height/channel;
- rank forest DBH area against shrub/sapling stem-base cross-section;
- interpret their ratio as biomass, productivity, or producer capacity.

Every finite positive event-specific sampled area is valid unless the protocol status says otherwise.
The former 50 m² minimum is removed.

## Structure estimates

For a supported plot-event and compatible channel:

- sampled presence with records is measured;
- explicit sampled absence is zero;
- impractical, dendrometer-only, invalid-area, or missing opportunity source is held/NA;
- density counts live measured stem rows per compatible sampled area;
- channel-qualified cross-sectional area per hectare is
  `sum(pi * (d / 200)^2) / area_ha`;
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

The promoted build carries an exact release receipt, source DOI, contract ID,
artifact hashes, bundle inventory, third-party license notices, deterministic
browser contracts, and codebooks for emitted fields and structural NA/support
states. App, search, PDF, ZIP, and CSV outputs use the canonical family and
consumer-parity gates. Extended public science, keyboard, and export QA on
Connect #57 proved the key JORN zero, WOOD held, and BART export-parity edges,
then found a return-to-Places picker-reset defect. Runtime PR #7
fixes that navigation state, and promotion `8389c9c` carries only its generated
manifest checksum. Exact-head run `29722349642`, merge `0709bd0`, and Pages are
green; main CI passed and Connect #58 reports the exact merge. Reset and compact
responsive proof passed, but fresh server logs exposed first-chart `baBar`
registration warnings. PR #8 `4ce0cb7` is the clean-log follow-up; merge
`d566b30` is now published as Connect #59, and its repeated-click browser and
worker-log receipts passed. Neither runtime defect weakens or expands the
certified scientific claim boundary.

## Release evidence

- Exact official-family candidate run `29715249829` passed under R 4.5.2.
- Promotion `800bd5e` contains exactly the candidate's 54 checksum-ledger
  payload paths; PR #4 merged as `987c102`.
- PR #5 merged the first site-state Plotly guard as `91a7814`; its inspected
  #56 window was clean, but #58 later proved that guard incomplete. PR #6 merged
  the accessibility/export closeout as `433bbd25`.
- Main CI `29720341082`, Pages `29720340743`, and Connect deployment #57 agree
  on merge `433bbd25`; Connect reports all 91 packages.
- Public #57 science-edge QA found JORN's 25 supported sampled-absence zeros
  separate from 25 held impractical contexts, WOOD at zero supported contexts
  without a false zero, and byte-identical BART active-channel standalone/ZIP
  plot summaries.
- PR #7 implementation `3835451` re-registers the validated server-backed place
  choices after reset; `8389c9c` promotes the exact generated manifest checksum.
  Exact-head run `29722349642` passed every `release_contracts` CI gate. Merge and
  Pages are green at `0709bd0`; main CI `29722614074` passed and Connect #58
  reports the exact merge under R 4.5.2 with 91 packages. Reset/responsive QA
  passed, but server-log cleanliness failed and cannot be inferred from the 71
  clean browser-log entries.
- PR #8 implementation `4ce0cb7` observes raw `plotly_click-baBar` before
  reading `event_data()`. First run `29723373295` failed closed only at derived
  equality; promotion `06904fe` carries the exact generated manifest checksum,
  and exact-head run `29723718100` passed every `release_contracts` CI gate. PR
  #8 merged as `d566b30`; main CI `29724062900`, Pages `29724062095`, and
  Connect #59 agree on that release. Exact #59 BART/JORN/WOOD, reset,
  responsive, repeated-click, browser-log, and worker-log receipts passed.

## Driver disposition

**HOLD / CONTEXT ONLY / NO DRIVER DATA BYTE CHANGE.**

The official RELEASE-2026 rebundle has passed the science fixtures and
consumer-parity tests, so Driver may review its channel-specific standing-
structure context. Adoption is still a separate adapter/rebuild decision;
passing this companion app cannot promote vegetation into the causal cascade or
change a Driver data byte.

## Sources

- [NEON Vegetation structure DP1.10098.001](https://data.neonscience.org/data-products/DP1.10098.001)
- [RELEASE-2026 DOI 10.48443/pypa-qf12](https://doi.org/10.48443/pypa-qf12)
- [NEON vegetation structure user guide, Rev G](https://data.neonscience.org/api/v0/documents/NEON_vegStructure_userGuide_vG?inline=true)
