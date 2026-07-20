# Vegetation Structure science contract

Contract: `NEON-VST-DP1.10098.001-v2`  
Source target: NEON `DP1.10098.001`, official `RELEASE-2026`, provisional data excluded  
Status: **HOLD until the official 42-site candidate and all parity gates pass**

This is the exact Pass 4 implementation contract. It does not certify the legacy
bundle family and does not authorize a Driver/Cascade data-byte change.

## Identity and support

- Physical plant: `plotID × individualID`.
- Published source-row identity: `uid`. Apparent-individual and opportunity UIDs
  are preserved in their source tables, and the exact mapping/tagging UID used
  for a measurement row is preserved as `mapping_source_uid`. A blank or
  duplicate source UID is a hard failure.
- Mapping/tagging identity is selected only when one row has the unambiguous
  latest created timestamp for `plotID × individualID`. Multiple rows tied at
  that latest timestamp are a hard failure; UID, row order, and taxon content
  are never used to invent a winner.
- NEON's documented apparent-individual locator is
  `eventID × individualID × tempStemID`. It has collisions in `RELEASE-2026` and
  is not treated as unique.
- Operational apparent-individual locator:
  `plotID × eventID × individualID × tempStemID`. Adding `plotID` excludes
  cross-plot reuse of the same tag from a within-plot identity conflict.
- Sampling-opportunity locator: `plotID × eventID`.
- Distinct-`uid` rows that collide on the operational locator are all preserved;
  none is selected by row order, date, metric value, status, or any other
  ranking. The affected physical channel is never supported. Its conflict
  count stays explicit, and the status is `held_identity_conflict` unless an
  earlier sampling, data-collected, area, or presence-record hold applies.
- Colliding source rows for one `plotID × eventID` opportunity are likewise all
  preserved. Because their opportunity state cannot be adjudicated without an
  arbitrary winner, both physical channels are `held_identity_conflict`.
- RELEASE-2026 contains 4,365 published apparent-individual rows across 49
  `plotID × eventID` keys at 11 sites with no matching published
  `vst_perplotperyear` row. Each measurement row is preserved and flagged. A
  measurement-only context is retained for audit, but its opportunity source
  UID/count, date/year, effort, presence, event type, design, coordinates, and
  sampled areas are not invented. Both physical channels are
  `held_opportunity_source_missing` and excluded from every zero, denominator,
  snapshot, taxon, growth, mortality, and site-summary derivation.
- Current-state summaries select the latest supported event for each plot, then
  the matching latest event for each composite plant. Repeated events are not
  pooled as independent current observations.
- A supported opportunity requires a matching published `vst_perplotperyear`
  source row, the matching event, a registered support
  state, a finite positive channel-compatible sampled area, and no eligible live
  channel row with a missing, non-finite, non-positive, or threshold-incompatible
  required diameter.
- `sampled_absence` is an observed zero. Every `held_*` state is unknown or
  unsupported and remains `NA`, with a reason.
- RELEASE-2026 presence values are normalized exactly: `Y`/`Yes` mean present
  and `N`/`No` mean absent, alongside the published textual present/absent
  forms. `N` with measurement records and `Y` without records are both
  `held_presence_record_conflict`; only absence with no records can contribute
  an observed zero.
- `held_metric_invalid` fails the entire plot event closed when even one eligible
  live tree row lacks valid DBH (finite and ≥10 cm) or one eligible live
  shrub/sapling row lacks valid positive basal stem diameter. A protocol or
  presence-record conflict takes precedence and remains visible when both occur.
- Published `dataQF` and related qualifier fields are preserved and quantified
  for review. They are not blanket row-exclusion rules; metric validity is tested
  directly from the channel's required measurement.
- Plot is the sampling unit for per-hectare summaries and uncertainty.

## Disjoint measurement channels

| Channel | Included growth forms | Measurement | Threshold | Event area | Meaning |
|---|---|---|---:|---|---|
| `tree_dbh` | `single bole tree`, `multi-bole tree` | `stemDiameter` | ≥10 cm | `area_trees` | bole cross-section at breast height |
| `shrub_sapling_basal` | `single shrub`, `small shrub`, `sapling` | `basalStemDiameter` | >0 cm | `area_shrub` | stem-base cross-section |

The channels are never pooled, ranked against one another, or used to classify a
site by raw magnitude. `small tree` is a nested DBH class: its rows remain in the
preserved download but are withheld from both summaries until a dedicated
nested-area DBH channel is registered and tested. Unknown growth forms are also
preserved, not silently assigned.

The site index retains one deterministic default presentation channel for the
national map. That default is a navigation choice based on supported sampling
representation, not a forest/shrubland classification. The network search keeps
an additional exact 42-site × two-channel grid, and every supported channel can
be opened and explored independently.

## Snapshot and sampled-plot estimator

For every plot, the implementation keeps its latest fully supported channel
event; if none exists, it keeps the latest held plot-event context so the reason
remains visible. A selected event is atomic: every preserved stem row from that
`plotID × eventID` remains in the snapshot even when row measurement dates differ
or are missing. From the matching snapshot it:

1. keeps eligible live stem rows in the active channel;
2. computes each stem's cross-section as
   `area_m2 = π × (diameter_cm / 200)^2`;
3. counts apparent-individual rows as stems and `plotID × individualID` keys as
   plants;
4. divides each plot-event total by its exact positive event area, retaining all
   valid areas, including 40 m² nested areas;
5. treats explicit sampled absence as zero and held opportunities as `NA`;
6. reports the equal-plot mean and `sd(plot values) / sqrt(n plots)`;
7. reports stem-weighted QMD as
   `sqrt(sum(diameter_cm²) / number_of_live_stem_rows)`.

The estimator describes supported sampled plots. It is not a wall-to-wall site
inventory and is not annual productivity, biomass, or carbon.

Taxon bars are equal-plot means of measured cross-sectional contribution within
one active channel: each supported plot is first divided by its exact compatible
area, then every supported plot—including explicit zero plots—receives equal
weight. They retain unresolved/coarse identifications; detection and
identification effort still affect what is represented, so the bars are not
ecological-dominance estimates.

## Change and status

Growth uses permanent `plotID × individualID` plants with at least two ordered
events. Tree events use equivalent diameter `sqrt(sum(d²))`; annual change is
the first-to-last difference divided by elapsed days/365.25. A changed point of
measurement is flagged and excluded from displayed growth summaries. Multi-stem
basal trajectories are withheld because `tempStemID` is not stable enough across
years to align shrub/sapling stems. Dead-to-live “resurrections” are held from
growth and flagged for review. Diameter decreases remain preserved and flagged;
their cause is not inferred.

The canonical basal point-of-measurement field is populated from the published
RELEASE-2026 `basalStemDiameterMsrmntHeight` field (with the older canonical-name
alias accepted only for compatibility). It is not inferred from diameter or
another measurement-height field.

Status is reduced once per composite plant and event: any live stem means Live;
all observed stems dead/downed means Dead; lost, removed, and unknown states are
not deaths. The compound mortality cohort starts live, requires at least two
known events and `n ≥ 10`, censors after the first censored event, and excludes
resurrection histories. It reports
`100 × [1 − (1 − deaths/n)^(1/mean_years)]`; its interval is a delete-one-plot
jackknife and is withheld with fewer than three usable plot clusters.

A single census can support a current sampled-plot snapshot but cannot support a
growth or mortality estimand.

## Permitted and prohibited claims

After release certification, the app may describe sampled-plot woody structure,
diameter/height distributions, tagged-plant records, status, and explicitly
supported remeasurement change.

It may not claim:

- wall-to-wall site inventory;
- annual productivity from standing cross-sectional area;
- biomass or carbon without a separately registered allometric model;
- causal climate response;
- cross-channel magnitude comparability;
- ecological dominance from raw measured contribution;
- recruitment, regeneration, or stand age from a size-class snapshot;
- woody absence from an unmatched or otherwise held denominator;
- annual growth or mortality from one census.

WOOD is the legacy counterexample: qualifying records and denominator rows use
nonmatching plot-ID families. Its correct state is held/unmatched, not “treeless.”

## Release and parity gates

Release requires:

1. an exact official-release source receipt, explicit `FULL_RELEASE` query
   selection, and complete 42-site inventory; bounded month queries are
   diagnostic-only and cannot become release/runtime bytes;
2. unique published source `uid` values; complete preservation and fail-closed
   handling of documented-locator, operational-locator, and plot-event source
   collisions, with no arbitrary winner; exact bidirectional algebra among
   published opportunity keys, measurement-only keys, and preserved measurement
   rows, with source-missing contexts held and visibly counted;
3. exact support-state vocabulary and reasons, positive supported areas, exact
   record/invalid-metric/identity-conflict counts recomputed from preserved rows,
   and records/status consistency in both the release verifier and the deployed
   runtime gate;
4. independent row-derived invariants in the runtime gate, release verifier,
   DQA, and consumer-parity gate: `live` from `plantStatus`, `year` from date,
   taxonomy label/species/resolution from preserved taxonomy, permanence from
   `individualID`, composite plant/event keys from their components, and mapping
   match state from `mapping_source_uid`;
5. deterministic snapshot, stand, growth, mortality, taxonomy, presentation-channel,
   index, search, report, and export fixtures;
6. independent consumer-side recomputation of every embedded site, physical-
   channel, and taxon summary plus the site/search indexes from preserved rows,
   including the 84-row site × physical-channel search grid; deterministic
   builder output alone is not evidence of summary correctness;
7. browser, responsive, accessibility, cover, image-provenance, offline-source,
   manifest, and Connect-deployment gates;
8. reviewed Driver and suite-learning handoffs.

## Driver disposition

**HOLD / CONTEXT / NO DRIVER BYTE CHANGE.** The candidate may supply method and
design evidence after validation, but no Vegetation value becomes a current
Driver field without a separately reviewed Driver parity/rebuild receipt.
