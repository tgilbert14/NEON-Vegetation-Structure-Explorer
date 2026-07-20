# Driver Cascade knowledge package

Source app: NEON Vegetation Structure Explorer (`DP1.10098.001`)

Disposition: **HOLD / CONTEXT ONLY / NO DRIVER DATA BYTE CHANGE**.

Vegetation Structure is a channel-qualified slow standing-structure context for
the suite. Its promoted RELEASE-2026 companion family now has a complete source
receipt and preserves the event/stem and sampling-opportunity identity needed to
prove each supported sampled-plot denominator. That makes the companion app
reviewable; it does not make any Vegetation field a current Driver field. WOOD
remains the denominator-integrity counterexample: measurement records and
published opportunity rows exist, but the relevant plot-ID families do not
join. Its held state cannot become “no woody stand” or a numeric zero.

The official RELEASE-2026 preflight adds a current-source coverage constraint:
4,365 measurement rows across 49 plot-events at 11 sites have no matching
published `vst_perplotperyear` source row. The app preserves those records under
`held_opportunity_source_missing`, but they are ineligible for absence, scaling,
structure summaries, longitudinal metrics, or Driver features. This is context
for coverage/QC, not a new cascade edge.

## Potential Driver adapter fields — not current Driver schema

| Field | Meaning | Mandatory support | Driver role |
|---|---|---|---|
| `veg_structure_status` | explicit supported/empty/held diagnostic | contract and reason code | eligibility gate |
| `veg_structure_channel` | exact `tree_dbh` or `shrub_sapling_basal` physical channel | measurement field, height, threshold, and protocol | mandatory split |
| `veg_cross_section_m2_ha` | equal-plot mean live tree-bole DBH cross-section or shrub/sapling stem-base cross-section per hectare | matched plot opportunities, exact channel, and `n_plots` | slow standing-structure context |
| `veg_cross_section_se` | SE across supported plots for that exact physical channel | same plot panel and channel | uncertainty |
| `veg_density_stems_ha` | supported live stem density | matching sampled area | context only |
| `veg_qmd_cm` | pooled quadratic mean diameter | stem and physical-channel support | context only |
| `veg_support_*` | sites, plots, events, years, matched/unmatched IDs, release/contract hashes | explicit | mandatory provenance |

The `tree_dbh` and `shrub_sapling_basal` channels share units but not measurement
height or physical interpretation. Driver must not use a flat cross-biome rank
without the channel and support fields.

## Explicit exclusions

- basal area as annual productivity, biomass, carbon, or ecosystem health;
- annual lag edges from a slow remeasurement state;
- a zero value inferred from no matching denominator;
- growth or mortality at one-census sites;
- pooled tower/distributed design as an unqualified design-based site mean;
- causal climate–structure edges from this descriptive app;
- any value lacking source app, official release, contract version, support, and
  exact source artifact links.

## Eligibility gate

Companion gates 1–7 are satisfied. Gate 8 is intentionally open because no
Driver adapter or rebuild was performed.

1. [x] Explicit official release and DOI plus raw/bundle digests.
2. [x] All 42 event-keyed bundles, every published opportunity row, and an exact
   measurement-only ledger with no invented opportunity fields;
3. [x] An explicit denominator diagnostic with no silent unmatched joins and
   exact counts for excluded source-missing events/records/sites.
4. [x] Registered `tree_dbh` and `shrub_sapling_basal` estimands with
   point-of-use physical labels.
5. [x] Deterministic fixtures and app/export/index parity.
6. [x] Plot support and uncertainty.
7. [x] A promoted app commit and manifest, green merge/Pages receipts, exact
   Connect deployment identity, and public semantic health. Core merge
   `987c102`, intermediate Plotly-guard merge `91a7814`, production merge `433bbd25`, and
   Connect #57 are durable receipts. PR #7 exact-head run `29722349642`, merge
   `0709bd0`, main CI `29722614074`, Pages, and Connect #58 are also exact; #58
   proved the picker reset and compact layouts. Fresh worker logs nevertheless
   exposed `baBar` registration warnings. PR #8 implementation `4ce0cb7`
   changes that lifecycle by waiting for raw `plotly_click-baBar` before reading
   event data. Promotion `06904fe`, exact-head run `29723718100`, merge
   `d566b30`, main CI `29724062900`, Pages `29724062095`, and Connect #59 are
   exact. The final repeated-click, responsive, reset, science-state, browser-
   log, and worker-log receipts passed.
8. [ ] A Driver adapter and rebuild from the exact promoted source with old/new
   field parity and an explicit `ADOPT`, `CONTEXT`, `HOLD`, or `REJECT`
   decision.

Until gate 8 passes, all field names above are design proposals only. They must
not be added to Driver search, Cascade edges, rankings, summaries, or exports.

## Design feedback for Driver

- Present Vegetation Structure as a companion lens while the HOLD is open.
- Separate standing structure, plant composition, and phenology; they answer
  different producer questions.
- Show missingness reason and support beside every producer-state value.
- Use the app's Living Poster measurement motif as the standing-structure thread
  in the eventual Driver master poster, without depicting generated art as data.

## Current Driver insight

The important finding is a **denominator-integrity gate**, not a new ecological
edge: a plausible stem table cannot support a per-hectare value until the
measurement event and sampled opportunity join exactly. Driver should fail
closed, carry the companion app's source-missing coverage diagnostic as context,
and link users back to the record-level audit rather than convert an unsupported
state to zero. This package contributes learning and a future adapter contract;
it contributes no Driver data byte today.

The public science-edge receipt strengthens that gate without creating a Driver
field: JORN keeps 25 sampled-absence plot zeros distinct from 25 impractical
holds, while WOOD keeps all 50 contexts held (14 source-missing and 36
opportunity-unknown) despite 452 preserved shrub/sapling rows. Those are support
semantics for any future adapter, not a new causal edge.
