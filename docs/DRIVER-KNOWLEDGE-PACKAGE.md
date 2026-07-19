# Driver Cascade knowledge package

Source app: NEON Vegetation Structure Explorer (`DP1.10098.001`)

Disposition: **HOLD / NO DRIVER BYTE CHANGE**.

Vegetation Structure is the suite's candidate slow producer-state context, but
the legacy family is not eligible for Driver ingestion. It lacks a complete
upstream receipt and discarded the event/stem and sampling-opportunity identity
needed to prove each stand denominator. WOOD demonstrates the risk: qualifying
measurement records and denominator rows exist, but their plot identifiers do
not join. An unavailable result there cannot be translated into “no woody stand.”

## Candidate contribution after validation

| Field | Meaning | Mandatory support | Driver role |
|---|---|---|---|
| `veg_structure_status` | explicit supported/empty/held diagnostic | contract and reason code | eligibility gate |
| `veg_structure_channel` | exact `tree_dbh` or `shrub_sapling_basal` physical channel | measurement field, height, threshold, and protocol | mandatory split |
| `veg_ba_m2_ha` | equal-plot mean live cross-section or basal cover per hectare | matched plot opportunities and `n_plots` | slow state context |
| `veg_ba_se` | SE across supported plots | same plot panel | uncertainty |
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

Driver may reconsider a Vegetation field only after the app provides:

1. an explicit official release and DOI plus raw/bundle digests;
2. all 42 event-keyed bundles and complete sampling-opportunity fields;
3. an explicit denominator diagnostic with no silent unmatched joins;
4. registered `tree_dbh` and `shrub_sapling_basal` estimands with point-of-use
   physical labels;
5. deterministic fixtures and app/export/index parity;
6. plot support and uncertainty;
7. a promoted app commit, manifest, Connect receipt, and public semantic health;
8. a Driver rebuild from the exact promoted source with old/new field parity and
   an explicit `ADOPT`, `CONTEXT`, `HOLD`, or `REJECT` decision.

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
measurement event and sampled opportunity join exactly. Driver should fail closed
and link users back to the companion app's diagnostic rather than convert an
unsupported state to zero.
