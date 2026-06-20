# NEON Vegetation Structure Explorer — Data Takeaways & Critical Review

_Suite audit — June 2026. NEON DP1.10098.001 (Vegetation structure)._

This app is the cascade's **slow STATE floor**: live woody **basal area (m²/ha)** as a producer
standing-stock index. The Driver Cascade calls the very same `stand_site()` from this app's
`R/veg_helpers.R` to fill `site_meta$veg_ba_ha` — so what this app computes _is_ the cascade's
veg rung. Numbers below are recomputed from the 42 committed bundles, not the picker cards.

## What the data actually shows

- **42 sites bundled, ~526k measurement bouts, ~148k distinct tagged individuals, 2014–2024** (median
  8.5 census years/site, range 1–10). `data/site_index.rds` classes **35 forest / 7 shrubland**
  by basal-area dominance.
- **The desert→forest basal-area gradient is clean and ~500×-wide.** Forest BA/ha (computed via the
  app's own `stand_site()`): median **24.5 m²/ha**, range **2.7 (HEAL boreal) → 56.3 (WREF
  old-growth Douglas-fir)**. Shrubland BA/ha: median **0.4**, range **0.1 (NOGP) → 11.0 (ONAQ
  sage)**, with SRER **5.1** and JORN **0.4**. This near-zero desert floor is exactly the standing-stock
  signal the cascade wants — and it is *directly measured*, not modeled.
- **Basal stem diameter is mandatory in deserts, not optional.** At SRER, **99.9%** of live stems
  (`basalStemDiameter`) carry a basal diameter but only **8.3%** carry a DBH (`stemDiameter`); JORN is
  **100% basal / 1.9% DBH**. The README's "~96–99%" claim is verified. A DBH-only stand metric would
  silently drop ~92% of desert stems — the adaptive paradigm is what makes the desert rung exist.
- **Basal area is NOT richness — they measure different things.** Across sites Spearman(BA/ha, woody
  species) = **0.605**, but within forests only **0.522**, and the contrast is explicit: **BONA**
  (boreal, BA 14 m²/ha, **8** species) vs **DELA** (S. mixed, BA 28.1, **47** species). This is the
  data backing the suite truth that *richness is composition, not productivity* — basal area is the
  better slow-state floor.
- **This is a STATE floor, not an annual flux.** NEON remeasures plots on a ~5-yr cadle; per-plant
  diameter growth is tiny — forest median **0.12 cm/yr** (IQR 0.10–0.17), SRER shrubs **0.11**, JORN
  **0.25**, ONAQ **−0.03 cm/yr** basal. Year-to-year basal-area change is well inside the across-plot SE,
  so BA is a slow ~5-yr STATE, not a yearly link — the app and the cascade both say so.
- **The honest small-n caveat lives in the plot count, and it's good news here.** n plots/site median
  **36** (range 3–50); only **3 sites have ≤6 plots** (MOAB 3, LAJA 4, KONZ 6). Unlike the n=6
  site-years regime elsewhere in the suite, the *plot* is the sampling unit and most sites are
  well-replicated — BA/ha ± SE is defensible (e.g. SRER 5.1 ± 0.7 over 38 plots; WREF 56.3 ± 4.5 over 39).
- **Three single-census shrubland sites carry NO temporal signal: DCFS, NOGP, WOOD** (one year each:
  2015/2015/2014). **WOOD returns NULL from `stand_site()`** — 36 grassland plots with no qualifying
  live woody stems, i.e. a treeless/shrubless site that classifies as "shrubland" with an empty stand.
  Growth is estimable at **39/42** sites, compound annual mortality at **38/42** (forest median
  **2.24 %/yr**, Sheil–May, with a binomial CI).
- **The QC machinery finds real, kept (not deleted) flags.** HARV: 131 "Recorded Live after Dead"
  (~1.9% of individuals), 2 implausible jumps, 2 large shrinks, **637 measurement-height moves**
  (excluded from growth, not from data). SRER: 13 / 1 / 23. Every flag is "verify, not wrong" and
  downloadable — the field-crew-useful QC bar.

## How it's built

- **Source:** NEON DP1.10098.001 tables `vst_mappingandtagging` (identity/species, 1 row/individual) ×
  `vst_apparentindividual` (the repeated diameter/height/status bouts) + `vst_perplotperyear`
  (plot sampled area, type, NLCD, coords). Pulled with R-4.1.1 (`loadByProduct`), bundled by
  `scripts/bundle_veg_data.R` → `data/sites/<SITE>.rds = list(trees, plots, meta)`.
- **`trees`** = one row per `individualID × date` bout (the growth career). **`plots`** carries
  `area_trees` (= `totalSampledAreaTrees`) and `area_shrub` (= `totalSampledAreaShrubSapling`), the
  per-hectare denominators.
- **Adaptive paradigm** (`size_spec()` / `classify_structure()` in `veg_helpers.R`): a site is **forest**
  if live tree basal area (DBH ≥10 cm) ≥ shrub basal area, else **shrubland**. Forest sizes by
  `stemDiameter` over `area_trees` with a 10 cm floor; shrubland by `basalStemDiameter` over
  `area_shrub` with no floor. Every label, size class, and Size-Lab axis re-derives from the spec.
- **Basal area metric:** per stem `ba_m2 = π·(d/200)²` (d in cm → m²), summed per plot, divided by the
  plot's sampled area in ha, then **averaged across plots** (equal plot weight, so a big plot doesn't
  dominate) with **SE across plots**. QMD is the pooled RMS diameter (`√(Σd²/Σstems)`), not a mean of
  per-plot QMDs. `stand_site()` is the one shared function the cascade reuses for `veg_ba_ha`.
- **Snapshot discipline:** stand metrics use `tree_snapshot()` (latest bout per individual) so a plant
  measured 5× counts once; growth is the explicit multi-bout metric, de-pseudoreplicated to one
  annualised rate per **permanent** id (TEMP.PLA ids are excluded — they're re-issued across years).
- **App renders:** Overview (species-by-basal-area + auto-written story), Stand Structure (size-class
  reverse-J with a de Liocourt fit, height profile, per-ha density banner), Growth & Mortality,
  Size Lab (pin-card size×height scatter), Champions, Plant Career (trading card + QC + per-bout CSV),
  Map. Exports: tidy `trees_long.csv` + `plots.csv` + `data_dictionary.csv` + README, zipped; plus a
  stand-report PDF.

## Critical findings by lens

### NEONize (suite cohesion / parity)
- **[low] Single-builder rule is honored — keep it that way.** The cascade imports `stand_site()` from
  this repo for `veg_ba_ha`; if the BA formula or the plot-area filter (`area_ha > 0.005`) ever changes
  here, regenerate the cascade bundle or the two will silently disagree. Fix: a comment cross-link in
  both files naming the shared contract.
- **[low] README says "42 sites (35 forest, 7 shrubland)"** — verified exactly. But it implies all
  carry a stand; **WOOD has no woody stand at all** (NULL BA). Fix: footnote WOOD/DCFS/NOGP as
  single-census / no-stand so a reviewer isn't surprised by blank cards.

### Ecological (Jornada / drylands)
- **[low] Basal area is correctly the headline, biomass is correctly omitted.** Directly-measured basal
  area over an allometric biomass model is the defensible dryland choice; the About panel says so. No
  change — this is the metric a Jornada reviewer wants.
- **[med] "Basal area" means two physically different things across the fork and the app should keep
  saying so.** Forest BA is cross-section at breast height (130 cm); shrubland BA is cross-section at
  the **base**. A 5.1 m²/ha desert "basal area" (SRER) is basal *cover*, not bole stocking — the
  Compare tab already warns when types are mixed; ensure any cross-biome BA figure repeats it.
- **[med] Shrubland multi-stem plants:** desert shrubs are multi-bole; `one_per_tree()` keeps the
  largest stem for the size headline but stand BA sums **all** snapshot stems (correct). Worth a one-line
  codebook note that shrubland BA is whole-plant basal cover, since a reviewer may expect per-genet.

### Data science (Quinn / FAIR / tidy)
- **[high] Codebook is incomplete for `plots.csv`.** `tidy_trees_export()` matches its codebook 20/20,
  but `plots_export()` emits **12 columns and `veg_codebook()` documents only 5** — undocumented:
  `plotType, nlcdClass, lat, lng, tallest_m, biggest_diam_cm, dominant_species`. Fix: add those rows to
  `veg_codebook()` (the export ships the dictionary, so the gap is shipped).
- **[low] NA semantics should be in the codebook, not just implied.** HARV `dbh_cm` is 28% NA,
  `basal_stem_diam_cm` 79% NA, `canopy_position` 70% NA — all *structural* (a forest stem has a DBH
  not a basal ø, and vice-versa). The dictionary should state "NA = not measured under this site's
  paradigm" so a downstream user doesn't impute.
- **[low] `plotType` is in the export and the README tells users to split tower vs distributed before
  pooling — good.** But the shipped per-ha numbers pool them (HARV 22 distributed + 20 tower). That's
  disclosed; consider also emitting a `plotType`-stratified BA in `plots.csv` so the design-based split
  is one groupby away.

### Statistics
- **[low] SE is across plots (the sampling unit) — correct, and CI is reported for mortality.** BA ± SE,
  density ± SE, binomial CI on mortality, and an n-gate (`stand_mortality` returns NULL when cohort <10).
  No overclaiming. Keep.
- **[med] Noisy `max.default` warnings on single-census sites.** `tree_growth()` / `stand_mortality()`
  emit "no non-missing arguments to max; returning -Inf" for DCFS/NOGP/WOOD before falling through to
  NULL. Harmless to output but pollutes logs and could mask a real warning. Fix: guard with an
  `if (all(is.na(date)))` early-return or `suppressWarnings` at the `max(date)` call.
- **[low] The Size-Lab/size-growth trend line is correctly gated** (n≥12, |Spearman r|≥0.15, p<0.05) and
  reverts to an honest "no clear trend" scatter — exactly the discipline the suite wants. Keep.

## Honest-stats & caveats

This app must **not** be read to claim:
- **Annual productivity or carbon flux.** Basal area here is a *standing-stock STATE* on a ~5-yr cadence
  (forest growth ≈0.12 cm/yr); it is not an annual NPP signal and the cascade must treat it as a
  slow floor / context covariate, never as a year-indexed plant rung.
- **A wall-to-wall inventory.** Every BA/ha and density is a plot-based **index** scaled to the hectare,
  with tower + distributed plots pooled. Use the `plotType` column for a design-based estimate.
- **Cross-biome BA equivalence.** Forest DBH-basal-area and shrubland basal-cover are different
  measurements; the ~500× forest:desert ratio is real *as a stocking gradient* but is partly a
  measurement-height difference, not purely biomass.
- **Richness as productivity.** BA and woody richness only loosely covary (ρ≈0.52 within forests);
  DELA holds 47 species at 28 m²/ha while boreal BONA holds 8 at 14 — they are orthogonal axes.
- **Mortality from the snapshot pie.** The live/dead pie is a point-in-time ratio; only the Sheil–May
  compound rate (38/42 sites) is an annual mortality rate, and "Lost track/removed" is held out as a
  data state, not a death.

## Place in the cascade

This app supplies the **producer standing-stock rung** of climate → plants → consumers. The Driver
Cascade's `build_cascade.R` calls this repo's `stand_site()` to populate `site_meta$veg_ba_ha`
(+`veg_ba_se`, `veg_type`, `veg_n_plots`), explicitly commented as "per-site context, not an annual
signal." Its role:

- **A slow STATE floor, not an annual link.** Because BA changes ~0.1 cm/yr per stem, it can't carry a
  sub-annual or even annual climate→plant lag. It is the *baseline producer capacity* a site offers
  consumers — the level the faster signals (green-up onset, richness) fluctuate around.
- **The honest replacement for richness as the plant rung.** The suite's lead result is
  temperature→green-up onset; richness inverts in drylands and is composition, not productivity. Basal
  area is the directly-measured, biome-comparable standing-stock covariate — strongest where it should
  be (the near-zero desert floor, the high-BA conifer ceiling) and replicated enough (median 36 plots)
  to report with an SE. It corroborates the producer axis the cascade needs without pretending to a
  yearly flux it doesn't have.
