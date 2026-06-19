# NEON Vegetation Structure Explorer

An (unofficial) R/Shiny explorer for NEON's **Vegetation structure** product
(**DP1.10098.001**) — a *NEONize* sibling of the [NEON Small Mammal Tracker][smt] and
[NEON Plant Diversity Explorer][pde], built to the same Desert Data Labs quality bar.

> NEON tags individual woody plants, maps them, and **remeasures** their diameter, height,
> and status over the years — so each has a *growth career*. The unit is the individual plant
> (closer to the mammal app's marked individuals than to plant cover).

Covers **all 42 NEON sites that publish veg structure, across every biome**. The app is
**adaptive**: **forest** sites size plants by tree **DBH** (≥10 cm, over the sampled tree area);
**desert / shrubland** sites — where shrubs are too short for a breast-height diameter — size them
by **basal stem diameter** (what NEON records for ~96–99% of desert stems), over the shrub-sapling
sampled area. Each site is auto-classified by basal-area dominance, and every label, metric, and
the Size Lab axes adapt accordingly.

## What it shows

| Tab | Content |
|---|---|
| **Overview** | Composition by basal area, an auto-written "story", the stand's headline numbers. |
| **Stand Structure** | The **size-class distribution** (forester's reverse-J for trees, basal classes for shrubs), the height profile, and per-hectare **basal area + stem density + QMD**. |
| **Growth & Mortality** | Diameter **growth rates** between remeasurements, the fastest-growing plants, and the live / standing-dead split. |
| **Size Lab** | The flagship: every plant as a dot in **size × height** space (DBH or basal ø), **tap-to-pin** cards, adaptive named quadrants, export-with-pins. |
| **Champions** | The record-holders — biggest, tallest, fastest-growing, longest-tracked — a re-sortable leaderboard + podium; tap a row to open its career. |
| **Plant Career** | The drill-down: a shareable holographic **plant card** (PNG) + a QC record (PNG) + raw per-bout data (CSV) — size/height/status, the **growth trajectory**, size-for-species percentile, and QC flags. |
| **Map** | Plot markers sized by basal area, coloured by your chosen metric. |
| **About** | Methods + caveats. |

Plus: a clickable hero band (→ ranked modals), **Compare two stands** head-to-head, a full-dataset **CSV/zip export + codebook**, and a one-page **stand report PDF**.

## Run it

R 4.5.x, bundle-only (no network):

```r
shiny::runApp(".", port = 8191)
```

The Harvard Forest (**HARV**) demo loads instantly. **All 42 NEON veg-structure sites** are
bundled (35 forest, 7 shrubland) — from WREF (Wind River old-growth, 60 m Douglas-firs, 146 cm
trunks) and PUUM (Hawaiian tropical wet forest) to SRER & JORN (Sonoran/Chihuahuan desert
shrublands), ONAQ (Great Basin sage), and CPER (shortgrass steppe) — spanning temperate &
boreal forest, desert, grassland, alpine, and tropical biomes.

## Data

Per-site bundles in `data/sites/<SITE>.rds` as `list(trees, plots, meta)`:

- **`trees`** — one row per individual × measurement bout (the growth career): `individualID,
  plotID, year, date, scientificName, family, growthForm, plantStatus, live, stemDiameter`
  (DBH cm @130), `basalStemDiameter, height` (m), `canopyPosition, measurementHeight, permanent`.
- **`plots`** — per plot: `plotType, nlcdClass, lat, lng, area_trees` (the per-ha denominator).

### Rebuild

NEON pulls need **R-4.1.1** (neonUtilities; R-4.5.2 crashes on `loadByProduct`) + a `.neon_token`:

1. Fetch: `Rscript-4.1.1 scripts/fetch_veg_data.R` (pulls **every** site NEON publishes from the
   API, skipping ones already in `../veg-data-fetch/`)
2. Bundle: `Rscript scripts/bundle_veg_data.R` (auto-detects every `*_raw.rds`; classifies each
   site forest/shrubland and carries the basal-diameter + crown columns)
3. Index: `Rscript scripts/build_site_index.R` (adaptive per-site headline numbers)

## Honesty notes

- **Two size paradigms, the right one per site:** forests are sized by tree DBH (≥10 cm); deserts
  & shrublands by basal stem diameter (shrubs are too short for a breast-height measurement). The
  app classifies each site by which growth form dominates *basal area* — so a mature forest with a
  dense shrub understory stays a forest.
- **Snapshot, not pooled:** stand metrics use each plant's *latest* measurement (NEON remeasures
  most plots every ~5 years; pooling bouts would count one many times). Growth is the explicit
  multi-bout metric, annualised between visits.
- **No biomass:** above-ground biomass needs an allometric model whose error compounds; basal
  area (directly measured) is the honest stand measure shown instead.
- Diameter *decreases* between visits are common and usually real (bark sloughing, drought, a
  changed measurement height) — kept and flagged, not deleted. Live/dead is a snapshot ratio,
  not an annual mortality rate. Size-Lab dots need both diameter and height; diameter-only stems
  are counted and noted, not silently dropped.

## Deploy & live

Hosted on **Posit Connect Cloud** (deployed from this repo via `manifest.json`), with a
**GitHub Pages** landing page + og card + cold-start pre-warm at
<https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/>, and an automatic monthly
data-refresh GitHub Action. Full steps in [`DEPLOY.md`](DEPLOY.md).

Built by Desert Data Labs · desertdatalabs@gmail.com. Not affiliated with NEON, Battelle, or the
NSF. An educational data-exploration tool. See [`docs/neonize-playbook.md`](docs/neonize-playbook.md).

[smt]: ../App-NEON-Small-Mammal-Tracker
[pde]: ../NEON-Plant-Diversity
