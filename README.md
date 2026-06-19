# NEON Vegetation Structure Explorer

An (unofficial) R/Shiny explorer for NEON's **Vegetation structure** product
(**DP1.10098.001**) — a *NEONize* sibling of the [NEON Small Mammal Tracker][smt] and
[NEON Plant Diversity Explorer][pde], built to the same Desert Data Labs quality bar.

> NEON tags individual woody stems, maps them, and **remeasures** their diameter, height,
> and status over the years — so every tree has a *growth career*. The unit here is the
> individual tree (closer to the mammal app's marked individuals than to plant cover).

## What it shows

| Tab | Content |
|---|---|
| **Overview** | Composition by basal area, an auto-written "story", the stand's headline numbers. |
| **Stand Structure** | The diameter **size-class distribution** (the forester's reverse-J), the height profile, and per-hectare **basal area + stem density + QMD**. |
| **Growth & Mortality** | Diameter **growth rates** between remeasurements, the fastest-growing trees, and the live / standing-dead split. |
| **Forest Size Lab** | The flagship: every tree as a dot in **diameter × height** space, **tap-to-pin** tree cards, named quadrants (Giants / Spires / Stout / Saplings), export-with-pins. |
| **Tree Career** | The drill-down: a downloadable tree card (PNG + CSV) — diameter/height/status, the **growth trajectory** over its remeasurements, size-for-species percentile, and QC flags. |
| **Map** | Plot markers sized by basal area, coloured by your chosen metric. |
| **About** | Methods + caveats. |

## Run it

R 4.5.x, bundle-only (no network):

```r
shiny::runApp(".", port = 8191)
```

The Harvard Forest (**HARV**) demo loads instantly. **WREF** (Wind River old-growth — 60 m
Douglas-firs, 146 cm trunks) and **SCBI** (Smithsonian mapped forest) are also bundled.

## Data

Per-site bundles in `data/sites/<SITE>.rds` as `list(trees, plots, meta)`:

- **`trees`** — one row per individual × measurement bout (the growth career): `individualID,
  plotID, year, date, scientificName, family, growthForm, plantStatus, live, stemDiameter`
  (DBH cm @130), `basalStemDiameter, height` (m), `canopyPosition, measurementHeight, permanent`.
- **`plots`** — per plot: `plotType, nlcdClass, lat, lng, area_trees` (the per-ha denominator).

### Rebuild

NEON pulls need **R-4.1.1** (neonUtilities; R-4.5.2 crashes on `loadByProduct`) + a `.neon_token`:

1. Fetch: `Rscript-4.1.1 ../App-NEON-Small-Mammal-Tracker/scripts/fetch_veg_demo.R`
2. Bundle: `Rscript scripts/bundle_veg_data.R`

## Honesty notes

- **Snapshot, not pooled:** stand metrics use each tree's *latest* measurement (NEON remeasures
  most plots every ~5 years; pooling bouts would count a tree many times). Growth is the explicit
  multi-bout metric, annualised between visits.
- **No biomass:** above-ground biomass needs an allometric model whose error compounds; basal
  area (directly measured) is the honest stand measure shown instead.
- Diameter *decreases* between visits are common and usually real (bark sloughing, drought, a
  changed measurement height) — kept and flagged, not deleted. Live/dead is a snapshot ratio,
  not an annual mortality rate. Size-Lab dots need both diameter and height; diameter-only stems
  are counted and noted, not silently dropped.

Built by Desert Data Labs · desertdatalabs@gmail.com. Not affiliated with NEON, Battelle, or the
NSF. An educational data-exploration tool. See [`docs/neonize-playbook.md`](docs/neonize-playbook.md).

[smt]: ../App-NEON-Small-Mammal-Tracker
[pde]: ../NEON-Plant-Diversity
