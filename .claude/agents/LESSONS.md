# LESSONS — NEON Vegetation Structure Explorer (project-local)

> Project-specific institutional memory for THIS app. Agents boot cold: read this on start (grep for your
> own name, `· <agent> ·`) and append a one-line lesson after a run that taught something durable.
>
> The **canonical, cross-cutting** log lives in `TG-Data-Apps/.claude/agents/LESSONS.md`; the deep NEON
> methodology lives in `docs/neonize-playbook.md` (the flagship `NEON-Small-Mammal-Tracker-App` copy is the
> reference). `curator` promotes recurring lessons up. Format + protocol:
> `TG-Data-Apps/.claude/agents/_CONVENTIONS.md`.

## How to write an entry
```
- [YYYY-MM-DD] <agent> · <verdict: confirmed|over-flagged|wrong|gap> · <the durable lesson, one line>
```

## Lessons

<!-- newest at the bottom; append, don't rewrite history. Seeded 2026-07-20 from the flagship's cross-agent pass. -->
- [2026-07-20] connor · confirmed · This app's release gate is byte-exact on BOTH `manifest.json` and
  `data/search_index.rds` (each regenerated twice, must be identical), so both must be deterministic: strip
  source-built `Built` fields, canonicalize the geo-pin lane (`Source`=CRAN, absolute `Repository`), freeze
  floating RSPM aliases by TARGETED text-substitution — never a `jsonlite` reserialize (it mangles
  `writeManifest`'s canonical format AND destroys the exact `url::` tarball refs in `RemotePkgRef`) — and
  pin `platform`=4.5.2 + `locale`="C". A read-only `verify_manifest.R` twin makes "committed == regenerated"
  enforceable without granting write.
- [2026-07-20] neonize · gap · The derived-bytes gate + `permissions: contents: read` + no local R = a
  mandatory MERGE LOOP; this app's Living-Poster PR #4 burned 13/30 CI runs on it. Escape: promote the
  VALIDATED `vegetation-structure-derived-<sha>-<run>` artifact (never an unvalidated one), commit it
  byte-for-byte, push ONCE, don't rapid re-push (`cancel-in-progress` cancels the run). Durable fix: mirror
  the flagship's `Regenerate manifest (manual)` workflow, adapted to regenerate `manifest.json` +
  `data/search_index.rds`. See flagship playbook §6.
- [2026-07-20] mara · confirmed · Keep the two physical channels separate: `tree_dbh` (bole cross-section at
  breast height) vs `shrub_sapling_basal` (stem-base cover) are different measurements. Carry channel +
  support on every value; `NA` is not "no woody stand"; never flatten into an unqualified cross-biome
  ranking. Standing structure is a slow sampled-plot state, not productivity or biomass.
