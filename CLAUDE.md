# NEON Vegetation Structure Explorer — working context for Claude

> Read this first. It orients an agent that boots cold in this repo. Depth lives in the docs it points at.
> This is a **Desert Data Labs (DDL)** project; the DDL business context + the full agent suite live in the
> `TG-Data-Apps` repo (and in user scope, so every agent is available here too).
>
> **Worked by more than one agent (Claude Code + ChatGPT/Codex).** The tool-neutral source of truth is
> `docs/neonize-playbook.md` (the flagship `NEON-Small-Mammal-Tracker-App` copy is the reference for
> §6–9), `docs/BUILD-TEST-HANDOFF.md`, `docs/SCIENCE-CONTRACT.md`, and `docs/VEGETATION-SOURCE-RECEIPT.md`
> — read those first; `CLAUDE.md` / `AGENTS.md` only add tool-specific notes on top. Close every session
> with a dated `BUILD-TEST-HANDOFF.md` entry tagged with your tool (`[Claude]` / `[Codex]`) and a one-line
> next action, so the other agent can pick up cold.
>
> **Before you code: plan, question, and challenge the work** — and always name at least one improvement
> you spot. The how-we-work principles are the flagship playbook §9.

## What this is

A **Shiny web app** exploring NEON's **Vegetation Structure** data product (**DP1.10098.001**) — every
tagged tree's diameter, height & growth career, stand size-structure & basal area, and a tap-to-pin
**Forest Size Lab**. Old-Growth Canopy theme. Part of the DDL **NEON explorer suite**.

## The stack + how it deploys (load-bearing facts)

- **Default branch: `main`** (watched by Posit Connect Cloud — a reviewed merge to `main` is the deploy).
  Branch defaults are split across the suite: Small Mammal + Vegetation are **`main`**, **Driver-Cascade
  is `master`** — never assume; check per repo.
- **Deploy = reviewed merge to watched `main`.** Refresh automation uploads read-only candidate artifacts;
  it never pushes production. See `AGENTS.md` → "Build, refresh, and release rules".
- **The terra/GDAL landmine.** Connect compiles native packages from source on jammy (GDAL 3.4.1); pin
  **terra 1.8-50** + the eight-package geospatial closure as real installed sources. The manifest writer
  records the actual installed versions; never hand-edit Version/RemoteSha.
- **The manifest/derived-bytes merge loop.** CI regenerates `manifest.json` **and** `data/search_index.rds`
  (twice, byte-identical) and fails if the committed copies differ. With no local R this forces a
  push → CI-red-by-design → promote-the-validated-artifact → re-run loop. Escape: promote the VALIDATED
  `vegetation-structure-derived-*` artifact (never an unvalidated one), push once, don't rapid re-push into
  `cancel-in-progress`. Durable fix: the flagship's byte-determinism recipe (playbook §6) + a
  `Regenerate manifest (manual)` workflow adapted to regenerate both derived files.

## Release boundary (the science is on HOLD — from AGENTS.md)

- Release status is **HOLD** until an official-release rebuild preserves event × individual ×
  temporary-stem identity and the per-plot/per-year sampling opportunity. The 42-site legacy family is for
  recovery/diagnostics; its upstream release/query receipt is incomplete.
- `tree_dbh` (bole cross-section at breast height) and `shrub_sapling_basal` (stem-base cover) are
  **different physical measurements** — keep the channel + support on every value; never flatten them into
  an unqualified cross-biome ranking.
- Standing structure is a slow sampled-plot state — not annual productivity, biomass, carbon, or a causal
  climate response. `NA` can mean no qualifying stems, one census, missing sampled area, or an unmatched
  key — never translate all unavailable states into "no woody stand."

## Which agents own what here

- **`neonize`** — suite methodology and the gold-standard build. **`connor`** — Connect Cloud deploy, the
  terra pin, manifest correctness. **`vgs` (R/Shiny mode)** — a full team review of the app. **`hk`** and
  its stats team — the statistics. **`cass`** — the cross-product Driver-Cascade synthesis. Call them by
  name; they're installed in user scope.

## The learning loop

- **`.claude/agents/LESSONS.md`** — project-local, one-line lessons; read on cold boot, append after a
  durable run. The canonical cross-cutting log lives in `TG-Data-Apps`; `curator` promotes recurring
  lessons up (and turns proven patterns into skills — playbook §9).
- **`docs/neonize-playbook.md`** — methodology (the flagship copy is the reference for §6–9).

## Working notes

- **Default the demo/example to an accessible forest site** (e.g. SCBI or HARV) — the clearest structure story.
- **Honesty discipline:** the caveat goes ON the number; recompute any headline straight from the `.rds`
  before trusting it; a QC flag that fires 0 times is guilty until proven innocent.
