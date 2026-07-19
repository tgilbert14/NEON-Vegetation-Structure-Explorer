# Vegetation Structure release checklist

Release status: **HOLD**. Check a box only with an attached exact receipt.

## Source and science

- [ ] Explicit official release and DOI selected (`RELEASE-2026` candidate).
- [ ] Exactly 42 raw site responses and per-file/aggregate SHA-256 ledger.
- [ ] Event × individual × temporary-stem identity preserved.
- [ ] Per-plot/per-year sampling opportunity and sampled areas preserved.
- [ ] No silent measurement-to-denominator mismatch; partial-match policy reviewed.
- [ ] `tree_dbh` and `shrub_sapling_basal` estimands, physical labels, and
      thresholds registered.
- [ ] Snapshot, growth, mortality, classification, support, and missingness fixtures pass.
- [ ] App, indexes, CSV/ZIP/PDF, and fixtures match exactly.
- [ ] Data Takeaways and Expert Review recomputed from the candidate.

## Build and exact bytes

- [ ] Pinned Ubuntu 22.04, R 4.5.2, dated Jammy snapshot, and one-thread Haswell runtime.
- [ ] Every tracked R file parses; app sources offline from 42 bundles.
- [ ] Browser/custom-handler/cover checks pass.
- [ ] Candidate builds twice from the same raw family with identical bytes.
- [ ] `data/site_index.rds` and `data/search_index.rds` deterministic and exact;
      search contains the canonical 42-site index plus the exact 84-row site ×
      physical-channel grid.
- [ ] `manifest.json` regenerated twice, exact, complete, and free of blank/relative repositories.
- [ ] Owner applied `build-vegetation-candidate` to the exact same-repository PR
      head; artifact identity and digest are attached to that head.
- [ ] No source-family or manifest file was hand-edited.

## Cover and interaction

- [ ] Living Poster hook/promise/CTA remain brief and legible.
- [ ] Generated illustration disclosure, alt text, source, and checksum verified.
- [ ] Separate 1200×630 social composition is nonblank and metadata-complete.
- [ ] Desktop plus 390/375/361/360/320 layouts have no overflow.
- [ ] Keyboard/focus order, 44×44 targets, reduced motion, and loading-dialog focus pass.
- [ ] Place, Change, Plant, More, search, compare, export, and tour paths work.
- [ ] Dual-channel sites switch cleanly; species and threshold search open the
      selected channel; supported-zero channels do not offer invalid plant picks.
- [ ] Every unavailable state explains its actual reason; WOOD is not labelled treeless.

## Export and deployment

- [ ] Whole-site ZIP, plot/plant CSV, QC export/card, pinned-chart image, and PDF inspected.
- [ ] Codebook covers every exported column and structural `NA` meaning.
- [ ] Exact promoted commit merged after green head checks and review.
- [ ] Connect reports the exact deployed commit and manifest.
- [ ] Public app reaches semantic app-ready and site-ready state.
- [ ] Pages serves exact cover/social receipts and canonical metadata.
- [ ] No console/Shiny output errors on desktop and all compact widths.
- [ ] Rollback target and failed-attempt evidence recorded.

## Suite closeout

- [ ] `BUILD-TEST-HANDOFF.md` finalized with commands, runs, hashes, failures, and residual risk.
- [ ] Source, science, art, Driver, suite-learning, Data Takeaways, and Expert Review agree.
- [ ] Driver evidence register/backlog/revamp plan/playbook updated.
- [ ] Driver disposition recorded; bytes changed only if separately authorized and rebuilt.
