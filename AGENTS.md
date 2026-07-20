# Repository operating instructions

These instructions apply to the entire repository. User and platform instructions
take precedence.

## Mandatory entry point

Before inspecting, changing, testing, rebuilding, publishing, or reporting on this
repository, read `docs/BUILD-TEST-HANDOFF.md`, `docs/SCIENCE-CONTRACT.md`,
`docs/VEGETATION-SOURCE-RECEIPT.md`, and
`docs/DRIVER-KNOWLEDGE-PACKAGE.md` completely. For suite work, also read the
Driver repository's complete `docs/NEON-SUITE-LEARNING-LOOP.md`,
`docs/NEON-SUITE-REVAMP-PLAN.md`, and `docs/neonize-playbook.md`.

Start and end every session with `git status --short --branch`. Preserve changes
you did not create. Record the source branch and commit, watched publication
branch, public Pages URL, Connect content and URL, source/manifest/index/art
receipts, and test state before changing release bytes.

## Scientific release boundary

- Product: NEON Vegetation structure `DP1.10098.001`.
- Release status is **HOLD** until an official-release rebuild preserves the
  event × individual × temporary-stem identity and the per-plot/per-year sampling
  opportunity needed to attach every measurement to the correct denominator.
- The current 42-site legacy family is useful for product recovery and
  diagnostics, but its upstream release/query receipt is incomplete and its
  lossy canonicalization is not release-certified.
- `tree_dbh` bole cross-section (DBH at breast height) and
  `shrub_sapling_basal` stem-base cover are different physical measurements.
  Keep the channel and
  support fields on every value; never flatten them into an unqualified
  cross-biome ranking.
- Standing structure is a slow sampled-plot state. It is not annual productivity,
  biomass, carbon, wall-to-wall inventory, or a causal climate response.
- `NULL`/`NA` may represent no qualifying stems, one census, missing or invalid
  sampled area, or an unmatched measurement/denominator key. Never translate
  all unavailable states into “no woody stand.”
- Any formula, key, support, threshold, classification, or missingness change is
  a scientific-contract change. Update fixtures, exports, UI copy, source receipt,
  Driver package, and Driver evidence before promotion.

## Build, refresh, and release rules

1. Runtime must boot from committed bundles with no startup network dependency.
2. A refresh selects an explicit official NEON release (currently
   `RELEASE-2026`), fetches all 42 registered sites into empty staging, records
   raw checksums and the release DOI, builds twice, and compares exact bytes.
3. Never delete a valid committed bundle before its complete isolated replacement
   passes. A missing site, table, support field, receipt, or denominator join is a
   hard failure—not a reduced-success threshold.
4. Scheduled and manual refreshes upload read-only candidate artifacts; they do
   not create branches, open PRs, or push data. The release route is the
   owner-only candidate label on an exact reviewed PR head. `skip_download`
   preserves the existing source receipt.
5. Generate `data/search_index.rds` and `manifest.json` only in the pinned Linux
   validator. Do not hand-edit either. Promote the exact validator artifact.
6. Keep R 4.5.2, the dated Jammy repository, geographic package closure,
   OpenBLAS core/thread settings, and workflow actions pinned. A complete HTTPS
   repository is required for every manifest package, including archived `wk`.
7. Every Shiny custom-message handler accepts exactly one payload argument.
   Essential browser dependencies are vendored; optional map tiles are not app
   health.
8. Release requires green checks on the exact head and merge, exact manifest and
   source receipts, a matching Connect-deployed commit, semantic app/site
   readiness, export inspection, and desktop plus 390/375/361/360/320 public QA.
   HTTP 200 alone is not health.
9. Cover and social art are separate tested surfaces. Keep art local, responsive,
   accessible, provenance-aware, openly illustrative, and compose the social
   card at exactly 1200×630.

## Durable closeout and suite learning

Immediately before editing a durable record, re-read its latest entry. Update
`docs/BUILD-TEST-HANDOFF.md` with timestamp/time zone, scope, exact commands and
environment, expected and actual results, hashes and release identities, failed
attempts and cleanup, residual risks, and the next concrete action. Update the
source, science, Driver, Data Takeaways, Expert Review, art, and suite handoff
documents whenever their facts change.

Every completed pass must update the Driver repository's evidence register,
implication backlog, revamp plan, and reusable playbook. Classify results as
app-local, suite-platform, scientific-contract, and/or Driver-impacting, and
record an explicit `ADOPT`, `HOLD`, `CONTEXT`, `COMPLEMENT`, `REJECT`, or `NONE`
disposition. Do not modify Driver artifact bytes until the evidence and decision
authorize it.
