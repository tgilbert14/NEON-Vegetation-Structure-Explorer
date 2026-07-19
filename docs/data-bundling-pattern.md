# Reviewed bundle release pattern

This app uses committed `.rds` files as an immutable, read-only runtime store.
That makes the public app fast and independent of the NEON API, but only when the
complete bundle family is built and promoted as one reviewed release unit.

## Runtime boundary

- The Shiny process reads committed bundles only (`VST_LIVE=0`).
- A missing, corrupt, legacy, or contract-incompatible bundle fails closed. The
  app must show an unavailable/HOLD state; it must not fetch live data as a
  fallback.
- `data/site_index.rds`, `data/search_index.rds`, and `manifest.json` are derived
  release bytes. The search index contains both the canonical 42-site default
  view and an exact 84-row site × physical-channel grid. They are generated in
  the pinned Linux validator and never hand-edited.
- Raw downloads and tokens never ship with the app. `NEON_TOKEN` exists only in
  the read-only fetch job.

The absence of a runtime network request is an availability feature and a
scientific guarantee: one user cannot silently see a different source vintage
from another user.

## Official-release refresh

The release workflow selects one explicit official NEON release and then:

1. creates an empty raw staging directory;
2. fetches all 42 registered sites and required tables;
3. records the official release, DOI, query bounds, fetch runtime, and per-file
   plus aggregate SHA-256 receipts;
4. fails the whole build when any expected site, table, identity key,
   opportunity field, or source receipt is missing;
5. creates two isolated candidates from the same source revision and raw family;
6. rebuilds site bundles, demo data, both indexes, source ledgers, and manifest;
7. runs schema, science, source-family, manifest, and offline-boot checks; and
8. requires byte-identical candidate inventories before uploading an artifact.

There is no release-grade "skip what already exists," reduced-success threshold,
or per-site `tryCatch` that converts a failed fetch into a smaller family. Those
techniques can be useful for disposable exploration, but not for a promoted
scientific source family.

## Candidate identity and review

The first reviewed candidate is built from an existing same-repository pull
request. The repository owner applies the `build-vegetation-candidate` label to
that exact PR head. A successful refresh run uploads:

`vegetation-release-candidate-<head_sha>-<run_id>`

Scheduled and manual runs are read-only diagnostics that upload the same form of
artifact. They do not create a branch, open a pull request, modify the invoking
branch, or publish data.

Before promotion, download the exact artifact and verify
`CANDIDATE-SHA256SUMS.txt`. Promote every path named by that ledger and no
unlisted artifact-root file onto the same reviewed PR branch. Re-run ordinary CI on that exact head,
inspect the source/science/empirical receipts, and merge only after every gate is
green. Never push generated data directly to `main`.

## Safe replacement

Candidate construction may clear files only inside its isolated runner staging
directory. It must never delete or overwrite the committed valid family in
place. The old public family remains recoverable until the complete replacement
has passed review and merge.

`skip_download=true` is intentionally narrow: it revalidates a previously
promoted v2 family while preserving that family's exact source receipt. It
rejects the legacy family and must not stamp a new vintage, release, DOI, or
source digest.

## Publication receipt

A merge is publication intent, not proof. Release closes only when the same
reviewed bytes have:

- green exact-head and merge checks;
- matching committed candidate checksums and manifest;
- a matching Connect-deployed commit and healthy bundle-only app;
- a matching GitHub Pages Living Poster;
- inspected app exports and held/zero behavior; and
- desktop plus required narrow-width public QA.

Record those identities in `docs/BUILD-TEST-HANDOFF.md`. Do not describe the
candidate as current or validated until the empirical run and public receipts
actually exist.
