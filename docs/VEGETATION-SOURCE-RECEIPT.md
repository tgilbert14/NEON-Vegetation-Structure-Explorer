# Vegetation source receipt

Status: **legacy-partial source receipt; scientific and current-source promotion HOLD**.

This receipt applies to the exact 42-site `DP1.10098.001` bundle family first
introduced together in repository commit
`6b758f993acb09b9a90425391213b26e2320d0ca` on 2026-06-19. That is a repository
receipt date. It is not the upstream fetch date, bundle build date, query cutoff,
or official NEON release.

## What is known

- Product: NEON Vegetation structure (`DP1.10098.001`).
- Inventory: exactly 42 `data/sites/<SITE>.rds` bundles.
- `sourceBundleCommit`: `6b758f993acb09b9a90425391213b26e2320d0ca`.
- `repositoryImportedAt`: `2026-06-19`.
- Frozen bundled family SHA-256: `b00197f2069c7f537a2e7736e33a3786853151cf55e7918eb910efcc2a7a670c`.
- The family hash uses files ordered by basename over exact inventory lines
  `<sha256> <basename>\n`.
- Current bundle observations span recorded years 2014–2024. That observation
  range is not an upstream query cutoff or freshness guarantee.

The hash identifies the exact 42 bundle bytes. `data/site_index.rds` and
`data/search_index.rds` are derived artifacts with separate checksums and
cross-index gates.

## What was not preserved

| Field | Legacy value | Meaning |
|---|---:|---|
| actual bundle build date | `NA` | Not recoverable from repository history or mtimes. |
| official NEON release | `NA` | No release tag was recorded as explicitly selected. |
| query start/cutoff | `NA` | The original query window was not preserved as a receipt. |
| immutable query/snapshot ID | `NA` | No upstream query identifier was preserved. |
| original raw family digest | `NA` | Raw source responses were not retained or checksummed. |
| fetch package/runtime | `NA` | The exact `neonUtilities` fetch version was not retained. |

Repository dates, filesystem mtimes, manifest checksums, and runtime hashes may
not fill these upstream fields.

## Known structural limitation

The legacy bundler collapsed measurement identity to a lossy `individualID ×
date` representation and did not preserve published source `uid`, event or
temporary-stem fields, the plot-scoped stem-event locator, or complete
per-plot/per-year sampling-opportunity source rows. That prevents a release-grade
proof that every stem measurement is joined to its correct sampled-area
denominator.

The consequence is visible at WOOD: the current bundle contains qualifying live
woody records, but their 14 unique measurement `plotID` values match none of the
36 plot-denominator `plotID` values. A `NULL` stand result there is therefore an
**unmatched-denominator condition**, not evidence of a treeless or shrubless
site. Current-source and Driver promotion remain held.

## Contract for the next source family

The next candidate targets the explicit immutable official release
`RELEASE-2026`, DOI <https://doi.org/10.48443/pypa-qf12>, with all 42 registered
sites. It must preserve:

1. the official release tag and product DOI;
2. actual candidate build date and builder commit as separate repository fields;
3. exact raw per-file and aggregate SHA-256 ledgers;
4. `neonUtilities` version and query subset (or explicit full-release selection);
5. every published source `uid`, plus the plot-scoped event × individual ×
   temporary-stem locator and its conflict audit;
6. every per-plot/per-year opportunity source row, plus protocol, plot type,
   sampled-opportunity/area, and conflict fields;
7. one identical receipt across every site bundle, both indexes, and durable
   source ledgers;
8. strict 42-site accounting and hard failure on every missing table/site/key.

`skip_download=true` is reserved for revalidating an already-promoted v2 family;
it deliberately rejects these legacy bundles. It preserves the promoted receipt
and must never stamp a workflow date, invent an official release, or reuse
partial inputs as a new source family.
