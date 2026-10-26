# Vegetation source receipt

Status: **official-release candidate; scientific and human review required before promotion**.

This receipt describes one complete 42-site candidate for NEON Vegetation structure `DP1.10098.001`, explicitly selected from the immutable `RELEASE-2026` data release. Release identity and DOI describe upstream provenance; candidate build date and repository commit remain separate fields.

## Candidate identity

- Receipt schema: `1`.
- Provenance class: `official-release`.
- Product: `DP1.10098.001`.
- Official NEON release: `RELEASE-2026`.
- Release DOI: `https://doi.org/10.48443/pypa-qf12`.
- Query window: `FULL_RELEASE` through `FULL_RELEASE` (`FULL_RELEASE` means no month subset was applied).
- Actual candidate bundle build date: `2026-10-26`.
- Builder commit: `99fd0a29b18a4db9e2908257e66e9a0fc374a919`.
- `neonUtilities` fetch version: `4.0.2`.
- Source normalization: `portable-vectors+published-uid-byte-order-v1` (portable vectors, then published-`uid` byte order).
- Immutable release-snapshot label: `VST-DP1.10098.001-RELEASE-2026-sha256-e8d78dd776fa4188c3f237548b7d2ab185eb5c03bc7b220991d03753ebca3e29`.
- Raw source family SHA-256: `e8d78dd776fa4188c3f237548b7d2ab185eb5c03bc7b220991d03753ebca3e29`.
- Bundled 42-site family SHA-256: `6e538d472b43e949911d5775a665f4fc1993dfd82a2f06f0da3db83167462fa1`.
- Deterministic 42-site × two-channel data-quality audit SHA-256: `098aee5b26f30bbdca2f907fd084f9c854afb48c1f97e294950302f6310cf30b`.
- Refresh workflow evidence: `https://github.com/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/40360523712`.
- Reviewed RELEASE-2026 linkage gap: `49` measurement-only plot-event contexts containing `4,365` preserved measurement rows across `11` sites.

Both family hashes use basename-ordered inventory lines in the exact form `<sha256> <basename>\n`. The raw and bundled per-file ledgers, aggregate hashes, fetch runtime, deterministic site × channel data-quality audit, and its checksum are preserved under `data/source/`. The raw response artifact is retained with the workflow run; the ledgers remain durable in the repository.

## Promotion contract

Promotion requires all 42 bundles, `data/site_index.rds`, `data/search_index.rds`, the source ledgers, science contract, user-facing claims, Driver package, suite handoff, and manifest to agree on this candidate. Bundles must preserve every published source `uid`, audit the plot-scoped event × individual × temporary-stem locator without choosing a winner, and preserve every plot-opportunity source row needed to review the denominator. Measurement-only contexts remain visible but carry no invented effort, absence, sampled area, or opportunity metadata; both analytical channels must hold them as `held_opportunity_source_missing`. Any missing site, mixed receipt, unmatched source digest, changed 49/4,365/11 linkage inventory, unreviewed identity or denominator condition, dropped support field, or stale empirical claim blocks promotion.

`skip_download=true` accepts only an already-promoted v3 family and revalidates its committed inputs. It must not change this receipt, stamp a new build date, invent a NEON release, or treat a repository/manifest time as upstream vintage.
