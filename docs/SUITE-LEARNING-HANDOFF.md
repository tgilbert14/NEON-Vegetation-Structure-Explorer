# Suite learning handoff

App: Vegetation Structure · Pass 4 release handoff · 2026-07-19

Status: **candidate and science gates verified; public deployment pending;
Driver HOLD / CONTEXT ONLY / NO DRIVER BYTE CHANGE**. This package records
reusable learning without claiming production before live proof exists.

## Product and cover lessons

1. A companion cover works best as a Living Poster: one app-native field action,
   a three-beat hook, one plain promise, and one invitation. Detailed method and
   caveats stay below the first screen.
2. Generated art must look openly editorial, not like documentary evidence. Put
   the disclosure beside the image and keep facts in live HTML.
3. Desktop hero art and the 1200×630 social card are different compositions and
   need separate receipts.
4. Non-scientist navigation should begin with questions: what stands here, how
   did this plant change, and how does this place compare?
5. Driver Cascade is the suite ambassador. Companion covers should state their
   own lens and point toward Driver without turning the page into a directory.

## Science and data lessons

1. Preserve the official observation key before making a “latest record.” For
   Vegetation Structure that includes event, individual, and temporary-stem
   identity—not only individual and date.
2. Preserve sampling opportunity before scaling a tally. A plot ID in a stem
   table does not prove the correct sampled-area denominator.
3. Never collapse all `NULL` states into ecological absence. Distinguish no
   qualifying records, one census, unmatched keys, invalid area, and genuine
   supported zero/empty states.
4. One census blocks change, not a supported current snapshot.
5. `tree_dbh` bole cross-section and `shrub_sapling_basal` stem-base cover need
   separate physical channel IDs even when both are expressed as m²/ha.
6. Standing structure is a slow state; composition and phenology belong to their
   respective companion apps.
7. A single default map row must not erase a supported secondary measurement
   channel. Network discovery needs an explicit site × channel index, and a
   search hit must carry that channel into the opened app view.
8. Preserve measurements when their opportunity source is absent, but never
   manufacture effort, absence, area, or a denominator. Give the event a
   dedicated status and separate published-opportunity counts from
   measurement-only context counts. Vegetation RELEASE-2026 makes this concrete:
   49 keys / 4,365 rows / 11 sites.

## Release lessons

1. Select an explicit immutable NEON release and DOI. Current/latest API output is
   not a substitute for a release receipt.
2. Fetch all registered sites into empty staging, hard-fail every missing site or
   table, and preserve raw per-file hashes.
3. Build twice from the same raw family and compare exact bytes.
4. A monthly schedule should have one actual monthly trigger; a delayed runner
   must not miss a calendar gate.
5. Refresh automation uploads a read-only candidate artifact and never creates a
   branch, opens a PR, or pushes data. Release begins from an existing reviewed
   PR whose exact head receives the owner-only candidate label.
6. `skip_download` accepts only an already-promoted v2 family, revalidates those
   bytes, and never mints a new source vintage.
7. The manifest is an exact generated closure. Exact URL-installed geo packages
   retain `RemoteType=url` and the full `url::<tarball>` reference while using
   `Source=CRAN` plus `https://cran.r-project.org` as Connect's deployment lane;
   only their non-semantic `Built` clocks are removed. Versions and `RemoteSha`
   are never fabricated. Ordinary packages remain on the dated Jammy snapshot,
   and `wk` can never inherit a relative `CRAN/...` path.
8. Source-family changes must force human review of science, empirical claims,
   cover facts, Driver disposition, and release receipts.
9. API row order is not evidence. Materialize each raw table, validate unique
   published row IDs, sort by that identity, reset row names, and hash the
   normalized extraction under a pinned serialization runtime.

## Dispositions from this pass

| Class | Finding | Disposition |
|---|---|---|
| app-local | Living Poster and question-led entry | `ADOPT`; public parity proof pending |
| suite-platform | isolated official-release candidate PR workflow | `ADOPT`; exact-head run `29715249829` passed |
| scientific-contract | event/stem identity and opportunity ledger | `PASS` for the companion app; preserve in every consumer |
| scientific-contract | 49 measurement-only events / 4,365 rows at 11 sites | `HOLD` from scaling and derived summaries; preserve and expose |
| Driver-impacting | Vegetation channel/support evidence | `HOLD / CONTEXT ONLY / NO DRIVER BYTE CHANGE` |
| complementary | Vegetation as slow standing-structure lens | `COMPLEMENT` Plant Diversity and Phenology |

## Pattern for the next app

- Read governance and source receipts first.
- Preserve observation opportunity and identity before feature work.
- When source opportunity is missing, preserve the observation and register the
  gap; do not turn a hard build stop into a hidden deletion or synthetic zero.
- Keep the poster face brief and artistic; place truth, provenance, and suite role
  below the fold.
- Vendor essential runtime dependencies, generate deterministic release bytes,
  and prove semantic public health across the suite breakpoints.
- End with an app-local handoff plus central Driver evidence/disposition update.

## Driver action after this app closes

Record the unmatched-denominator lesson in the central evidence register and
playbook now, but keep Driver data bytes unchanged. Revisit the vegetation rung
only after the exact official-release app artifact is promoted and the Driver is
rebuilt from that source with explicit parity receipts.
