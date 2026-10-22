<!-- neonize-playbook-stub: v1 -->
# NEONize playbook: pointer for Vegetation Structure Explorer

The NEON suite playbook has one home, read by Claude Code and Codex alike:
https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md
(side-by-side clone: `../NEON-Driver-Cascade/docs/neonize-playbook.md`). Read it before
suite work. Fold every suite rule, pattern or gotcha into that file, never into this one.
Anything here beyond this pointer and the app facts below is a stale fork: report it.

## This app

- Repo `NEON-Vegetation-Structure-Explorer` · watched branch `main` · product `DP1.10098.001`
- Rung specialist: `cedar`
- Refresh: `refresh-data.yml` (monthly, day 3) runs read-only jobs and uploads a review artifact; it writes nothing. The manual `regenerate-manifest.yml` pushes regenerated derived bytes to the review branch it was dispatched on and refuses `main`. A person's merge to `main` is the deploy.
- Local authority files: `docs/SCIENCE-CONTRACT.md` (identity, opportunity, channels, support states), `docs/VEGETATION-SOURCE-RECEIPT.md` (official-release source receipt), `docs/BUILD-TEST-HANDOFF.md` (commands, evidence, session log), `DEPLOY.md` and `docs/RELEASE-CHECKLIST.md` (candidate and release gates), `docs/EXPERT-REVIEW.md` (product-expert review), `docs/DRIVER-KNOWLEDGE-PACKAGE.md` (Driver disposition). The site-picker contract that `R/map_picker.R` points to is canonical §2g.

## App-specific rules

- None beyond the local authority files above.

## Owner-settled house rules (both tools)

- Default demo, startup and picker pre-selection site: **SRER** (then JORN, SJER, MOAB, ONAQ) unless this app documents a reason.
- Copy voice on covers, share cards and in-app copy: plain words a non-expert follows; say what NEON is; no em dashes; no sales pitch (price, access, ease); no hype adjectives; no sincerity labels ("honest", "real data"); never state what the reader can already see.
- No visible "illustration" badge on poster art; the art's status lives in alt text and the image provenance record.

Cold boot: also grep the fleet's `.claude/agents/LESSONS.md` in TG-Data-Apps (Codex: the
`dev-teams` mirror) for your role name: `grep -E '^- \[[0-9-]*\] [a-z0-9+-]*<role>[ +]'`.
