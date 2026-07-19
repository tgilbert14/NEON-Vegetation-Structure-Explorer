#!/usr/bin/env bash
set -euo pipefail

site_dir=${VST_SITE_DIR:-data/sites}
receipt=${VST_SOURCE_RECEIPT:-docs/VEGETATION-SOURCE-RECEIPT.md}
expected_count=42

if [[ ! -d "$site_dir" || ! -f "$receipt" ]]; then
  echo "missing site directory or source receipt" >&2
  exit 1
fi

files=()
while IFS= read -r path; do
  files+=("$path")
done < <(find "$site_dir" -maxdepth 1 -type f -name '*.rds' -print | LC_ALL=C sort)
if [[ ${#files[@]} -ne $expected_count ]]; then
  echo "expected $expected_count site bundles, found ${#files[@]}" >&2
  exit 1
fi

inventory=$(mktemp)
trap 'rm -f "$inventory"' EXIT
for path in "${files[@]}"; do
  printf '%s %s\n' "$(sha256sum "$path" | awk '{print $1}')" "$(basename "$path")" >> "$inventory"
done
actual=$(sha256sum "$inventory" | awk '{print $1}')
expected=$(sed -nE 's/^- (Frozen bundled family|Bundled 42-site family) SHA-256: `([0-9a-f]{64})`\.$/\2/p' "$receipt" | head -n 1)
if [[ ! "$expected" =~ ^[0-9a-f]{64}$ ]]; then
  echo "source receipt does not contain a recognized bundled-family SHA-256" >&2
  exit 1
fi
if [[ "$actual" != "$expected" ]]; then
  echo "bundled family differs from source receipt: expected=$expected actual=$actual" >&2
  exit 1
fi
echo "source family OK: $actual (${#files[@]} sites)"
