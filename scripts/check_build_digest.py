#!/usr/bin/env python3
"""Compute the expected ddl-build-digest from manifest.json.

The digest is sha256 over the sorted lines "<path>:<md5>\\n" from the
manifest "files" map, the same text R/build_receipt.R veg_build_digest()
hashes from the files on disk. The post-deploy workflow compares this value
with the <meta name="ddl-build-digest"> tag on the live page.

Usage:
  python3 scripts/check_build_digest.py [manifest.json]
      prints the expected digest
  python3 scripts/check_build_digest.py manifest.json --files LIST --r-digest HEX
      also requires the manifest file set to equal LIST (one path per line,
      for example from veg_app_files()) and the digest to equal HEX
"""
import argparse
import hashlib
import json
import re
import sys


def manifest_digest(path):
    with open(path, encoding="utf-8") as handle:
        files = json.load(handle)["files"]
    lines = []
    for name, info in files.items():
        checksum = info.get("checksum", "")
        if not re.fullmatch(r"[0-9a-f]{32}", checksum):
            raise SystemExit(f"manifest checksum for {name} is not an md5 hex digest")
        lines.append(f"{name}:{checksum}")
    text = "".join(line + "\n" for line in sorted(lines))
    return sorted(files), hashlib.sha256(text.encode("utf-8")).hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("manifest", nargs="?", default="manifest.json")
    parser.add_argument("--files", help="file holding the expected app file list")
    parser.add_argument("--r-digest", help="digest computed by R veg_build_digest()")
    args = parser.parse_args()

    names, digest = manifest_digest(args.manifest)
    ok = True
    if args.files:
        with open(args.files, encoding="utf-8") as handle:
            expected = sorted(line.strip() for line in handle if line.strip())
        if expected != names:
            ok = False
            missing = sorted(set(expected) - set(names))
            extra = sorted(set(names) - set(expected))
            print(f"file set differs: missing={missing} extra={extra}", file=sys.stderr)
    if args.r_digest and args.r_digest != digest:
        ok = False
        print(f"digest differs: manifest={digest} r={args.r_digest}", file=sys.stderr)
    print(digest)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
