#!/usr/bin/env python3
"""Fail-closed, read-only audit of a Vegetation release-candidate artifact.

This intentionally uses only the Python 3.9+ standard library.  It validates
the promotion allowlist and its human-readable CSV/JSON/Markdown receipts; it
does not deserialize RDS files or attempt to replace the repository's pinned R
validator.

Usage:
    python3 work/inspect_vegetation_candidate.py /path/to/extracted/candidate

On success, concise empirical summaries are followed by one line beginning
``SUMMARY_JSON=``.  On failure the process exits non-zero and emits a failure
summary in the same machine-readable form.
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import io
import json
import os
from pathlib import Path, PurePosixPath
import re
import stat
import sys
from typing import Any, Dict, Iterable, List, Mapping, MutableMapping, Sequence, Tuple
from urllib.parse import urlsplit


SITES: Tuple[str, ...] = (
    "ABBY", "BART", "BLAN", "BONA", "CLBJ", "CPER", "DCFS", "DEJU",
    "DELA", "DSNY", "GRSM", "GUAN", "HARV", "HEAL", "JERC", "JORN",
    "KONZ", "LAJA", "LENO", "MLBS", "MOAB", "NIWO", "NOGP", "ONAQ",
    "ORNL", "OSBS", "PUUM", "RMNP", "SCBI", "SERC", "SJER", "SOAP",
    "SRER", "STEI", "TALL", "TEAK", "TREE", "UKFS", "UNDE", "WOOD",
    "WREF", "YELL",
)
CHANNELS: Tuple[str, ...] = ("tree_dbh", "shrub_sapling_basal")

PRODUCT = "DP1.10098.001"
RELEASE = "RELEASE-2026"
RELEASE_DOI = "https://doi.org/10.48443/pypa-qf12"
CONTRACT_ID = "NEON-VST-DP1.10098.001-v2"
AUDIT_SCHEMA = "NEON-VST-data-quality-audit-v2"
SOURCE_NORMALIZATION = "portable-vectors+published-uid-byte-order-v1"
PINNED_REPOSITORY = (
    "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
)
CRAN_REPOSITORY = "https://cran.r-project.org"
EXPECTED_MEASUREMENT_ONLY_CONTEXTS = 49
EXPECTED_MEASUREMENT_RECORDS_WITHOUT_OPPORTUNITY_SOURCE = 4_365
EXPECTED_SITES_WITH_MEASUREMENT_ONLY_CONTEXTS = 11

SOURCE_LEDGER_PATHS: Tuple[str, ...] = (
    "data/source/vegetation-raw-SHA256SUMS.txt",
    "data/source/vegetation-raw-family-SHA256.txt",
    "data/source/vegetation-fetch-runtime.txt",
    "data/source/vegetation-bundle-SHA256SUMS.txt",
    "data/source/vegetation-bundle-family-SHA256.txt",
    "data/source/vegetation-data-quality-audit.csv",
    "data/source/vegetation-data-quality-audit-SHA256.txt",
)

SITE_PATHS: Tuple[str, ...] = tuple(f"data/sites/{site}.rds" for site in SITES)
CANDIDATE_PAYLOAD_PATHS: Tuple[str, ...] = tuple(sorted(
    SITE_PATHS
    + (
        "data/site_index.rds",
        "data/search_index.rds",
        "data-sample/demo.rds",
        "manifest.json",
        "docs/VEGETATION-SOURCE-RECEIPT.md",
    )
    + SOURCE_LEDGER_PATHS
))
CANDIDATE_LEDGER = "CANDIDATE-SHA256SUMS.txt"
EXPECTED_CANDIDATE_FILES = frozenset(CANDIDATE_PAYLOAD_PATHS + (CANDIDATE_LEDGER,))
EXPECTED_DIRECTORIES = frozenset((
    "data", "data/sites", "data/source", "data-sample", "docs",
))

WWW_RUNTIME_FILES: Tuple[str, ...] = (
    "www/app.js",
    "www/assets/vegetation-living-poster-840.webp",
    "www/assets/vegetation-living-poster.png",
    "www/assets/vegetation-living-poster.webp",
    "www/pincards.js",
    "www/styles.css",
    "www/veg.css",
    "www/vendor/THIRD_PARTY_NOTICES.md",
    "www/vendor/driver/LICENSE",
    "www/vendor/driver/driver.css",
    "www/vendor/driver/driver.js.iife.js",
    "www/vendor/html-to-image/LICENSE",
    "www/vendor/html-to-image/html-to-image.js",
    "www/vendor/sweetalert2/LICENSE",
    "www/vendor/sweetalert2/sweetalert2.all.min.js",
    "www/vendor/sweetalert2/sweetalert2.min.css",
)
EXPECTED_RUNTIME_FILES = frozenset(
    (
        "global.R", "ui.R", "server.R",
        "R/map_picker.R", "R/report_pdf.R", "R/site_metadata.R",
        "R/veg_helpers.R",
        "data/site_index.rds", "data/search_index.rds",
        "data-sample/demo.rds",
    )
    + WWW_RUNTIME_FILES
    + SITE_PATHS
)

REQUIRED_RUNTIME_PACKAGES = frozenset((
    "shiny", "bslib", "bsicons", "dplyr", "tidyr", "stringr", "tibble",
    "plotly", "leaflet", "DT", "shinyjs", "shinycssloaders",
    "RColorBrewer", "htmltools", "digest", "jsonlite", "data.table",
))
FORBIDDEN_BUILD_PACKAGES = frozenset(("neonUtilities", "arrow", "rsconnect"))
PINNED_GEO_VERSIONS: Mapping[str, str] = {
    "terra": "1.8-50",
    "sf": "1.1-1",
    "s2": "1.1.11",
    "units": "1.0-1",
    "wk": "0.9.5",
    "classInt": "0.4-11",
    "raster": "3.6-32",
    "sp": "2.2-1",
}
PINNED_GEO_URLS: Mapping[str, str] = {
    "terra": "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-50.tar.gz",
    "sf": "https://cran.r-project.org/src/contrib/sf_1.1-1.tar.gz",
    "s2": "https://cran.r-project.org/src/contrib/s2_1.1.11.tar.gz",
    "units": "https://cran.r-project.org/src/contrib/units_1.0-1.tar.gz",
    "wk": "https://cran.r-project.org/src/contrib/wk_0.9.5.tar.gz",
    "classInt": "https://cran.r-project.org/src/contrib/classInt_0.4-11.tar.gz",
    "raster": "https://cran.r-project.org/src/contrib/raster_3.6-32.tar.gz",
    "sp": "https://cran.r-project.org/src/contrib/sp_2.2-1.tar.gz",
}

HELD_COUNT_FIELDS: Tuple[str, ...] = (
    "n_held_sampling_impractical",
    "n_held_dendrometer_only",
    "n_held_missing_area",
    "n_held_opportunity_unknown",
    "n_held_presence_record_conflict",
    "n_held_metric_invalid",
    "n_held_identity_conflict",
    "n_held_opportunity_source_missing",
)
HELD_STATUS_TO_FIELD: Mapping[str, str] = {
    field.removeprefix("n_"): field for field in HELD_COUNT_FIELDS
}
AUDIT_NUMERIC_FIELDS: Tuple[str, ...] = (
    "n_plot_event_contexts",
    "n_published_opportunity_keys",
    "n_opportunity_source_records",
    "n_measurement_only_contexts",
    "n_measurement_records_without_opportunity_source",
    "n_channel_measurement_only_contexts_with_records",
    "n_channel_measurement_records_without_opportunity_source",
    "n_opportunity_key_conflict_groups",
    "n_supported_contexts",
    "n_explicit_absences",
    "n_held_contexts",
    "n_sampled_with_records",
    "n_sampled_absence",
) + HELD_COUNT_FIELDS + (
    "n_measurement_records",
    "n_live_measurement_records",
    "n_invalid_metric_records",
    "n_protocol_identity_conflict_keys",
    "n_protocol_identity_conflict_records",
    "n_nonblank_dataqf_records",
    "n_nonblank_tag_status_records",
    "n_non_ok_tag_status_records",
    "n_nonblank_changed_measurement_location_records",
    "n_changed_measurement_location_records",
)
AUDIT_HEADER: Tuple[str, ...] = (
    "site",
    "channel",
    "audit_schema",
    "contract_id",
    "contract_version",
    "product",
    "source_release",
    "release_doi",
    "source_receipt_id",
    "raw_source_digest",
    "source_normalization",
    "n_plot_event_contexts",
    "n_published_opportunity_keys",
    "n_opportunity_source_records",
    "n_measurement_only_contexts",
    "n_measurement_records_without_opportunity_source",
    "n_channel_measurement_only_contexts_with_records",
    "n_channel_measurement_records_without_opportunity_source",
    "n_opportunity_key_conflict_groups",
    "n_supported_contexts",
    "n_explicit_absences",
    "n_held_contexts",
    "n_sampled_with_records",
    "n_sampled_absence",
    "n_held_sampling_impractical",
    "n_held_dendrometer_only",
    "n_held_missing_area",
    "n_held_opportunity_unknown",
    "n_held_presence_record_conflict",
    "n_held_metric_invalid",
    "n_held_identity_conflict",
    "n_held_opportunity_source_missing",
    "held_reason_counts",
    "n_measurement_records",
    "n_live_measurement_records",
    "n_invalid_metric_records",
    "n_protocol_identity_conflict_keys",
    "n_protocol_identity_conflict_records",
    "n_nonblank_dataqf_records",
    "dataqf_value_counts",
    "dataqf_handling",
    "n_nonblank_tag_status_records",
    "n_non_ok_tag_status_records",
    "non_ok_tag_status_value_counts",
    "n_nonblank_changed_measurement_location_records",
    "n_changed_measurement_location_records",
    "changed_measurement_location_value_counts",
)

SHA256_RE = re.compile(r"[0-9a-f]{64}")
MD5_RE = re.compile(r"[0-9a-f]{32}")
NONNEGATIVE_INTEGER_RE = re.compile(r"0|[1-9][0-9]*")
COMMIT_RE = re.compile(r"[0-9a-f]{40}")
MONTH_RE = re.compile(r"[0-9]{4}-(?:0[1-9]|1[0-2])")
PACKAGE_VERSION_RE = re.compile(r"[0-9]+(?:[.-][0-9]+)*")


class AuditError(RuntimeError):
    """Expected validation failure."""


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AuditError(message)


def display_set(values: Iterable[str]) -> str:
    ordered = sorted(values)
    return ",".join(ordered) if ordered else "none"


def validate_relative_name(name: str, label: str, *, basename_only: bool = False) -> str:
    require(isinstance(name, str) and bool(name), f"{label}: blank path")
    require("\\" not in name, f"{label}: backslash is forbidden: {name!r}")
    require("\x00" not in name, f"{label}: NUL is forbidden")
    require(not any(ord(char) < 32 for char in name),
            f"{label}: control character is forbidden: {name!r}")
    pure = PurePosixPath(name)
    require(not pure.is_absolute(), f"{label}: absolute path is forbidden: {name!r}")
    require(all(part not in ("", ".", "..") for part in pure.parts),
            f"{label}: path traversal or empty component: {name!r}")
    require(pure.as_posix() == name,
            f"{label}: path is not in canonical POSIX form: {name!r}")
    if basename_only:
        require(len(pure.parts) == 1, f"{label}: expected a basename: {name!r}")
    return name


def inventory_tree(root: Path) -> Tuple[frozenset[str], frozenset[str]]:
    """Inventory without following links; reject every non-file/non-directory."""
    try:
        root_lstat = root.lstat()
    except OSError as exc:
        raise AuditError(f"cannot inspect candidate root: {exc}") from exc
    require(not stat.S_ISLNK(root_lstat.st_mode), "candidate root must not be a symlink")
    require(stat.S_ISDIR(root_lstat.st_mode), "candidate root is not a directory")

    files: set[str] = set()
    directories: set[str] = set()
    stack: List[Tuple[Path, Tuple[str, ...]]] = [(root, ())]
    while stack:
        directory, rel_parts = stack.pop()
        try:
            entries = list(os.scandir(directory))
        except OSError as exc:
            rel = "/".join(rel_parts) or "."
            raise AuditError(f"cannot scan candidate directory {rel}: {exc}") from exc
        for entry in entries:
            require("\\" not in entry.name,
                    f"candidate tree contains a backslash name: {entry.name!r}")
            require(entry.name not in (".", ".."),
                    f"candidate tree contains traversal component: {entry.name!r}")
            require(not any(ord(char) < 32 for char in entry.name),
                    f"candidate tree contains a control-character name: {entry.name!r}")
            parts = rel_parts + (entry.name,)
            rel = "/".join(parts)
            validate_relative_name(rel, "candidate tree")
            try:
                mode = entry.stat(follow_symlinks=False).st_mode
            except OSError as exc:
                raise AuditError(f"cannot stat candidate entry {rel}: {exc}") from exc
            require(not stat.S_ISLNK(mode), f"candidate contains symlink: {rel}")
            if stat.S_ISDIR(mode):
                directories.add(rel)
                stack.append((Path(entry.path), parts))
            elif stat.S_ISREG(mode):
                files.add(rel)
            else:
                raise AuditError(f"candidate contains non-regular entry: {rel}")
    return frozenset(files), frozenset(directories)


def read_limited_bytes(path: Path, label: str, limit: int) -> bytes:
    try:
        size = path.stat().st_size
        require(size <= limit, f"{label} is unexpectedly large ({size} bytes)")
        return path.read_bytes()
    except AuditError:
        raise
    except OSError as exc:
        raise AuditError(f"cannot read {label}: {exc}") from exc


def decode_utf8(data: bytes, label: str) -> str:
    require(not data.startswith(b"\xef\xbb\xbf"), f"{label} must not contain a UTF-8 BOM")
    try:
        return data.decode("utf-8")
    except UnicodeDecodeError as exc:
        raise AuditError(f"{label} is not strict UTF-8: {exc}") from exc


class Hashes:
    def __init__(self, root: Path) -> None:
        self.root = root
        self.cache: Dict[Tuple[str, str], str] = {}

    def file(self, rel: str, algorithm: str = "sha256") -> str:
        key = (algorithm, rel)
        if key in self.cache:
            return self.cache[key]
        validate_relative_name(rel, "hash target")
        try:
            digest = hashlib.new(algorithm)
            with (self.root / rel).open("rb") as handle:
                while True:
                    block = handle.read(1024 * 1024)
                    if not block:
                        break
                    digest.update(block)
        except (OSError, ValueError) as exc:
            raise AuditError(f"cannot hash {rel} with {algorithm}: {exc}") from exc
        value = digest.hexdigest()
        self.cache[key] = value
        return value


def strict_lines(data: bytes, label: str) -> List[str]:
    require(data.endswith(b"\n"), f"{label} must end with one LF")
    require(b"\r" not in data, f"{label} contains CR; LF-only bytes required")
    text = decode_utf8(data, label)
    lines = text[:-1].split("\n")
    require(bool(lines) and all(lines), f"{label} contains a blank line")
    return lines


def parse_hash_ledger(
    path: Path,
    label: str,
    expected_names: Sequence[str],
    *,
    basename_only: bool = False,
) -> Tuple[Dict[str, str], bytes]:
    data = read_limited_bytes(path, label, 2 * 1024 * 1024)
    lines = strict_lines(data, label)
    parsed: Dict[str, str] = {}
    order: List[str] = []
    for index, line in enumerate(lines, 1):
        match = re.fullmatch(r"([0-9a-f]{64}) ([^\n\r]+)", line)
        require(match is not None,
                f"{label} line {index} is not strict '<sha256> <name>'")
        digest, name = match.groups()
        validate_relative_name(name, f"{label} line {index}", basename_only=basename_only)
        require(name not in parsed, f"{label} contains duplicate name: {name}")
        parsed[name] = digest
        order.append(name)
    expected = list(expected_names)
    require(order == expected,
            f"{label} name/order mismatch: missing=[{display_set(set(expected) - set(order))}] "
            f"extra=[{display_set(set(order) - set(expected))}]")
    return parsed, data


def parse_family_digest(path: Path, label: str) -> str:
    data = read_limited_bytes(path, label, 256)
    lines = strict_lines(data, label)
    require(len(lines) == 1 and SHA256_RE.fullmatch(lines[0]) is not None,
            f"{label} must contain exactly one lowercase SHA-256")
    return lines[0]


def duplicate_rejecting_object(pairs: Sequence[Tuple[str, Any]]) -> Dict[str, Any]:
    result: Dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise AuditError(f"JSON contains duplicate object key: {key}")
        result[key] = value
    return result


def reject_json_constant(value: str) -> Any:
    raise AuditError(f"JSON contains non-finite constant: {value}")


def parse_json_text(text: str, label: str) -> Any:
    try:
        return json.loads(
            text,
            object_pairs_hook=duplicate_rejecting_object,
            parse_constant=reject_json_constant,
        )
    except AuditError:
        raise
    except json.JSONDecodeError as exc:
        raise AuditError(f"{label} is invalid JSON: {exc}") from exc


def parse_fetch_runtime(path: Path) -> Dict[str, str]:
    label = "fetch runtime ledger"
    data = read_limited_bytes(path, label, 64 * 1024)
    lines = strict_lines(data, label)
    expected_order = [
        "product", "officialNeonRelease", "releaseDoi", "queryStart",
        "queryEnd", "neonUtilities", "sourceNormalization",
    ]
    values: Dict[str, str] = {}
    order: List[str] = []
    for index, line in enumerate(lines, 1):
        require("=" in line, f"{label} line {index} lacks '='")
        key, value = line.split("=", 1)
        require(re.fullmatch(r"[A-Za-z][A-Za-z0-9]*", key) is not None,
                f"{label} line {index} has invalid key")
        require(key not in values, f"{label} contains duplicate key: {key}")
        require(bool(value.strip()) and value == value.strip(),
                f"{label} key {key} is blank or padded")
        order.append(key)
        values[key] = value
    require(order == expected_order,
            f"{label} keys/order differ: expected={expected_order!r} actual={order!r}")
    require(values["product"] == PRODUCT, f"{label} product mismatch")
    require(values["officialNeonRelease"] == RELEASE, f"{label} release mismatch")
    require(values["releaseDoi"] == RELEASE_DOI, f"{label} DOI mismatch")
    validate_query_window(values["queryStart"], values["queryEnd"], label)
    version = values["neonUtilities"]
    require(PACKAGE_VERSION_RE.fullmatch(version) is not None,
            f"{label} has invalid neonUtilities version: {version!r}")
    numeric = tuple(int(item) for item in re.split(r"[.-]", version))
    padded = numeric + (0,) * max(0, 3 - len(numeric))
    require(padded[:3] >= (3, 0, 3),
            f"{label} neonUtilities version is below 3.0.3: {version}")
    require(values["sourceNormalization"] == SOURCE_NORMALIZATION,
            f"{label} source normalization mismatch")
    return values


def validate_query_window(start: str, end: str, label: str) -> None:
    if start == "FULL_RELEASE" or end == "FULL_RELEASE":
        require(start == end == "FULL_RELEASE",
                f"{label} query window mixes FULL_RELEASE and month subset")
        return
    require(MONTH_RE.fullmatch(start) is not None and MONTH_RE.fullmatch(end) is not None,
            f"{label} query window must be FULL_RELEASE/FULL_RELEASE or YYYY-MM months")
    require(end >= start, f"{label} query end precedes query start")


def one_receipt_value(text: str, pattern: str, label: str) -> str:
    matches = re.findall(pattern, text, flags=re.MULTILINE)
    require(len(matches) == 1, f"source receipt must contain exactly one {label}")
    value = matches[0]
    require(isinstance(value, str) and bool(value.strip()) and value == value.strip(),
            f"source receipt {label} is blank or padded")
    return value


def validate_https_url(value: str, label: str, *, github_run: bool = False) -> None:
    parsed = urlsplit(value)
    require(parsed.scheme == "https" and bool(parsed.hostname),
            f"{label} must be a complete HTTPS URL: {value!r}")
    require(parsed.username is None and parsed.password is None,
            f"{label} must not contain credentials")
    require(not parsed.fragment, f"{label} must not contain a fragment")
    if github_run:
        require(
            parsed.hostname == "github.com"
            and re.fullmatch(
                r"/tgilbert14/NEON-Vegetation-Structure-Explorer/actions/runs/[0-9]+",
                parsed.path,
            ) is not None
            and not parsed.query,
            f"refresh evidence is not the exact repository Actions run URL: {value!r}",
        )


def parse_receipt(path: Path, runtime: Mapping[str, str]) -> Dict[str, str]:
    data = read_limited_bytes(path, "source receipt", 2 * 1024 * 1024)
    require(b"\r" not in data, "source receipt must use LF-only text")
    text = decode_utf8(data, "source receipt")
    require(text.endswith("\n"), "source receipt must end with LF")
    require(
        "Status: **official-release candidate; scientific and human review required before promotion**."
        in text,
        "source receipt does not carry the official-release candidate status",
    )

    receipt: Dict[str, str] = {
        "schema_version": one_receipt_value(
            text, r"^- Receipt schema: `([^`]+)`\.$", "receipt schema"
        ),
        "provenance_class": one_receipt_value(
            text, r"^- Provenance class: `([^`]+)`\.$", "provenance class"
        ),
        "product": one_receipt_value(
            text, r"^- Product: `([^`]+)`\.$", "product"
        ),
        "release": one_receipt_value(
            text, r"^- Official NEON release: `([^`]+)`\.$", "official release"
        ),
        "release_doi": one_receipt_value(
            text, r"^- Release DOI: `([^`]+)`\.$", "release DOI"
        ),
        "built_at": one_receipt_value(
            text, r"^- Actual candidate bundle build date: `([^`]+)`\.$", "build date"
        ),
        "builder_commit": one_receipt_value(
            text, r"^- Builder commit: `([^`]+)`\.$", "builder commit"
        ),
        "neon_utilities_version": one_receipt_value(
            text, r"^- `neonUtilities` fetch version: `([^`]+)`\.$",
            "neonUtilities version",
        ),
        "source_normalization": one_receipt_value(
            text, r"^- Source normalization: `([^`]+)` ",
            "source normalization",
        ),
        "source_receipt_id": one_receipt_value(
            text, r"^- Immutable release-snapshot label: `([^`]+)`\.$",
            "source receipt ID",
        ),
        "raw_family_sha256": one_receipt_value(
            text, r"^- Raw source family SHA-256: `([0-9a-f]{64})`\.$",
            "raw family SHA-256",
        ),
        "bundle_family_sha256": one_receipt_value(
            text, r"^- Bundled 42-site family SHA-256: `([0-9a-f]{64})`\.$",
            "bundle family SHA-256",
        ),
        "audit_sha256": one_receipt_value(
            text,
            r"^- Deterministic 42-site × two-channel data-quality audit SHA-256: `([0-9a-f]{64})`\.$",
            "audit SHA-256",
        ),
        "refresh_run_url": one_receipt_value(
            text, r"^- Refresh workflow evidence: `([^`]+)`\.$", "refresh run URL"
        ),
    }
    query_matches = re.findall(
        r"^- Query window: `([^`]+)` through `([^`]+)` ", text, flags=re.MULTILINE
    )
    require(len(query_matches) == 1, "source receipt must contain exactly one query window")
    receipt["query_start"], receipt["query_end"] = query_matches[0]

    require(receipt["schema_version"] == "1", "source receipt schema must be 1")
    require(receipt["provenance_class"] == "official-release",
            "source receipt provenance class mismatch")
    require(receipt["product"] == PRODUCT, "source receipt product mismatch")
    require(receipt["release"] == RELEASE, "source receipt release mismatch")
    require(receipt["release_doi"] == RELEASE_DOI, "source receipt DOI mismatch")
    require(COMMIT_RE.fullmatch(receipt["builder_commit"]) is not None,
            "source receipt builder commit must be a lowercase 40-hex SHA")
    try:
        dt.date.fromisoformat(receipt["built_at"])
    except ValueError as exc:
        raise AuditError("source receipt build date is not an ISO calendar date") from exc
    validate_query_window(receipt["query_start"], receipt["query_end"], "source receipt")
    validate_https_url(receipt["release_doi"], "release DOI")
    validate_https_url(receipt["refresh_run_url"], "refresh run URL", github_run=True)
    require(receipt["neon_utilities_version"] == runtime["neonUtilities"],
            "source receipt neonUtilities version differs from fetch runtime")
    require(receipt["source_normalization"] == SOURCE_NORMALIZATION,
            "source receipt normalization mismatch")
    require(receipt["source_normalization"] == runtime["sourceNormalization"],
            "source receipt normalization differs from fetch runtime")
    require(receipt["query_start"] == runtime["queryStart"]
            and receipt["query_end"] == runtime["queryEnd"],
            "source receipt query window differs from fetch runtime")
    expected_id = f"VST-{PRODUCT}-{RELEASE}-sha256-{receipt['raw_family_sha256']}"
    require(receipt["source_receipt_id"] == expected_id,
            "source receipt immutable label differs from product/release/raw digest")
    return receipt


def parse_nonnegative_integer(value: str, label: str) -> int:
    require(NONNEGATIVE_INTEGER_RE.fullmatch(value) is not None,
            f"{label} must be a canonical nonnegative integer, found {value!r}")
    return int(value)


def utf8_sort_key(value: str) -> bytes:
    return value.encode("utf-8")


def parse_value_counts(value: str, label: str) -> Tuple[int, Dict[str, int]]:
    payload = parse_json_text(value, label)
    require(isinstance(payload, list), f"{label} must be a JSON array")
    counts: Dict[str, int] = {}
    order: List[str] = []
    for index, item in enumerate(payload):
        require(isinstance(item, dict) and set(item) == {"value", "count"},
                f"{label}[{index}] must contain exactly value/count")
        item_value = item["value"]
        count = item["count"]
        require(isinstance(item_value, str) and bool(item_value.strip()),
                f"{label}[{index}].value must be nonblank text")
        require(isinstance(count, int) and not isinstance(count, bool) and count > 0,
                f"{label}[{index}].count must be a positive integer")
        require(item_value not in counts, f"{label} duplicates value {item_value!r}")
        counts[item_value] = count
        order.append(item_value)
    require(order == sorted(order, key=utf8_sort_key), f"{label} is not byte-sorted")
    return sum(counts.values()), counts


def parse_held_reasons(
    value: str, label: str
) -> Tuple[int, Dict[str, int], List[Dict[str, Any]]]:
    payload = parse_json_text(value, label)
    require(isinstance(payload, list), f"{label} must be a JSON array")
    by_status: Dict[str, int] = {status: 0 for status in HELD_STATUS_TO_FIELD}
    seen: set[Tuple[str, str]] = set()
    order: List[Tuple[str, str]] = []
    normalized: List[Dict[str, Any]] = []
    for index, item in enumerate(payload):
        require(isinstance(item, dict) and set(item) == {"status", "reason", "count"},
                f"{label}[{index}] must contain exactly status/reason/count")
        status_value = item["status"]
        reason = item["reason"]
        count = item["count"]
        require(status_value in HELD_STATUS_TO_FIELD,
                f"{label}[{index}] has unknown held status: {status_value!r}")
        require(isinstance(reason, str) and bool(reason.strip()),
                f"{label}[{index}].reason must be nonblank text")
        require(isinstance(count, int) and not isinstance(count, bool) and count > 0,
                f"{label}[{index}].count must be a positive integer")
        pair = (status_value, reason)
        require(pair not in seen, f"{label} duplicates status/reason pair {pair!r}")
        seen.add(pair)
        order.append(pair)
        by_status[status_value] += count
        normalized.append({"status": status_value, "reason": reason, "count": count})
    require(
        order == sorted(order, key=lambda pair: (utf8_sort_key(pair[0]), utf8_sort_key(pair[1]))),
        f"{label} is not byte-sorted by status/reason",
    )
    return sum(by_status.values()), by_status, normalized


def parse_audit(path: Path, receipt: Mapping[str, str]) -> Tuple[List[Dict[str, Any]], Dict[str, Any]]:
    data = read_limited_bytes(path, "data-quality audit", 32 * 1024 * 1024)
    require(b"\x00" not in data, "data-quality audit contains NUL")
    require(b"\r" not in data, "data-quality audit must use LF-only CSV")
    text = decode_utf8(data, "data-quality audit")
    require(text.endswith("\n"), "data-quality audit must end with LF")
    csv.field_size_limit(2 * 1024 * 1024)
    try:
        rows_raw = list(csv.reader(io.StringIO(text, newline=""), strict=True))
    except csv.Error as exc:
        raise AuditError(f"data-quality audit is malformed CSV: {exc}") from exc
    require(bool(rows_raw), "data-quality audit is empty")
    header = rows_raw[0]
    require(len(header) == len(set(header)), "data-quality audit has duplicate header names")
    require(tuple(header) == AUDIT_HEADER,
            "data-quality audit header/order differs from the registered v2 schema")
    data_rows = rows_raw[1:]
    require(len(data_rows) == len(SITES) * len(CHANNELS),
            f"data-quality audit must contain exactly 84 rows, found {len(data_rows)}")

    expected_keys = [(site, channel) for site in SITES for channel in CHANNELS]
    seen: set[Tuple[str, str]] = set()
    parsed_rows: List[Dict[str, Any]] = []
    for index, values in enumerate(data_rows, 2):
        require(len(values) == len(AUDIT_HEADER),
                f"data-quality audit row {index} has {len(values)} fields, expected {len(AUDIT_HEADER)}")
        raw = dict(zip(AUDIT_HEADER, values))
        key = (raw["site"], raw["channel"])
        require(key not in seen, f"data-quality audit duplicates key {key!r}")
        seen.add(key)
        require(key == expected_keys[index - 2],
                f"data-quality audit key/order mismatch at row {index}: {key!r}")
        require(raw["audit_schema"] == AUDIT_SCHEMA,
                f"audit row {index} schema mismatch")
        require(raw["contract_id"] == CONTRACT_ID,
                f"audit row {index} contract mismatch")
        require(raw["contract_version"] == "2",
                f"audit row {index} contract version mismatch")
        require(raw["product"] == PRODUCT, f"audit row {index} product mismatch")
        require(raw["source_release"] == RELEASE,
                f"audit row {index} release mismatch")
        require(raw["release_doi"] == RELEASE_DOI,
                f"audit row {index} DOI mismatch")
        require(raw["source_receipt_id"] == receipt["source_receipt_id"],
                f"audit row {index} source receipt ID mismatch")
        require(raw["raw_source_digest"] == receipt["raw_family_sha256"],
                f"audit row {index} raw source digest mismatch")
        require(raw["source_normalization"] == SOURCE_NORMALIZATION,
                f"audit row {index} source normalization mismatch")
        require(raw["source_normalization"] == receipt["source_normalization"],
                f"audit row {index} normalization differs from source receipt")
        require(raw["dataqf_handling"] == "preserved_and_counted_not_excluded",
                f"audit row {index} dataQF handling mismatch")

        row: Dict[str, Any] = dict(raw)
        for field in AUDIT_NUMERIC_FIELDS:
            row[field] = parse_nonnegative_integer(raw[field], f"audit row {index} {field}")

        supported = row["n_sampled_with_records"] + row["n_sampled_absence"]
        held = sum(row[field] for field in HELD_COUNT_FIELDS)
        require(row["n_plot_event_contexts"] > 0,
                f"audit row {index} has no plot-event contexts")
        require(
            row["n_plot_event_contexts"]
            == row["n_published_opportunity_keys"] + row["n_measurement_only_contexts"],
            f"audit row {index} context/source partition fails",
        )
        require(
            row["n_opportunity_source_records"] >= row["n_published_opportunity_keys"],
            f"audit row {index} has fewer source records than published opportunity keys",
        )
        require(
            row["n_opportunity_key_conflict_groups"]
            <= row["n_published_opportunity_keys"],
            f"audit row {index} has more opportunity conflicts than published keys",
        )
        require(
            row["n_opportunity_source_records"] - row["n_published_opportunity_keys"]
            >= row["n_opportunity_key_conflict_groups"],
            f"audit row {index} conflict groups are not supported by duplicate source rows",
        )
        require(row["n_supported_contexts"] == supported,
                f"audit row {index} supported-context algebra fails")
        require(row["n_explicit_absences"] == row["n_sampled_absence"],
                f"audit row {index} explicit-absence algebra fails")
        require(row["n_held_contexts"] == held,
                f"audit row {index} held-context algebra fails")
        require(row["n_plot_event_contexts"] == supported + held,
                f"audit row {index} context status partition fails")
        require(
            row["n_supported_contexts"] <= row["n_published_opportunity_keys"],
            f"audit row {index} supports a context without a published opportunity",
        )
        require(
            row["n_held_opportunity_source_missing"]
            == row["n_measurement_only_contexts"],
            f"audit row {index} source-missing contexts did not receive their exact hold",
        )
        require(
            row["n_channel_measurement_only_contexts_with_records"]
            <= row["n_measurement_only_contexts"],
            f"audit row {index} channel source-missing contexts exceed the site context total",
        )
        require(
            row["n_channel_measurement_records_without_opportunity_source"]
            <= row["n_measurement_records"],
            f"audit row {index} source-missing channel records exceed measurements",
        )
        require(
            row["n_channel_measurement_records_without_opportunity_source"]
            <= row["n_measurement_records_without_opportunity_source"],
            f"audit row {index} source-missing channel records exceed the site total",
        )
        require(
            (row["n_measurement_only_contexts"] == 0)
            == (row["n_measurement_records_without_opportunity_source"] == 0),
            f"audit row {index} source-missing context/measurement presence disagrees",
        )
        require(row["n_live_measurement_records"] <= row["n_measurement_records"],
                f"audit row {index} live records exceed measurement records")
        require(row["n_invalid_metric_records"] <= row["n_live_measurement_records"],
                f"audit row {index} invalid metrics exceed live records")
        require(
            row["n_protocol_identity_conflict_keys"]
            <= row["n_protocol_identity_conflict_records"],
            f"audit row {index} identity-conflict keys exceed affected source records",
        )
        require(
            row["n_protocol_identity_conflict_records"]
            <= row["n_measurement_records"],
            f"audit row {index} identity-conflict records exceed measurement records",
        )
        require(
            (row["n_protocol_identity_conflict_keys"] == 0)
            == (row["n_protocol_identity_conflict_records"] == 0),
            f"audit row {index} identity-conflict key/record presence disagrees",
        )
        require(
            row["n_protocol_identity_conflict_keys"] == 0
            or row["n_held_contexts"] > 0,
            f"audit row {index} reports identity conflicts without a held context",
        )
        require(
            row["n_held_identity_conflict"]
            >= row["n_opportunity_key_conflict_groups"],
            f"audit row {index} opportunity conflicts did not receive first-precedence identity holds",
        )
        require(row["n_sampled_with_records"] <= row["n_measurement_records"],
                f"audit row {index} sampled-with-record opportunities exceed records")
        require(row["n_nonblank_dataqf_records"] <= row["n_measurement_records"],
                f"audit row {index} dataQF count exceeds records")
        require(row["n_nonblank_tag_status_records"] <= row["n_measurement_records"],
                f"audit row {index} tag-status count exceeds records")
        require(row["n_non_ok_tag_status_records"] <= row["n_nonblank_tag_status_records"],
                f"audit row {index} non-ok tags exceed nonblank tags")
        require(
            row["n_nonblank_changed_measurement_location_records"]
            <= row["n_measurement_records"],
            f"audit row {index} location-review count exceeds records",
        )
        require(
            row["n_changed_measurement_location_records"]
            <= row["n_nonblank_changed_measurement_location_records"],
            f"audit row {index} changed-location count exceeds nonblank locations",
        )
        require(
            row["n_held_metric_invalid"] == 0 or row["n_invalid_metric_records"] > 0,
            f"audit row {index} holds metric-invalid opportunities without invalid records",
        )
        require(
            row["n_held_identity_conflict"] == 0
            or row["n_protocol_identity_conflict_keys"] > 0
            or row["n_opportunity_key_conflict_groups"] > 0,
            f"audit row {index} holds identity conflicts without a source-key conflict",
        )

        held_total, held_by_status, held_reasons = parse_held_reasons(
            raw["held_reason_counts"], f"audit row {index} held_reason_counts"
        )
        require(held_total == row["n_held_contexts"],
                f"audit row {index} held-reason JSON total mismatch")
        for status_value, field in HELD_STATUS_TO_FIELD.items():
            require(held_by_status[status_value] == row[field],
                    f"audit row {index} held-reason JSON disagrees with {field}")

        dataqf_total, dataqf_counts = parse_value_counts(
            raw["dataqf_value_counts"], f"audit row {index} dataqf_value_counts"
        )
        require(dataqf_total == row["n_nonblank_dataqf_records"],
                f"audit row {index} dataQF JSON total mismatch")
        tag_total, tag_counts = parse_value_counts(
            raw["non_ok_tag_status_value_counts"],
            f"audit row {index} non_ok_tag_status_value_counts",
        )
        require(tag_total == row["n_non_ok_tag_status_records"],
                f"audit row {index} non-ok tag JSON total mismatch")
        location_total, location_counts = parse_value_counts(
            raw["changed_measurement_location_value_counts"],
            f"audit row {index} changed_measurement_location_value_counts",
        )
        require(location_total == row["n_changed_measurement_location_records"],
                f"audit row {index} changed-location JSON total mismatch")

        row["held_reasons"] = held_reasons
        row["dataqf_counts"] = dataqf_counts
        row["non_ok_tag_counts"] = tag_counts
        row["changed_location_counts"] = location_counts
        parsed_rows.append(row)

    for site in SITES:
        site_rows = [row for row in parsed_rows if row["site"] == site]
        require(len(site_rows) == 2, f"audit site {site} does not have two channel rows")
        for field in (
            "n_plot_event_contexts",
            "n_published_opportunity_keys",
            "n_opportunity_source_records",
            "n_measurement_only_contexts",
            "n_measurement_records_without_opportunity_source",
            "n_opportunity_key_conflict_groups",
        ):
            require(
                site_rows[0][field] == site_rows[1][field],
                f"audit site {site} channels disagree on shared field {field}",
            )
        registered_channel_missing_records = sum(
            row["n_channel_measurement_records_without_opportunity_source"]
            for row in site_rows
        )
        all_growth_form_missing_records = site_rows[0][
            "n_measurement_records_without_opportunity_source"
        ]
        require(
            registered_channel_missing_records <= all_growth_form_missing_records,
            f"audit site {site} registered-channel source-missing records "
            "exceed the all-growth-form total",
        )
        unregistered_channel_missing_records = (
            all_growth_form_missing_records - registered_channel_missing_records
        )
        # The two physical analysis channels intentionally exclude records such
        # as lianas, small trees, and rows without a registered growth form.
        # Retain that legitimate residual instead of pretending the registered
        # channels partition the all-growth-form preservation total.
        for row in site_rows:
            row[
                "n_registered_channel_measurement_records_without_opportunity_source"
            ] = registered_channel_missing_records
            row[
                "n_unregistered_channel_measurement_records_without_opportunity_source"
            ] = unregistered_channel_missing_records

    site_rows_once = [
        next(row for row in parsed_rows if row["site"] == site) for site in SITES
    ]
    measurement_only_contexts = sum(
        row["n_measurement_only_contexts"] for row in site_rows_once
    )
    missing_measurement_records = sum(
        row["n_measurement_records_without_opportunity_source"]
        for row in site_rows_once
    )
    affected_sites = sum(
        row["n_measurement_only_contexts"] > 0 for row in site_rows_once
    )
    require(
        measurement_only_contexts == EXPECTED_MEASUREMENT_ONLY_CONTEXTS,
        "RELEASE-2026 measurement-only context total differs: "
        f"expected {EXPECTED_MEASUREMENT_ONLY_CONTEXTS}, found {measurement_only_contexts}",
    )
    require(
        missing_measurement_records
        == EXPECTED_MEASUREMENT_RECORDS_WITHOUT_OPPORTUNITY_SOURCE,
        "RELEASE-2026 preserved source-missing measurement total differs: "
        f"expected {EXPECTED_MEASUREMENT_RECORDS_WITHOUT_OPPORTUNITY_SOURCE}, "
        f"found {missing_measurement_records}",
    )
    require(
        affected_sites == EXPECTED_SITES_WITH_MEASUREMENT_ONLY_CONTEXTS,
        "RELEASE-2026 source-missing site total differs: "
        f"expected {EXPECTED_SITES_WITH_MEASUREMENT_ONLY_CONTEXTS}, found {affected_sites}",
    )

    summary = summarize_audit(parsed_rows)
    return parsed_rows, summary


def empty_channel_summary() -> Dict[str, int]:
    return {
        "sites_with_supported_contexts": 0,
        "sites_all_held": 0,
        "plot_event_contexts": 0,
        "published_opportunity_keys": 0,
        "opportunity_source_records": 0,
        "measurement_only_contexts": 0,
        "measurement_only_contexts_with_records": 0,
        "measurement_records_without_opportunity_source": 0,
        "opportunity_key_conflict_groups": 0,
        "supported_contexts": 0,
        "sampled_with_records": 0,
        "explicit_absences": 0,
        "held_contexts": 0,
        "held_opportunity_source_missing": 0,
        "measurement_records": 0,
        "live_measurement_records": 0,
        "invalid_metric_records": 0,
        "identity_conflict_keys": 0,
        "identity_conflict_records": 0,
        "held_identity_conflicts": 0,
        "nonblank_dataqf_records": 0,
        "non_ok_tag_status_records": 0,
        "changed_measurement_location_records": 0,
    }


def summarize_audit(rows: Sequence[Mapping[str, Any]]) -> Dict[str, Any]:
    channels: Dict[str, Dict[str, int]] = {}
    for channel in CHANNELS:
        summary = empty_channel_summary()
        channel_rows = [row for row in rows if row["channel"] == channel]
        for row in channel_rows:
            if row["n_supported_contexts"] > 0:
                summary["sites_with_supported_contexts"] += 1
            else:
                summary["sites_all_held"] += 1
            summary["plot_event_contexts"] += row["n_plot_event_contexts"]
            summary["published_opportunity_keys"] += row[
                "n_published_opportunity_keys"
            ]
            summary["opportunity_source_records"] += row["n_opportunity_source_records"]
            summary["measurement_only_contexts"] += row[
                "n_measurement_only_contexts"
            ]
            summary["measurement_only_contexts_with_records"] += row[
                "n_channel_measurement_only_contexts_with_records"
            ]
            summary["measurement_records_without_opportunity_source"] += row[
                "n_channel_measurement_records_without_opportunity_source"
            ]
            summary["opportunity_key_conflict_groups"] += row[
                "n_opportunity_key_conflict_groups"
            ]
            summary["supported_contexts"] += row["n_supported_contexts"]
            summary["sampled_with_records"] += row["n_sampled_with_records"]
            summary["explicit_absences"] += row["n_explicit_absences"]
            summary["held_contexts"] += row["n_held_contexts"]
            summary["held_opportunity_source_missing"] += row[
                "n_held_opportunity_source_missing"
            ]
            summary["measurement_records"] += row["n_measurement_records"]
            summary["live_measurement_records"] += row["n_live_measurement_records"]
            summary["invalid_metric_records"] += row["n_invalid_metric_records"]
            summary["identity_conflict_keys"] += row[
                "n_protocol_identity_conflict_keys"
            ]
            summary["identity_conflict_records"] += row[
                "n_protocol_identity_conflict_records"
            ]
            summary["held_identity_conflicts"] += row["n_held_identity_conflict"]
            summary["nonblank_dataqf_records"] += row["n_nonblank_dataqf_records"]
            summary["non_ok_tag_status_records"] += row["n_non_ok_tag_status_records"]
            summary["changed_measurement_location_records"] += row[
                "n_changed_measurement_location_records"
            ]
        channels[channel] = summary

    qc = {
        key: sum(channel[key] for channel in channels.values())
        for key in (
            "measurement_records",
            "live_measurement_records",
            "invalid_metric_records",
            "nonblank_dataqf_records",
            "non_ok_tag_status_records",
            "changed_measurement_location_records",
        )
    }
    wood: Dict[str, Any] = {}
    for row in rows:
        if row["site"] != "WOOD":
            continue
        wood[row["channel"]] = {
            "plot_event_contexts": row["n_plot_event_contexts"],
            "published_opportunity_keys": row["n_published_opportunity_keys"],
            "measurement_only_contexts": row["n_measurement_only_contexts"],
            "measurement_only_contexts_with_records": row[
                "n_channel_measurement_only_contexts_with_records"
            ],
            "measurement_records_without_opportunity_source": row[
                "n_channel_measurement_records_without_opportunity_source"
            ],
            "supported_contexts": row["n_supported_contexts"],
            "sampled_with_records": row["n_sampled_with_records"],
            "explicit_absences": row["n_explicit_absences"],
            "held_contexts": row["n_held_contexts"],
            "invalid_metric_records": row["n_invalid_metric_records"],
            "opportunity_key_conflict_groups": row[
                "n_opportunity_key_conflict_groups"
            ],
            "identity_conflict_keys": row["n_protocol_identity_conflict_keys"],
            "identity_conflict_records": row[
                "n_protocol_identity_conflict_records"
            ],
            "held_reasons": row["held_reasons"],
        }
    require(set(wood) == set(CHANNELS), "WOOD audit summary is incomplete")
    site_rows_once = [next(row for row in rows if row["site"] == site) for site in SITES]
    source_gaps = {
        "measurement_only_contexts": sum(
            row["n_measurement_only_contexts"] for row in site_rows_once
        ),
        "preserved_measurement_records": sum(
            row["n_measurement_records_without_opportunity_source"]
            for row in site_rows_once
        ),
        "affected_sites": sum(
            row["n_measurement_only_contexts"] > 0 for row in site_rows_once
        ),
        "registered_channel_measurement_records": sum(
            row[
                "n_registered_channel_measurement_records_without_opportunity_source"
            ]
            for row in site_rows_once
        ),
        "unregistered_channel_measurement_records": sum(
            row[
                "n_unregistered_channel_measurement_records_without_opportunity_source"
            ]
            for row in site_rows_once
        ),
        "unregistered_channel_records_by_site": {
            row["site"]: row[
                "n_unregistered_channel_measurement_records_without_opportunity_source"
            ]
            for row in site_rows_once
            if row[
                "n_unregistered_channel_measurement_records_without_opportunity_source"
            ] > 0
        },
    }
    return {"channels": channels, "qc": qc, "source_gaps": source_gaps, "wood": wood}


def validate_manifest(
    path: Path, root: Path, hashes: Hashes
) -> Dict[str, Any]:
    data = read_limited_bytes(path, "manifest.json", 32 * 1024 * 1024)
    text = decode_utf8(data, "manifest.json")
    manifest = parse_json_text(text, "manifest.json")
    require(isinstance(manifest, dict), "manifest root must be an object")
    require(manifest.get("version") == 1, "manifest version must be 1")
    # The immutable Linux validator emits the client locale as `C`. Posit
    # Connect's manifest schema accepts any string here; this inspector binds
    # the artifact to the actual pinned builder instead of the superseded
    # workstation-generated `en_US` manifest.
    require(manifest.get("locale") == "C", "manifest locale must be C")
    require(manifest.get("platform") == "4.5.2", "manifest R platform must be 4.5.2")
    metadata = manifest.get("metadata")
    require(isinstance(metadata, dict) and metadata.get("appmode") == "shiny",
            "manifest appmode must be shiny")

    files = manifest.get("files")
    require(isinstance(files, dict), "manifest files must be an object")
    for name in files:
        validate_relative_name(name, "manifest runtime file")
    actual_files = set(files)
    require(actual_files == set(EXPECTED_RUNTIME_FILES),
            "manifest runtime inventory differs: "
            f"missing=[{display_set(set(EXPECTED_RUNTIME_FILES) - actual_files)}] "
            f"extra=[{display_set(actual_files - set(EXPECTED_RUNTIME_FILES))}]")
    for name, entry in files.items():
        require(isinstance(entry, dict), f"manifest file entry is not an object: {name}")
        checksum = entry.get("checksum")
        require(isinstance(checksum, str) and MD5_RE.fullmatch(checksum) is not None,
                f"manifest file checksum is not lowercase MD5: {name}")
        if name in EXPECTED_CANDIDATE_FILES:
            require(hashes.file(name, "md5") == checksum,
                    f"manifest checksum differs from candidate bytes: {name}")

    packages = manifest.get("packages")
    require(isinstance(packages, dict) and bool(packages),
            "manifest packages must be a nonempty object")
    package_names = set(packages)
    missing_required = REQUIRED_RUNTIME_PACKAGES - package_names
    forbidden = FORBIDDEN_BUILD_PACKAGES & package_names
    require(not missing_required,
            f"manifest lacks runtime packages: {display_set(missing_required)}")
    require(not forbidden,
            f"manifest contains build-only packages: {display_set(forbidden)}")
    for package, entry in packages.items():
        require(isinstance(package, str) and bool(package), "manifest has blank package name")
        require(isinstance(entry, dict), f"manifest package entry is not an object: {package}")
        description = entry.get("description")
        require(isinstance(description, dict),
                f"manifest package {package} lacks description metadata")
        require(description.get("Package") == package,
                f"manifest package identity differs for {package}")
        version = description.get("Version")
        require(isinstance(version, str) and bool(version),
                f"manifest package {package} has a blank Version")
        source = entry.get("Source")
        require(source == "CRAN",
                f"manifest package {package} Source is not CRAN: {source!r}")
        repository = entry.get("Repository")
        require(isinstance(repository, str) and bool(repository),
                f"manifest package {package} has a blank Repository")
        validate_https_url(repository, f"manifest package {package} Repository")
        if package in PINNED_GEO_VERSIONS:
            require(repository == CRAN_REPOSITORY,
                    f"manifest geo package {package} lacks the absolute CRAN lane: {repository}")
        else:
            require(repository == PINNED_REPOSITORY,
                    f"manifest package {package} is not on the pinned Jammy snapshot: {repository}")

    for package, expected_version in PINNED_GEO_VERSIONS.items():
        require(package in packages, f"manifest lacks pinned geographic package {package}")
        description = packages[package].get("description")
        require(isinstance(description, dict),
                f"manifest package {package} lacks description metadata")
        require(description.get("Version") == expected_version,
                f"manifest package {package} version is {description.get('Version')!r}, "
                f"expected {expected_version}")
        require(description.get("RemoteType") == "url",
                f"manifest package {package} RemoteType is not url")
        expected_ref = f"url::{PINNED_GEO_URLS[package]}"
        require(description.get("RemotePkgRef") == expected_ref,
                f"manifest package {package} RemotePkgRef differs from {expected_ref}")
        require(not description.get("Built"),
                f"manifest package {package} retains a non-semantic Built clock")

    return {
        "r_version": manifest["platform"],
        "appmode": metadata["appmode"],
        "runtime_files": len(files),
        "packages": len(packages),
        "ordinary_repository": PINNED_REPOSITORY,
        "geo_repository": CRAN_REPOSITORY,
        "geo_versions": dict(PINNED_GEO_VERSIONS),
        "forbidden_packages_present": [],
    }


def verify_candidate_checksums(root: Path, hashes: Hashes) -> Tuple[Dict[str, str], str]:
    ledger, data = parse_hash_ledger(
        root / CANDIDATE_LEDGER,
        "candidate checksum ledger",
        CANDIDATE_PAYLOAD_PATHS,
    )
    for name in CANDIDATE_PAYLOAD_PATHS:
        actual = hashes.file(name)
        require(actual == ledger[name],
                f"candidate checksum mismatch for {name}: expected={ledger[name]} actual={actual}")
    return ledger, hashlib.sha256(data).hexdigest()


def audit_candidate(root: Path) -> Dict[str, Any]:
    files, directories = inventory_tree(root)
    require(files == EXPECTED_CANDIDATE_FILES,
            "candidate file inventory differs: "
            f"missing=[{display_set(EXPECTED_CANDIDATE_FILES - files)}] "
            f"extra=[{display_set(files - EXPECTED_CANDIDATE_FILES)}]")
    require(directories == EXPECTED_DIRECTORIES,
            "candidate directory inventory differs: "
            f"missing=[{display_set(EXPECTED_DIRECTORIES - directories)}] "
            f"extra=[{display_set(directories - EXPECTED_DIRECTORIES)}]")
    require(len(files) == 55 and len(CANDIDATE_PAYLOAD_PATHS) == 54,
            "internal promotion inventory count contract is not 54+1")

    hashes = Hashes(root)
    _, candidate_ledger_sha256 = verify_candidate_checksums(root, hashes)

    raw_names = [f"{site}_raw.rds" for site in SITES]
    raw_ledger, raw_ledger_bytes = parse_hash_ledger(
        root / "data/source/vegetation-raw-SHA256SUMS.txt",
        "raw source checksum ledger",
        raw_names,
        basename_only=True,
    )
    del raw_ledger  # Syntax/inventory is material; raw bytes are retained in Actions.
    raw_family = parse_family_digest(
        root / "data/source/vegetation-raw-family-SHA256.txt", "raw family checksum"
    )
    require(hashlib.sha256(raw_ledger_bytes).hexdigest() == raw_family,
            "raw family checksum differs from exact raw checksum-ledger bytes")

    bundle_names = [f"{site}.rds" for site in SITES]
    bundle_ledger, bundle_ledger_bytes = parse_hash_ledger(
        root / "data/source/vegetation-bundle-SHA256SUMS.txt",
        "bundle checksum ledger",
        bundle_names,
        basename_only=True,
    )
    for site in SITES:
        basename = f"{site}.rds"
        rel = f"data/sites/{basename}"
        require(bundle_ledger[basename] == hashes.file(rel),
                f"bundle checksum ledger differs from exact site bytes: {site}")
    bundle_family = parse_family_digest(
        root / "data/source/vegetation-bundle-family-SHA256.txt",
        "bundle family checksum",
    )
    require(hashlib.sha256(bundle_ledger_bytes).hexdigest() == bundle_family,
            "bundle family checksum differs from exact bundle checksum-ledger bytes")

    audit_ledger, _ = parse_hash_ledger(
        root / "data/source/vegetation-data-quality-audit-SHA256.txt",
        "data-quality audit checksum ledger",
        ["vegetation-data-quality-audit.csv"],
        basename_only=True,
    )
    audit_sha256 = hashes.file("data/source/vegetation-data-quality-audit.csv")
    require(audit_ledger["vegetation-data-quality-audit.csv"] == audit_sha256,
            "data-quality audit checksum differs from exact CSV bytes")

    runtime = parse_fetch_runtime(
        root / "data/source/vegetation-fetch-runtime.txt"
    )
    receipt = parse_receipt(root / "docs/VEGETATION-SOURCE-RECEIPT.md", runtime)
    require(receipt["raw_family_sha256"] == raw_family,
            "source receipt raw family checksum differs from nested ledger")
    require(receipt["bundle_family_sha256"] == bundle_family,
            "source receipt bundle family checksum differs from nested ledger")
    require(receipt["audit_sha256"] == audit_sha256,
            "source receipt audit checksum differs from nested ledger")

    rows, audit_summary = parse_audit(
        root / "data/source/vegetation-data-quality-audit.csv", receipt
    )
    del rows
    manifest_summary = validate_manifest(root / "manifest.json", root, hashes)

    require(hashes.file("data-sample/demo.rds") == hashes.file("data/sites/HARV.rds"),
            "demo.rds is not byte-identical to the HARV runtime bundle")

    return {
        "ok": True,
        "candidate_root": str(root),
        "inventory": {
            "files": len(files),
            "payload_files": len(CANDIDATE_PAYLOAD_PATHS),
            "checksum_ledgers": 1 + 3,
            "sites": len(SITES),
            "audit_rows": len(SITES) * len(CHANNELS),
        },
        "identity": {
            "product": PRODUCT,
            "release": RELEASE,
            "release_doi": RELEASE_DOI,
            "contract_id": CONTRACT_ID,
            "audit_schema": AUDIT_SCHEMA,
            "query_start": receipt["query_start"],
            "query_end": receipt["query_end"],
            "built_at": receipt["built_at"],
            "builder_commit": receipt["builder_commit"],
            "source_receipt_id": receipt["source_receipt_id"],
            "neon_utilities_version": receipt["neon_utilities_version"],
            "source_normalization": receipt["source_normalization"],
            "refresh_run_url": receipt["refresh_run_url"],
        },
        "hashes": {
            "candidate_ledger_sha256": candidate_ledger_sha256,
            "raw_family_sha256": raw_family,
            "bundle_family_sha256": bundle_family,
            "audit_sha256": audit_sha256,
            "manifest_sha256": hashes.file("manifest.json"),
            "site_index_sha256": hashes.file("data/site_index.rds"),
            "search_index_sha256": hashes.file("data/search_index.rds"),
            "demo_sha256": hashes.file("data-sample/demo.rds"),
        },
        "manifest": manifest_summary,
        "channels": audit_summary["channels"],
        "qc": audit_summary["qc"],
        "source_gaps": audit_summary["source_gaps"],
        "wood": audit_summary["wood"],
    }


def compact_held_reasons(items: Sequence[Mapping[str, Any]]) -> str:
    if not items:
        return "none"
    return ";".join(
        f"{item['status']}:{item['reason']}={item['count']}" for item in items
    )


def print_success(summary: Mapping[str, Any]) -> None:
    inventory = summary["inventory"]
    manifest = summary["manifest"]
    identity = summary["identity"]
    print(
        "PASS "
        f"files={inventory['files']} payload={inventory['payload_files']} "
        f"sites={inventory['sites']} audit_rows={inventory['audit_rows']} "
        f"R={manifest['r_version']} runtime_files={manifest['runtime_files']} "
        f"packages={manifest['packages']}"
    )
    print(
        "IDENTITY "
        f"product={identity['product']} release={identity['release']} "
        f"query={identity['query_start']}..{identity['query_end']} "
        f"builder={identity['builder_commit']} built={identity['built_at']}"
    )
    for channel in CHANNELS:
        value = summary["channels"][channel]
        print(
            f"CHANNEL {channel} "
            f"support_sites={value['sites_with_supported_contexts']}/{len(SITES)} "
            f"contexts={value['plot_event_contexts']} "
            f"published={value['published_opportunity_keys']} "
            f"measurement_only={value['measurement_only_contexts']} "
            f"supported={value['supported_contexts']} "
            f"with_records={value['sampled_with_records']} "
            f"absences={value['explicit_absences']} held={value['held_contexts']} "
            f"measurements={value['measurement_records']} "
            f"invalid={value['invalid_metric_records']}"
        )
    source_gaps = summary["source_gaps"]
    print(
        "SOURCE_GAPS "
        f"measurement_only_contexts={source_gaps['measurement_only_contexts']} "
        f"preserved_measurements={source_gaps['preserved_measurement_records']} "
        f"registered_channel_measurements="
        f"{source_gaps['registered_channel_measurement_records']} "
        f"unregistered_channel_measurements="
        f"{source_gaps['unregistered_channel_measurement_records']} "
        f"affected_sites={source_gaps['affected_sites']}"
    )
    qc = summary["qc"]
    print(
        "QC "
        f"measurements={qc['measurement_records']} live={qc['live_measurement_records']} "
        f"invalid_metric={qc['invalid_metric_records']} "
        f"dataQF_nonblank={qc['nonblank_dataqf_records']} "
        f"non_ok_tag={qc['non_ok_tag_status_records']} "
        f"changed_location={qc['changed_measurement_location_records']}"
    )
    for channel in CHANNELS:
        value = summary["wood"][channel]
        print(
            f"WOOD {channel} supported={value['supported_contexts']} "
            f"with_records={value['sampled_with_records']} "
            f"absences={value['explicit_absences']} held={value['held_contexts']} "
            f"invalid={value['invalid_metric_records']} "
            f"held_reasons={compact_held_reasons(value['held_reasons'])}"
        )
    print("SUMMARY_JSON=" + json.dumps(summary, sort_keys=True, separators=(",", ":")))


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Fail-closed audit of one extracted Vegetation candidate root."
    )
    parser.add_argument(
        "candidate_root",
        help="directory containing CANDIDATE-SHA256SUMS.txt, data/, data-sample/, docs/, and manifest.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    root = Path(os.path.abspath(os.path.expanduser(args.candidate_root)))
    try:
        summary = audit_candidate(root)
    except (AuditError, OSError) as exc:
        failure = {
            "ok": False,
            "candidate_root": str(root),
            "error": str(exc),
        }
        print(f"FAIL {exc}", file=sys.stderr)
        print("SUMMARY_JSON=" + json.dumps(failure, sort_keys=True, separators=(",", ":")))
        return 1
    except Exception as exc:  # Fail closed even if the artifact finds a parser edge.
        failure = {
            "ok": False,
            "candidate_root": str(root),
            "error": f"unexpected {type(exc).__name__}: {exc}",
        }
        print(f"FAIL unexpected {type(exc).__name__}: {exc}", file=sys.stderr)
        print("SUMMARY_JSON=" + json.dumps(failure, sort_keys=True, separators=(",", ":")))
        return 1
    print_success(summary)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
