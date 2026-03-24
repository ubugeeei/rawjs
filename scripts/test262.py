#!/usr/bin/env python3

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


FRONTMATTER_RE = re.compile(r"/\*---\n(.*?)\n---\*/", re.DOTALL)
KEY_RE = re.compile(r"^([A-Za-z0-9_-]+):(?:\s*(.*))?$")


@dataclass
class NegativeExpectation:
    phase: str | None = None
    error_type: str | None = None


@dataclass
class Metadata:
    flags: list[str]
    includes: list[str]
    negative: NegativeExpectation


@dataclass
class RunResult:
    ok: bool
    reason: str
    variant: str
    stdout: str
    stderr: str


def parse_list(value: str) -> list[str]:
    value = value.strip()
    if not value.startswith("[") or not value.endswith("]"):
        return []
    inner = value[1:-1].strip()
    if not inner:
        return []
    return [part.strip() for part in inner.split(",")]


def parse_metadata(source: str) -> Metadata:
    match = FRONTMATTER_RE.search(source)
    if not match:
        return Metadata(flags=[], includes=[], negative=NegativeExpectation())

    flags: list[str] = []
    includes: list[str] = []
    negative = NegativeExpectation()

    current_key: str | None = None
    current_indent = 0

    for raw_line in match.group(1).splitlines():
        if not raw_line.strip():
            continue

        indent = len(raw_line) - len(raw_line.lstrip(" "))
        line = raw_line.strip()

        top_match = KEY_RE.match(line) if indent == 0 else None
        if top_match:
            current_key = top_match.group(1)
            current_indent = indent
            value = top_match.group(2) or ""

            if current_key == "flags":
                flags = parse_list(value)
                current_key = None
            elif current_key == "includes":
                includes = parse_list(value)
                current_key = None
            elif current_key == "negative" and value:
                current_key = None
            continue

        if current_key == "negative" and indent > current_indent:
            nested = KEY_RE.match(line)
            if not nested:
                continue
            key = nested.group(1)
            value = (nested.group(2) or "").strip()
            if key == "phase":
                negative.phase = value
            elif key == "type":
                negative.error_type = value

    return Metadata(flags=flags, includes=includes, negative=negative)


def load_harness(test262_root: Path, names: Iterable[str]) -> str:
    parts: list[str] = []
    for name in names:
        harness_path = test262_root / "harness" / name
        parts.append(harness_path.read_text())
    return "\n".join(parts) + "\n"


def build_variants(metadata: Metadata) -> list[str]:
    flags = set(metadata.flags)
    if "module" in flags:
        return ["module"]
    if "onlyStrict" in flags:
        return ["strict"]
    if "noStrict" in flags:
        return ["default"]
    return ["default", "strict"]


def build_source(
    test262_root: Path,
    source: str,
    metadata: Metadata,
    variant: str,
) -> str:
    flags = set(metadata.flags)
    parts: list[str] = []

    if variant == "strict":
        parts.append('"use strict";\n')

    if "raw" not in flags:
        harness_names = ["sta.js", "assert.js"]
        if "async" in flags:
            harness_names.append("doneprintHandle.js")
        harness_names.extend(metadata.includes)

        seen: set[str] = set()
        deduped = [name for name in harness_names if not (name in seen or seen.add(name))]
        parts.append(load_harness(test262_root, deduped))

    parts.append(source)
    return "".join(parts)


def run_variant(
    engine: Path,
    test262_root: Path,
    test_path: Path,
    metadata: Metadata,
    variant: str,
) -> RunResult:
    if variant == "module":
        return RunResult(
            ok=False,
            reason="module not supported by runner",
            variant=variant,
            stdout="",
            stderr="",
        )

    source = test_path.read_text()
    full_source = build_source(test262_root, source, metadata, variant)

    with tempfile.NamedTemporaryFile("w", suffix=".js", delete=False) as tmp:
        tmp.write(full_source)
        tmp_path = Path(tmp.name)

    try:
        proc = subprocess.run(
            [str(engine), str(tmp_path)],
            capture_output=True,
            text=True,
        )
    finally:
        try:
            tmp_path.unlink()
        except FileNotFoundError:
            pass

    stdout = proc.stdout
    stderr = proc.stderr
    negative = metadata.negative

    if negative.phase == "parse":
        expected = negative.error_type or "SyntaxError"
        ok = proc.returncode != 0 and expected in stderr
        reason = "expected parse failure" if ok else f"wanted parse {expected}"
        return RunResult(ok=ok, reason=reason, variant=variant, stdout=stdout, stderr=stderr)

    if negative.error_type:
        expected = negative.error_type
        ok = proc.returncode != 0 and expected in stderr
        reason = "expected runtime failure" if ok else f"wanted runtime {expected}"
        return RunResult(ok=ok, reason=reason, variant=variant, stdout=stdout, stderr=stderr)

    if "async" in metadata.flags:
        if proc.returncode != 0:
            return RunResult(
                ok=False,
                reason="async test exited non-zero",
                variant=variant,
                stdout=stdout,
                stderr=stderr,
            )
        if "Test262:AsyncTestFailure" in stdout:
            return RunResult(
                ok=False,
                reason="async failure marker",
                variant=variant,
                stdout=stdout,
                stderr=stderr,
            )
        ok = "Test262:AsyncTestComplete" in stdout
        reason = "async complete" if ok else "missing async completion marker"
        return RunResult(ok=ok, reason=reason, variant=variant, stdout=stdout, stderr=stderr)

    ok = proc.returncode == 0
    reason = "ok" if ok else "non-zero exit"
    return RunResult(ok=ok, reason=reason, variant=variant, stdout=stdout, stderr=stderr)


def iter_tests(paths: list[Path]) -> list[Path]:
    tests: list[Path] = []
    for path in paths:
        if path.is_dir():
            tests.extend(sorted(p for p in path.rglob("*.js") if p.is_file()))
        else:
            tests.append(path)
    return tests


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="+", help="test files or directories")
    parser.add_argument(
        "--test262-root",
        default="/tmp/test262-rawjs-65296",
        help="path to the test262 checkout",
    )
    parser.add_argument(
        "--engine",
        default="./target/debug/rawjs",
        help="path to the rawjs executable",
    )
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument("--fail-fast", action="store_true")
    args = parser.parse_args()

    test262_root = Path(args.test262_root)
    engine = Path(args.engine)
    test_paths = iter_tests([Path(p) for p in args.paths])

    passed = 0
    failed = 0
    skipped = 0

    for test_path in test_paths:
        metadata = parse_metadata(test_path.read_text())
        variants = build_variants(metadata)
        results = [
            run_variant(engine, test262_root, test_path, metadata, variant)
            for variant in variants
        ]

        module_only = all(r.reason == "module not supported by runner" for r in results)
        if module_only:
            skipped += 1
            if args.verbose:
                print(f"SKIP {test_path}")
            continue

        if all(r.ok for r in results):
            passed += 1
            if args.verbose:
                print(f"PASS {test_path}")
            continue

        failed += 1
        first_failure = next(r for r in results if not r.ok)
        print(f"FAIL {test_path} [{first_failure.variant}] {first_failure.reason}")
        if args.verbose:
            if first_failure.stderr.strip():
                print(first_failure.stderr.strip())
            elif first_failure.stdout.strip():
                print(first_failure.stdout.strip())

        if args.fail_fast:
            break

    total = passed + failed + skipped
    print(f"TOTAL={total} PASS={passed} FAIL={failed} SKIP={skipped}")
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
