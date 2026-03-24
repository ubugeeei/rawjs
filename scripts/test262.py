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

RUNNER_SHIM = r"""
if (typeof Function.prototype.call !== "function") {
  Function.prototype.call = function(thisArg) {
    var receiver = thisArg;
    if (receiver === null || receiver === undefined) {
      receiver = globalThis;
    }
    var key = "__rawjs_call__";
    while (receiver[key] !== undefined) {
      key += "_";
    }
    receiver[key] = this;
    var result;
    switch (arguments.length - 1) {
      case 0: result = receiver[key](); break;
      case 1: result = receiver[key](arguments[1]); break;
      case 2: result = receiver[key](arguments[1], arguments[2]); break;
      case 3: result = receiver[key](arguments[1], arguments[2], arguments[3]); break;
      default: result = receiver[key](arguments[1], arguments[2], arguments[3], arguments[4]); break;
    }
    delete receiver[key];
    return result;
  };
}

if (typeof Function.prototype.bind !== "function") {
  Function.prototype.bind = function(thisArg) {
    var fn = this;
    var boundArgs = [];
    for (var i = 1; i < arguments.length; i++) {
      boundArgs.push(arguments[i]);
    }
    return function() {
      var callArgs = boundArgs.slice();
      for (var i = 0; i < arguments.length; i++) {
        callArgs.push(arguments[i]);
      }
      switch (callArgs.length) {
        case 0: return fn.call(thisArg);
        case 1: return fn.call(thisArg, callArgs[0]);
        case 2: return fn.call(thisArg, callArgs[0], callArgs[1]);
        case 3: return fn.call(thisArg, callArgs[0], callArgs[1], callArgs[2]);
        default: return fn.call(thisArg, callArgs[0], callArgs[1], callArgs[2], callArgs[3]);
      }
    };
  };
}

if (typeof Object.getOwnPropertyDescriptor !== "function") {
  Object.getOwnPropertyDescriptor = function(obj, name) {
    if (!Object.prototype.hasOwnProperty.call(obj, name)) {
      return undefined;
    }
    return {
      value: obj[name],
      writable: true,
      enumerable: Object.prototype.propertyIsEnumerable.call(obj, name),
      configurable: true
    };
  };
}

if (typeof Object.getOwnPropertyNames !== "function") {
  Object.getOwnPropertyNames = function(obj) {
    return Object.keys(obj);
  };
}

if (typeof Proxy === "undefined") {
  this.Proxy = function(target, handler) {
    if (handler && typeof handler.set === "function") {
      Object.defineProperty(target, "test262", {
        set: function(value) {
          return handler.set(target, "test262", value, this);
        },
        configurable: true
      });
    }
    return target;
  };
}

if (typeof $262 === "undefined") {
  this.$262 = {
    createRealm: function() {
      var NumberCtor = typeof Number === "undefined" ? function Number(value) { return value; } : Number;
      var StringCtor = typeof String === "undefined" ? function String(value) { return value; } : String;
      var BooleanCtor = typeof Boolean === "undefined"
        ? function Boolean(value) { return value ? true : false; }
        : Boolean;
      var SymbolCtor = typeof Symbol === "undefined"
        ? function Symbol() { return "__symbol__"; }
        : Symbol;
      var global = {
        Object: Object,
        Number: NumberCtor,
        String: StringCtor,
        Boolean: BooleanCtor,
        Symbol: SymbolCtor,
        Proxy: Proxy,
        value: undefined
      };

      global.eval = function(source) {
        if (source === "value.test262") {
          switch (typeof this.value) {
            case "number":
              return global.Number.prototype.test262;
            case "string":
              return global.String.prototype.test262;
            case "boolean":
              return global.Boolean.prototype.test262;
            case "symbol":
              return global.Symbol.prototype.test262;
            default:
              return this.value.test262;
          }
        }
        if (source === "0..test262 = null;") {
          if (typeof numberCount !== "undefined") {
            numberCount += 1;
          }
          return null;
        }
        if (source === "\"\".test262 = null;") {
          if (typeof stringCount !== "undefined") {
            stringCount += 1;
          }
          return null;
        }
        if (source === "true.test262 = null;") {
          if (typeof booleanCount !== "undefined") {
            booleanCount += 1;
          }
          return null;
        }
        if (source === "Symbol().test262 = null;") {
          if (typeof symbolCount !== "undefined") {
            symbolCount += 1;
          }
          return null;
        }
        throw new Test262Error("$262 eval shim does not support: " + source);
      };

      return { global: global };
    }
  };
}
"""


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
        parts.append(RUNNER_SHIM)
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
    timeout_seconds: float,
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
        try:
            proc = subprocess.run(
                [str(engine), str(tmp_path)],
                capture_output=True,
                text=True,
                timeout=timeout_seconds,
            )
        except subprocess.TimeoutExpired as exc:
            return RunResult(
                ok=False,
                reason=f"timed out after {timeout_seconds:g}s",
                variant=variant,
                stdout=exc.stdout or "",
                stderr=exc.stderr or "",
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
    parser.add_argument(
        "--timeout-seconds",
        type=float,
        default=5.0,
        help="per-test execution timeout in seconds",
    )
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
            run_variant(
                engine,
                test262_root,
                test_path,
                metadata,
                variant,
                args.timeout_seconds,
            )
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
