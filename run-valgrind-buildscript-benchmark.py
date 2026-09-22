#!/usr/bin/env python3
"""Benchmark this checkout's build scripts with Valgrind; compare saved runs.

Requires Python 3.11+, Cargo and Valgrind. See BUILDSCRIPT_BENCHMARK.md.
"""

import argparse
import hashlib
import json
import os
import re
import shutil
import statistics
import subprocess
from collections.abc import Mapping, Sequence
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, NotRequired, TypeAlias, TypedDict, cast

import tomllib

StrPath: TypeAlias = str | os.PathLike[str]
Metrics: TypeAlias = dict[str, int]
FileHashes: TypeAlias = dict[str, str]


class BenchmarkSample(TypedDict):
    fixture: str
    tool: str
    repeat: int
    metrics: Metrics
    output_hashes: FileHashes


class BenchmarkResults(TypedDict):
    schema: int
    status: str
    repetitions: int
    # Metadata varies with the selected tools and how far preparation completed.
    metadata: dict[str, Any]
    samples: list[BenchmarkSample]
    error: NotRequired[str]


ROOT: Path = Path(__file__).resolve().parent
FIXTURES: dict[str, str] = {"cpp": "cpp_tests", "jni": "jni_tests", "python": "python_tests"}
CALLGRIND: list[str] = ["--tool=callgrind", "--dump-instr=yes", "--cache-sim=yes",
             "--collect-jumps=yes", "--I1=32768,8,64", "--D1=32768,8,64",
             "--LL=8388608,16,64"]
MASSIF: list[str] = ["--tool=massif", "--time-unit=i", "--peak-inaccuracy=0",
          "--stacks=no", "--pages-as-heap=no"]


def execute(
    command: Sequence[StrPath],
    *,
    cwd: StrPath | None = None,
    env: Mapping[str, str] | None = None,
    log: StrPath | None = None,
) -> str:
    command = [str(x) for x in command]
    if log:
        with Path(log).open("w") as handle:
            result = subprocess.run(command, cwd=cwd, env=env, text=True, check=False,
                                    stdout=handle, stderr=subprocess.STDOUT)
        output = Path(log).read_text()
    else:
        result = subprocess.run(command, cwd=cwd, env=env, text=True, check=False,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        output = result.stdout
    if result.returncode:
        raise RuntimeError(f"Command failed ({result.returncode}): {command}\n"
                           + (f"See {log}\n" if log else "") + output[-8000:])
    return output


def digest(path: StrPath) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def write_json(path: StrPath, value: object) -> None:
    Path(path).write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def read_json(path: StrPath) -> Any:
    return json.loads(Path(path).read_text())


def toml_value(value: object) -> str:
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, bool):
        return str(value).lower()
    if isinstance(value, list):
        return "[" + ", ".join(map(toml_value, value)) + "]"
    if isinstance(value, dict):
        return "{ " + ", ".join(f"{json.dumps(k)} = {toml_value(v)}"
                                  for k, v in value.items()) + " }"
    return str(value)


def environment() -> dict[str, str]:
    # Avoid ambient flags, logging and library overrides changing an experiment.
    allowed = ("PATH", "HOME", "CARGO_HOME", "RUSTUP_HOME", "RUSTUP_TOOLCHAIN",
               "JAVA_HOME", "LIBCLANG_PATH", "LD_LIBRARY_PATH")
    env = {k: os.environ[k] for k in allowed if k in os.environ}
    env.update(LC_ALL="C", TZ="UTC", CARGO_INCREMENTAL="0")
    return env


def make_drivers(tree: Path, directory: Path, fixtures: Sequence[str]) -> None:
    directory.mkdir(exist_ok=True)
    members: list[str] = []
    for name in fixtures:
        fixture = FIXTURES[name]
        package = directory / name
        package.mkdir(exist_ok=True)
        config = tomllib.loads((tree / fixture / "Cargo.toml").read_text())
        dependencies = config["build-dependencies"]
        for value in dependencies.values():
            if isinstance(value, dict) and "path" in value:
                value["path"] = str((tree / fixture / value["path"]).resolve())
        text = (f'[package]\nname = "bench-{name}"\nversion = "0.0.0"\n'
                'edition = "2021"\nbuild = false\n\n[[bin]]\n'
                f'name = "bench-{name}"\npath = {toml_value(str(tree / fixture / "build.rs"))}\n'
                '\n[dependencies]\n')
        text += "\n".join(f"{k} = {toml_value(v)}" for k, v in dependencies.items())
        text += "\n\n[features]\n"
        text += "\n".join(f"{k} = {toml_value(v)}" for k, v in config.get("features", {}).items())
        (package / "Cargo.toml").write_text(text + "\n")
        members.append(name)
    (directory / "Cargo.toml").write_text(
        f'[workspace]\nmembers = {toml_value(members)}\nresolver = "2"\n'
        '[profile.release]\nopt-level = 3\ndebug = true\nincremental = false\n'
        '[profile.release.build-override]\nopt-level = 3\ndebug = true\n')


def runtime_env(
    env: Mapping[str, str],
    stage: Path,
    target: str,
    pointer_width: str,
    benchmark: bool = True,
) -> dict[str, str]:
    result = dict(env, OUT_DIR=str(stage / "out"), TARGET=target, HOST=target,
                  CARGO_CFG_TARGET_POINTER_WIDTH=pointer_width,
                  CARGO_MANIFEST_DIR=str(stage), PROFILE="release", OPT_LEVEL="3")
    if benchmark:
        result["FLAPIGEN_BENCHMARK"] = "1"
    return result


def generated_hashes(stage: Path) -> FileHashes:
    return {str(p.relative_to(stage)): digest(p) for p in sorted(stage.rglob("*"))
            if p.is_file() and p.relative_to(stage).parts[0] != "src"
            and p.name != "jni_c_header.rs"}


def parse_callgrind(path: StrPath) -> Metrics:
    text = Path(path).read_text()
    events = re.search(r"^events: (.+)$", text, re.MULTILINE)
    totals = re.search(r"^totals: (.+)$", text, re.MULTILINE)
    if not events or not totals:
        raise ValueError(f"Missing Callgrind events/totals in {path}")
    names, counts = events.group(1).split(), list(map(int, totals.group(1).split()))
    if len(names) != len(counts) or "Ir" not in names:
        raise ValueError(f"Invalid Callgrind totals in {path}")
    return dict(zip(names, counts))


def parse_massif(path: StrPath) -> Metrics:
    snapshots: list[Metrics] = []
    for block in re.split(r"^snapshot=\d+\s*$", Path(path).read_text(), flags=re.MULTILINE)[1:]:
        values = dict(re.findall(r"^(mem_heap_B|mem_heap_extra_B|mem_stacks_B)=(\d+)$",
                                 block, re.MULTILINE))
        if len(values) != 3:
            raise ValueError(f"Incomplete Massif snapshot in {path}")
        snapshots.append({k: int(v) for k, v in values.items()})
    if not snapshots:
        raise ValueError(f"Missing Massif snapshots in {path}")
    return {"heap_bytes": max(s["mem_heap_B"] for s in snapshots),
            "heap_with_overhead_bytes": max(s["mem_heap_B"] + s["mem_heap_extra_B"]
                                            for s in snapshots)}


def reset_stage(stage: Path, fixture: str, cache: StrPath | None = None) -> None:
    if stage.exists():
        shutil.rmtree(stage)
    stage.mkdir()
    shutil.copytree(ROOT / FIXTURES[fixture] / "src", stage / "src")
    (stage / "out").mkdir()
    if fixture == "cpp":
        (stage / "c++/rust_interface").mkdir(parents=True)
    elif fixture == "jni":
        (stage / "java/com/example/rust").mkdir(parents=True)
    if cache:
        shutil.copy2(cache, stage / "out/jni_c_header.rs")


def source_hashes(fixtures: Sequence[str]) -> FileHashes:
    paths = [p for p in (ROOT / "macroslib/src").rglob("*") if p.is_file()]
    paths += [ROOT / "macroslib/Cargo.toml", ROOT / "macroslib/build.rs"]
    for fixture in fixtures:
        directory = ROOT / FIXTURES[fixture]
        paths += [directory / "build.rs", directory / "Cargo.toml"]
        paths += [p for p in (directory / "src").rglob("*") if p.is_file()]
    return {str(p.relative_to(ROOT)): digest(p) for p in sorted(set(paths))}


def run(args: argparse.Namespace) -> None:
    parent = args.output_dir.resolve()
    parent.mkdir(parents=True, exist_ok=True)
    # Refuse simultaneous runs: they would race on staging and Cargo manifests.
    lock = parent / ".running"
    with lock.open("x") as handle:
        handle.write(str(os.getpid()) + "\n")
    try:
        run_locked(args, parent)
    finally:
        lock.unlink()


def run_locked(args: argparse.Namespace, parent: Path) -> None:
    env = environment()
    revision = execute(["git", "rev-parse", "HEAD"], cwd=ROOT).strip()
    short = execute(["git", "rev-parse", "--short", "HEAD"], cwd=ROOT).strip()
    patch = execute(["git", "diff", "HEAD", "--"], cwd=ROOT)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S.%fZ")
    output = parent / f"{timestamp}-{short}{'-dirty' if patch else ''}"
    output.mkdir()
    print(f"Results: {output}", flush=True)
    results: BenchmarkResults = {
        "schema": 1,
        "status": "running",
        "repetitions": args.repetitions,
        "metadata": {},
        "samples": [],
    }
    write_json(output / "results.json", results)
    try:
        rust = execute(["rustc", "-vV"], env=env)
        target = cast(re.Match[str], re.search(r"^host: (.+)$", rust, re.MULTILINE)).group(1)
        cfg = execute(["rustc", "--print", "cfg"], env=env)
        pointer_width = cast(re.Match[str], re.search(r'target_pointer_width="(\d+)"', cfg)).group(1)
        meta: dict[str, Any] = {
            "revision": revision,
            "short_sha": short,
            "dirty": bool(patch),
            "timestamp": timestamp,
            "rust": rust,
            "cargo": execute(["cargo", "--version"], env=env).strip(),
            "valgrind": execute(["valgrind", "--version"], env=env).strip(),
            "system": execute(["uname", "-a"]).strip(),
            "target": target,
            "pointer_width": pointer_width,
            "environment": env,
            "callgrind": CALLGRIND,
            "massif": MASSIF,
            "fixtures": args.fixtures,
            "tools": args.tools,
            "harness_sha256": digest(__file__),
            "sources": source_hashes(args.fixtures),
            "build_profile": {"opt-level": 3, "debug": True, "incremental": False},
        }
        results["metadata"] = meta
        (output / "checkout.patch").write_text(patch)
        (output / "git-status.txt").write_text(execute(["git", "status", "--short"], cwd=ROOT))
        shutil.copy2(__file__, output / Path(__file__).name)
        drivers = parent / "build"
        make_drivers(ROOT, drivers, args.fixtures)
        lockfile = drivers / "Cargo.lock"
        if not lockfile.exists() and (ROOT / "Cargo.lock").is_file():
            shutil.copy2(ROOT / "Cargo.lock", lockfile)
        command: list[StrPath] = ["cargo", "build", "--release", "--workspace", "--manifest-path",
                                  drivers / "Cargo.toml"]
        if args.offline:
            command.append("--offline")
        write_json(output / "build-command.json", {"argv": list(map(str, command)), "env": env})
        print("Building optimized build-script executables", flush=True)
        execute(command, env=env, log=output / "build.log")
        manifests = output / "manifests"
        manifests.mkdir()
        shutil.copy2(lockfile, output / "Cargo.lock")
        shutil.copy2(drivers / "Cargo.toml", manifests / "Cargo.toml")
        for fixture in args.fixtures:
            shutil.copy2(drivers / fixture / "Cargo.toml", manifests / f"{fixture}.toml")
        meta["packages"] = sorted(
            [{k: p[k] for k in ("name", "version", "source", "checksum") if k in p}
             for p in tomllib.loads(lockfile.read_text())["package"]],
            key=lambda p: (p["name"], p["version"]))
        meta["executables"] = {f: digest(drivers / f"target/release/bench-{f}")
                               for f in args.fixtures}
        stage = parent / "stage"
        cache: Path | None = None
        if "jni" in args.fixtures:
            cache = output / "jni_c_header.rs"
            if args.jni_cache:
                shutil.copy2(args.jni_cache, cache)
                meta["jni_cache_source"] = str(args.jni_cache.resolve())
            else:
                if "JAVA_HOME" not in env:
                    raise RuntimeError("JNI setup needs JAVA_HOME pointing at a JDK, or --jni-cache PATH")
                print("Generating JNI headers outside measurement", flush=True)
                reset_stage(stage, "jni")
                setup_env = runtime_env(env, stage, target, pointer_width, benchmark=False)
                executable = drivers / "target/release/bench-jni"
                write_json(output / "jni-setup-command.json",
                           {"argv": [str(executable)], "cwd": str(stage), "env": setup_env})
                execute([executable], cwd=stage, env=setup_env, log=output / "jni-setup.log")
                shutil.copy2(stage / "out/jni_c_header.rs", cache)
                java_home = Path(env["JAVA_HOME"])
                meta["jni_headers"] = {str(p.relative_to(java_home)): digest(p)
                    for p in sorted((java_home / "include").rglob("*.h"))}
            meta["jni_cache_sha256"] = digest(cache)
        write_json(output / "results.json", results)
        reference: dict[str, FileHashes] = {}
        for repeat in range(args.repetitions):
            for fixture in args.fixtures:
                for tool in args.tools:
                    flags, parser = ((CALLGRIND, parse_callgrind) if tool == "callgrind"
                                     else (MASSIF, parse_massif))
                    label = f"{fixture}-{tool}-{repeat + 1}"
                    print(label, flush=True)
                    reset_stage(stage, fixture, cache if fixture == "jni" else None)
                    profile = output / f"{label}.out"
                    command = ["valgrind", *flags, f"--{tool}-out-file={profile}",
                               drivers / f"target/release/bench-{fixture}"]
                    run_env = runtime_env(env, stage, target, pointer_width)
                    write_json(output / f"{label}.command.json",
                               {"argv": list(map(str, command)), "cwd": str(stage), "env": run_env})
                    execute(command, cwd=stage, env=run_env, log=output / f"{label}.log")
                    hashes = generated_hashes(stage)
                    if not hashes:
                        raise RuntimeError(f"No generated output: {label}")
                    if fixture in reference and hashes != reference[fixture]:
                        write_json(output / f"{label}.hashes.json", hashes)
                        raise RuntimeError(f"Generated output differs between repetitions: {label}")
                    reference[fixture] = hashes
                    results["samples"].append({
                        "fixture": fixture,
                        "tool": tool,
                        "repeat": repeat + 1,
                        "metrics": parser(profile),
                        "output_hashes": hashes,
                    })
                    write_json(output / "results.json", results)
        if source_hashes(args.fixtures) != meta["sources"]:
            raise RuntimeError("Sources changed during measurement; discard this run")
        results["status"] = "complete"
    except BaseException as error:
        results["status"] = "failed"
        results["error"] = f"{type(error).__name__}: {error}"
        raise
    finally:
        write_json(output / "results.json", results)
    print(f"Saved {output / 'results.json'}", flush=True)


def load_results(path: Path) -> tuple[Path, BenchmarkResults]:
    path = path / "results.json" if path.is_dir() else path
    doc = read_json(path)
    if doc.get("schema") != 1:
        raise ValueError(f"Unsupported results schema: {path}")
    return path, cast(BenchmarkResults, doc)


def compare(args: argparse.Namespace) -> None:
    documents = [load_results(path) for path in args.results]
    base_meta = documents[0][1]["metadata"]
    groups: dict[tuple[str, str, str, int], list[int]] = {}
    hashes: dict[str, FileHashes] = {}
    warnings: list[str] = []
    for index, (path, doc) in enumerate(documents, 1):
        if doc["status"] != "complete":
            warnings.append(f"Run {index} is {doc['status']}: {doc.get('error', 'no error recorded')}")
        meta = doc["metadata"]
        expected = doc["repetitions"] * len(meta.get("fixtures", [])) * len(meta.get("tools", []))
        identities = {(s["fixture"], s["tool"], s["repeat"]) for s in doc["samples"]}
        if len(doc["samples"]) != expected or len(identities) != expected:
            warnings.append(f"Run {index} has an incomplete or duplicate sample matrix")
        for key in ("rust", "cargo", "valgrind", "system", "target", "pointer_width",
                    "callgrind", "massif", "harness_sha256", "build_profile", "environment",
                    "packages", "jni_cache_sha256", "fixtures", "tools"):
            if meta.get(key) != base_meta.get(key):
                warnings.append(f"Run {index} differs from baseline in {key}")
        baseline_inputs = {k: v for k, v in base_meta.get("sources", {}).items()
                           if not k.startswith("macroslib/")}
        inputs = {k: v for k, v in meta.get("sources", {}).items() if not k.startswith("macroslib/")}
        if inputs != baseline_inputs:
            warnings.append(f"Run {index} has different fixture inputs/build scripts")
        for sample in doc["samples"]:
            fixture = sample["fixture"]
            if fixture in hashes and hashes[fixture] != sample["output_hashes"]:
                warning = f"Run {index}: generated output differs for {fixture}"
                if warning not in warnings:
                    warnings.append(warning)
            hashes.setdefault(fixture, sample["output_hashes"])
            for metric, value in sample["metrics"].items():
                groups.setdefault((fixture, sample["tool"], metric, index), []).append(value)
    lines = ["# Build-script benchmark comparison", ""]
    for index, (path, doc) in enumerate(documents, 1):
        meta = doc["metadata"]
        lines.append(f"- Run {index}: `{path.parent.name}`; HEAD `{meta.get('short_sha', '?')}`; "
                     f"{doc['status']}; {doc['repetitions']} repetition(s).")
    if warnings:
        lines += ["", "## Comparison cautions", ""] + [f"- {w}" for w in warnings]
    else:
        lines += ["", "Workload/tool metadata and generated output match across runs."]
    lines += ["", "Instructions describe CPU work, not elapsed time; cache events are simulated.",
              "Memory is heap usage, not RSS; stack usage is excluded. Run 1 is the baseline.", "",
              "| Name | Tool / metric | Run | N | Median | Min–max | min=max | Change | Interpretation |",
              "|---|---|---:|---:|---:|---:|---|---:|---|"]
    for (fixture, tool, metric, index), values in sorted(groups.items()):
        baseline = groups.get((fixture, tool, metric, 1))
        median = statistics.median(values)
        change, verdict = "n/a", "no baseline"
        if baseline:
            baseline_median = statistics.median(baseline)
            if baseline_median:
                change = f"{100 * (median / baseline_median - 1):+.6f}%"
            overlap = max(min(values), min(baseline)) <= min(max(values), max(baseline))
            verdict = "baseline" if index == 1 else ("inconclusive (ranges overlap)" if overlap
                      else "lower" if median < baseline_median else "higher")
        if len(values) == 1 or (baseline and len(baseline) == 1):
            verdict += "; repeatability untested"
        median_text = f"{median:,}".removesuffix(".0")
        lines.append(f"| {fixture} | {tool} / {metric} | {index} | {len(values)} | {median_text} | "
                     f"{min(values)}–{max(values)} | {len(set(values)) == 1} | {change} | {verdict} |")
    lines += ["", "Negative changes mean less measured work or memory. No automatic winner is selected.",
              "Counts need not be identical: assess the observed ranges before attributing a difference to code."]
    report = "\n".join(lines) + "\n"
    if args.output:
        args.output.write_text(report)
    else:
        print(report)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    runner = commands.add_parser("run", help="build and measure the current checkout")
    runner.add_argument("--output-dir", type=Path, default=ROOT / "target/buildscript-benchmarks")
    runner.add_argument("--fixtures", nargs="+", choices=FIXTURES, default=list(FIXTURES))
    runner.add_argument("--tools", nargs="+", choices=("callgrind", "massif"),
                        default=["callgrind", "massif"])
    runner.add_argument("--repetitions", type=int, default=5)
    runner.add_argument("--jni-cache", type=Path)
    runner.add_argument("--offline", action="store_true")
    runner.set_defaults(function=run)
    comp = commands.add_parser("compare", help="compare result directories or JSON files")
    comp.add_argument("results", nargs="+", type=Path)
    comp.add_argument("--output", type=Path)
    comp.set_defaults(function=compare)
    args = parser.parse_args()
    if getattr(args, "repetitions", 1) < 1:
        parser.error("--repetitions must be positive")
    for key in ("fixtures", "tools"):
        if hasattr(args, key) and len(set(getattr(args, key))) != len(getattr(args, key)):
            parser.error(f"--{key} must not contain duplicates")
    try:
        args.function(args)
    except (RuntimeError, ValueError, OSError, KeyError) as error:
        parser.exit(1, f"error: {error}\n")


if __name__ == "__main__":
    main()
