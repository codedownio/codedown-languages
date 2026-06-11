#!/usr/bin/env python3
"""Survey how many crates build and compile out of the box in the Rust kernel.

For each crate the survey runs two phases:

  1. BUILD   - `nix build .#rustCrateProbe` with the crate as the only package.
               Exercises the Nix side: vendoring, lockfile resolution, the
               evcxr-config generation, and the wrapper (which is where the
               defaultCrateOverrides-derived native deps get baked in).

  2. COMPILE - drive the built evcxr REPL with a trivial cell. Because the crate
               is a direct dependency, cargo compiles it (and runs its build
               script) at evcxr runtime -- this is where the pkg-config / native
               library failures surface. Success is detected via a sentinel
               print that only runs if the whole crate graph compiled.

Each crate is classified into one of:
  ok                     - built and compiled
  build:not_in_index     - crate/version absent from the pinned crates.io index
  build:toml_parse       - Cargo.toml the python toml step couldn't parse
  build:other            - some other Nix-build failure
  compile:pkg_config     - build script needed pkg-config / a missing native lib
  compile:build_script   - a build script failed for another reason
  compile:link           - linker couldn't find a library
  compile:error          - ordinary rustc compile error
  compile:timeout / build:timeout

Results stream to results.csv; full logs land in logs/<crate>.{build,compile}.txt.

Usage:
    ./survey.py --top 100                      # fetch + survey the top 100
    ./survey.py --crates serde,plotters,rusqlite
    ./survey.py --file crates.txt --jobs 6
    ./survey.py --top 50 --out-dir runs/2026-06-11

Note: the COMPILE phase compiles on the host, so a C compiler must be on PATH
(it is, in the project dev shell). This is a faithful proxy for the wrapper's
native-dep handling, which is the point of the survey.
"""
import argparse
import concurrent.futures
import csv
import json
import os
import pathlib
import subprocess
import sys
import threading
import time

REPO = pathlib.Path(__file__).resolve().parents[2]
SENTINEL = "PROBE_OK_42"

_print_lock = threading.Lock()


def log(msg):
    with _print_lock:
        print(msg, flush=True)


def classify_build(stderr: str) -> str:
    s = stderr
    if "TomlDecodeError" in s:
        return "build:toml_parse"
    if ("no matching package" in s or "failed to select a version" in s
            or "could not be found in registry" in s or "Couldn't find" in s):
        return "build:not_in_index"
    return "build:other"


def classify_compile(out: str, err: str) -> str:
    s = out + "\n" + err
    low = s.lower()
    if "pkg-config" in low and ("could not be found" in low or "not be found" in low):
        return "compile:pkg_config"
    if "failed to run custom build command" in s:
        # Distinguish a pkg-config miss even when phrased differently.
        if "pkg-config" in low or "pkgconfig" in low:
            return "compile:pkg_config"
        return "compile:build_script"
    if "cannot find -l" in s or "error: linking with" in s or "undefined reference" in s:
        return "compile:link"
    return "compile:error"


def find_evcxr_repl(out_path: str):
    """Locate the evcxr REPL binary inside a built environment."""
    base = pathlib.Path(out_path)
    for kj in base.glob("lib/**/kernel.json"):
        try:
            argv0 = json.load(open(kj))["argv"][0]
        except Exception:
            continue
        repl = os.path.join(os.path.dirname(argv0), "evcxr")
        if os.path.exists(repl):
            return repl
    return None


def run_build(crate: str, timeout: int):
    env = dict(os.environ, CODEDOWN_RUST_PROBE_PACKAGES=json.dumps([crate]))
    try:
        cp = subprocess.run(
            ["nix", "build", "--impure", ".#rustCrateProbe",
             "--no-link", "--print-out-paths"],
            cwd=REPO, env=env, capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired as e:
        return None, "build:timeout", (e.stderr or b"").decode(errors="replace") if isinstance(e.stderr, bytes) else (e.stderr or "")
    if cp.returncode != 0:
        return None, classify_build(cp.stderr), cp.stdout + cp.stderr
    out_path = cp.stdout.strip().splitlines()[-1].strip()
    return out_path, None, cp.stderr


def run_compile(repl: str, timeout: int):
    code = f'println!("{SENTINEL}");\n:quit\n'
    try:
        cp = subprocess.run([repl], input=code, capture_output=True, text=True,
                            timeout=timeout, env=dict(os.environ))
    except subprocess.TimeoutExpired:
        return "compile:timeout", "", "timed out"
    ok = SENTINEL in cp.stdout
    if ok:
        return "ok", cp.stdout, cp.stderr
    return classify_compile(cp.stdout, cp.stderr), cp.stdout, cp.stderr


def survey_one(crate: str, logs_dir: pathlib.Path, build_timeout: int, compile_timeout: int):
    t0 = time.time()
    out_path, build_class, build_log = run_build(crate, build_timeout)
    (logs_dir / f"{crate}.build.txt").write_text(build_log or "")
    if build_class is not None:
        return dict(crate=crate, status=build_class, seconds=round(time.time() - t0, 1),
                    detail="(see build log)")

    repl = find_evcxr_repl(out_path)
    if repl is None:
        return dict(crate=crate, status="build:other", seconds=round(time.time() - t0, 1),
                    detail="no evcxr REPL found in env")

    comp_class, comp_out, comp_err = run_compile(repl, compile_timeout)
    (logs_dir / f"{crate}.compile.txt").write_text((comp_out or "") + "\n--- STDERR ---\n" + (comp_err or ""))
    detail = "" if comp_class == "ok" else first_error_line(comp_err)
    return dict(crate=crate, status=comp_class, seconds=round(time.time() - t0, 1), detail=detail)


def first_error_line(stderr: str) -> str:
    for line in (stderr or "").splitlines():
        l = line.strip()
        if l.startswith("error") or "failed to run custom build" in l or "panicked" in l:
            return l[:200]
    return ""


def load_crates(args):
    if args.crates:
        return [c.strip() for c in args.crates.split(",") if c.strip()]
    if args.file:
        return [l.strip() for l in open(args.file) if l.strip() and not l.startswith("#")]
    # fall back to fetching
    fetch = pathlib.Path(__file__).resolve().parent / "fetch_crates.py"
    out = subprocess.run([sys.executable, str(fetch), "--top", str(args.top), "--sort", args.sort],
                        capture_output=True, text=True, check=True)
    return [l.strip() for l in out.stdout.splitlines() if l.strip()]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    src = ap.add_mutually_exclusive_group()
    src.add_argument("--crates", help="comma-separated crate names")
    src.add_argument("--file", help="file with one crate name per line")
    ap.add_argument("--top", type=int, default=100, help="if fetching, how many crates")
    ap.add_argument("--sort", default="recent-downloads", choices=["recent-downloads", "downloads"])
    ap.add_argument("--jobs", type=int, default=4, help="concurrent crates (default 4)")
    ap.add_argument("--build-timeout", type=int, default=1800)
    ap.add_argument("--compile-timeout", type=int, default=1200)
    ap.add_argument("--out-dir", default=str(pathlib.Path(__file__).resolve().parent / "results"))
    args = ap.parse_args()

    crates = load_crates(args)
    out_dir = pathlib.Path(args.out_dir)
    logs_dir = out_dir / "logs"
    logs_dir.mkdir(parents=True, exist_ok=True)
    csv_path = out_dir / "results.csv"

    log(f"Surveying {len(crates)} crates with {args.jobs} jobs -> {csv_path}")

    results = []
    counter = {"done": 0}
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["crate", "status", "seconds", "detail"])
        writer.writeheader()
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as ex:
            futs = {ex.submit(survey_one, c, logs_dir, args.build_timeout, args.compile_timeout): c
                    for c in crates}
            for fut in concurrent.futures.as_completed(futs):
                r = fut.result()
                results.append(r)
                with _print_lock:
                    writer.writerow(r)
                    f.flush()
                counter["done"] += 1
                mark = "OK " if r["status"] == "ok" else "FAIL"
                log(f"[{counter['done']:>3}/{len(crates)}] {mark} {r['crate']:<28} {r['status']:<22} {r['seconds']}s")

    print_summary(results, out_dir)


def print_summary(results, out_dir):
    from collections import Counter
    by_status = Counter(r["status"] for r in results)
    ok = by_status.get("ok", 0)
    total = len(results)
    log("\n==================== SUMMARY ====================")
    log(f"  total: {total}   ok: {ok} ({100*ok//max(total,1)}%)   failed: {total-ok}")
    for status, n in by_status.most_common():
        if status == "ok":
            continue
        log(f"    {status:<24} {n}")
    failing = sorted([r for r in results if r["status"] != "ok"], key=lambda r: r["status"])
    summary_path = pathlib.Path(out_dir) / "summary.json"
    summary_path.write_text(json.dumps({
        "total": total, "ok": ok,
        "by_status": dict(by_status),
        "failing": [{"crate": r["crate"], "status": r["status"], "detail": r["detail"]} for r in failing],
    }, indent=2))
    log(f"\n  Wrote {summary_path}")
    log("  (per-crate logs under <out-dir>/logs/)")


if __name__ == "__main__":
    main()
