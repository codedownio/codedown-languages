#!/usr/bin/env python3
"""Fetch a popularity-ranked list of crates from the crates.io API.

Writes one crate name per line to stdout (or --out). Used to feed survey.py.

Examples:
    ./fetch_crates.py --top 100 --sort recent-downloads > crates-top100.txt
    ./fetch_crates.py --top 200 --sort downloads --no-filter > crates-raw.txt

By default we drop crates that are almost never typed into a notebook as a
`:dep` -- proc-macro / internal / platform-shim crates -- so the survey measures
something closer to "crates a user would actually add". Pass --no-filter to keep
the raw ranking.
"""
import argparse
import re
import sys
import time
import urllib.parse
import urllib.request

API = "https://crates.io/api/v1/crates"
UA = "codedown-rust-survey (https://github.com/codedownio/codedown-languages)"

# Patterns for crates that dominate raw download rankings but are virtually never
# a direct, user-facing dependency in a notebook.
NOISE_PATTERNS = [
    re.compile(r".*-sys$"),          # native binding shims (pulled in transitively)
    re.compile(r".*-impl$"),         # proc-macro impl halves
    re.compile(r".*-macros?$"),      # derive/macro crates
    re.compile(r".*-derive$"),
    re.compile(r"^windows[-_].*"),   # windows target shims
    re.compile(r"^windows$"),
    re.compile(r"^proc-macro.*"),
    re.compile(r"^syn$|^quote$|^unicode-ident$|^autocfg$|^cfg-if$"),
]


def is_noise(name: str) -> bool:
    return any(p.match(name) for p in NOISE_PATTERNS)


def fetch_page(sort: str, page: int, per_page: int):
    qs = urllib.parse.urlencode({"sort": sort, "per_page": per_page, "page": page})
    req = urllib.request.Request(f"{API}?{qs}", headers={"User-Agent": UA})
    import json
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r).get("crates", [])


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--top", type=int, default=100, help="number of crates to emit (after filtering)")
    ap.add_argument("--sort", default="recent-downloads",
                    choices=["recent-downloads", "downloads"],
                    help="popularity metric (default: recent-downloads, last 90d)")
    ap.add_argument("--no-filter", action="store_true", help="keep raw ranking (don't drop -sys/-macro/etc.)")
    ap.add_argument("--out", help="write to this file instead of stdout")
    args = ap.parse_args()

    names, page, per_page = [], 1, 100
    while len(names) < args.top and page <= 20:
        crates = fetch_page(args.sort, page, per_page)
        if not crates:
            break
        for c in crates:
            name = c["id"]
            if args.no_filter or not is_noise(name):
                names.append(name)
                if len(names) >= args.top:
                    break
        page += 1
        time.sleep(0.3)  # be polite to crates.io

    out = open(args.out, "w") if args.out else sys.stdout
    for n in names[:args.top]:
        out.write(n + "\n")
    if args.out:
        out.close()
        print(f"Wrote {min(len(names), args.top)} crate names to {args.out}", file=sys.stderr)


if __name__ == "__main__":
    main()
