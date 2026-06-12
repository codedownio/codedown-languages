#!/usr/bin/env python3
"""Build a crate -> [features] map from a crates.io-index checkout.

The index stores, for every crate, one JSON object per published version. Each
object carries the crate's `features` map (and `features2` for newer dep:/weak
feature syntax). We extract the feature names for the latest non-yanked version
of every crate that has any, keyed by the crate's (lowercased) index file name --
which is exactly how modules/kernels/rust/all_package_names.nix names them.

Crates with no features are omitted to keep the map small; callers treat a miss
as "no known features" (free-form input).

Usage: build_crates_features.py <index-dir> <out.json>
"""
import json
import os
import sys

index_dir = sys.argv[1]
out_path = sys.argv[2]

db = {}
for root, dirs, files in os.walk(index_dir):
    # Skip VCS / metadata dirs; crate files live in 1/, 2/, 3/x/, ab/cd/ shards.
    dirs[:] = [d for d in dirs if not d.startswith(".")]
    for fn in files:
        if fn == "config.json" or "." in fn:
            # Crate files have no extension; this skips README.md, config.json, etc.
            continue
        path = os.path.join(root, fn)
        try:
            with open(path, encoding="utf-8") as f:
                lines = [l for l in f.read().splitlines() if l.strip()]
        except (OSError, UnicodeDecodeError):
            continue
        if not lines:
            continue

        # Prefer the latest non-yanked version; fall back to the latest line.
        chosen = None
        for line in reversed(lines):
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            if chosen is None:
                chosen = obj
            if not obj.get("yanked", False):
                chosen = obj
                break
        if not chosen or "name" not in chosen:
            continue

        feats = set((chosen.get("features") or {}).keys())
        feats |= set((chosen.get("features2") or {}).keys())
        if feats:
            db[fn] = sorted(feats)

with open(out_path, "w", encoding="utf-8") as f:
    json.dump(db, f, separators=(",", ":"), sort_keys=True)
