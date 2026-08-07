"""Warm Jedi's parse cache for the whole environment.

Run at build time with XDG_CACHE_HOME pointed at the derivation output. Jedi parses
lazily and reads/writes pickled parse trees via parso, keyed by absolute path and
invalidated by mtime; nix store paths are immutable with fixed mtimes, so a cache
built here stays valid for every sandbox using the environment.

The cache is a plain per-file parse cache, so rather than chasing what inference
touches (jedi.preload_module only covers a module's top level), parse every Python
source in the environment directly with parso. This also covers the typeshed stubs
bundled with jedi, without importing anything.
"""

import os
import sys
import time

import parso

cache_path = os.path.join(os.environ["XDG_CACHE_HOME"], "jedi")

roots = sorted({p for p in sys.path if p.endswith("site-packages")})
roots.append(os.path.dirname(os.__file__))  # the interpreter's stdlib

grammar = parso.load_grammar()

t0 = time.time()
files = 0
for root in roots:
    print(f"Pre-parsing {root}", flush=True)
    # followlinks: a withPackages env's site-packages is a forest of symlinks into the
    # per-package store paths, and jedi sees files through the env paths
    for dirpath, _dirnames, filenames in os.walk(root, followlinks=True):
        for filename in filenames:
            if not filename.endswith((".py", ".pyi")):
                continue
            path = os.path.join(dirpath, filename)
            try:
                grammar.parse(path=path, cache=True, cache_path=cache_path)
            except Exception as e:
                print(f"  failed to parse {path}: {e}", flush=True)
            files += 1
            # parso keeps every parsed module in an in-process cache too; drop it
            # periodically so this scales to big environments
            if files % 2000 == 0:
                parso.cache.parser_cache.clear()
                print(f"  {files} files, {time.time() - t0:.0f}s", flush=True)

print(f"Pre-parsed {files} files in {time.time() - t0:.0f}s", flush=True)
