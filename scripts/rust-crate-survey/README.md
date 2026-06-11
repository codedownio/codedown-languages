# Rust crate survey

Measures how many popular crates build and compile out of the box in the Rust
(evcxr) kernel — i.e. how much of the "native-dependency whack-a-mole" remains
after wiring `defaultCrateOverrides` into the evcxr wrapper.

## How it works

For each crate, two phases:

1. **BUILD** — `nix build .#rustCrateProbe` with the crate as the sole package
   (`rustCrateProbe` is a flake output that reads `CODEDOWN_RUST_PROBE_PACKAGES`).
   Exercises vendoring, lockfile resolution, evcxr-config generation, and the
   wrapper that bakes in the override-derived native deps.

2. **COMPILE** — drive the built evcxr REPL with a trivial cell. The crate is a
   direct dependency, so cargo compiles it and runs its build script at evcxr
   runtime — this is where pkg-config / native-lib failures surface. Success is
   detected by a sentinel `println!` that only runs if the whole crate graph
   compiled.

Each crate is classified: `ok`, `build:not_in_index`, `build:toml_parse`,
`build:other`, `compile:pkg_config`, `compile:build_script`, `compile:link`,
`compile:error`, or a `*:timeout`.

## Usage

```sh
# Survey the top 100 (fetches a filtered, popularity-ranked list)
./survey.py --top 100 --jobs 4

# Specific crates
./survey.py --crates serde,plotters,rusqlite

# From a file (one crate per line, # comments allowed)
./fetch_crates.py --top 200 --sort recent-downloads > crates.txt
./survey.py --file crates.txt --jobs 6 --out-dir runs/$(date +%F)
```

Outputs (default `./results/`):
- `results.csv` — streamed per-crate rows (`crate,status,seconds,detail`)
- `summary.json` — totals, status histogram, and the failing crates
- `logs/<crate>.{build,compile}.txt` — full logs for debugging

## Caveats

- **Popularity vs. relevance.** Raw download rankings are dominated by transitive
  crates (`syn`, `hashbrown`, the `windows_*` shims) that nearly always "just
  work", which understates real pain. `fetch_crates.py` drops `-sys`/`-macro`/
  platform-shim crates by default so the sample is closer to crates a user would
  actually `:dep`. Use `--no-filter` for the raw ranking.
- **Pinned index.** The kernel vendors against a fixed crates.io-index snapshot
  (see `modules/kernels/rust/evcxr/withPackages.nix`). Crates published after the
  snapshot show up as `build:not_in_index` — a tooling artifact, not a real
  failure. Bump the snapshot to retest those.
- **Host compiler.** The COMPILE phase compiles on the host, so a C compiler must
  be on PATH (it is in the dev shell). This faithfully exercises the wrapper's
  native-dep handling, which is what the survey is about.
- When the survey turns up an uncovered `-sys` crate, add a one-line entry to
  `extraCrateOverrides` in `modules/kernels/rust/evcxr/default.nix` (it can carry
  `nativeBuildInputs`, `buildInputs`, and scalar env vars like `LIBCLANG_PATH`).
- **Feature-flag crates.** A few crates can't be fixed by native deps alone
  because they default to a build mode that doesn't work against read-only
  vendored sources. `rdkafka` is the canonical example: it compiles a bundled
  librdkafka unless its `dynamic-linking` feature is on. Request it as
  `{ name = "rdkafka"; features = ["dynamic-linking"]; }` (a plain `rdkafka`
  will fail). The survey only tests plain crate names, so such crates show up as
  failures even though a feature-enabled spec works.
