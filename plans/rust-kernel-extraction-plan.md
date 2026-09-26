# Plan: extract the evcxr / crates.io-pin layer into its own repo

## Motivation

The part of the Rust kernel that depends on the pinned crates.io index — vendoring,
the `defaultCrateOverrides`-derived native-dep wrapper, the supplement, the feature
extraction, and the crate survey — is self-contained and benefits from independent CI.
We want: **bump the pin → CI runs the survey → codedown-languages adopts the new pin by
bumping a flake input.** This isolates the thing that needs verification behind a stable
interface and keeps the survey harness next to what it tests.

## Proposed repo

Name: `evcxr-nix` (or `codedown-rust`). A flake that owns everything derived from the pin.

### What moves out of codedown-languages

- `modules/kernels/rust/evcxr/**`
  - `default.nix` — the wrapper builder (override-driven native deps, env-var +
    `LD_LIBRARY_PATH` propagation, `extraCrateOverrides` supplement)
  - `withPackages.nix` — vendoring, `crate-names.nix` emission, **the crates.io-index pin**
  - `python/build_init_evcxr.py`
- `scripts/rust-crate-survey/**` — becomes the repo's CI
- (with Q1 done) the `featuresOf` / feature-extraction helper, since it reads the same pin

### What stays in codedown-languages (consumes the new flake)

- `modules/kernels/rust/{default.nix, kernel.nix, module.nix, all_package_names.nix}`
- `modules/kernels/rust/language_server_rust_analyzer/**` (rust-analyzer LSP)
- the package searcher / `packageOptions` integration, `makeEnvironment` wiring
- the Haskell kernel tests (they need the full integrated env)

## Interface

The new flake exposes a small, stable surface (per system):

```nix
lib.${system} = {
  # Build a wrapped evcxr for a package set. Returns the evcxr derivation with
  # passthru { cargoHome; cratesIndex; }.
  evcxrWithPackages = { cargo, rustPlatform, packages }: <drv>;

  # The pinned index + helpers derived from it (for Q1 feature extraction).
  cratesIndex = <fetchFromGitHub source>;
  featuresOf = crateName: [ <feature strings> ];
};
```

codedown's `modules/kernels/rust/default.nix` changes from
`(callPackage ./evcxr {...}).override {...}.withPackages packages`
to `rustNix.lib.${system}.evcxrWithPackages { inherit cargo rustPlatform packages; }`.

## Design decisions

- **nixpkgs as a `follows` input.** codedown controls the nixpkgs version so that
  `defaultCrateOverrides` (and the toolchain) stay consistent across both repos. The
  `extraCrateOverrides` supplement lives in the new repo but resolves against the
  consumer's nixpkgs.
- **CI = the survey.** Reduced top-N (e.g. top-100) on every PR for speed; full top-1000
  on pin bumps / nightly. The survey tests `evcxrWithPackages [crate]` directly (build
  wrapper + drive the REPL) — no `makeEnvironment`/LSP — so CI is lighter than the runs
  done in codedown-languages.
- **Probe.** Move the `rustCrateProbe` concept into the new repo, simplified to the
  wrapper-only path the survey needs.

## Costs / caveats

- Cross-repo IFD (cargoNix, crate-names, featuresOf) works but needs
  `allow-import-from-derivation` (already enabled here).
- A wrapper-interface change becomes a two-repo change — but the interface is tiny and stable.
- One more flake input for codedown to bump; the survey's green run is the adoption gate.

## Rough steps

1. Scaffold the new flake; move `evcxr/**` in; expose `evcxrWithPackages` + `cratesIndex`
   (+ `featuresOf` once Q1 lands).
2. Move `scripts/rust-crate-survey/**`; point it at `evcxrWithPackages` instead of the
   codedown `rustCrateProbe`.
3. Add GitHub Actions: `nix build` smoke + survey (top-N on PR, top-1000 on pin bump).
4. In codedown-languages: add the flake input (nixpkgs `follows`), rewrite
   `modules/kernels/rust/default.nix` to consume it, drop the moved files.
5. Verify: sample env builds, Haskell Rust kernel tests pass, data-science test passes.
