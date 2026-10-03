# Vendored julia.withPackages

`default.nix`, `depot.nix`, `package-closure.nix`, `util.nix`, `stdlib-infos.nix`,
`extra-libs.nix`, `extra-python-packages.nix`, `resolve_packages.jl`,
`extract_artifacts*.jl` and `python/` are copied from nixpkgs
`pkgs/development/julia-modules`, at rev `e7215ec9581d62b85a3b1b870cd40cc353a06816`
(the nixpkgs we pin in `flake.lock`).

They carry two build-speed patches that aren't upstream yet:

* `package-closure.nix`: symlink the augmented registry into the depot instead of
  `Pkg.Registry.add()`, which recursively copies all ~52k files of the General
  registry (~250 MB) before resolution can start.
* `depot.nix` + `default.nix`: keep Julia's own depots on `JULIA_DEPOT_PATH`, so we
  reuse the precompiled stdlib caches that ship in `$prefix/share/julia/compiled`
  instead of rebuilding Pkg and its stdlib dependencies from scratch every build.

Once those land upstream, delete these files and set `juliaWithPackagesBase` in
`../default.nix` back to `julia.withPackages`.

Not copied from nixpkgs: its `tests/` directory, which we never wired up here.

`registry.nix`, `package-names.nix`, `generate-package-names.nix`,
`generate_packages_names.sh` and `indexing/` are ours, not vendored.
