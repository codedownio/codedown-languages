# Vendored julia.withPackages

`default.nix`, `depot.nix`, `package-closure.nix`, `util.nix`, `stdlib-infos.nix`,
`extra-libs.nix`, `extra-python-packages.nix`, `resolve_packages.jl`,
`extract_artifacts*.jl` and `python/` are copied from nixpkgs
`pkgs/development/julia-modules`, at rev `fd33cbc554eea069c5fbd63c3019c03cabfcdd44`
(the nixpkgs we pin in `flake.lock`).

They carry one patch that isn't upstream yet:

* `python/minimal_registry.py`: list every package that any version range of a kept
  package's `Deps.toml` or `WeakDeps.toml` mentions. Pkg 1.13 looks up all of those names
  when it builds a package (IJulia's Conda dependency, for one), and fails on any the
  minimal registry left out.

Once that lands upstream, delete these files and set `juliaWithPackagesBase` in
`../default.nix` back to `julia.withPackages`, passing our `registry.nix` as its
`augmentedRegistry`.

Not copied from nixpkgs: its `tests/` directory, which we never wired up here.

`registry.nix`, `package-names.nix`, `generate-package-names.nix`,
`generate_packages_names.sh` and `indexing/` are ours, not vendored. `registry.nix` stays
on an older General registry than nixpkgs': newer ones resolve LanguageServer.jl 6, which
dropped the SymbolServer.jl API our language server config uses, and a ZeroMQ_jll whose
libzmq wants a libsodium soname that no compatible libsodium_jll ships.
