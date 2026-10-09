# deadnix finds dead code: unused callPackage arguments, unused let bindings.
# statix finds anti-patterns; see statix.toml.
#
# Run it with `.aliases/dev-lint-nix` or `nix build .#checks.<system>.lint-nix`.

{ lib
, runCommand

, deadnix
, statix

  # Off until someone runs `statix fix`: ~110 findings, all style rather than bugs.
, enableStatix ? false
}:

let
  # Mirrored by `ignore` in statix.toml; deadnix takes paths where statix takes globs.
  excluded = [
    # node2nix output, marked "Do not edit!".
    "modules/language_servers/diagnostic-languageserver"

    # cabal2nix output. Its headers list test and bench deps the expression never uses.
    "modules/kernels/haskell/language-server-hls/lsp-types.nix"
    "modules/kernels/haskell/language-server-hls/myers-diff.nix"
    "modules/kernels/rust/language_server_rust_analyzer/lsp-types.nix"
    "modules/kernels/rust/language_server_rust_analyzer/myers-diff.nix"

    # Copied from nixpkgs; tidying these would only make the next re-vendor harder to diff.
    # julia-modules/VENDORED.md says which files are theirs. The rest are ours and get linted.
    "modules/kernels/julia/julia-modules/default.nix"
    "modules/kernels/julia/julia-modules/depot.nix"
    "modules/kernels/julia/julia-modules/extra-libs.nix"
    "modules/kernels/julia/julia-modules/extra-python-packages.nix"
    "modules/kernels/julia/julia-modules/package-closure.nix"
    "modules/kernels/julia/julia-modules/stdlib-infos.nix"
    "modules/kernels/julia/julia-modules/util.nix"

    # Dead trees, kept for reference.
    "old_languages"
    "modules/kernels/haskell/old"

    # uv2nix scratch env with its own flake. Its shell.nix is commented out end to end, so
    # it isn't parseable Nix.
    "modules/kernels/python/envs/python314"
  ];

  nixFilesUnder = p: lib.fileset.fileFilter (f: f.hasExt "nix") p;

  # Enumerated, not globbed from the root, so untracked files (tests/test_runs, result
  # symlinks) can't change what gets linted.
  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions ([
      ../codedown.nix
      ../default.nix
      ../flake.nix
      ../sample_environments.nix
      ../statix.toml
    ] ++ map nixFilesUnder [
      ../modules
      ../nix
      ../old_languages
      ../sample_environments
    ]);
  };

in

runCommand "lint-nix" {
  nativeBuildInputs = [deadnix] ++ lib.optional enableStatix statix;
} ''
  cd ${src}

  failed=0

  echo "==> deadnix"
  # --exclude takes every value in one flag and can't be repeated, hence the trailing --.
  deadnix --fail --exclude ${lib.concatMapStringsSep " " lib.escapeShellArg excluded} -- . || failed=1

  ${lib.optionalString enableStatix ''
    echo "==> statix"
    statix check . || failed=1
  ''}

  if [ "$failed" -ne 0 ]; then
    echo
    echo "Most findings can be applied automatically:"
    echo "  deadnix --edit <file>"
    echo "  statix fix <file>"
    echo
    echo "For a wrong finding, put '# deadnix: skip' on the line immediately above it --"
    echo "it must be the nearest comment, or it silences nothing."
    exit 1
  fi

  touch $out
''
