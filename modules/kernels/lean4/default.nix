{ callPackage
, lib
, runCommand
, symlinkJoin

, lean4
, leanPackages

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  kernelName = "lean4";

  displayName = "Lean " + lean4.version;

  repl = callPackage ./repl.nix { inherit lean4; };

  lean4-jupyter = callPackage ./lean4-jupyter.nix { inherit repl; };

  # nixpkgs' buildLakePackage installs the entire Lake tree, build artifacts and all:
  # batteries ships a 272 MB `runLinter`, importGraph a 257 MB executable, proofwidgets a
  # 316 MB npm tree for its TypeScript widgets, and each package carries its intermediate C
  # under .lake/build/ir. None of that is reachable from a notebook. Dropping it takes the
  # set from 1.2 GB to under 300 MB.
  #
  # The .lean sources stay. Jump to definition into a library resolves the module name
  # through LEAN_SRC_PATH rather than through the .olean, and comes back empty if the
  # sources aren't there -- see "jumps into a package's source" in the Lean 4 spec.
  #
  # Lake's .trace files go too. They're incrementality bookkeeping that nothing reads unless
  # you `lake build` in the tree, and each one names the toolchain it was built by -- which
  # drags leanPackages' own 4.29.0 (a 3.1 GB closure) into an environment that already has
  # 4.29.1 and never runs it.
  slimLakePackage = pkg: runCommand "${pkg.name}-slim" {
    inherit (pkg) version;
    meta = pkg.meta // { inherit (pkg) version; };
  } ''
    cp -r --no-preserve=mode,ownership ${pkg} $out
    rm -rf "$out/.lake/build/bin" "$out/.lake/build/ir" "$out/bin" "$out/widget"
    find "$out" -name '*.trace' -delete
  '';

  # leanPackages.lean4 is the package set's own toolchain (4.29.0), not a library, so it
  # isn't something to offer. It doesn't have to match ours either: oleans are compatible
  # across a Lean patch series, and 4.29.0's load fine under the 4.29.1 we run.
  packageOptions = lib.mapAttrs (_: slimLakePackage) (
    lib.filterAttrs (n: v: n != "lean4" && lib.isDerivation v) leanPackages
  );
  packageSearch = common.searcher packageOptions;

  chosenPackages = map (x: lib.getAttr x packageOptions) packages;

  languageServers =
    []
    ++ lib.optionals settings.lsp.lean.enable [(callPackage ./language-server-lean/default.nix {
      inherit attrs chosenPackages extensions kernelName lean4;
    })]
  ;

  kernel = callPackage ./kernel.nix {
    inherit attrs chosenPackages displayName extensions lean4 lean4-jupyter repl;
  };

in

symlinkJoin {
  name = "lean4-${lean4.version}";

  paths = [
    kernel

    lean4
    repl
  ]
  ++ languageServers
  ;

  passthru = {
    meta = lean4.meta // {
      baseName = "lean4";
      inherit displayName settingsSchema;
      inherit (lean4) version;
      icon = ./lean4-logo-64x64.png;
      iconMonochrome = ./lean4-monochrome.svg;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      lean4 = lean4.version;
      repl = (import ./repl-version.nix).rev;
    };
    inherit settings settingsSchema;
    inherit (kernel) repls;
    modes = {
      inherit attrs extensions;
      # What lean4_jupyter itself reports in language_info.codemirror_mode, so passthru has
      # to agree with it. There's no Lean mode in stock CodeMirror -- the upstream kernel
      # ships a JupyterLab extension to register one, which isn't usable from here.
      code_mirror_mode = "lean4";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    # Lean has no official formatter and `lean --server` advertises no formatting provider,
    # so this stays empty and the Formatting column stays blank.
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
