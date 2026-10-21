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

  # buildLakePackage installs the whole Lake tree, build artifacts and all -- 1.2 GB for
  # the set, mostly test executables and intermediate C. Keep the sources: jump to
  # definition resolves through LEAN_SRC_PATH and comes back empty without them. Drop the
  # .trace files too; each names the toolchain that built it, pulling leanPackages' own
  # 4.29.0 (3.1 GB) into an environment that already has 4.29.1. Leaves ~300 MB.
  slimLakePackage = pkg: runCommand "${pkg.name}-slim" {
    inherit (pkg) version;
    meta = pkg.meta // { inherit (pkg) version; };
  } ''
    cp -r --no-preserve=mode,ownership ${pkg} $out
    rm -rf "$out/.lake/build/bin" "$out/.lake/build/ir" "$out/bin" "$out/widget"
    find "$out" -name '*.trace' -delete
  '';

  # leanPackages.lean4 is the set's own toolchain, not a library. It needn't match ours:
  # oleans are compatible across a patch series, so its 4.29.0 loads under our 4.29.1.
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
      # Matches what lean4_jupyter reports. No such mode in stock CodeMirror; upstream
      # ships a JupyterLab extension registering one, which isn't usable from here.
      code_mirror_mode = "lean4";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    # Lean has no formatter and the server advertises none, so this stays empty.
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
