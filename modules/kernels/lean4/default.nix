{ callPackage
, lib
, runCommand
, symlinkJoin

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  leanPkgs = callPackage ./nixpkgs.nix {};
  inherit (leanPkgs) leanPackages;
  # The set's own toolchain, so the packages' oleans match it exactly.
  lean4 = assert lib.assertMsg (leanPackages.lean4.version == version)
    "version.nix is at ${version} but lean4 is ${leanPackages.lean4.version}; run .aliases/dev-update-lean4";
    leanPackages.lean4;

  # Known without importing leanPkgs, so describing the kernel doesn't fetch its nixpkgs.
  version = import ./version.nix;

  kernelName = "lean4";

  displayName = "Lean " + version;

  repl = leanPkgs.callPackage ./repl.nix { inherit lean4; };

  lean4-jupyter = callPackage ./lean4-jupyter.nix { inherit repl; };

  # As mathlib's own, which packs without zstd; stock leantar can't read the result.
  leangz-raw = leanPkgs.leangz.overrideAttrs { cargoBuildNoDefaultFeatures = true; };

  # buildLakePackage installs the whole Lake tree: test executables, intermediate C, npm
  # trees. Keep the sources, which jump to definition resolves through LEAN_SRC_PATH. Skip
  # nix-support, whose propagated-build-inputs points back at the unslimmed dependencies.
  # mathlib is unpacked straight from its cached archive, skipping the 8.7 GB full tree.
  slimLakePackage = pkg: let
    packed = pkg.passthru.mathlib__archive or null;
  in runCommand "${pkg.name}-slim" {
    inherit (pkg) version;
    nativeBuildInputs = lib.optional (packed != null) leangz-raw;
    meta = pkg.meta // { inherit (pkg) version; };
  } ''
    mkdir $out
    tar -C ${if packed != null then packed else pkg} -c \
      --exclude=./.lake/build/bin --exclude=./.lake/build/ir --exclude=./bin \
      --exclude=./widget --exclude=./nix-support . \
      | tar -C $out -x
    chmod -R u+w $out
    ${lib.optionalString (packed != null) ''
      oleans=$out/.lake/build/lib/lean
      find $oleans -name '*.ltar' -exec leantar -C $oleans -C $TMPDIR/ir -x {} +
      find $oleans -name '*.ltar' -delete
    ''}
  '';

  libraries = lib.filterAttrs (n: v: lib.isDerivation v && !(lib.elem n ["lean4" "mathlib__archive"])) leanPackages;

  packageOptions = lib.mapAttrs (_: slimLakePackage) libraries;
  packageSearch = common.searcher packageOptions;

  # buildLakePackage hands dependencies on through a setup hook, not in the output, so
  # LEAN_PATH needs the whole closure or `import Aesop` can't find Batteries.
  nameByOutPath = lib.mapAttrs' (n: v: lib.nameValuePair (builtins.unsafeDiscardStringContext v.outPath) n) libraries;
  withDeps = n: [n] ++ map (d: nameByOutPath.${builtins.unsafeDiscardStringContext d.outPath}) (libraries.${n}.allLeanDeps or []);
  chosenPackages = map (n: packageOptions.${n}) (lib.unique (lib.concatMap withDeps packages));

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
  name = "lean4-${version}";

  paths = [
    kernel

    lean4
    repl
  ]
  ++ languageServers
  ;

  passthru = {
    meta = {
      name = "lean4-${version}";
      baseName = "lean4";
      description = "Automatic and interactive theorem prover";
      homepage = "https://leanprover.github.io/";
      changelog = "https://github.com/leanprover/lean4/blob/v${version}/RELEASES.md";
      license = lib.licenses.asl20;
      mainProgram = "lean";
      inherit displayName settingsSchema version;
      icon = ./lean4-logo-64x64.png;
      iconMonochrome = ./lean4-monochrome.svg;
      hasPackages = true;
    };
    inherit packageOptions packageSearch;
    versions = {
      lean4 = version;
      repl = (import ./repl-version.nix).tag;
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
