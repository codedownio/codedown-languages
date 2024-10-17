{ pkgsStable
, pkgsMaster
, requiredPackages ? []
, system ? "x86_64-linux"
}:

let
  common = pkgsStable.callPackage ./modules/kernels/common.nix {};

  callPackage = pkgsStable.callPackage;

  lib = pkgsStable.lib;

in

rec {
  spellchecker = pkgsMaster.callPackage ./modules/language_servers/markdown-spellcheck-lsp {};

  testing = {
    builds-forever = pkgsMaster.callPackage ./modules/testing/builds-forever.nix {};
  };

  # Exported so clients can build searchers for other package sets, like "codedown.searcher nixpkgs"
  searcher = common.searcher;

  settingsSchemas = lib.mapAttrs (attr: value: value.meta.settingsSchema or []) kernels;

  evaluateConfig = callPackage ./nix/evaluate-config.nix {
    inherit pkgsStable pkgsMaster;
  };

  everythingConfig = let
    base = evaluateConfig {};
    kernelNames = builtins.attrNames base.options.kernels;
    shellNames = builtins.attrNames base.options.shells;
    exporterNames = builtins.attrNames base.options.exporters;
  in
    builtins.foldl' lib.recursiveUpdate {} (
      (map (n: { kernels.${n}.enable = true; }) kernelNames)
      ++ (map (n: { shells.${n}.enable = true; }) shellNames)
      ++ (map (n: { exporters.${n}.enable = true; }) exporterNames)
    );

  everythingEnv = evaluateConfig everythingConfig;

  packageSearch = common.searcher' {
    # Note that we deliberately don't include "testing" packages in the searcher
    packages = everythingEnv.config.builtKernels
      // (lib.mapAttrs' (n: v: lib.nameValuePair ("exporters." + n) v) everythingEnv.config.builtExporters)
      // (lib.mapAttrs' (n: v: lib.nameValuePair n v) everythingEnv.config.packages)
      // { "language-servers.spellchecker" = spellchecker; }
    ;
  };

  kernels = everythingEnv.config.builtKernels;

  makeEnvironment = callPackage ./nix/makeEnvironment.nix {
    inherit pkgsStable pkgsMaster;
  };

  validateEnvironment = callPackage ./nix/validateEnvironment.nix {};

  # Exposed for consumers to pin and use to gather metadata from other channels like Nixpkgs
  chooseInterestingMeta = callPackage ./nix/choose-interesting-meta.nix {};

  # Exposed so it's easier to compute build dependencies in the presence of IFD
  inherit pkgsStable pkgsMaster requiredPackages;
}
