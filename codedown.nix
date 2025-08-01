{ pkgsStableSrc
, pkgsStable
, pkgsMasterSrc
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
    builds-forever = pkgsMaster.callPackage ./modules/testing/builds-forever/default.nix {};
    builder-uid = pkgsMaster.callPackage ./modules/testing/builder-uid/default.nix {};
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
    packages =
      (lib.mapAttrs' (n: v: lib.nameValuePair ("kernels." + n) v) everythingEnv.config.builtKernels)
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
  chooseMeta = callPackage ./nix/choose-meta.nix {};

  nixpkgsOverlay = callPackage ./nix/nixpkgs-overlay.nix { inherit searcher; };

  importChannel = maybeBootstrap: name: value:
    let
      imported = import value;
      overlays = [nixpkgsOverlay];
    in
      # Import codedown-languages
      if (builtins.isFunction imported && builtins.hasAttr "isCodeDown" (builtins.functionArgs imported)) then imported ({ inherit system; } // lib.optionalAttrs (maybeBootstrap != null) { inherit (maybeBootstrap) fetchFromGitHub; })
      # Import Nixpkgs
      else if (builtins.isFunction imported && builtins.hasAttr "overlays" (builtins.functionArgs imported)) then imported { inherit overlays system; }
      # Generic import
      else if (builtins.isFunction imported) then imported { inherit system; }
      else imported
  ;

  # Exposed so it's easier to compute build dependencies in the presence of IFD
  inherit pkgsStableSrc pkgsStable pkgsMasterSrc pkgsMaster requiredPackages;
}
