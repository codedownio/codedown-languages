{ callPackage
, lib
, fetchgit
, fetchFromGitHub
, symlinkJoin
, system
, writeTextDir
, writeText

, pkgsStable
, pkgsUnstable
, pkgsMaster

, name ? "codedown-environment"
}:

config:

with lib;

let
  evaluateConfig = callPackage ./evaluate-config.nix {
    pkgs = pkgsStable;
  };

  evaluated = evaluateConfig config;

  builtKernels = mapAttrs (_: kernel:
    kernel.overrideAttrs (old: {
      passthru = old.passthru // {
        name = kernel.name;
        # channel = kernel.channel;
        channel = "codedown";
      };
    })) evaluated.config.builtKernels;
  builtShells = evaluated.config.builtShells;
  builtExporters = evaluated.config.builtExporters;

  repls = let
    shellToReplInfo = shell: {
      name = shell.name;
      display_name = shell.meta.displayName;
      attr = shell.meta.attr;
      args = shell.meta.args;
      icon = shell.meta.icon;
    };
  in
    map shellToReplInfo (attrValues builtShells)
    ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (if kernel.passthru ? "repls" then kernel.passthru.repls else {})) (attrValues builtKernels)
  ;

  exporters = concatMap (exporter: exporter.meta.exporterInfos) (attrValues builtExporters);

  uiMetadata = callPackage ./uiMetadata.nix {};

in

symlinkJoin {
  inherit name;
  paths =
    attrValues (evaluated.config.builtKernels)
    ++ lib.optionals (builtins.length repls > 0) [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
    ++ lib.optionals (builtins.length exporters > 0) [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporters))]
    ++ evaluated.config.packages
  ;

  passthru = rec {
    inherit evaluated;

    inherit channels;

    ui_metadata = {
      # channels = lib.mapAttrsToList uiMetadata.mkChannelUiMetadata channels;

      kernels = map uiMetadata.mkKernelUiMetadata (attrValues builtKernels);

      # other_packages = map uiMetadata.mkOtherPackageUiMetadata otherPackages;
    };

    ui_metadata_yaml = writeText "ui-metadata.yaml" (lib.generators.toYAML {} ui_metadata);
  };
}
