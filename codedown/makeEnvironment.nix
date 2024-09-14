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

channels:

config:

with lib;

let
  importedOverlays = [];

  importedChannels = builtins.mapAttrs (name: value: let
    imported = import value;
  in
    if (builtins.isFunction imported && builtins.hasAttr "overlays" (builtins.functionArgs imported)) then imported { overlays = importedOverlays; inherit system; }
    else if (builtins.isFunction imported) then imported { inherit fetchgit fetchFromGitHub; }
    else imported
  ) channels;

  evaluated = lib.evalModules {
    modules = [
      ../modules/base.nix

      ../exporters/module.nix

      ../languages/bash/module.nix

      ../shells/bash/module.nix
      ../shells/fish/module.nix
      ../shells/zsh/module.nix

      {
        config = {
          pkgs = pkgsStable;
        };
      }
      {
        inherit config;
      }
    ];
  };

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
