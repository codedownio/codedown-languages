{ callPackage
, lib
, fetchgit
, fetchFromGitHub
, symlinkJoin
, system
, writeTextDir
, writeText

, pkgsStable
, pkgsMaster

, name ? "codedown-environment"
}:

config:

with lib;

let
  evaluated = (callPackage ./evaluate-config.nix { inherit pkgsStable pkgsMaster; }) config;

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
      inherit (shell.meta) attr args icon;
    };
  in
    map shellToReplInfo (attrValues builtShells)
    ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (kernel.repls or {})) (attrValues builtKernels)
  ;

  exporters = concatMap (exporter: exporter.meta.exporterInfos) (attrValues builtExporters);

in

symlinkJoin {
  inherit name;
  paths =
    attrValues (evaluated.config.builtKernels)
    ++ lib.optionals (builtins.length repls > 0) [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
    ++ lib.optionals (builtins.length exporters > 0) [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporters))]
    ++ evaluated.config.packages
    ++ map (x: x.contents) evaluated.config.labeledPackages
  ;

  passthru = rec {
    inherit evaluated;

    inherit channels;

    ui_metadata = let
      uiMetadata = callPackage ./uiMetadata.nix {};
    in
      {
        # channels = lib.mapAttrsToList (name: channel: channel // {
        #   name = name;
        # }) channels;
        channels = [];

        kernels = map uiMetadata.mkKernelUiMetadata (attrValues builtKernels);

        other_packages = map (p: {
          channel = p.channel;
          attr = p.attr;
          meta = if p.contents ? "meta" then uiMetadata.chooseInterestingMeta p.contents else {};
        }) evaluated.config.labeledPackages;
      };

    ui_metadata_yaml = writeText "ui-metadata.yaml" (lib.generators.toYAML {} ui_metadata);
  };
}
