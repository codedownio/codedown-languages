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
  removeNonDefaultSettings = callPackage ./remove-non-default-settings.nix {};

  builtExporters = evaluated.config.builtExporters;
  builtKernels = mapAttrs (_: kernel:
    kernel.overrideAttrs (old: {
      passthru = old.passthru // {
        name = "kernels." + kernel.name;

        # channel = kernel.channel;
        channel = "codedown";

        settings = removeNonDefaultSettings kernel.settingsSchema kernel.settings;
        settingsSchema = mapAttrs (_: v: removeAttrs v ["loc"]) kernel.settingsSchema;
      };
    })) evaluated.config.builtKernels;
  builtLanguageServers = evaluated.config.builtLanguageServers;
  builtShells = evaluated.config.builtShells;

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

  uiMetadata = callPackage ./uiMetadata.nix {};

  mkPackageUiMetadata = let
    # This is duplicated from kernels/common.nix, which we'd rather not import here
    packageName = p: if lib.isString p then p else p.name;

    mkSubPackageMetadata = pkg: p: {
      name = packageName p;
      meta = if lib.hasAttrByPath ["packageOptions" (packageName p)] pkg then uiMetadata.chooseInterestingMeta (pkg.packageOptions.${packageName p}) else {};
    } // (lib.optionalAttrs (lib.isAttrs p && p ? "settings") {
      inherit (p) settings;
    });
  in
    pkg: {
      # Dry
      name = pkg.name;
      settings = if pkg ? "settings" then pkg.settings else {};

      # Different for hydrated
      packages = map (p: mkSubPackageMetadata pkg p) (pkg.settings.packages or []);

      # Hydrated
      meta = uiMetadata.chooseInterestingMeta pkg;
    };

in

symlinkJoin {
  inherit name;
  paths =
    attrValues (evaluated.config.builtKernels)
    ++ attrValues (evaluated.config.builtLanguageServers)
    ++ lib.optionals (builtins.length repls > 0) [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
    ++ lib.optionals (builtins.length exporters > 0) [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporters))]
    ++ evaluated.config.packages
    ++ map (x: x.contents) evaluated.config.labeledPackages
  ;

  passthru = rec {
    inherit evaluated;

    inherit channels;

    ui_metadata = {
      channels = lib.mapAttrsToList (name: channel: channel // {
        name = name;
      }) evaluated.config.channels;

      packages =
        (mapAttrs' (n: v: nameValuePair "exporters.${n}" (mkPackageUiMetadata v)) builtExporters)
        // (mapAttrs' (n: v: nameValuePair "kernels.${n}" (mkPackageUiMetadata v)) builtKernels)
        // (mapAttrs' (n: v: nameValuePair "language-servers.${n}" (mkPackageUiMetadata v)) builtLanguageServers)
        // (mapAttrs' (n: v: nameValuePair "shells.${n}" (mkPackageUiMetadata v)) builtShells)
      ;

      other_packages = map (p: {
        channel = p.channel;
        attr = p.attr;
        meta = if p.contents ? "meta" then uiMetadata.chooseInterestingMeta p.contents else {};
      }) evaluated.config.labeledPackages;
    };

    ui_metadata_yaml = writeText "ui-metadata.yaml" (lib.generators.toYAML {} ui_metadata);
  };
}
