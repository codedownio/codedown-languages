{ callPackage
, lib
, fetchgit
, fetchFromGitHub
, linkFarm
, runCommand
, symlinkJoin
, writeTextDir
, writeText

, pkgsStable
, pkgsMaster
}:

config:

with lib;

let
  chooseMeta = callPackage ./choose-meta.nix {};
  evaluated = (callPackage ./evaluate-config.nix { inherit pkgsStable pkgsMaster; }) config;
  removeNonDefaultSettings = callPackage ./remove-non-default-settings.nix {};
  # Only used for the channel-level environment settings_schema, which is currently exposed on the
  # Nixpkgs-channel overlay makeEnvironment only (see nix/nixpkgs-overlay.nix), not here.
  # nixosOptionsToSettingsSchema = callPackage ./nixos-options-to-settings-schema.nix {};

  builtExporters = evaluated.config.builtExporters;
  builtKernels = mapAttrs (_: kernel:
    kernel.overrideAttrs (old: {
      passthru = old.passthru // {
        name = "kernels." + kernel.name;

        settings = removeNonDefaultSettings kernel.settingsSchema kernel.settings;
        settingsSchema = mapAttrs (_: v: removeAttrs v ["loc"]) kernel.settingsSchema;
      };
    })) evaluated.config.builtKernels;
  builtLanguageServers = evaluated.config.builtLanguageServers;

  exporters = concatMap (exporter: exporter.meta.exporterInfos) (attrValues builtExporters);

  # This is duplicated from kernels/common.nix, which we'd rather not import here
  packageName = p: if lib.isString p then p else p.name;

  mkSubPackageMetadata = pkg: p:
    let
      meta = chooseMeta (pkg.packageOptions.${packageName p} or {});
      settings = if lib.isAttrs p
                 then removeNonDefaultSettings (meta.settings_schema or {}) (lib.removeAttrs p ["name"])
                 else {};
    in {
      name = packageName p;
      inherit meta;
    } // lib.optionalAttrs (builtins.length (builtins.attrNames settings) > 0) {
      inherit settings;
    };

  mkPackageUiMetadata = pkg: {
    # Dry
    name = pkg.name;
    settings = lib.removeAttrs (pkg.settings or {}) ["packages"];

    # Different for hydrated
    packages = map (p: mkSubPackageMetadata pkg p) (pkg.settings.packages or []);

    # Hydrated
    meta = chooseMeta pkg;
  };

  linkBinaries = newBin: inputs: runCommand "codedown-environment-${newBin}" {} ''
    mkdir -p "$out/${newBin}"

    for input in ${builtins.toString inputs}; do
      find "$input/bin" -type f -executable | while read -r executable; do
        ln -s "$executable" "$out/${newBin}/$(basename "$executable")"
      done
    done
  '';

  extraNixCode = evaluated.config.environment.extraNix;
  extraNixDrv =
    if extraNixCode == "" then []
    else [(import (builtins.toFile "extra-nix.nix" "{ pkgs }:\n${extraNixCode}") { pkgs = pkgsStable; })];

  allIcons =
    let
      getAllIcons = pkg: filter (x: x != null) (
        [(pkg.meta.icon or null) (pkg.meta.iconMonochrome or null)]
        ++
        (concatMap (subPackageName: getAllIcons (pkg.packageOptions.${packageName subPackageName} or {})) (pkg.settings.packages or []))
      );

      allPaths =
        concatLists (mapAttrsToList (n: v: getAllIcons v) builtExporters)
        ++ concatLists (mapAttrsToList (n: v: getAllIcons v) builtKernels)
        ++ concatLists (mapAttrsToList (n: v: getAllIcons v) builtLanguageServers)
        ++ concatLists (mapAttrsToList (n: v: getAllIcons v) evaluated.config.packages)
        ;
    in
      linkFarm "all-icons" (map (path: {
        name = "lib/codedown/icons/" + builtins.hashString "md5" (toString path);
        path = path;
      }) allPaths);

in

symlinkJoin {
  name = evaluated.config.name;

  paths =
    attrValues evaluated.config.builtKernels
    ++ attrValues evaluated.config.builtLanguageServers
    ++ attrValues evaluated.config.builtExporters
    ++ lib.optionals (builtins.length exporters > 0) [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporters))]
    ++ attrValues evaluated.config.packages
    ++ lib.mapAttrsToList linkBinaries evaluated.config.extraBinDirs
    # environment.variables isn't wired up to anything at runtime yet, so skip the .env file for now.
    # ++ [(writeTextDir "lib/codedown/.env" (lib.generators.toKeyValue {} evaluated.config.environment.variables))]
    ++ extraNixDrv
    ++ [allIcons]
  ;

  passthru = rec {
    inherit evaluated;

    ui_metadata = {
      packages =
        (mapAttrs' (n: v: nameValuePair "exporters.${n}" (mkPackageUiMetadata v)) builtExporters)
        // (mapAttrs' (n: v: nameValuePair "kernels.${n}" (mkPackageUiMetadata v)) builtKernels)
        // (mapAttrs' (n: v: nameValuePair "language-servers.${n}" (mkPackageUiMetadata v)) builtLanguageServers)
        // (mapAttrs' (n: v: nameValuePair n (mkPackageUiMetadata v)) evaluated.config.packages)
      ;

      # No channel-level environment settings are exposed here yet (they currently live only on the
      # Nixpkgs-channel overlay makeEnvironment; see nix/nixpkgs-overlay.nix). Leave the field present
      # but empty so the shape is stable and we can populate it later.
      settings_schema = {};
      # settings_schema = nixosOptionsToSettingsSchema { componentsToDrop = 1; } evaluated.options.environment;
    };

    ui_metadata_yaml = writeText "ui-metadata.yaml" (lib.generators.toYAML {} ui_metadata);
  };
}
