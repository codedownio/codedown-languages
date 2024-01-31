{ callPackage
, lib
, linkFarm
, symlinkJoin
, writeText
, writeTextDir

, requiredPackages
, languages
}:

args@{
  channels
  , environmentName ? "codedown-environment"
  , kernels ? []
  , otherPackages ? []
}:

with lib;

let
  languagesCommon = callPackage ../languages/common.nix {};
  shellsCommon = callPackage ../shells/common.nix {};

  builtKernels = map (x: let kernel = (getAttr x.name languages).build x.args; in
                         kernel.overrideAttrs (old: {
                           passthru = old.passthru // {
                             name = x.name;
                             channel = x.channel;
                           };
                         })) kernels;

  shells = filter (x: lib.hasPrefix "shells." x.attr) otherPackages;

  exporters = filter (x: lib.hasPrefix "exporters." x.attr) otherPackages;
  exporterInfos = concatMap (exporter: exporter.contents.meta.exporterInfos) exporters;

  repls = let
    shellToReplInfo = shell: {
      name = shell.contents.name;
      display_name = shell.contents.displayName;
      attr = shell.contents.attr;
      args = ["${shell.contents}/lib/codedown/shell"];
      icon = shell.contents.icon;
    };
  in
    map shellToReplInfo shells
    ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (if kernel.passthru ? "repls" then kernel.passthru.repls else {})) builtKernels
  ;

  chooseInterestingMeta = lib.filterAttrs (n: v:
    n == "description"
    || n == "homepage"
    || n == "changelog"
    || (n == "available" && !v)
    || (n == "broken" && v)
    || (n == "unfree" && v)
    || (n == "unsupported" && v)
    || (n == "insecure" && v)
    # || (n == "license.spdxId") # TODO
  );

  mkChannelUiMetadata = name: channel: {
    foo = "bar";
  };

  mkKernelUiMetadata = kernel: {
    # Dry
    channel = kernel.channel;
    name = kernel.name;
    packages = kernel.args.packages;
    settings = if kernel ? "settings" then kernel.settings else {};

    # Hydrated
    display_name = "todo";
    icon = if kernel.meta ? "icon" then kernel.meta.icon else null;
    modes = kernel.modes;
    settings_schema = if kernel ? "settingsSchema" then kernel.settingsSchema else {};

    # TODO?
    languageServerNames = [];
  };

  mkOtherPackageUiMetadata = package: {
    channel = package.channel;
    attr = package.attr;
    meta = if package.contents ? "meta" then chooseInterestingMeta package.contents.meta else {};
  };

  ui_metadata = {
    channels = lib.mapAttrsToList mkChannelUiMetadata channels;

    kernels = map mkKernelUiMetadata builtKernels;

    other_packages = map mkOtherPackageUiMetadata otherPackages;
  };

  ui_metadata_yaml = writeText "ui-metadata.yaml" (lib.generators.toYAML {} ui_metadata);

in

symlinkJoin {
  name = environmentName;
  paths = builtKernels
          ++ [((callPackage ./spec_yaml.nix {}) (args //  { inherit shells exporters; kernels = builtKernels; }))]
          ++ [(shellsCommon.wrapShells shells)]
          ++ (map (x: x.contents) otherPackages)
          ++ requiredPackages
          ++ [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
          ++ [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporterInfos))]
  ;

  passthru = {
    inherit ui_metadata ui_metadata_yaml;
  };
}
