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

  builtKernels = map (x: let kernel = (getAttr x.name languages).override x.args; in
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

  uiMetadata = callPackage ./uiMetadata.nix {};

in

symlinkJoin {
  name = environmentName;
  paths = builtKernels
          ++ [(shellsCommon.wrapShells shells)]
          ++ (map (x: x.contents) otherPackages)
          ++ requiredPackages
          ++ [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
          ++ [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporterInfos))]
  ;

  passthru = rec {
    ui_metadata = {
      channels = lib.mapAttrsToList uiMetadata.mkChannelUiMetadata channels;

      kernels = map uiMetadata.mkKernelUiMetadata builtKernels;

      other_packages = map uiMetadata.mkOtherPackageUiMetadata otherPackages;
    };

    ui_metadata_yaml = writeText "ui-metadata.yaml" (lib.generators.toYAML {} ui_metadata);
  };
}
