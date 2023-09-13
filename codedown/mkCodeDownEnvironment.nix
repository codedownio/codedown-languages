{ lib
, callPackage
, writeTextDir
, symlinkJoin

, requiredPackages
, languages
}:

args@{
  channels
  , environmentName ? "codedown-environment"
  , kernels ? []
  , otherPackages ? []
  , metaOnly ? false
  , ...
}:

with lib;

let
  shellsCommon = callPackage ../shells/common.nix {};

  builtKernels = map (x: let kernel = (getAttr x.name languages).build (x.args // { inherit metaOnly; }); in
                         kernel.overrideAttrs (old: {
                           passthru = old.passthru // {
                             name = x.name;
                             channel = x.channel;
                           };
                         })) kernels;

  shellToReplInfo = shell: {
    name = shell.contents.name;
    display_name = shell.contents.displayName;
    executable_name = shell.contents.executableName;
    args = ["${shell.contents}/lib/codedown/shell"];
    icon = shell.contents.icon;
  };

  shells = filter (x: lib.hasPrefix "shells." x.attr) otherPackages;

  exporters = filter (x: lib.hasPrefix "exporters." x.attr) otherPackages;
  exporterInfos = concatMap (exporter: exporter.contents.meta.exporterInfos) exporters;

  repls =
    map shellToReplInfo shells
    ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (if kernel.passthru ? "repls" then kernel.passthru.repls else {})) builtKernels
  ;

in

symlinkJoin {
  name = environmentName;
  paths = builtKernels
          ++ [((callPackage ./spec_yaml.nix {}) (args //  { inherit shells exporters; kernels = builtKernels; }))]
          ++ (if metaOnly then [] else [(shellsCommon.wrapShells shells)])
          ++ (if metaOnly then [] else (map (x: x.contents) otherPackages))
          ++ (if metaOnly then [] else requiredPackages)
          ++ [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
          ++ [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporterInfos))]
  ;
}
