{ callPackage
, lib
, linkFarm
, symlinkJoin
, writeTextDir

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
  languagesCommon = callPackage ../languages/common.nix {};
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
    attr = shell.contents.attr;
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

  mkKernelUiMetadata = kernel: {
    channel = kernel.channel;
    name = kernel.name;

    modes = let
      addKernelInfo = modes: modes // { kernel = kernel.passthru.name; channel = kernel.passthru.channel; };
    in
      (if kernel.passthru ? "modes" then (map addKernelInfo kernel.passthru.modes) else []);

    languageServerNames = [];

    settings = if kernel ? "settings" then kernel.settings else {};
    settingsSchema = if kernel ? "settingsSchema" then kernel.settingsSchema else {};

    icon = if kernel.meta ? "icon" then kernel.meta.icon else null;
  };

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

  passthru = {
    ui_metadata = {
      kernels = map mkKernelUiMetadata builtKernels;

      icons = let
        uniquePaths = map (v: languagesCommon.safeEval (lib.attrByPath ["meta" "icon"] "" v)) builtKernels;
      in
        linkFarm "all-environment-icons" (map (path: {
          name = builtins.hashString "md5" (toString path);
          path = path;
        }) uniquePaths);

      # modeInfos = concatMap (kernel: let
      #   addKernelInfo = modes: modes // { kernel = kernel.passthru.name; channel = kernel.passthru.channel; };
      # in
      #   (if kernel.passthru ? "modes" then (map addKernelInfo kernel.passthru.modes) else [])
      # ) builtKernels;
    };
  };
}
