{ callPackage
, lib
, fetchgit
, fetchFromGitHub
, system

, mkCodeDownEnvironment
, name ? "codedown-environment"
}:

{
  channels ? {}
  , packages ? {}
}:

with lib;

let
  importedOverlays = [];

  importedChannels = builtins.mapAttrs (name: value: let imported = import value; in
    if (builtins.isFunction imported && builtins.hasAttr "overlays" (builtins.functionArgs imported)) then imported { overlays = importedOverlays; inherit system; }
    else if (builtins.isFunction imported) then imported { inherit fetchgit fetchFromGitHub; }
    else imported
  ) channels;

  kernelPackages = filterAttrs (k: _: hasPrefix "codedown.kernels." k) packages;
  nonKernelPackages = filterAttrs (k: _: !(hasPrefix "codedown.kernels." k)) packages;

  getByChannelAndPackageName = k: v: let
    parts = strings.splitString "." k;
    in
      if builtins.length parts == 1 then null
      else {
        channel = head parts;
        attr = concatStringsSep "." (tail parts);
      };

  repackPackages = packages: if builtins.isList packages then packages else
    mapAttrsToList (n: v: { name = n; settings = v; }) packages;

in

mkCodeDownEnvironment {
  channels = null;
  environmentName = name;

  kernels = mapAttrsToList (n: v: {
    name = builtins.substring (builtins.stringLength "codedown.kernels.") (builtins.stringLength n) n;
    channel = "codedown";
    args = if hasAttr "packages" v then v // { packages = repackPackages v.packages; } else v;
  }) kernelPackages;

  otherPackages = let
    byChannelAndPackageName = filterAttrs (n: v: v != null) (mapAttrs (k: v: getByChannelAndPackageName k v) nonKernelPackages);
    in
      mapAttrsToList (n: v: {
        channel = v.channel;
        attr = v.attr;
        contents = getAttr v.attr importedChannels.${v.channel};
      }) byChannelAndPackageName;
}
