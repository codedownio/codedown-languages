{ callPackage
, lib
, fetchgit
, fetchFromGitHub

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
    if (builtins.isFunction imported && builtins.hasAttr "overlays" (builtins.functionArgs imported)) then imported { overlays = importedOverlays; }
    else if (builtins.isFunction imported) then imported { inherit fetchgit fetchFromGitHub; }
    else imported
  ) channels;

  kernels = let
    kernelPackages = filterAttrs (k: _: hasPrefix "codedown.kernels." k) packages;
    in
      mapAttrsToList (n: v: {
        name = builtins.substring (builtins.stringLength "codedown.kernels.") (builtins.stringLength n) n;
        channel = "codedown";
        args = v;
      }) kernelPackages;

  getByChannelAndPackageName = k: v: null;

  otherPackages = let
    byChannelAndPackageName = filterAttrs (n: v: v != null) (mapAttrs (k: v: getByChannelAndPackageName k v) packages);
    in
      mapAttrsToList (n: v: {
        channel = v.channel;
        attr = v.attr;
        contents = getAttr v.attr importedChannels.${v.channel};
      }) byChannelAndPackageName;

in

mkCodeDownEnvironment {
  channels = null;
  environmentName = name;
  inherit kernels otherPackages;
}
