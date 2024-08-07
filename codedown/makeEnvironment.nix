{ callPackage
, fetchgit
, fetchFromGitHub

, mkCodeDownEnvironment
, name ? "codedown-environment"
}:

{
  channels ? {}
  , packages ? {}
}:

let
  importedOverlays = [];

  importedChannels = builtins.mapAttrs (name: value: let imported = import value; in
    if (builtins.isFunction imported && builtins.hasAttr "overlays" (builtins.functionArgs imported)) then imported { overlays = importedOverlays; }
    else if (builtins.isFunction imported) then imported { inherit fetchgit fetchFromGitHub; }
    else imported
  ) channels;

  kernels = [];

  otherPackages = [];

in

mkCodeDownEnvironment {
  channels = null;
  environmentName = name;
  inherit kernels otherPackages;
}
