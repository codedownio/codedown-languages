{ callPackage
, lib
, fetchgit
, fetchFromGitHub
, symlinkJoin
, system
, writeTextDir

, pkgsStable
, pkgsUnstable
, pkgsMaster

, name ? "codedown-environment"
}:

channels:

config:

with lib;

let
  importedOverlays = [];

  importedChannels = builtins.mapAttrs (name: value: let
    imported = import value;
  in
    if (builtins.isFunction imported && builtins.hasAttr "overlays" (builtins.functionArgs imported)) then imported { overlays = importedOverlays; inherit system; }
    else if (builtins.isFunction imported) then imported { inherit fetchgit fetchFromGitHub; }
    else imported
  ) channels;

  evaluated = lib.evalModules {
    modules = [
      ../modules/base.nix

      ../languages/bash/module.nix

      ../shells/fish/module.nix

      {
        config = {
          pkgs = pkgsStable;
        };
      }
      {
        inherit config;
      }
    ];
  };

  builtKernels = evaluated.config.builtKernels;
  builtShells = evaluated.config.builtShells;

  repls = let
    shellToReplInfo = shell: {
      name = shell.name;
      display_name = shell.displayName;
      attr = shell.attr;
      args = ["${shell}/lib/codedown/shell"];
      icon = shell.icon;
    };
  in
    map shellToReplInfo (attrValues builtShells)
    ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (if kernel.passthru ? "repls" then kernel.passthru.repls else {})) (attrValues builtKernels)
  ;

in

symlinkJoin {
  inherit name;
  paths =
    attrValues (evaluated.config.builtKernels)
    ++ [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
  ;

  passthru = {
    inherit evaluated;
  };
}
