{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    shells.fish = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the Fish shell.";
      };
    };
  };

  config = mkIf config.shells.fish.enable {
    builtShells.fish = let
      baseDerivation = config.pkgs.callPackage ./fish.nix {};
    in
      baseDerivation.overrideAttrs (old: {
        meta = old.meta // {
          icon = ./icon-64x64.png;
          displayName = "Fish " + baseDerivation.version;
          attr = "fish";
          args = ["${baseDerivation}/bin/fish"];
        };
      });
  };
}
