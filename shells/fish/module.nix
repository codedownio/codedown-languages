{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    shells.fish = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf config.kernels.bash.enable {
    builtShells.fish = let
      baseDerivation = config.pkgs.callPackage ./fish.nix {};
      common = config.pkgs.callPackage ../common.nix {};
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
