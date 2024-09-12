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
      common.wrapShell {
        executableName = "fish";
        inherit baseDerivation;
        displayName = "Fish " + baseDerivation.version;
        attr = "fish";
        icon = ./icon-64x64.png;
      };
  };
}
