{ config, lib, ... }:

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
    packages = [(config.pkgs.callPackage ./. {})];
  };
}
