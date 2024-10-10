{ config, lib, ... }:

with lib;

{
  options = {
    shells.zsh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the ZSH shell.";
      };

      powerline = mkOption {
        type = types.bool;
        default = false;
        description = "Enable Powerline in the ZSH shell.";
      };
    };
  };

  config = mkIf config.shells.zsh.enable {
    packages = [ (config.pkgs.callPackage ./. {}) ];
  };
}
