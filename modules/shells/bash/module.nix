{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    shells.bash = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the Bash shell.";
      };
    };
  };

  config = mkIf config.shells.bash.enable {
    builtShells.bash = config.pkgs.bashInteractive.overrideAttrs (old: {
      meta = old.meta // {
        icon = ../default_icon_64x64.png;
        displayName = "Bash " + config.pkgs.bashInteractive.version;
        attr = "bash";
        args = ["${config.pkgs.bashInteractive}/bin/bash"];
      };
    });
  };
}
