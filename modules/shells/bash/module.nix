{ config, lib, ... }:

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
    packages = {
      "shells.bash" = config.pkgs.callPackage ./. {};
    };
  };
}
