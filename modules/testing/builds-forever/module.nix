{ config, lib, ... }:

with lib;

{
  options = {
    testing.builds-forever = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Build the builds-forever test.";
        visible = false;
      };
    };
  };

  config = mkIf config.testing.builds-forever.enable {
    packages = {
      "testing.builds-forever" = config.pkgs.callPackage ./. {};
    };
  };
}
