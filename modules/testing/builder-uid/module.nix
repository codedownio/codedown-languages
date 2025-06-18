{ config, lib, ... }:

with lib;

{
  options = {
    testing.builder-uid = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Build the builder-uid test.";
      };
    };
  };

  config = mkIf config.testing.builder-uid.enable {
    packages = {
      "testing.builder-uid" = config.pkgs.callPackage ./. {};
    };
  };
}
