{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.postgres = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["postgres"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["sql"];
      };
    };
  };

  config = mkIf config.kernels.postgres.enable {
    builtKernels.postgres = config.pkgs.callPackage ./. {
      settings = config.kernels.postgres;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.postgres;
    };
  };
}
