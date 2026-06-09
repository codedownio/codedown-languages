{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.postgres = {
      enable = mkOption {
        title = "Enable PostgreSQL kernel";
        type = types.bool;
        default = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["postgres"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
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
