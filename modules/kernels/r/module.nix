{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.R = {
      enable = mkOption {
        title = "Enable R kernel";
        type = types.bool;
        default = false;
        visible = false;
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
        default = ["r" "R"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["r"];
      };

      lsp.languageserver.enable = mkOption {
        title = "Enable languageserver";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.R.enable {
    builtKernels.R = config.pkgs.callPackage ./. {
      R = config.pkgs.R;
      settings = config.kernels.R;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.R;
    };
  };
}
