{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.R = {
      enable = mkOption {
        example = "Enable R kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of packages";
        type = types.listOf types.str;
        default = [];
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["r" "R"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["r"];
      };

      lsp.languageserver.enable = mkOption {
        example = "Enable languageserver";
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
