{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.R = {
      enable = mkOption {
        description = "Enable R kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["r" "R"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["r"];
      };

      lsp.languageserver.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable languageserver";
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
