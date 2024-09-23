{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.octave = {
      enable = mkOption {
        description = "Enable Octave kernel";
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
        default = ["octave"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["m"];
      };

      settings.extraJupyterConfig = mkOption {
        type = types.str;
        default = "";
        description = "Extra Jupyter configuration.";
      };
    };
  };

  config = mkIf config.kernels.octave.enable {
    builtKernels.octave = config.pkgs.callPackage ./. {
      octave = config.pkgs.octave;
      inherit (config.kernels.octave) packages attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.octave;
    };
  };
}
