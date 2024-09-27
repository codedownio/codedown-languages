{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

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

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["octave"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["m"];
      };

      extraJupyterConfig = mkOption {
        type = types.str;
        default = "";
        description = "Extra Jupyter configuration.";
      };
    };
  };

  config = mkIf config.kernels.octave.enable {
    builtKernels.octave = config.pkgs.callPackage ./. {
      octave = config.pkgs.octave;
      settings = config.kernels.octave;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.octave;
    };
  };
}
