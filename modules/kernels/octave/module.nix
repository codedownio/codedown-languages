{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.octave = {
      enable = mkOption {
        example = "Enable Octave kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
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
        example = "Extra Jupyter configuration";
        type = types.str;
        default = ''
          # use Qt as the default backend for plots
          # c.OctaveKernel.plot_settings = dict(backend='qt')
        '';
      };
    };
  };

  config = mkIf config.kernels.octave.enable {
    builtKernels.octave = pkgsToUse.callPackage ./. {
      octave = pkgsToUse.octave;
      settings = config.kernels.octave;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.octave;
    };
  };
}
