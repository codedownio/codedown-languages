{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.octave = {
      enable = mkOption {
        title = "Enable Octave kernel";
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
        default = ["octave"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["m"];
      };

      extraJupyterConfig = mkOption {
        title = "Extra Jupyter configuration";
        type = types.str;
        default = ''
          # use Qt as the default backend for plots
          # c.OctaveKernel.plot_settings = dict(backend='qt')
        '';
      };

      misc.enableVariableInspector = mkOption {
        title = "Enable the variable inspector";
        description = "This will show a summary of the currently defined variables in the UI.";
        type = types.bool;
        default = true;
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
