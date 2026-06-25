{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  # Ark requires rustc >= 1.94, which is newer than the stable nixpkgs pin
  # ships, so build it (and its R) from the master pin like the Rust kernel.
  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.R-ark = {
      enable = mkOption {
        title = "Enable R (Ark) kernel";
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

      misc.enableVariableInspector = mkOption {
        title = "Enable the variable inspector";
        description = "This will show a summary of the currently defined variables in the UI.";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.R-ark.enable {
    builtKernels.R-ark = pkgsToUse.callPackage ./. {
      R = pkgsToUse.R;
      settings = config.kernels.R-ark;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.R-ark;
    };
  };
}
