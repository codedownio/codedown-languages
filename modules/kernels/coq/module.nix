{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.coq = {
      enable = mkOption {
        title = "Enable Coq kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of Coq packages to use";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      coqPackages = mkOption {
        title = "Coq packages set";
        type = types.enum (
          ["coqPackages"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "coqPackages_") name == "coqPackages_")
                              (builtins.attrNames pkgsToUse))
        );
        default = "coqPackages";
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["coq"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["v"];
      };
    };
  };

  config = mkIf config.kernels.coq.enable {
    builtKernels.coq = pkgsToUse.callPackage ./. {
      coqPackages = getAttr config.kernels.coq.coqPackages pkgsToUse;

      settings = config.kernels.coq;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.coq;
    };
  };
}
