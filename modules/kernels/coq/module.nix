{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.coq = {
      enable = mkOption {
        example = "Enable Coq kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of Coq packages to use";
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };
      coqPackages = mkOption {
        example = "Coq packages set";
        type = types.enum (
          ["coqPackages"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "coqPackages_") name == "coqPackages_")
                              (builtins.attrNames config.pkgs))
        );
        default = "coqPackages";
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["coq"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["v"];
      };
    };
  };

  config = mkIf config.kernels.coq.enable {
    builtKernels.coq = config.pkgs.callPackage ./. {
      coqPackages = getAttr config.kernels.coq.coqPackages config.pkgs;

      settings = config.kernels.coq;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.coq;
    };
  };
}
