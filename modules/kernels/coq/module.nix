{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

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
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      coqPackages = mkOption {
        example = "Coq packages set";
        type = types.enum (
          ["coqPackages"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "coqPackages_") name == "coqPackages_")
                              (builtins.attrNames pkgsToUse))
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

      lsp.coq-lsp.enable = mkOption {
        example = "Enable coq-lsp language server";
        type = types.bool;
        default = true;
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
