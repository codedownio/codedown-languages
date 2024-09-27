{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.go = {
      enable = mkOption {
        description = "Enable Go kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      goPackage = mkOption {
        type = types.enum (
          ["go"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "go_") name == "go_")
                              (builtins.attrNames config.pkgs))
        );
        default = "go";
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["go"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["go"];
      };

      lsp.gopls.enable = mkOption {
        type = types.bool;
        description = "Enable gopls language server";
        default = true;
      };

      go.gocache = mkOption {
        type = types.str;
        description = "Value of GOCACHE environment variable";
        default = "/home/.gocache";
      };
    };
  };

  config = mkIf config.kernels.go.enable {
    builtKernels.go = config.pkgs.callPackage ./. {
      go = getAttr config.kernels.go.goPackage config.pkgs;

      settings = config.kernels.go;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.go;
    };
  };
}
