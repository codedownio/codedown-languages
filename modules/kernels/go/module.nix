{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.go = {
      enable = mkOption {
        example = "Enable Go kernel";
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

      goPackage = mkOption {
        example = "Go version";
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
        example = "Enable gopls language server";
        type = types.bool;
        default = true;
      };
      lsp.gopls.debug = mkOption {
        example = "Gopls: enable debug output";
        type = types.bool;
        default = false;
      };
      lsp.gopls.super-debug = mkOption {
        example = "Gopls: enable verbose debug output";
        type = types.bool;
        default = false;
      };

      go.gocache = mkOption {
        example = "Value of GOCACHE environment variable";
        type = types.str;
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
