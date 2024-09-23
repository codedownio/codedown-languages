{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

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

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["go"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["go"];
      };

      settings = {
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
  };

  config = mkIf config.kernels.go.enable {
    builtKernels.go = config.pkgs.callPackage ./. {
      go = getAttr config.kernels.go.goPackage config.pkgs;

      inherit (config.kernels.go) packages attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.go;
    };
  };
}
