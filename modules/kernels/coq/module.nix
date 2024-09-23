{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.coq = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      coqPackages = mkOption {
        type = types.enum (
          ["coqPackages"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "coqPackages_") name == "coqPackages_")
                              (builtins.attrNames config.pkgs))
        );
        default = "coqPackages";
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["coq"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["v"];
      };
    };
  };

  config = mkIf config.kernels.coq.enable {
    builtKernels.coq = config.pkgs.callPackage ./. {
      coqPackages = getAttr config.kernels.coq.coqPackages config.pkgs;

      inherit (config.kernels.coq) packages attrs extensions;
      settings = {};
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.coq;
    };
  };
}
