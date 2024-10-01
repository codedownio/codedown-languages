{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.cpp = {
      enable = mkOption {
        example = "Enable C++ kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of packages";
        type = types.listOf types.str;
        default = [];
      };

      flavor = mkOption {
        example = "C++ flavor";
        type = types.enum [
          "c++11"
          "c++14"
          "c++17"
          "c++20"
          "c++23"

          "gnu++11"
          "gnu++14"
          "gnu++17"
          "gnu++20"
          "gnu++23"
        ];

        default = "c++20";
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["cpp"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
      };
    };
  };

  config = mkIf config.kernels.cpp.enable {
    builtKernels.cpp = config.pkgs.callPackage ./. {
      settings = config.kernels.cpp;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.cpp;
    };
  };
}
