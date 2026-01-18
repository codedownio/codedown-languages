{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

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
        visible = false;
      };

      flavor = mkOption {
        example = "C++ flavor";
        type = types.enum [
          # "c++11"
          # "c++14"
          "c++17"
          "c++20"
          "c++23"
          "c++2c"

          # "gnu++11"
          # "gnu++14"
          "gnu++17"
          "gnu++20"
          "gnu++23"
          "gnu++2c"
        ];

        default = "c++23";
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

      lsp.clangd.enable = mkOption {
        example = "Enable clangd language server";
        type = types.bool;
        default = true;
      };
      lsp.clangd.debug = mkOption {
        example = "Clangd: enable debug output";
        type = types.bool;
        default = false;
      };
      lsp.clangd.super-debug = mkOption {
        example = "Clangd: enable verbose debug output";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf config.kernels.cpp.enable {
    builtKernels.cpp = pkgsToUse.callPackage ./. {
      settings = config.kernels.cpp;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.cpp;
    };
  };
}
