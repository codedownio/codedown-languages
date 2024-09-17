{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.cpp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      flavor = mkOption {
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

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["cpp"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
      };

      settings = {};
    };
  };

  config = mkIf config.kernels.cpp.enable {
    builtKernels.cpp = config.pkgs.callPackage ./. {
      inherit (config.kernels.cpp) flavor packages extensions settings;

      attrs = [config.kernels.cpp.flavor] ++ config.kernels.cpp.attrs;

      settingsSchema = nixosOptionsToSettingsSchema options.kernels.cpp;
    };
  };
}
