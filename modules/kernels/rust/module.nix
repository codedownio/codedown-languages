{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.rust = {
      enable = mkOption {
        description = "Enable Rust kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str (types.submodule {
          options = {
            name = mkOption rec {
              type = types.str;
              description = "Package name";
            };
            features = mkOption rec {
              type = types.listOf types.str;
              description = "Features to enable for the package";
            };
          };
        }));
        default = [];
      };

      rustPackage = mkOption {
        type = types.enum (
          ["rust"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "rust_") name == "rust_")
                              (builtins.attrNames config.pkgs))
        );
        default = "rust";
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["rust"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["rs" "rlib"];
      };

      lsp.rust-analyzer.enable = mkOption {
        type = types.bool;
        description = "Rust-analyzer: enable";
        default = true;
      };

      lsp.rust-analyzer.debug = mkOption {
        type = types.bool;
        description = "Rust-analyzer: debug output";
        default = false;
      };
    };
  };

  config = mkIf config.kernels.rust.enable {
    builtKernels.rust = config.pkgs.callPackage ./. {
      rust = getAttr config.kernels.rust.rustPackage config.pkgs;

      settings = config.kernels.rust;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.rust;
    };
  };
}
