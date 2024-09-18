{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.rust = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
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

      settings = {
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
  };

  config = mkIf config.kernels.rust.enable {
    builtKernels.rust = config.pkgs.callPackage ./. {
      rust = getAttr config.kernels.rust.rustPackage config.pkgs;

      inherit (config.kernels.rust) packages attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema {
        componentsToDrop = 2;
      } options.kernels.rust;
    };
  };
}
