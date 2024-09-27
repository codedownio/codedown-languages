{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.rust = {
      enable = mkOption {
        example = "Enable Rust kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of packages";
        type = types.listOf (types.either types.str (types.submodule {
          options = {
            name = mkOption rec {
              description = "Package name";
              type = types.str;
            };
            features = mkOption rec {
              example = "Features to enable for the package";
              type = types.listOf types.str;
            };
          };
        }));
        default = [];
      };

      rustPackage = mkOption {
        example = "Rust version";
        type = types.enum (
          ["rust"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "rust_") name == "rust_")
                              (builtins.attrNames config.pkgs))
        );
        default = "rust";
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["rust"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["rs" "rlib"];
      };

      lsp.rust-analyzer.enable = mkOption {
        example = "Rust-analyzer: enable";
        type = types.bool;
        default = true;
      };
      lsp.rust-analyzer.debug = mkOption {
        example = "Rust-analyzer: debug output";
        type = types.bool;
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
