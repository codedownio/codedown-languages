{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  subPackage = {
    features = mkOption {
      title = "Features to enable for the package";
      type = types.listOf types.str;
      default = [];
    };
  };

  subPackageEvaluated = lib.evalModules {
    modules = [
      { options = { inherit subPackage; }; }
    ];
  };

  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.rust = {
      enable = mkOption {
        title = "Enable Rust kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf (types.either types.str (types.submodule {
          options = subPackage // {
            name = mkOption {
              description = "Package name";
              type = types.str;
              visible = false;
            };
          };
        }));
        default = [];
      };

      rustPackage = mkOption {
        title = "Rust version";
        type = types.enum (
          ["rust"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "rust_") name == "rust_")
                              (builtins.attrNames pkgsToUse))
        );
        default = "rust";
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["rust"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["rs" "rlib"];
      };

      lsp.rust-analyzer.enable = mkOption {
        title = "Rust-analyzer: enable";
        type = types.bool;
        default = true;
      };
      lsp.rust-analyzer.debug = mkOption {
        title = "Rust-analyzer: debug output";
        type = types.bool;
        default = false;
      };

      misc.enableVariableInspector = mkOption {
        title = "Enable the variable inspector";
        description = "This will show a summary of the currently defined variables in the UI.";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.rust.enable {
    builtKernels.rust = pkgsToUse.callPackage ./. {
      rust = getAttr config.kernels.rust.rustPackage pkgsToUse;

      settings = config.kernels.rust;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.rust;
      subPackageSettingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 1; } subPackageEvaluated.options.subPackage;
    };
  };
}
