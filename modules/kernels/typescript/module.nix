{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgs = config.pkgs;

in

{
  options = {
    kernels.typescript = {
      enable = mkOption {
        title = "Enable TypeScript kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      nodejsPackage = mkOption {
        title = "Node.js version";
        type = types.enum ["nodejs" "nodejs_22" "nodejs_24"];
        default = "nodejs";
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["typescript" "ts"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["ts" "tsx"];
      };

      lsp.typescript-language-server.enable = mkOption {
        title = "Enable the TypeScript language server";
        type = types.bool;
        default = true;
      };
      lsp.typescript-language-server.debug = mkOption {
        title = "TypeScript language server: debug output";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf config.kernels.typescript.enable {
    # Same package as the JavaScript kernel: tslab is one binary that registers both, and they
    # share the npm package set and node_modules machinery.
    builtKernels.typescript = pkgs.callPackage ../javascript {
      variant = "typescript";

      nodejs = getAttr config.kernels.typescript.nodejsPackage pkgs;

      settings = config.kernels.typescript;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.typescript;
    };
  };
}
