{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgs = config.pkgs;

in

{
  options = {
    kernels.javascript = {
      enable = mkOption {
        title = "Enable JavaScript kernel";
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
        default = ["javascript" "js"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["js" "mjs" "cjs"];
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

  config = mkIf config.kernels.javascript.enable {
    builtKernels.javascript = pkgs.callPackage ./. {
      nodejs = getAttr config.kernels.javascript.nodejsPackage pkgs;

      settings = config.kernels.javascript;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.javascript;
    };
  };
}
