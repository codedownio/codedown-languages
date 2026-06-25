{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.clojure = {
      enable = mkOption {
        title = "Enable Clojure kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["clojure"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["clj"];
      };

      lsp.clojure-lsp.enable = mkOption {
        title = "Enable clojure-lsp language server";
        type = types.bool;
        default = true;
      };

      misc.enableVariableInspector = mkOption {
        title = "Enable the variable inspector";
        description = "This will show a summary of the currently defined variables in the UI.";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.clojure.enable {
    builtKernels.clojure = config.pkgs.callPackage ./. {
      clojure = config.pkgs.clojure;
      settings = config.kernels.clojure;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.clojure;
    };
  };
}
