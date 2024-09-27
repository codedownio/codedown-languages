{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.clojure = {
      enable = mkOption {
        description = "Enable Clojure kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["clojure"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["clj"];
      };

      lsp.clojure-lsp.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable clojure-lsp language server";
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
