{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.clojure = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["clojure"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["clj"];
      };

      settings.lsp.clojure-lsp.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable clojure-lsp language server";
      };
    };
  };

  config = mkIf config.kernels.clojure.enable {
    builtKernels.clojure = config.pkgs.callPackage ./. {
      clojure = config.pkgs.clojure;
      inherit (config.kernels.clojure) attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.clojure;
    };
  };
}
