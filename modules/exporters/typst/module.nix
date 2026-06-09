{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  subPackage = types.submodule {
    options = {
      name = mkOption {
        description = "Package name";
        type = types.str;
      };
      outputs = mkOption {
        title = "Package outputs to include";
        type = types.listOf types.str;
      };
    };
  };

in

{
  options = {
    exporters.typst = {
      enable = mkOption {
        type = types.bool;
        title = "Enable Typst exporter";
        description = "Enable the Typst exporters.";
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf (types.either types.str subPackage);
        default = [];
        visible = false;
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["typst"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["typ"];
      };

      lsp.tinymist.enable = mkOption {
        title = "Enable tinymist language server";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.exporters.typst.enable {
    builtExporters.typst = config.pkgsMaster.callPackage ./default.nix {
      settings = config.exporters.typst;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.exporters.typst;
    };
  };
}
