{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    exporters.typst = {
      enable = mkOption {
        type = types.bool;
        example = "Enable Typst exporter";
        description = "Enable the Typst exporters.";
        default = false;
        visible = false;
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["typst"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["typ"];
      };

      lsp.tinymist.enable = mkOption {
        example = "Enable tinymist language server";
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
