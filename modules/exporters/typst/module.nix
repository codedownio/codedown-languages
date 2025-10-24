{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    exporters.typst = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the typst exporter.";
        default = false;
        visible = false;
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
