{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    exporters.nbconvert = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the nbconvert exporters.";
      };

      texliveScheme = mkOption {
        type = types.enum [
          "scheme-basic"
          "scheme-bookpub"
          "scheme-full"
          "scheme-medium"
          "scheme-minimal"
          "scheme-small"
          "scheme-tetex"
        ];
        default = "scheme-medium";
        description = "The TeX Live scheme to use, as an attribute of pkgs.texlive.combined.*";
      };
    };
  };

  config = mkIf config.exporters.nbconvert.enable {
    builtExporters.nbconvert = config.pkgsMaster.callPackage ./nbconvert.nix {
      texliveScheme = config.pkgs.texlive.combined.${config.exporters.nbconvert.texliveScheme};

      settings = config.exporters.nbconvert;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.exporters.nbconvert;
    };
  };
}
