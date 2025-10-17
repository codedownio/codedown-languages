{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    exporters.nbconvert = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the nbconvert exporters.";
        default = false;
        visible = false;
      };

      texliveScheme = mkOption {
        type = types.enum [
          # Fails with "OSError: xelatex not found on PATH"
          # "scheme-minimal"

          # Fails with "OSError: xelatex not found on PATH"
          # "scheme-basic"

          # Fails with "LaTeX Error: File `tcolorbox.sty' not found."
          # "scheme-small"

          # Fails with "LaTeX Error: File `tcolorbox.sty' not found."
          # "scheme-medium"

          # Fails with "OSError: xelatex not found on PATH"
          # "scheme-bookpub"

          # Fails with "LaTeX Error: File `tcolorbox.sty' not found."
          # "scheme-tetex"

          "scheme-full"
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
