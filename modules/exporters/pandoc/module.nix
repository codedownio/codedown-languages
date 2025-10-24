{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    exporters.pandoc = {
      enable = mkOption {
        type = types.bool;
        example = "Enable pandoc exporters";
        description = "Enable the pandoc exporters.";
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
        default = "scheme-full";
        example = "TeX Live scheme";
        description = "The TeX Live scheme to use, as an attribute of pkgs.texlive.combined.*";
      };
    };
  };

  config = mkIf config.exporters.pandoc.enable {
    builtExporters.pandoc = config.pkgsMaster.callPackage ./default.nix {
      texliveScheme = config.pkgs.texlive.combined.${config.exporters.pandoc.texliveScheme};

      settings = config.exporters.pandoc;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.exporters.pandoc;
    };
  };
}
