{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    exporters.pandoc = {
      enable = mkOption {
        type = types.bool;
        title = "Enable pandoc exporters";
        description = "Enable the pandoc exporters.";
        default = false;
        visible = false;
      };

      texliveScheme = mkOption {
        type = types.enum [
          "scheme-small"
          "scheme-medium"
          "scheme-full"
        ];
        default = "scheme-full";
        title = "TeX Live scheme";
        description = "The TeX Live scheme to use, as an attribute of pkgs.texlive.pkgs.*";
      };
    };
  };

  config = mkIf config.exporters.pandoc.enable {
    builtExporters.pandoc = config.pkgs.callPackage ./default.nix {
      # latexmk runs the compile; cm-super provides the Type1 TS1 fonts
      texliveScheme = config.pkgs.texlive.withPackages (ps: [
        ps.${config.exporters.pandoc.texliveScheme}
        ps.latexmk
        ps.cm-super
      ]);

      settings = config.exporters.pandoc;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.exporters.pandoc;
    };
  };
}
