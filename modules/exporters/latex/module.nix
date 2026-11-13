{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    exporters.latex = {
      enable = mkOption {
        type = types.bool;
        title = "Enable LaTeX exporter";
        description = "Enable the LaTeX exporter, which renders .tex notebooks to PDF as you type.";
        default = false;
        visible = false;
      };

      texliveScheme = mkOption {
        type = types.enum [
          "scheme-small"
          "scheme-medium"
          "scheme-full"
        ];
        default = "scheme-medium";
        title = "TeX Live scheme";
        description = "The TeX Live scheme to use, as an attribute of pkgs.texlive.pkgs.*";
      };

      engine = mkOption {
        type = types.enum ["pdflatex" "xelatex" "lualatex"];
        default = "pdflatex";
        title = "TeX engine";
        description = "The TeX engine that compiles the document.";
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["latex"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["tex"];
      };

      lsp.texlab.enable = mkOption {
        title = "Enable texlab language server";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.exporters.latex.enable {
    builtExporters.latex = config.pkgs.callPackage ./default.nix {
      # latexmk runs the compile; cm-super provides the Type1 TS1 fonts
      texliveToUse = config.pkgs.texlive.withPackages (ps: [
        ps.${config.exporters.latex.texliveScheme}
        ps.latexmk
        ps.cm-super
      ]);

      settings = config.exporters.latex;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.exporters.latex;
    };
  };
}
