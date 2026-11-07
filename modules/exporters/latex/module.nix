{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    exporters.latex = {
      enable = mkOption {
        type = types.bool;
        title = "Enable LaTeX exporter";
        description = "Enable the LaTeX exporter, which compiles .tex notebooks to PDF with latexmk.";
        default = false;
        visible = false;
      };

      texliveScheme = mkOption {
        type = types.enum [
          "scheme-medium"
          "scheme-full"
        ];
        default = "scheme-medium";
        title = "TeX Live scheme";
        description = "The TeX Live scheme to use, as an attribute of pkgs.texlive.combined.*";
      };

      engine = mkOption {
        type = types.enum ["pdflatex" "xelatex" "lualatex"];
        default = "pdflatex";
        title = "TeX engine";
        description = "The engine latexmk compiles with.";
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
      texliveToUse = config.pkgs.texlive.combined.${config.exporters.latex.texliveScheme};

      settings = config.exporters.latex;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.exporters.latex;
    };
  };
}
