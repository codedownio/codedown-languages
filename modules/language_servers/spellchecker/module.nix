{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

  cfg = config.language-servers.spellchecker;

in

{
  options = {
    language-servers.spellchecker = {
      enable = mkOption {
        type = types.bool;
        title = "Enable spellchecker";
        description = "Enable the spellchecker language server(s).";
        default = false;
        visible = false;
      };

      markdown.enable = mkOption {
        type = types.bool;
        title = "Check Markdown";
        description = "Include the Markdown spellchecker (.md, .ipynb).";
        default = true;
      };

      typst.enable = mkOption {
        type = types.bool;
        title = "Check Typst";
        description = "Include the Typst spellchecker (.typ).";
        default = true;
      };

      typst.checkLineComments = mkOption {
        type = types.bool;
        title = "Check Typst line comments";
        description = "Spellcheck the prose inside Typst line comments (// ...).";
        default = false;
      };

      typst.checkBlockComments = mkOption {
        type = types.bool;
        title = "Check Typst block comments";
        description = "Spellcheck the prose inside Typst block comments (/* ... */).";
        default = false;
      };
    };
  };

  # One built package "spellchecker" that bundles both the markdown and typst servers as equals
  # (markdown-spellchecker + typst-spellchecker). The bundle is what environments track as
  # language-servers.spellchecker, so hydration keys line up; enabling it gives both servers.
  config = mkIf cfg.enable {
    builtLanguageServers.spellchecker =
      let
        typstSettingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 3; } options.language-servers.spellchecker.typst;

        markdown = pkgsToUse.callPackage ../markdown-spellcheck-lsp {};
        typst = pkgsToUse.callPackage ../typst-spellcheck-lsp {
          settings = cfg.typst;
          settingsSchema = typstSettingsSchema;
        };

        parts = (optional cfg.markdown.enable markdown)
             ++ (optional cfg.typst.enable typst);
      in
        pkgsToUse.symlinkJoin {
          name = "codedown-spellchecker";
          paths = parts;
          meta = markdown.meta;
          passthru = {
            languageServerNames = concatMap (p: p.languageServerNames) parts;
            settings = cfg.typst;
            settingsSchema = typstSettingsSchema;
          };
        };
  };
}
