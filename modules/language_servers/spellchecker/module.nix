{ config, options, lib, nixosOptionsToSettingsSchema, ... }:

with lib;

let
  pkgsToUse = config.pkgs;

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
        # Schema over the whole spellchecker subtree so both "Check Markdown" and the Typst options show up.
        # componentsToDrop = 2 keeps the nested keys distinct (markdown.enable, typst.enable, ...) and
        # avoids the empty loc that the top-level `enable` produces at drop 3.
        spellcheckerSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.language-servers.spellchecker;

        markdown = pkgsToUse.callPackage ../markdown-spellcheck-lsp {};
        typst = pkgsToUse.callPackage ../typst-spellcheck-lsp {
          settings = cfg.typst;
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
            settings = cfg;
            settingsSchema = spellcheckerSchema;
          };
        };
  };
}
