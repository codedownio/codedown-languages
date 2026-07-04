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

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.markdown.enable {
      builtLanguageServers.markdown-spellchecker = pkgsToUse.callPackage ../markdown-spellcheck-lsp {};
    })

    (mkIf cfg.typst.enable {
      builtLanguageServers.typst-spellchecker = pkgsToUse.callPackage ../typst-spellcheck-lsp {
        settings = cfg.typst;
        settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 3; } options.language-servers.spellchecker.typst;
      };
    })
  ]);
}
