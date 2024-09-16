{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    language-servers.spellchecker = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the Markdown spellchecker.";
      };
    };
  };

  config = mkIf config.language-servers.spellchecker.enable {
    environmentPackages = [
      (config.pkgs.callPackage ./default.nix {})
    ];
  };
}
