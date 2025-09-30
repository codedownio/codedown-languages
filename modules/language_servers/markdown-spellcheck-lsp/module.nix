{ config, lib, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

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
    builtLanguageServers.spellchecker = pkgsToUse.callPackage ./. {};
  };
}
