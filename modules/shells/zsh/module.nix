{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    shells.zsh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the ZSH shell.";
      };

      powerline = mkOption {
        type = types.bool;
        default = false;
        description = "Enable Powerline in the ZSH shell.";
      };
    };
  };

  config = mkIf config.shells.zsh.enable {
    builtShells.zsh = let
      baseDerivation = config.pkgs.callPackage ./zsh-with-theme {};
    in
      baseDerivation.overrideAttrs (old: {
        meta = old.meta // {
          icon = ../default_icon_64x64.png;
          displayName = "ZSH " + baseDerivation.version;
          attr = "zsh";
          args = ["${baseDerivation}/bin/zsh-with-theme"];
        };
      });
  };
}
