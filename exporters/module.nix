{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    exporters.nbconvert-exporters = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the nbconvert exporters version.";
      };

      texliveScheme = mkOption {
        type = types.package;
        default = config.pkgs.texlive.combined.scheme-small; # scheme-full
        description = "Enable the nbconvert exporters version.";
      };
    };
  };

  config = mkIf config.exporters.nbconvert-exporters.enable {
    builtExporters.nbconvert-exporters = config.pkgs.callPackage ./nbconvert.nix {
      texliveScheme = config.exporters.nbconvert-exporters.texliveScheme;
    };
  };
}
