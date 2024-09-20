{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    exporters.nbconvert = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the nbconvert exporters.";
      };

      texliveScheme = mkOption {
        type = types.enum [
          "scheme-basic"
          "scheme-bookpub"
          "scheme-full"
          "scheme-medium"
          "scheme-minimal"
          "scheme-small"
          "scheme-tetex"
        ];
        default = "scheme-small";
        description = "The TeX Live scheme to use, as an attribute of pkgs.texlive.combined.*";
      };
    };
  };

  config = mkIf config.exporters.nbconvert.enable {
    builtExporters.nbconvert = config.pkgs.callPackage ./nbconvert.nix {
      texliveScheme = config.pkgs.texlive.combined.${config.exporters.nbconvert.texliveScheme};
    };
  };
}
