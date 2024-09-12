{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    exporters.nbconvert-exporters = {
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

  config = mkIf config.exporters.nbconvert-exporters.enable {
    builtExporters.nbconvert-exporters = config.pkgs.callPackage ./nbconvert.nix {
      texliveScheme = config.pkgs.texlive.combined.${config.exporters.nbconvert-exporters.texliveScheme};
    };
  };
}
