{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    environmentPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      example = lib.literalExpression "[ pkgs.firefox pkgs.thunderbird ]";
      description = ''
        The set of packages that are symlinked into the environment.
      '';
    };
  };

  config = {};
}
