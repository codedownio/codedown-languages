{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    pkgs = lib.mkOption {
      type = lib.types.attrs;
      default = pkgs;
    };

    pkgsMaster = lib.mkOption {
      type = lib.types.attrs;
      default = pkgs;
    };

    builtExporters = lib.mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    builtKernels = lib.mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    builtShells = lib.mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      example = lib.literalExpression "[ pkgs.firefox pkgs.thunderbird ]";
      description = ''
        The set of packages that are symlinked into the environment.
      '';
    };
  };

  config = {

  };
}
