{ lib, pkgs, ... }:

with lib;

{
  options = {
    pkgs = mkOption {
      type = types.attrs;
      default = pkgs;
    };

    pkgsMaster = mkOption {
      type = types.attrs;
      default = pkgs;
    };

    builtExporters = mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    builtKernels = mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    builtLanguageServers = mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    packages = mkOption {
      type = types.attrsOf types.package;
      default = {};
    };
  };

  config = {

  };
}
