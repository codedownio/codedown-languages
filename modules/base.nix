{ lib, pkgs, ... }:

with lib;

{
  options = {
    name = mkOption {
      type = types.str;
      default = "codedown-environment";
    };

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

    extraBinDirs = mkOption {
      type = types.attrsOf (types.listOf types.package);
      default = {};
    };
  };

  config = {

  };
}
