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
      visible = false;
      internal = true;
    };

    pkgsMaster = mkOption {
      type = types.attrs;
      default = pkgs;
      visible = false;
      internal = true;
    };

    builtExporters = mkOption {
      type = types.attrsOf types.package;
      default = {};
      visible = false;
      internal = true;
    };

    builtKernels = mkOption {
      type = types.attrsOf types.package;
      default = {};
      visible = false;
      internal = true;
    };

    builtLanguageServers = mkOption {
      type = types.attrsOf types.package;
      default = {};
      visible = false;
      internal = true;
    };

    packages = mkOption {
      type = types.attrsOf types.package;
      default = {};
      visible = false;
      internal = true;
    };

    extraBinDirs = mkOption {
      type = types.attrsOf (types.listOf types.package);
      default = {};
      visible = false;
      internal = true;
    };
  };

  config = {

  };
}
