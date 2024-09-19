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

    labeledPackages = lib.mkOption {
      type = lib.types.listOf (types.submodule {
        options = {
          channel = mkOption rec {
            type = types.str;
            description = "Channel name";
          };
          attr = mkOption rec {
            type = types.str;
            description = "Attr name";
          };
          contents = mkOption rec {
            type = types.package;
            description = "Package";
          };
        };
      });
      default = [];
      description = ''
        Packages that are labeled with channels and attributes. Used to generate UI metadata.
      '';
    };
  };

  config = {

  };
}
