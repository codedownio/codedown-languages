{ config, lib, pkgs, ... }:

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

    builtShells = mkOption {
      type = types.attrsOf types.package;
      default = {};
    };

    packages = mkOption {
      type = types.listOf types.package;
      default = [];
      example = literalExpression "[ pkgs.firefox pkgs.thunderbird ]";
      description = ''
        The set of packages that are symlinked into the environment.
      '';
    };

    channels = mkOption {
      type = types.attrs;
      default = {};
      description = ''
        Channels, passed through to UI metadata.
      '';
    };

    labeledPackages = mkOption {
      type = types.listOf (types.submodule {
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
