{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgs = config.pkgsMaster;

in

{
  options = {
    kernels.ruby = {
      enable = mkOption {
        example = "Enable Ruby kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of packages";
        type = types.listOf types.str;
        default = [];
      };

      rubyPackage = mkOption {
        example = "Ruby version";
        type = types.enum (
          ["ruby"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "ruby_") name == "ruby_")
                              (builtins.attrNames pkgs))
        );
        default = "ruby";
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["ruby"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["rb"];
      };

      lsp.solargraph.enable = mkOption {
        example = "Enable Solargraph language server";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.ruby.enable {
    builtKernels.ruby = pkgs.callPackage ./. {
      ruby = getAttr config.kernels.ruby.rubyPackage pkgs;

      settings = config.kernels.ruby;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.ruby;
    };
  };
}
