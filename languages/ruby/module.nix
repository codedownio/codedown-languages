{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.ruby = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      rubyPackage = mkOption {
        type = types.package;
        default = config.pkgs.ruby;
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["ruby"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["rb"];
      };

      settings.lsp.solargraph.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable Solargraph language server";
      };
    };
  };

  config = mkIf config.kernels.ruby.enable {
    builtKernels.ruby = config.pkgs.callPackage ./full.nix {
      ruby = config.kernels.ruby.rubyPackage;
      inherit (config.kernels.ruby) packages attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema options.kernels.ruby;
    };
  };
}
