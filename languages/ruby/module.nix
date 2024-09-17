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
        type = types.enum (
          ["ruby"]
          ++ (builtins.filter (name: builtins.substring 0 (builtins.stringLength "ruby_") name == "ruby_")
                              (builtins.attrNames config.pkgs))
        );
        default = "ruby";
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
    builtKernels.ruby = config.pkgs.callPackage ./. {
      ruby = getAttr config.kernels.ruby.rubyPackage config.pkgs;

      inherit (config.kernels.ruby) packages attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema options.kernels.ruby;
    };
  };
}
