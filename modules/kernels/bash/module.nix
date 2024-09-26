{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.bash = {
      enable = mkOption {
        description = "Enable Bash kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["bash"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["sh" "bash"];
      };

      lsp.bash-language-server.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable Bash language server";
      };
    };
  };

  config = mkIf config.kernels.bash.enable {
    builtKernels.bash = config.pkgs.callPackage ./. {
      bash = config.pkgs.bash;
      settings = config.kernels.bash;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.bash;
    };
  };
}
