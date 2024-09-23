{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

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

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["bash"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["sh" "bash"];
      };

      settings.lsp.bash-language-server.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable Bash language server";
      };
    };
  };

  config = mkIf config.kernels.bash.enable {
    builtKernels.bash = config.pkgs.callPackage ./. {
      bash = config.pkgs.bash;
      inherit (config.kernels.bash) attrs extensions settings;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.bash;
    };
  };
}
