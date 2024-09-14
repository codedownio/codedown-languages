{ config, lib, pkgs, ... }:

with lib;

rec {
  options = {
    kernels.bash = {
      enable = mkOption {
        type = types.bool;
        default = false;
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
    builtKernels.bash = config.pkgs.callPackage ./full.nix {
      bash = config.pkgs.bash;
      inherit (config.kernels.bash) attrs extensions settings;
      settingsSchema = config.nixosOptionsToSettingsSchema options;
    };
  };
}
