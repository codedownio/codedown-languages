{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.bash = {
      enable = mkOption {
        example = "Enable Bash kernel";
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
        example = "Enable Bash language server";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.bash.enable {
    builtKernels.bash = pkgsToUse.callPackage ./. {
      bash = pkgsToUse.bash;
      settings = config.kernels.bash;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.bash;
    };
  };
}
