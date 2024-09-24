{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.julia = {
      enable = mkOption {
        description = "Enable Haskell kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        type = types.listOf (types.either types.str types.attrs);
        default = [];
      };

      juliaPackage = mkOption {
        type = types.enum (
          ["julia"]
          ++ (builtins.filter (n:
            builtins.match "^julia_[0-9].*" n != null
            || builtins.match "^julia-lts.*" n != null
            || builtins.match "^julia-stable.*" n != null
          ) (builtins.attrNames config.pkgs))
        );
        default = "julia";
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["julia"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["jl"];
      };

      precompile = mkOption {
        description = "Precompile Julia environment for faster imports. In some cases, precompilation can make the build fail, so turning this off can help.";
        type = types.bool;
        default = true;
      };

      lsp.LanguageServer.enable = mkOption {
        description = "Enable LanguageServer language server";
        type = types.bool;
        default = true;
      };
      lsp.LanguageServer.index = mkOption {
        description = "LanguageServer: auto-index packages when building environment";
        type = types.bool;
        default = true;
      };
      lsp.LanguageServer.debug = mkOption {
        description = "LanguageServer: log debug messages to stderr";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf config.kernels.julia.enable {
    builtKernels.julia = config.pkgs.callPackage ./. {
      julia = getAttr config.kernels.julia.juliaPackage config.pkgs;

      settings = config.kernels.julia;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.julia;
    };
  };
}
