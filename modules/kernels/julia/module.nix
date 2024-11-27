{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.julia = {
      enable = mkOption {
        example = "Enable Haskell kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        example = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      juliaPackage = mkOption {
        example = "Julia version";
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

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["julia"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["jl"];
      };

      precompile = mkOption {
        example = "Precompile Julia environment for faster imports";
        description = "In some cases, precompilation can make the build fail, so turning this off can help.";
        type = types.bool;
        default = true;
      };

      lsp.LanguageServer.enable = mkOption {
        example = "Enable LanguageServer language server";
        type = types.bool;
        default = true;
      };
      lsp.LanguageServer.index = mkOption {
        example = "Auto-index packages when building environment";
        type = types.bool;
        default = true;
      };
      lsp.LanguageServer.debug = mkOption {
        example = "Log debug messages to stderr";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf config.kernels.julia.enable {
    builtKernels.julia = config.pkgs.callPackage ./. {
      julia = let
        juliaPackage = config.kernels.julia.juliaPackage;
        requestedJulia = getAttr juliaPackage config.pkgs;
        in
          if requestedJulia.meta.unsupported
          then (if builtins.hasAttr (juliaPackage + "-bin") config.pkgs then config.pkgs.${juliaPackage + "-bin"} else throw "${juliaPackage} is not supported on this system and fallback ${juliaPackage}-bin was not found.")
          else requestedJulia;

      settings = config.kernels.julia;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.julia;
    };
  };
}
