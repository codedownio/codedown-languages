{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

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
          ) (builtins.attrNames pkgsToUse))
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
    builtKernels.julia = pkgsToUse.callPackage ./. {
      julia = let
        juliaPackage = config.kernels.julia.juliaPackage;
        requestedJulia = getAttr juliaPackage pkgsToUse;
        hasBin = builtins.hasAttr (juliaPackage + "-bin") pkgsToUse;
        # On Darwin, nixpkgs lists julia source builds as supported (meta.unsupported = false)
        # but the actual build fails (PCRE2 build errors). Prefer the -bin variant on Darwin
        # whenever it's available, since it works in practice.
        preferBin = requestedJulia.meta.unsupported || (pkgsToUse.stdenv.isDarwin && hasBin);
        in
          if preferBin
          then (if hasBin then pkgsToUse.${juliaPackage + "-bin"} else throw "${juliaPackage} is not supported on this system and fallback ${juliaPackage}-bin was not found.")
          else requestedJulia;

      settings = config.kernels.julia;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.julia;
    };
  };
}
