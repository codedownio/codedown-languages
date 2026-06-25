{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgsMaster;

in

{
  options = {
    kernels.julia = {
      enable = mkOption {
        title = "Enable Haskell kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      juliaPackage = mkOption {
        title = "Julia version";
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
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["julia"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["jl"];
      };

      precompile = mkOption {
        title = "Precompile Julia environment for faster imports";
        description = "In some cases, precompilation can make the build fail, so turning this off can help.";
        type = types.bool;
        default = true;
      };

      lsp.LanguageServer.enable = mkOption {
        title = "Enable LanguageServer language server";
        type = types.bool;
        default = true;
      };
      lsp.LanguageServer.index = mkOption {
        title = "Auto-index packages when building environment";
        type = types.bool;
        default = true;
      };
      lsp.LanguageServer.debug = mkOption {
        title = "Log debug messages to stderr";
        type = types.bool;
        default = false;
      };

      misc.enableVariableInspector = mkOption {
        title = "Enable the variable inspector";
        description = "This will show a summary of the currently defined variables in the UI.";
        type = types.bool;
        default = true;
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
