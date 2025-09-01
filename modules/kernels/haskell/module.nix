{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgs;

in

{
  options = {
    kernels.haskell = {
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

      ghcPackage = mkOption {
        example = "GHC version";
        type = types.enum ["ghc94" "ghc96" "ghc98" "ghc910" "ghc912"];
        default = "ghc910";
      };

      interface.attrs = mkOption {
        example = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["haskell"];
      };
      interface.extensions = mkOption {
        example = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["hs"];
      };

      lsp.haskell-language-server.enable = mkOption {
        example = "Enable haskell-language-server";
        type = types.bool;
        default = true;
      };
      lsp.haskell-language-server.debug = mkOption {
        example = "Haskell-language-server: enable debug output";
        type = types.bool;
        default = false;
      };
      lsp.haskell-language-server.super-debug = mkOption {
        example = "Haskell-language-server: enable verbose debug output";
        type = types.bool;
        default = false;
      };

      enableHlintOutput = mkOption {
        example = "Enable hlint warnings in Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server.";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf config.kernels.haskell.enable {
    builtKernels.haskell = let
      compilers = pkgsToUse.callPackage ./compilers.nix {
        ihaskell-source = pkgsToUse.fetchFromGitHub {
          owner = "codedownio";
          repo = "IHaskell";
          rev = "0cd3dc2a930581eaee4560175486b9ceb5945632";
          sha256 = "sha256-2SeTakYt/DPQ7S+uHVOVbL/xAcil7g5OGPIx+ZSKYCA=";
        };
      };

      compilerName = config.kernels.haskell.ghcPackage;

      compilerNameToUse =
        if compilerName == "ghc910" then "ghc9102"
        else compilerName;

    in

      pkgsToUse.callPackage ./. {
        compilerName = compilerNameToUse;
        snapshot = compilers.${compilerNameToUse};

        settings = config.kernels.haskell;
        settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.haskell;
      };
  };
}
