{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

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
      };
      ghcPackage = mkOption {
        example = "GHC version";
        type = types.enum ["ghc92" "ghc94" "ghc96" "ghc98"];
        default = "ghc92";
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
      pkgs = config.pkgs;

      compilers = pkgs.callPackage ./compilers.nix {
        ihaskell-source = pkgs.fetchFromGitHub {
          owner = "codedownio";
          repo = "IHaskell";
          rev = "72e663bcc1af12fc136d19941cf21efdf7341379";
          sha256 = "WSXrx+/iAiGa8qIJc7Wt6VxL9adw5KFt6FfaiOH/mjg=";
        };
      };

    in

      pkgs.callPackage ./. {
        compilerName = config.kernels.haskell.ghcPackage;
        snapshot = getAttr config.kernels.haskell.ghcPackage compilers;

        settings = config.kernels.haskell;
        settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.haskell;
      };
  };
}
