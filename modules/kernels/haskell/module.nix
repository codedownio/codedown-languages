{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, ... }:

with lib;

{
  options = {
    kernels.haskell = {
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

      ghcPackage = mkOption {
        type = types.enum ["ghc92" "ghc94" "ghc96" "ghc98"];
        default = "ghc92";
      };

      attrs = mkOption {
        type = types.listOf types.str;
        default = ["haskell"];
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = ["hs"];
      };

      settings = {
        lsp.haskell-language-server.enable = mkOption {
          description = "Enable haskell-language-server";
          type = types.bool;
          default = true;
        };
        lsp.haskell-language-server.debug = mkOption {
          description = "Haskell-language-server: enable debug output";
          type = types.bool;
          default = false;
        };
        lsp.haskell-language-server.super-debug = mkOption {
          description = "Haskell-language-server: enable verbose debug output";
          type = types.bool;
          default = false;
        };
        enableHlintOutput = mkOption {
          description = "Enable hlint warnings in Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server.";
          type = types.bool;
          default = false;
        };
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

        inherit (config.kernels.haskell) packages attrs extensions settings;
        settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.haskell;
      };
  };
}
