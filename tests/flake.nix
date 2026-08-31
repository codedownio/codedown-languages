{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  # A Nixpkgs haskell.nix has built GHC against, so we fetch the compiler rather than build it.
  inputs.nixpkgsHaskellNix.follows = "haskellNix/nixpkgs-2605";

  # The suite lists sample_environments at compile time, and it lives a directory up.
  inputs.repo = { url = "path:.."; flake = false; };

  outputs = { self, flake-utils, nixpkgs, haskellNix, nixpkgsHaskellNix, repo }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # hackage.nix has a package whose Cabal flag is literally named "3d", which it emits
        # unquoted, so importing it is a Nix syntax error.
        haskellNixOverlay = (import (haskellNix + "/overlays") {
          sources = haskellNix.inputs // {
            hackage-for-stackage = (import nixpkgsHaskellNix { inherit system; }).applyPatches {
              name = "hackage-for-stackage-hgg-3d-flag";
              src = haskellNix.inputs.hackage-for-stackage;
              patches = [ ./nix/haskell-nix-patches/fix-hgg-3d-flag.patch ];
            };
          };
        }).combined;

        pkgsHaskellNix = import nixpkgsHaskellNix {
          inherit system;
          overlays = [ haskellNixOverlay ];
          inherit (haskellNix) config;
        };

        # Narrowed to the one directory, so the suite doesn't rebuild when anything else in the
        # repo changes.
        sampleEnvironments = builtins.path {
          name = "sample_environments";
          path = repo + "/sample_environments";
        };

        src = pkgsHaskellNix.runCommand "codedown-languages-tests-source" {} ''
          cp -r ${./.} $out
          chmod -R u+w $out
          cp -r ${sampleEnvironments} $out/sample_environments
        '';

        # compiler-nix-name must be the GHC stack.yaml's resolver targets, or haskell.nix
        # rebuilds the compiler's boot libs and configure fails on missing dependencies.
        testsFlake = (pkgsHaskellNix.haskell-nix.hix.project {
          inherit src;
          projectFileName = "stack.yaml";
          compiler-nix-name = "ghc9124";
          modules = [
            (import ./nix/fix-ghc-pkgs-module.nix)
            (import ./nix/os-string-module.nix)
          ];
        }).flake {};
      in
        {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                openssh
                pkg-config
                postgresql
                postgresql.dev
                postgresql.pg_config
                zlib

                haskell.compiler.ghc9124
              ];

              NIX_PATH = "nixpkgs=${pkgs.path}";
            };
          };

          packages = {
            inherit (pkgs) cabal2nix;

            tests = testsFlake.packages."tests:exe:tests";

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";

            # Print a trivial PATH that we can use to run kernel and LSP tests, to ensure
            # they aren't depending on anything on the test machine's PATH.
            print-basic-path = pkgs.writeShellScriptBin "basic-path.sh" ''
              echo ${pkgs.lib.makeBinPath (with pkgs; [coreutils bash])}
            '';

            inherit (pkgs) direnv stack;
          };
        }
    );
}
