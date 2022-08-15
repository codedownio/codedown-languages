{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/051448e41537c3463ae776d46115d01afb6c498d";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        myLspTest = with pkgs; haskell.lib.overrideSrc haskellPackages.lsp-test { src = /home/tom/tools/haskell-lsp/lsp-test; };
        myLspTypes = with pkgs; haskell.lib.overrideSrc haskellPackages.lsp-types { src = /home/tom/tools/haskell-lsp/lsp-types; };

        tests = pkgs.haskell.packages.ghc8107.callPackage ./tests.nix {
          lsp-test = myLspTest;
          lsp-types = myLspTypes;
        };
      in
        rec {
          packages = rec {
            inherit tests;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.tests;

          inherit myLspTypes;

          nixpkgsPath = pkgs.path;
          testPath = with pkgs; lib.makeBinPath [stack];

          devShell = tests.env;

          stack = pkgs.stack;
        }
    );
}
