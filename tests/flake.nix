{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ce6aa13369b667ac2542593170993504932eb836";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        tests = pkgs.haskell.packages.ghc8107.callPackage ./tests.nix {};
      in
        rec {
          packages = rec {
            inherit tests;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.tests;

          nixpkgsPath = pkgs.path;

          devShell = tests.env;

          stack = pkgs.stack;
        }
    );
}
