{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        tests = pkgs.haskell.packages.ghc928.callPackage ./tests.nix {};
      in
        rec {
          packages = rec {
            inherit tests;
            inherit (pkgs) cabal2nix;

            # Print a trivial PATH that we can use to run kernel and LSP tests, to ensure
            # they aren't depending on anything on the test machine's PATH.
            print-basic-path = pkgs.writeShellScriptBin "codedown-artifact-sizes.sh" ''
              echo ${pkgs.lib.makeBinPath (with pkgs; [busybox bash])}
            '';
          };

          defaultPackage = packages.tests;

          nixpkgsPath = pkgs.path;

          devShell = tests.env;

          stack = pkgs.stack;
        }
    );
}
