{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        tests = pkgs.haskell.packages.ghc965.callPackage ./tests.nix {};
      in
        rec {
          packages = rec {
            inherit tests;
            inherit (pkgs) cabal2nix;

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";

            # Print a trivial PATH that we can use to run kernel and LSP tests, to ensure
            # they aren't depending on anything on the test machine's PATH.
            print-basic-path = pkgs.writeShellScriptBin "codedown-artifact-sizes.sh" ''
              echo ${pkgs.lib.makeBinPath (with pkgs; [busybox bash])}
            '';
          };

          defaultPackage = packages.tests;

          devShell = tests.env;

          stack = pkgs.stack;
        }
    );
}
