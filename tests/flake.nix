{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        tests = pkgs.haskell.packages.ghc965.callPackage ./tests.nix {};
      in
        rec {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                openssh
                pkg-config
                postgresql
                postgresql.dev
                postgresql.pg_config
                zlib

                haskell.compiler.ghc96
              ];

              NIX_PATH = "nixpkgs=${pkgs.path}";
            };
          };

          packages = {
            inherit tests;
            inherit (pkgs) cabal2nix;

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";

            # Print a trivial PATH that we can use to run kernel and LSP tests, to ensure
            # they aren't depending on anything on the test machine's PATH.
            print-basic-path = pkgs.writeShellScriptBin "codedown-artifact-sizes.sh" ''
              echo ${pkgs.lib.makeBinPath (with pkgs; [coreutils bash])}
            '';
          };

          defaultPackage = packages.tests;

          stack = pkgs.stack;
        }
    );
}
