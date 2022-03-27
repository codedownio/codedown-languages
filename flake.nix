{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/973910f5c31b9ba6c171c33a8bd7199990b14c72";
  # inputs.codedown = ./default.nix;
  # overlay = import ./.;

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [ x86_64-linux ]) (system:
      let
        overlays = [ (import ./default.nix) ];
        pkgs = import nixpkgs { inherit system overlays; };
      in
        {
          languagesSearcher = (pkgs.callPackage ./codedown.nix {}).languagesSearcher;
          # packages.x86_64-linux.languagesSearcher2 = (pkgs.callPackage ./codedown.nix {}).languagesSearcher;
          # defaultPackage.x86_64-linux = self.packages.x86_64-linux.languagesSearcher;
        }
    );
}
