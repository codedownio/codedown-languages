{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/973910f5c31b9ba6c171c33a8bd7199990b14c72";
  # inputs.codedown = ./default.nix;
  # overlay = import ./.;

  outputs = { self, nixpkgs }:
    let
      codedown = nixpkgs.pkgs.callPackage ./codedown.nix {};
      nixpkgs.overlays = [ codedown ];
    in
      {

        packages.x86_64-linux.languagesSearcher = codedown.languagesSearcher;

        # defaultPackage.x86_64-linux = self.packages.x86_64-linux.languagesSearcher;
      };
}
