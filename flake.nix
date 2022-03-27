{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/973910f5c31b9ba6c171c33a8bd7199990b14c72";
  # inputs.codedown = ./default.nix;
  # overlay = import ./.;

  outputs = { self, nixpkgs }:
    let
      codedown = nixpkgs.packages.callPackage ./codedown.nix {};
      basePkgs = import nixpkgs { system = "x86_64-linux"; };
      overlays = [ (basePkgs.callPackage ./codedown.nix {}) ];
      pkgs = import nixpkgs { system = "x86_64-linux"; inherit overlays; };
    in
      {
        packages.x86_64-linux.languagesSearcher = pkgs.codedown.languagesSearcher;

        # defaultPackage.x86_64-linux = self.packages.x86_64-linux.languagesSearcher;
      };
}
