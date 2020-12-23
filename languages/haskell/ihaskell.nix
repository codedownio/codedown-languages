let nixpkgs = import <nixpkgs> {}; in
with nixpkgs;

callPackage ./IHaskell/release-8.8.nix {
  nixpkgs = nixpkgs;
  compiler = "ghc884";
  pythonPackages = _: [];
}
