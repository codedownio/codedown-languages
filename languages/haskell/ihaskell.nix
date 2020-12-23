with import <nixpkgs> {};
with makeWrapper;

callPackage ./IHaskell/release-8.8.nix {
  nixpkgs = nixpkgs;
  compiler = "ghc884";
  pythonPackages = _: [];
}
