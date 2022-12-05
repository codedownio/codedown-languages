{ fetchFromGitHub
}:

let
  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "f42a45c015f28ac3beeb0df360e50cdbf495d44b"; # release-22.05
    sha256 = "0vj20i6wd6307pdqg31xprz0jb1xy7s39nbi3zvm46n7nqi6adjj";
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4c80e3665c019daaa91347fbcb4e5ae078cc39d3"; # nixpkgs-unstable
    sha256 = "09jybd1wxja7pynrjfz2bh3syffp9cq2m0g1d4ikcnhmxyc8p3j7";
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
