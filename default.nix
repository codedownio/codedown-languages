{ fetchFromGitHub
, ...
}:

let
  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b4b5c34deb3e45ac0761eb22f69db01ec52e526b"; # nixpkgs-rev
    sha256 = "sha256-QGExaYnu5l5XtDDtkf5T1BufDkbYh05iVEz2ELcm3cA="; # nixpkgs-sha256
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "61202fc8677a6e9e0a82eb6610eeef28852fc790"; # nixpkgs-unstable-rev
    sha256 = "sha256-iMsZCHbMArLfg9pP5xzSSQf0/IvQ9kAAQ4w0a3sQtn8="; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
