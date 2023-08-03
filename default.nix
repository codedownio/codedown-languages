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
    rev = "4a56ce9727a0c5478a836a0d8a8f641c5b9a3d5f"; # nixpkgs-unstable-rev
    sha256 = "Qq/MPkhS12Bl0X060pPvX3v9ac3f2rRQfHjjozPh/Qs="; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
