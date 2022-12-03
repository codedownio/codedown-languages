{ fetchFromGitHub
}:

let
  pkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "f42a45c015f28ac3beeb0df360e50cdbf495d44b"; # release-22.05
    sha256 = "sha256-UjZlIrbHGlL3H3HZNPTxPSwJfr49jIfbPWCYxk0EQm4=";
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4c80e3665c019daaa91347fbcb4e5ae078cc39d3"; # nixpkgs-unstable
    sha256 = "sha256-R46LmO8VWjYjaeGBKjBL1zmvB1ziO5mtv0fJzkNbXiY=";
  }) {};

in

pkgs.callPackage ./codedown.nix { inherit pkgsUnstable; }
