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
    rev = "cd99c2b3c9f160cd004318e0697f90bbd5960825"; # nixpkgs-unstable-rev
    sha256 = "sha256-cdW6qUL71cNWhHCpMPOJjlw0wzSRP0pVlRn2vqX/VVg="; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
