{ fetchFromGitHub
, ...
}:

let
  overlays = [(import ./overlays.nix)];

  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b4b5c34deb3e45ac0761eb22f69db01ec52e526b"; # nixpkgs-rev
    sha256 = "sha256-QGExaYnu5l5XtDDtkf5T1BufDkbYh05iVEz2ELcm3cA="; # nixpkgs-sha256
  }) { inherit overlays; };

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "61202fc8677a6e9e0a82eb6610eeef28852fc790"; # nixpkgs-unstable-rev
    sha256 = "sha256-iMsZCHbMArLfg9pP5xzSSQf0/IvQ9kAAQ4w0a3sQtn8="; # nixpkgs-unstable-sha256
  }) { inherit overlays; };

  pkgsMaster = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "56746b6d6cf8557ce5d7f811c33ed75170c8b4e1"; # nixpkgs-master-rev
    sha256 = "sha256-JKigW8FiR1JcYC5T1sqS03z24G7uo+S0+MhKQEgen0I="; # nixpkgs-master-sha256
  }) { inherit overlays; };

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable pkgsMaster; }
