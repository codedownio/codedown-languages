{ fetchFromGitHub
}:

let
  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "0b1bb5d28101cbb3fa798f100c618cb17f1632ef"; # nixpkgs-rev
    sha256 = "sha256-ZWekhqQrt9wETxTz19LssfeW6zzUOIG1fq6j+Ri0V3k="; # nixpkgs-sha256
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a2d2f70b82ada0eadbcb1df2bca32d841a3c1bf1"; # nixpkgs-unstable-rev
    sha256 = "sha256-EcDfOiTHs0UBAtyGc0wxJJdhcMjrJEgWXjJutxZGA3E="; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
