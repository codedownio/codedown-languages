{ fetchFromGitHub
}:

let
  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "6d3fc36c541ae715d43db5c1355890f39024b26f"; # nixpkgs-rev
    sha256 = "sha256-cRsIC0Ft5McBSia0rDdJIHy3muWqKn3rvjFx92DU2dY="; # nixpkgs-sha256
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a2d2f70b82ada0eadbcb1df2bca32d841a3c1bf1"; # nixpkgs-unstable-rev
    sha256 = "sha256-EcDfOiTHs0UBAtyGc0wxJJdhcMjrJEgWXjJutxZGA3E="; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
