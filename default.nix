{ fetchFromGitHub
, ...
}:

let
  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "3079baffea86d39727f653cc517d627b23a67f30"; # nixpkgs-rev
    sha256 = "1rnd98d7z9x8qfi0acg1acbb9nm91a9l4grqp92f6nr6wfj723b3"; # nixpkgs-sha256
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a2d2f70b82ada0eadbcb1df2bca32d841a3c1bf1"; # nixpkgs-unstable-rev
    sha256 = "sha256-EcDfOiTHs0UBAtyGc0wxJJdhcMjrJEgWXjJutxZGA3E="; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
