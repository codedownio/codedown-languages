
let
  pkgsStable = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "6d3fc36c541ae715d43db5c1355890f39024b26f"; # nixpkgs-rev
    ref = "release-22.11";
    shallow = true;
  }) {};

in

pkgsStable.callPackage ./codedown.nix {
  inherit pkgsStable;
  pkgsUnstable = pkgsStable.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a2d2f70b82ada0eadbcb1df2bca32d841a3c1bf1"; # nixpkgs-unstable-rev
    sha256 = "sha256-EcDfOiTHs0UBAtyGc0wxJJdhcMjrJEgWXjJutxZGA3E="; # nixpkgs-unstable-sha256
  };
}
