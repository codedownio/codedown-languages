
let
  pkgsStable = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "6d3fc36c541ae715d43db5c1355890f39024b26f"; # nixpkgs-rev
    ref = "release-22.11";
  }) {};

  pkgsUnstable = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "a2d2f70b82ada0eadbcb1df2bca32d841a3c1bf1"; # nixpkgs-unstable-rev
    ref = "nixpkgs-unstable";
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
