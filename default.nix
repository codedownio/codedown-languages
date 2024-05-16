{ fetchFromGitHub
, ...
}:

let
  overlays = [(import ./overlays.nix)];

  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "c6149db25c6ca2d606c5f2425a08f458e7f0123b"; # nixpkgs-rev
    sha256 = "sha256-AYZLTctK+w2Xh/BXl5fm7DOecq7rnsfL+UAWbI0VXyw="; # nixpkgs-sha256
  }) { inherit overlays; };

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "d03a4482228d4d6dbd2d4b425b6dfcd49ebe765f"; # nixpkgs-unstable-rev
    sha256 = "sha256-FBVpEX0eLiqX3jnSL3rmJHqHhbuCikJZyDyV3Cl3qAY="; # nixpkgs-unstable-sha256
  }) { inherit overlays; };

  pkgsMaster = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a9f70e6bba466a6db6f0e5689608bf76c1e126c2"; # nixpkgs-master-rev
    sha256 = "sha256-y6v4t6UZMjwbDU5QMgHQEkb5jAFu1V0KUHVoeSSXUik="; # nixpkgs-master-sha256
  }) { inherit overlays; };

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable pkgsMaster; }
