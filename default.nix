{ fetchFromGitHub
, ...
}:

let
  overlays = [(import ./overlays.nix)];

  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "de429c2a20520e0f81a1fd9d2677686a68cae739"; # nixpkgs-rev
    hash = "sha256-D2YTs7K33zAzoQdAcVzePgDn6bdIEexGgHluoB07+Yw="; # nixpkgs-sha256
  }) { inherit overlays; };

  pkgsMaster = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "05bbf675397d5366259409139039af8077d695ce"; # nixpkgs-master-rev
    hash = "sha256-IE7PZn9bSjxI4/MugjAEx49oPoxu0uKXdfC+X7HcRuQ="; # nixpkgs-master-sha256
  }) { inherit overlays; };

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsMaster; }
