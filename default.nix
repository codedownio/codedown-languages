{ fetchFromGitHub ? (import <nixpkgs> {}).fetchFromGitHub
, isCodeDown ? true # For introspection using builtins.functionArgs
, ...
}:

let
  overlays = [];

  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "6af28b834daca767a7ef99f8a7defa957d0ade6f"; # nixpkgs-rev
    hash = "sha256-W4YZ3fvWZiFYYyd900kh8P8wU6DHSiwaH0j4+fai1Sk="; # nixpkgs-sha256
  }) { inherit overlays; };

  pkgsMaster = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "25068c534b2c34bbff27c71af515177ad0cce061"; # nixpkgs-master-rev
    hash = "sha256-UOxazsKzbMALQlr5UWJ/4YKBzCsneXfdE/MkZZH9dFg="; # nixpkgs-master-sha256
  }) { inherit overlays; };

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsMaster; }
