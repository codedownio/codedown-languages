{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub ? null
, ...
}:

let
  stableFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "6af28b834daca767a7ef99f8a7defa957d0ade6f"; # nixpkgs-rev
    hash = "sha256-W4YZ3fvWZiFYYyd900kh8P8wU6DHSiwaH0j4+fai1Sk="; # nixpkgs-hash
  };
  stableFetchTree = builtins.fetchTree {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "6af28b834daca767a7ef99f8a7defa957d0ade6f"; # nixpkgs-rev
    narHash = "sha256-W4YZ3fvWZiFYYyd900kh8P8wU6DHSiwaH0j4+fai1Sk="; # nixpkgs-hash
  };
  pkgsStable = import (if fetchFromGitHub != null then stableFetchFromGitHub else stableFetchTree) ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

  masterFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "25068c534b2c34bbff27c71af515177ad0cce061"; # nixpkgs-master-rev
    hash = "sha256-UOxazsKzbMALQlr5UWJ/4YKBzCsneXfdE/MkZZH9dFg="; # nixpkgs-master-hash
  };
  masterFetchTree = builtins.fetchTree {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "25068c534b2c34bbff27c71af515177ad0cce061"; # nixpkgs-master-rev
    narHash = "sha256-UOxazsKzbMALQlr5UWJ/4YKBzCsneXfdE/MkZZH9dFg="; # nixpkgs-master-hash
  };
  pkgsMaster = import (if fetchFromGitHub != null then masterFetchFromGitHub else masterFetchTree) ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsMaster; }
