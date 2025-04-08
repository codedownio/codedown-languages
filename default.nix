{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub
, ...
}:

let
  stableRev = "6af28b834daca767a7ef99f8a7defa957d0ade6f"; # nixpkgs-rev
  stableFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = stableRev;
    hash = "sha256-W4YZ3fvWZiFYYyd900kh8P8wU6DHSiwaH0j4+fai1Sk="; # nixpkgs-hash
  };
  pkgsStable = import stableFetchFromGitHub ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

  masterRev = "25068c534b2c34bbff27c71af515177ad0cce061"; # nixpkgs-master-rev
  masterFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = masterRev;
    hash = "sha256-UOxazsKzbMALQlr5UWJ/4YKBzCsneXfdE/MkZZH9dFg="; # nixpkgs-master-hash
  };
  pkgsMaster = import masterFetchFromGitHub ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsMaster; }
