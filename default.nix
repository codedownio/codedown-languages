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
  stableBuiltins = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6af28b834daca767a7ef99f8a7defa957d0ade6f.tar.gz"; # nixpkgs-rev
    sha256 = "0afmlbvgky283wd2qjn7l19k1zzh454x6z97cdc22rnnzgfik1jv"; # nixpkgs-sha256
  };
  pkgsStable = import (if fetchFromGitHub != null then stableFetchFromGitHub else stableBuiltins) ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

  masterFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "25068c534b2c34bbff27c71af515177ad0cce061"; # nixpkgs-master-rev
    hash = "sha256-UOxazsKzbMALQlr5UWJ/4YKBzCsneXfdE/MkZZH9dFg="; # nixpkgs-master-hash
  };
  masterBuiltins = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/25068c534b2c34bbff27c71af515177ad0cce061.tar.gz"; # nixpkgs-master-rev
    sha256 = "0n3lzn8na97k2gfpfy975g6830p1gxi53yas885w0v5kqb75mv2h"; # nixpkgs-master-sha256
  };
  pkgsMaster = import (if fetchFromGitHub != null then masterFetchFromGitHub else masterBuiltins) ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsMaster; }
