{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub ? null
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
  stableBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${stableRev}.tar.gz'';
    sha256 = "0afmlbvgky283wd2qjn7l19k1zzh454x6z97cdc22rnnzgfik1jv"; # nixpkgs-sha256
  };
  pkgsStableSrc = if fetchFromGitHub != null then stableFetchFromGitHub else stableBuiltins;
  pkgsStable = import pkgsStableSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

  masterRev = "024c9aa846fa3ef3ef1c9f42c4a289c75a8f6f32"; # nixpkgs-master-rev
  masterFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = masterRev;
    hash = "sha256-EVAD9sH6hIeAxyubLNfU11GljL/0FCMautcUQxI3f5U="; # nixpkgs-master-hash
  };
  masterBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${masterRev}.tar.gz'';
    sha256 = "15bz6w94656pp8d2657lpy6aalfpskbjr6rbqy08g17sq7v06l0i"; # nixpkgs-master-sha256
  };
  pkgsMasterSrc = if fetchFromGitHub != null then masterFetchFromGitHub else masterBuiltins;
  pkgsMaster = import pkgsMasterSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix {
  inherit
    pkgsStableSrc pkgsStable
    pkgsMasterSrc pkgsMaster
  ;
}
