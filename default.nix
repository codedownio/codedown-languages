{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub ? null
, ...
}:

let
  stableRev = "e7215ec9581d62b85a3b1b870cd40cc353a06816"; # nixpkgs-rev
  stableFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = stableRev;
    hash = "sha256-/PS45tTr/AN2x7rjT/fj1Scc5Fz0WjePt9davTICV8E="; # nixpkgs-hash
  };
  stableBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${stableRev}.tar.gz'';
    sha256 = "1hap08rbsnnpny7kfnplbkj1q9ymwgvlzqxsqxv07z7bskkbix7w"; # nixpkgs-sha256
  };
  pkgsStableSrc = if fetchFromGitHub != null then stableFetchFromGitHub else stableBuiltins;
  pkgsStable = import pkgsStableSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix {
  inherit pkgsStableSrc pkgsStable;
}
