# codedown.nix detects a codedown repo with
# `builtins.hasAttr "isCodeDown" (builtins.functionArgs imported)`, so isCodeDown has to stay
# in this header under exactly this name even though nothing in here reads it.
# deadnix: skip
{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub ? null
, ...
}:

let
  stableRev = "fd33cbc554eea069c5fbd63c3019c03cabfcdd44"; # nixpkgs-rev
  stableFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = stableRev;
    hash = "sha256-RZl2QoNE9HGliNKO2JGI4q3AYYr3m2ZvnyGub4B4bg0="; # nixpkgs-hash
  };
  stableBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${stableRev}.tar.gz'';
    sha256 = "03bfg206zbi1kxpnd6zpi9hw1bg2i28xi3nji2jp3x24hd17d6a5"; # nixpkgs-sha256
  };
  pkgsStableSrc = if fetchFromGitHub != null then stableFetchFromGitHub else stableBuiltins;
  pkgsStable = import pkgsStableSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix {
  inherit pkgsStableSrc pkgsStable;
  system = pkgsStable.stdenv.hostPlatform.system;
}
