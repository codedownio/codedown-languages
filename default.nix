final: prev:

with final;
with final.lib;

let
  # This must be chosen to match haskellNix.sources.nixpkgs!
  # We do it ourselves because we want to use fetchFromGitHub instead of fetchTarball.
  nixpkgsSrc = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "110a2c9ebbf5d4a94486854f18a37a938cfacbbb";
    sha256 = "0v12ylqxy1kl06dgln6h5k8vhlfzp8xvdymljj7bl0avr0nrgrcm";
  };

  haskellNix = import (fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell.nix";
    rev = "a3acfa49682870cc1db1120bd0c5f4a593de5884";
    sha256 = "1hxx57zcffi3ajibqrm9y3sssp8bwbdr0fyhr2lrvmzvv0k8n6ci";
  }) { pkgs = import nixpkgsSrc {}; };

in

rec {
  haskell-nix = (import nixpkgsSrc haskellNix.nixpkgsArgs).haskell-nix;
  ghc-boot-packages = (import nixpkgsSrc haskellNix.nixpkgsArgs).ghc-boot-packages;

  codedown = callPackage ./codedown.nix {};
}
