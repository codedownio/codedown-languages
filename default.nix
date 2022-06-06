
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
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "f3ea06dcacc8a46b4a207a6a1fad14bc5ea41b19";
    sha256 = "0fs3b52k8qcpqz4mlq970djnww16dcngwscsfn84h4n9j3ng4glr";
  }) { pkgs = import nixpkgsSrc {}; };

in

rec {
  haskell-nix = (import nixpkgsSrc haskellNix.nixpkgsArgs).haskell-nix;
  ghc-boot-packages = (import nixpkgsSrc haskellNix.nixpkgsArgs).ghc-boot-packages;

  codedown = callPackage ./codedown.nix {};
}
