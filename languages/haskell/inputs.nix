{ fetchFromGitHub }:

rec {
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
    rev = "4cda3aeea8acd0837f6ad4a18a793d3d5901862a";
    sha256 = "sha256-D9xRMBhsjHi5Ox2SsnzlwgcMXZlAXWP5c/AqFY0kxl4=";
  }) { pkgs = import nixpkgsSrc {}; };
}
