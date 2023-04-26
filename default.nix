{ fetchFromGitHub
, ...
}:

let
  pkgsStable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "3079baffea86d39727f653cc517d627b23a67f30"; # nixpkgs-rev
    sha256 = "1rnd98d7z9x8qfi0acg1acbb9nm91a9l4grqp92f6nr6wfj723b3"; # nixpkgs-sha256
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2362848adf8def2866fabbffc50462e929d7fffb"; # nixpkgs-unstable-rev
    sha256 = "0wjr874z2y3hc69slaa7d9cw7rj47r1vmc1ml7dw512jld23pn3p"; # nixpkgs-unstable-sha256
  }) {};

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsUnstable; }
