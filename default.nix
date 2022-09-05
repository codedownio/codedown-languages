{ fetchFromGitHub
}:

let
  pkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "50c62eeda9df340ff6b83a0e2343a447af04237c"; # release-22.05
    sha256 = "00pvx1jykgz0k71wkgk60gfbchi7sa66m5xa89z3vcmsb04lfhin";
  }) {};

  pkgsUnstable = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "21de2b973f9fee595a7a1ac4693efff791245c34"; # nixpkgs-unstable
    sha256 = "1kicpg62jsyh4blyykvbkq1vg4rv56k22qpp8kr9mj3fjabkrr27";
  }) {};

in

pkgs.callPackage ./codedown.nix { inherit pkgsUnstable; }
