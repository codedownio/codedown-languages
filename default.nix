{ fetchFromGitHub
}:

let
  nixpkgsSrc = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ce6aa13369b667ac2542593170993504932eb836"; # 22.05
    sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
  };

  pkgs = import nixpkgsSrc {};

in

pkgs.callPackage ./codedown.nix {}
