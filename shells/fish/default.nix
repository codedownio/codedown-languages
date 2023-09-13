{ callPackage
, runCommand
, makeWrapper
, stdenv
, fish
}:

let
  common = callPackage ../common.nix {};
  baseDerivation = callPackage ./fish.nix {};

in

common.wrapShell {
  executableName = "fish";
  inherit baseDerivation;
  displayName = "Fish " + baseDerivation.version;
  attr = "fish";
  icon = ./icon-64x64.png;
}
