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

common.wrapShell "fish" baseDerivation ("Fish " + baseDerivation.version) ./icon-64x64.png
