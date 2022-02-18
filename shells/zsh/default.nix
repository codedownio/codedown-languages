{ callPackage
, runCommand
, makeWrapper
, stdenv
, zsh
}:

let
  common = callPackage ../common.nix {};
  baseDerivation = callPackage ./zsh-with-theme {};

in

common.wrapShell "zsh-with-theme" baseDerivation ("ZSH " + baseDerivation.version) ../default_icon_64x64.png
