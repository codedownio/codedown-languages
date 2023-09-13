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

common.wrapShell {
  executableName = "zsh-with-theme";
  inherit baseDerivation;
  displayName = "ZSH " + baseDerivation.version;
  attr = "zsh";
  icon = ../default_icon_64x64.png;
}
