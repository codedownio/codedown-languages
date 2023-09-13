{ callPackage
, bashInteractive
}:


let
  common = callPackage ../common.nix {};

in

common.wrapShell {
  executableName = "bash";
  baseDerivation = bashInteractive;
  displayName = "Bash " + bashInteractive.version;
  attr = "bash";
  icon = ../default_icon_64x64.png;
}
