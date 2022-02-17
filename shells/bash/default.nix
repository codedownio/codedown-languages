{ callPackage
, bashInteractive
}:


let
  common = callPackage ../common.nix {};

in

common.wrapShell "bash" bashInteractive ("Bash " + bashInteractive.version) ../default_icon.png
