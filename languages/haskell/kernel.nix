{ lib
, jupyter-kernel
, snapshot
, attrs
, extensions
, displayName
, compiler
, callPackage
, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

  ihaskell = callPackage ./ihaskell.nix { inherit compiler; };

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      argv = [
        "${ihaskell}/bin/ihaskell"
        "kernel"
        "{connection_file}"
        "--stack"
        "+RTS" "-M3g" "-N2" "-RTS"
      ];
      language = head attrs;
      logo32 = ./haskell-logo-32x32.png;
      logo64 = ./haskell-logo-64x64.png;
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
    };
  }]
)
