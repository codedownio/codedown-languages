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

let
  ihaskell = callPackage ./ihaskell.nix { inherit compiler; };

in

jupyter-kernel.create {
  definitions = {
    haskell = {
      inherit displayName;
      argv = [
        "${ihaskell}/bin/ihaskell"
        "kernel"
        "{connection_file}"
        "--stack"
        "+RTS" "-M3g" "-N2" "-RTS"
      ];
      language = lib.head attrs;
      logo32 = null;
      logo64 = ./logo-64x64.svg;
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
    };
  };
}
