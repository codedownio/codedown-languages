{ lib
, jupyter-kernel
, snapshot
, attrs
, extensions
, displayName
, ihaskell
, ghc
, callPackage
, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

  repls = [{
    display_name = "GHCi";
    proc = "${ghc.out}/bin/ghci";
  }];

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
      inherit repls;
      metadata = {
        codedown = {
          inherit attrs extensions repls;
          priority = 1;
        };
      };
    };
  }]
)
